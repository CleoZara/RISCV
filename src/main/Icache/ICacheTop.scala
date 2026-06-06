package icache

import chisel3._
import chisel3.util._
import parameterized_cache.{CacheParams, ICachePerfEvents, MemBusIO}
import parameterized_cache.TreePLRU

// ============================================================
//  ICacheTop：I-Cache 顶层
//
//  子模块连线总览：
//
//   addr ─► addrTag ──────────────────────────► IHitTest.tag
//         ► addrIdx  ─► ITagArray.idx
//                     ► IDataArray.idx
//                     ► TreePLRU.idx（正常）
//         ► addrWoff ─► IDataArray.wordsoff
//                     ► InstSelect.wordsoff
//
//   ITagArray.tagData ───────────────────────► IHitTest.tagData
//   IHitTest.isHit ──────────────────────────► TreePLRU.updateEn（命中路径）
//   IHitTest.hitWay ─────────────────────────► TreePLRU.updateWay
//                                            ► InstSelect.hitWay
//   IHitTest.missValid ──────────────────────► ICacheMissFSM.missValid
//                                            ► io.missOut（OR FSM.stall）
//
//   ICacheMissFSM.refillEn   ───────────────► IDataArray.refillDataEn
//   ICacheMissFSM.refillDone ─────────────── ► ITagArray.refillTagEn（miss）
//   ICacheMissFSM.prefillDone ──────────────► ITagArray.refillTagEn（预取）
//   ICacheMissFSM.refillWay  ───────────────► IDataArray/ITagArray.refillWay
//                                            ► TreePLRU.updateWay（回填路径）
//   ICacheMissFSM.stall ─────────────────────► io.missOut（OR hitTest.missValid）
//
//   IDataArray.rawData0/1 ──────────────────► InstSelect.rawData0/1
//   InstSelect.inst0/1 ─────────────────────► io.insts(0/1)
//   InstSelect.slot1Valid ───────────────────► io.instValids(1) 门控
//
//  PLRU idx MUX 策略（见下方详细说明）：
//   - 正常取指：plru.idx = addrIdx
//   - FSM idle + 预取请求 + 无 miss：plru.idx = pfIdx
//     此时 plru.evictWay 即为 pfIdx 上的驱逐路，传给 FSM 作 pfEvictWay
// ============================================================
class ICacheTop(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    // ── CPU / IF 级接口 ────────────────────────────────────────
    val addr       = Input(UInt(p.ADDR_WIDTH.W))
    val valid      = Input(Bool())    // 取指请求有效
    val flush      = Input(Bool())    // 流水线 flush，清空所有 valid 位

    // 双槽取指响应
    val insts      = Output(Vec(2, UInt(p.DATA_WIDTH.W)))
    val instValids = Output(Vec(2, Bool()))
    val respValid  = Output(Bool())   // 命中且 bundle 整体有效
    val missOut    = Output(Bool())   // 未命中信号，高电平期间流水线应 stall

    // ── 预取接口（来自 NextLinePrefetcher）────────────────────
    // pfReqAddr 须为 64B 对齐地址（见 spec §6.6）
    val pfReqValid = Input(Bool())
    val pfReqReady = Output(Bool())
    val pfReqAddr  = Input(UInt(p.ADDR_WIDTH.W))

    // ── 外部内存总线 ──────────────────────────────────────────
    val mem = new MemBusIO(p)

    // ── 性能事件（单周期脉冲，仅用于统计）─────────────────────
    val perf = Output(new ICachePerfEvents)
  })

  // ── 地址分解 ──────────────────────────────────────────────────
  // 默认参数（8 KB, 4-way, 64B line）：
  //   offset[5:0]  → addr[5:0]
  //   wordsoff[5:2]→ addr[5:2]（4 bit，16 words/line）
  //   idx[10:6]    → addr[10:6]（5 bit，32 sets）
  //   tag[31:11]   → addr[31:11]（21 bit）
  val addrTag  = io.addr(p.ADDR_WIDTH - 1,          p.OFFSET_W + p.INDEX_W)
  val addrIdx  = io.addr(p.OFFSET_W + p.INDEX_W - 1, p.OFFSET_W)
  val addrWoff = io.addr(p.OFFSET_W - 1,             2)   // word offset = addr[5:2]

  // 预取地址分解（pfReqAddr 应已 64B 对齐，低 6 位为 0）
  val pfTag = io.pfReqAddr(p.ADDR_WIDTH - 1,          p.OFFSET_W + p.INDEX_W)
  val pfIdx = io.pfReqAddr(p.OFFSET_W + p.INDEX_W - 1, p.OFFSET_W)

  // ── 子模块例化 ────────────────────────────────────────────────
  val tagArray  = Module(new ITagArray(p))
  val dataArray = Module(new IDataArray(p))
  val plru      = Module(new TreePLRU(p))
  val hitTest   = Module(new IHitTest(p))
  val instSel   = Module(new InstSelect(p))
  val missFsm   = Module(new ICacheMissFSM(p))

  // ── PLRU idx MUX ──────────────────────────────────────────────
  // 当 FSM 空闲、有预取请求且当前无 miss 时，将 PLRU 指向 pfIdx
  // 以获取正确的预取驱逐路；否则指向 addrIdx 服务正常取指/更新
  val plruQueryForPf = missFsm.io.isIdle &&
                       io.pfReqValid &&
                       !hitTest.io.missValid
  plru.io.idx := Mux(plruQueryForPf, pfIdx, addrIdx)

  // ── IHitTest ──────────────────────────────────────────────────
  hitTest.io.tagData  := tagArray.io.tagData
  hitTest.io.tag      := addrTag
  hitTest.io.reqValid := io.valid

  val isHit  = hitTest.io.isHit
  val hitWay = hitTest.io.hitWay

  // ── ITagArray ─────────────────────────────────────────────────
  tagArray.io.flush       := io.flush
  tagArray.io.idx         := addrIdx
  // miss 和预取都通过同一组 refill 信号写入 TagArray（time-shared）
  tagArray.io.refillTagEn := missFsm.io.refillDone || missFsm.io.prefillDone
  tagArray.io.refillWay   := missFsm.io.refillWay
  tagArray.io.refillIdx   := missFsm.io.refillIdx
  tagArray.io.refillTag   := missFsm.io.refillTag

  // ── TreePLRU ──────────────────────────────────────────────────
  // 更新时机：
  //   1. 正常命中（plruQueryForPf=false，否则 idx 已切换到 pfIdx，
  //      此时不能按 addrIdx 更新 PLRU——概率极低，可接受小误差）
  //   2. miss 回填完成
  //   3. 预取回填完成（此时 plru.idx 应在 pfIdx；由于 sPrefillDone
  //      也是单拍脉冲，与 plruQueryForPf 逻辑不冲突）
  plru.io.updateEn  := (isHit && io.valid && !plruQueryForPf) ||
                        missFsm.io.refillDone  ||
                        missFsm.io.prefillDone
  plru.io.updateWay := Mux(
    missFsm.io.refillDone || missFsm.io.prefillDone,
    missFsm.io.refillWay,
    hitWay
  )

  // ── IDataArray ────────────────────────────────────────────────
  dataArray.io.idx          := addrIdx
  dataArray.io.wordsoff     := addrWoff
  dataArray.io.refillDataEn := missFsm.io.refillEn
  dataArray.io.refillWay    := missFsm.io.refillWay
  dataArray.io.refillIdx    := missFsm.io.refillIdx
  dataArray.io.refillWord   := missFsm.io.refillWord
  dataArray.io.refillData   := missFsm.io.refillData

  // ── InstSelect ────────────────────────────────────────────────
  instSel.io.hitWay   := hitWay
  instSel.io.wordsoff := addrWoff
  instSel.io.rawData0 := dataArray.io.rawData0
  instSel.io.rawData1 := dataArray.io.rawData1

  // ── ICacheMissFSM ─────────────────────────────────────────────
  missFsm.io.missValid := hitTest.io.missValid
  missFsm.io.missTag   := addrTag
  missFsm.io.missIdx   := addrIdx
  // evictWay：plruQueryForPf=false 时 plru.io.evictWay 对应 addrIdx，正确
  missFsm.io.evictWay  := plru.io.evictWay

  // 预取接口：屏蔽掉与 miss 同周期的预取请求（miss 优先级更高）
  missFsm.io.pfReqValid := io.pfReqValid && !hitTest.io.missValid
  missFsm.io.pfReqTag   := pfTag
  missFsm.io.pfReqIdx   := pfIdx
  // pfEvictWay：plruQueryForPf=true 时 plru.io.evictWay 对应 pfIdx，正确
  missFsm.io.pfEvictWay := plru.io.evictWay

  io.pfReqReady := missFsm.io.pfReqReady

  // 内存总线直连
  io.mem <> missFsm.io.mem

  // ── 预取 useful 统计标记 ─────────────────────────────────────
  val prefetched = RegInit(VecInit(Seq.fill(p.SET_NUM)(
    VecInit(Seq.fill(p.WAY_NUM)(false.B))
  )))
  val demandLookup = io.valid && !io.flush
  val demandHit = demandLookup && isHit
  val demandMiss = demandLookup && hitTest.io.missValid && missFsm.io.isIdle
  val prefetchUseful = demandHit && prefetched(addrIdx)(hitWay)

  when(io.flush) {
    for (s <- 0 until p.SET_NUM) {
      for (w <- 0 until p.WAY_NUM) {
        prefetched(s)(w) := false.B
      }
    }
  }.otherwise {
    when(prefetchUseful) {
      prefetched(addrIdx)(hitWay) := false.B
    }
    when(missFsm.io.refillDone) {
      prefetched(missFsm.io.refillIdx)(missFsm.io.refillWay) := false.B
    }
    when(missFsm.io.prefillDone) {
      prefetched(missFsm.io.refillIdx)(missFsm.io.refillWay) := true.B
    }
  }

  // ── 对外输出 ──────────────────────────────────────────────────
  io.insts(0)      := instSel.io.inst0
  io.insts(1)      := instSel.io.inst1
  // slot0 有效：命中且本拍取指请求有效
  io.instValids(0) := isHit && io.valid
  // slot1 有效：在 slot0 有效的基础上，还须未到行尾
  io.instValids(1) := isHit && io.valid && instSel.io.slot1Valid
  io.respValid     := isHit && io.valid

  // missOut 保持高电平直至回填完成：
  //   - hitTest.io.missValid：本周期新检测到 miss（FSM 还未启动）
  //   - missFsm.io.stall：FSM 正在处理 miss（sRefillReq/Resp/sDone）
  // 两者 OR 覆盖 miss 的完整生命周期
  io.missOut := hitTest.io.missValid || missFsm.io.stall

  io.perf.access           := demandHit || demandMiss
  io.perf.hit              := demandHit
  io.perf.miss             := demandMiss
  io.perf.demandRefill     := missFsm.io.refillDone
  io.perf.prefetchReq      := io.pfReqValid
  io.perf.prefetchAccepted := io.pfReqValid && missFsm.io.pfReqReady
  io.perf.prefetchDropped  := io.pfReqValid && !missFsm.io.pfReqReady
  io.perf.prefetchRefill   := missFsm.io.prefillDone
  io.perf.prefetchUseful   := prefetchUseful
}

// ============================================================
//  伴生对象：提供默认参数实例，方便顶层例化
// ============================================================
object ICacheTop {
  // 默认：8 KB，4-way，64 B line，32-bit 数据，32-bit 地址
  val defaultParams = CacheParams(
    ADDR_WIDTH = 32,
    DATA_WIDTH = 32,
    CACHE_SIZE = 8 * 1024,
    WAY_NUM    = 4,
    LINE_BYTES = 64
  )
}
