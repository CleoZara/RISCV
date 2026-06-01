package riscv

import chisel3._
import chisel3.util._
import icache.ICacheTop
import parameterized_cache.MemBusIO

// ============================================================
//  FetchStage：IF 级取指模块（iCache.scala）
//
//  修复点（相比原版）：
//   1. 不再是接口占位，改为完整的 IF 级实现
//   2. 内部例化 icache.ICacheTop（真正的 I-Cache 本体）
//   3. 内部例化 NextLinePrefetcher，并连接至 ICacheTop pfReq 端口
//   4. PC 复位 0x80000000，顺序步长 N*4（PcGen 负责，此处接收）
//   5. icache.io.valid 驱动策略：
//        io.valid = !flush（flush 时不发取指请求）
//        stall 期间 flush=0 故 valid=1，ICacheTop 继续服务
//        同一 PC 在 stall 期间会命中（refill 完成后），
//        不会形成组合环（见注释）
//   6. 输出 IF/ID 流水线寄存器内容（Vec(N, IFIDSlot)）
//
//  信号流概览：
//
//   PcGen.pcFetch ─────────────────────────► ICacheTop.addr
//   PcGen.currPc  ─────────────────────────► NextLinePrefetcher.currAddr
//
//   ICacheTop.respValid ────────────────────► NextLinePrefetcher.cacheHit
//   ICacheTop.missOut   ────────────────────► NextLinePrefetcher.cacheStall
//                                           ► io.icacheStall
//
//   NextLinePrefetcher.pfReqValid ──────────► ICacheTop.pfReqValid
//   NextLinePrefetcher.pfReqAddr  ──────────► ICacheTop.pfReqAddr
//   ICacheTop.pfReqReady ───────────────────► NextLinePrefetcher.pfReqReady
//
//   ICacheTop.insts/instValids ─────────────► io.ifidSlots[i].inst / ctrl.valid
//
//  组合环分析：
//   io.icacheStall (output) = ICacheTop.missOut
//                           = IHitTest.missValid || FSM.stall
//   HazardUnit 以 icacheStall 为输入，输出 stallIF（注册时序）
//   stallIF 送回 PcGen（不送回此模块 valid 端口）
//   ∴ 本模块内无组合环 ✓
// ============================================================
class FetchStage(val N: Int = 2) extends Module {

  // 复用 ICacheTop 伴生对象中的默认参数
  private val p = ICacheTop.defaultParams

  val io = IO(new Bundle {
    // ── 来自 PcGen ────────────────────────────────────────
    val pcFetch = Input(UInt(32.W))   // 本周期取指地址
    val currPc  = Input(UInt(32.W))   // 当前 PC（供预取器计算下一行）

    // ── 流水线控制 ────────────────────────────────────────
    // flushIf：来自 HazardUnit.flushIF（EX/ID redirect 触发）
    val flushIf = Input(Bool())
    // stallIf：来自 HazardUnit.stallIF；此处仅用于 valid 门控外的
    //          PLRU 保护：stall 时不希望 ICacheTop 做多余更新，
    //          通过 valid 的语义已部分覆盖
    // （注：stallIf 不在此模块使用，由 PcGen 保持 PC 实现停顿）

    // ── CSR 控制 ──────────────────────────────────────────
    val prefetchEn = Input(Bool())   // CSR prefetch_ctrl[0]

    // ── 输出到 IF/ID 流水线寄存器 ─────────────────────────
    // 上层 Core 根据 icacheStall / flushIf 决定是否锁存
    val ifidSlots = Output(Vec(N, new IFIDSlot))

    // ── 反馈给 HazardUnit ─────────────────────────────────
    val icacheStall = Output(Bool())  // I-Cache miss → 全流水线 stall

    // ── 外部内存总线 ──────────────────────────────────────
    val mem = new MemBusIO(p)
  })

  // ── 子模块例化 ────────────────────────────────────────────
  val icache     = Module(new ICacheTop(p))
  val prefetcher = Module(new NextLinePrefetcher)

  // ── ICacheTop 连线 ────────────────────────────────────────
  // valid：flush 时停止取指；stall 期间继续（同 PC 命中，
  //        不会触发新 miss；若 miss 正在处理中，FSM 自行继续）
  icache.io.addr  := io.pcFetch
  icache.io.valid := !io.flushIf
  icache.io.flush := io.flushIf

  // ── NextLinePrefetcher 连线 ───────────────────────────────
  prefetcher.io.currAddr   := io.currPc
  prefetcher.io.cacheHit   := icache.io.respValid
  prefetcher.io.cacheStall := icache.io.missOut
  prefetcher.io.prefetchEn := io.prefetchEn

  // 预取握手：预取器 → ICacheTop（pfReq）
  icache.io.pfReqValid     := prefetcher.io.pfReqValid
  icache.io.pfReqAddr      := prefetcher.io.pfReqAddr
  prefetcher.io.pfReqReady := icache.io.pfReqReady

  // ── 内存总线直连 ──────────────────────────────────────────
  io.mem <> icache.io.mem

  // ── 构造 IF/ID 流水线寄存器内容 ───────────────────────────
  // slot i 的 PC = pcFetch + i*4（双发射：slot0=PC，slot1=PC+4）
  for (i <- 0 until N) {
    io.ifidSlots(i).pc   := io.pcFetch + (i * 4).U
    io.ifidSlots(i).inst := icache.io.insts(i)
    // ctrl.valid：仅在 ICacheTop 整体命中 AND 本槽有效时为真
    io.ifidSlots(i).ctrl.valid   := icache.io.instValids(i)
    // ctrl.kill：flush 时本槽作废
    io.ifidSlots(i).ctrl.kill    := io.flushIf
    // ctrl.allowIn：固定 true（上游 FetchStage 本身无背压逻辑，
    //               背压由 PcGen stall 实现）
    io.ifidSlots(i).ctrl.allowIn := true.B
  }

  // ── 反馈给 HazardUnit ─────────────────────────────────────
  io.icacheStall := icache.io.missOut
}
