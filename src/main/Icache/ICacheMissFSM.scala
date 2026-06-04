package icache

import chisel3._
import chisel3.util._
import parameterized_cache.{CacheParams, MemBusIO}

// ============================================================
//  ICacheMissFSM：I-Cache 缺失与预取状态机
//
//  与 DCacheMissFSM 的核心区别：
//   - 无脏行写回：去掉 sCheck / sWbReq / sWbResp 三个状态
//     Miss → 直接回填，无需检查 dirty / evict 旧行内容
//   - 支持后台预取（sPrefill / sPrefillResp / sPrefillDone）：
//     仅在 sIdle 且无 miss 时处理；预取期间 stall=0，流水线
//     继续推进（命中原来已缓存的行）
//   - stall 仅在处理真正 miss（sRefillReq/Resp/sDone）时置高
//
//  状态转换：
//
//   sIdle ─[missValid]──────────────► sRefillReq
//     │                                   │ req.fire
//     │                               sRefillResp
//     │                                   │ resp.fire
//     │                     lastWord? ────┤
//     │                  No ↙       Yes ↘
//     │             sRefillReq        sDone ──► sIdle（refillDone=1）
//     │
//     └─[pfReqValid & !missValid]──► sPrefill
//                                        │ req.fire
//                                    sPrefillResp
//                                        │ resp.fire
//                          lastWord? ────┤
//                       No ↙       Yes ↘
//                   sPrefill       sPrefillDone ──► sIdle（prefillDone=1）
// ============================================================
class ICacheMissFSM(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    // ── Miss 输入（来自 ICacheTop，优先级高于预取）──────────────
    val missValid = Input(Bool())
    val missTag   = Input(UInt(p.TAG_W.W))
    val missIdx   = Input(UInt(p.INDEX_W.W))
    val evictWay  = Input(UInt(p.WAY_W.W))   // PLRU 在 missIdx 上的驱逐路

    // ── 预取输入（低优先级，仅 sIdle 且无 miss 时接受）──────────
    val pfReqValid  = Input(Bool())
    val pfReqReady  = Output(Bool())
    val pfReqTag    = Input(UInt(p.TAG_W.W))
    val pfReqIdx    = Input(UInt(p.INDEX_W.W))
    val pfEvictWay  = Input(UInt(p.WAY_W.W)) // PLRU 在 pfIdx 上的驱逐路

    // ── 外部内存总线 ──────────────────────────────────────────
    val mem = new MemBusIO(p)

    // ── 回填输出 → ICacheTop → ITagArray / IDataArray ─────────
    val refillEn    = Output(Bool())            // 当拍 refillData 有效，写 DataArray
    val refillWay   = Output(UInt(p.WAY_W.W))
    val refillIdx   = Output(UInt(p.INDEX_W.W))
    val refillWord  = Output(UInt(p.WORD_CNT_W.W))
    val refillData  = Output(UInt(p.DATA_WIDTH.W))
    val refillTag   = Output(UInt(p.TAG_W.W))
    val refillDone  = Output(Bool())            // miss 回填完成，TagArray 写入
    val prefillDone = Output(Bool())            // 预取回填完成，TagArray 写入

    // ── 状态输出 ──────────────────────────────────────────────
    val stall  = Output(Bool())  // 仅 miss 流程置高，预取不 stall
    val isIdle = Output(Bool())  // 供 ICacheTop 做 PLRU idx MUX
  })

  // ── 状态定义 ─────────────────────────────────────────────────
  val sIdle :: sRefillReq :: sRefillResp :: sRefillWrite :: sDone :: sPrefill :: sPrefillResp :: sPrefillWrite :: sPrefillDone :: Nil = Enum(9)

  val state     = RegInit(sIdle)
  val nextState = WireDefault(state)

  // ── 锁存字段 ──────────────────────────────────────────────────
  val wTag    = Reg(UInt(p.TAG_W.W))
  val wIdx    = Reg(UInt(p.INDEX_W.W))
  val wWay    = Reg(UInt(p.WAY_W.W))
  val wordCnt = Reg(UInt(p.WORD_CNT_W.W))
  val lineBuf = Reg(Vec(p.LINE_WORDS, UInt(p.DATA_WIDTH.W)))

  val lastWord = wordCnt === (p.LINE_WORDS - 1).U

  // ── 状态转换 ──────────────────────────────────────────────────
  switch(state) {
    is(sIdle) {
      when(io.missValid) {
        nextState := sRefillReq
      }.elsewhen(io.pfReqValid) {
        nextState := sPrefill
      }
    }
    is(sRefillReq) {
      when(io.mem.req.fire) { nextState := sRefillResp }
    }
    is(sRefillResp) {
      when(io.mem.resp.fire) { nextState := sRefillWrite }
    }
    is(sRefillWrite) {
      when(lastWord) { nextState := sDone }
    }
    is(sDone) { nextState := sIdle }

    is(sPrefill) {
      when(io.mem.req.fire) { nextState := sPrefillResp }
    }
    is(sPrefillResp) {
      when(io.mem.resp.fire) { nextState := sPrefillWrite }
    }
    is(sPrefillWrite) {
      when(lastWord) { nextState := sPrefillDone }
    }
    is(sPrefillDone) { nextState := sIdle }
  }
  state := nextState

  // ── 锁存逻辑（sIdle 出口时捕获） ──────────────────────────────
  when(state === sIdle && io.missValid) {
    wTag    := io.missTag
    wIdx    := io.missIdx
    wWay    := io.evictWay
    wordCnt := 0.U
  }.elsewhen(state === sIdle && !io.missValid && io.pfReqValid) {
    wTag    := io.pfReqTag
    wIdx    := io.pfReqIdx
    wWay    := io.pfEvictWay
    wordCnt := 0.U
  }.elsewhen((state === sRefillResp || state === sPrefillResp) && io.mem.resp.fire) {
    lineBuf := io.mem.resp.bits.rline
    wordCnt := 0.U
  }.elsewhen(state === sRefillWrite || state === sPrefillWrite) {
    wordCnt := Mux(lastWord, 0.U, wordCnt + 1.U)
  }

  // ── 内存总线地址：{tag, idx, wordCnt, 2'b00} ──────────────────
  // byteOffW = OFFSET_W - WORD_CNT_W = 6 - 4 = 2（32 位字内字节偏移宽度）
  val fetchAddr = Cat(wTag, wIdx, 0.U(p.OFFSET_W.W))

  val isRequesting = state === sRefillReq  || state === sPrefill
  val isWaiting    = state === sRefillResp || state === sPrefillResp

  io.mem.req.valid      := isRequesting
  io.mem.req.bits.addr  := fetchAddr
  io.mem.req.bits.wdata := 0.U
  io.mem.req.bits.wline := 0.U.asTypeOf(Vec(p.LINE_WORDS, UInt(p.DATA_WIDTH.W)))
  io.mem.req.bits.wen   := false.B
  io.mem.req.bits.wmask := 0.U
  io.mem.req.bits.line  := true.B
  io.mem.resp.ready     := isWaiting

  // ── 回填输出 ──────────────────────────────────────────────────
  io.refillEn    := state === sRefillWrite || state === sPrefillWrite
  io.refillWay   := wWay
  io.refillIdx   := wIdx
  io.refillWord  := wordCnt
  io.refillData  := lineBuf(wordCnt)
  io.refillTag   := wTag
  io.refillDone  := state === sDone
  io.prefillDone := state === sPrefillDone

  // ── 状态输出 ──────────────────────────────────────────────────
  // stall 仅在处理真正 miss 的三个状态内置高；预取不影响流水线
  io.stall  := state === sRefillReq || state === sRefillResp || state === sRefillWrite || state === sDone
  io.isIdle := state === sIdle

  // 预取握手：仅在 sIdle 且无 miss 时接受
  io.pfReqReady := (state === sIdle) && !io.missValid
}
