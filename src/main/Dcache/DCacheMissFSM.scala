package parameterized_cache

import chisel3._
import chisel3.util._

class DCacheMissFSMIO(p: CacheParams) extends Bundle {
  val missValid   = Input(Bool())
  val missTag     = Input(UInt(p.TAG_W.W))
  val missIsStore = Input(Bool())

  val evictIdx    = Input(UInt(p.INDEX_W.W))
  val evictWay    = Input(UInt(p.WAY_W.W))
  val evictTag    = Input(UInt(p.TAG_W.W))
  val evictDirty  = Input(Bool())
  val evictLine   = Input(Vec(p.LINE_WORDS, UInt(p.DATA_WIDTH.W)))

  val mem         = new MemBusIO(p)

  val refillEn    = Output(Bool())
  val refillWay   = Output(UInt(p.WAY_W.W))
  val refillIdx   = Output(UInt(p.INDEX_W.W))
  val refillWord  = Output(UInt(p.WORD_CNT_W.W))
  val refillData  = Output(UInt(p.DATA_WIDTH.W))
  val refillTag   = Output(UInt(p.TAG_W.W))
  val refillDone  = Output(Bool())
  val refillIsStore = Output(Bool())

  val stall       = Output(Bool())
}

class DCacheMissFSM(p: CacheParams) extends Module {
  val io = IO(new DCacheMissFSMIO(p))

  val sIdle :: sCheck :: sWbReq :: sWbResp :: sRefillReq :: sRefillResp :: sDone :: Nil = Enum(7)
  val state = RegInit(sIdle)
  val nextState = WireDefault(state)

  val missTag   = Reg(UInt(p.TAG_W.W))
  val evictTag  = Reg(UInt(p.TAG_W.W))
  val idx       = Reg(UInt(p.INDEX_W.W))
  val way       = Reg(UInt(p.WAY_W.W))
  val dirty     = Reg(Bool())
  val isStore   = Reg(Bool())
  val wordCnt   = Reg(UInt(p.WORD_CNT_W.W))
  val lineBuf   = Reg(Vec(p.LINE_WORDS, UInt(p.DATA_WIDTH.W)))

  val lastWord = wordCnt === (p.LINE_WORDS - 1).U

  switch(state) {
    is(sIdle)       { when(io.missValid) { nextState := sCheck } }
    is(sCheck)      { nextState := Mux(dirty, sWbReq, sRefillReq) }
    is(sWbReq)      { when(io.mem.req.fire) { nextState := sWbResp } }
    is(sWbResp)     { when(io.mem.resp.fire) { nextState := Mux(lastWord, sRefillReq, sWbReq) } }
    is(sRefillReq)  { when(io.mem.req.fire) { nextState := sRefillResp } }
    is(sRefillResp) { when(io.mem.resp.fire) { nextState := Mux(lastWord, sDone, sRefillReq) } }
    is(sDone)       { nextState := sIdle }
  }
  state := nextState

  when(state === sIdle && io.missValid) {
    missTag  := io.missTag
    evictTag := io.evictTag
    idx      := io.evictIdx
    way      := io.evictWay
    dirty    := io.evictDirty
    isStore  := io.missIsStore
    wordCnt  := 0.U
    lineBuf  := io.evictLine
  }.elsewhen((state === sWbResp || state === sRefillResp) && io.mem.resp.fire) {
    wordCnt := Mux(lastWord, 0.U, wordCnt + 1.U)
  }

  val wbAddr     = Cat(evictTag, idx, wordCnt, 0.U((p.OFFSET_W - p.WORD_CNT_W).W))
  val refillAddr = Cat(missTag, idx, wordCnt, 0.U((p.OFFSET_W - p.WORD_CNT_W).W))

  val isWb       = state === sWbReq
  val isRefill   = state === sRefillReq
  io.mem.req.valid      := isWb || isRefill
  io.mem.req.bits.addr  := Mux(isWb, wbAddr, refillAddr)
  io.mem.req.bits.wdata := Mux(isWb, lineBuf(wordCnt), 0.U)
  io.mem.req.bits.wen   := isWb
  io.mem.req.bits.wmask := Mux(isWb, ~0.U(p.WMASK_BITS.W), 0.U)
  io.mem.resp.ready     := state === sWbResp || state === sRefillResp

  io.refillEn     := state === sRefillResp && io.mem.resp.fire
  io.refillWay    := way
  io.refillIdx    := idx
  io.refillWord   := wordCnt
  io.refillData   := io.mem.resp.bits.rdata
  io.refillTag    := missTag
  io.refillDone   := state === sDone
  io.refillIsStore := isStore
  io.stall        := state =/= sIdle
}
