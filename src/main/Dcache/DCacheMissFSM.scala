package parameterized_cache

import chisel3._
import chisel3.util._

class DCacheMissFSMIO(p: CacheParams) extends Bundle {
  val missValid   = Input(Bool())
  val missTag     = Input(UInt(p.TAG_W.W))
  val missIsStore = Input(Bool())
  val missIsPrefetch = Input(Bool())
  val missWordOff = Input(UInt(p.WORD_CNT_W.W))
  val missWdata   = Input(UInt(p.DATA_WIDTH.W))
  val missWmask   = Input(UInt(p.WMASK_BITS.W))

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
  val isIdle      = Output(Bool())
}

class DCacheMissFSM(p: CacheParams) extends Module {
  val io = IO(new DCacheMissFSMIO(p))

  val sIdle :: sCheck :: sWbReq :: sWbResp :: sRefillReq :: sRefillResp :: sRefillWrite :: sDone :: Nil = Enum(8)
  val state = RegInit(sIdle)
  val nextState = WireDefault(state)

  val missTag   = Reg(UInt(p.TAG_W.W))
  val evictTag  = Reg(UInt(p.TAG_W.W))
  val idx       = Reg(UInt(p.INDEX_W.W))
  val way       = Reg(UInt(p.WAY_W.W))
  val dirty     = Reg(Bool())
  val isStore   = Reg(Bool())
  val isPrefetch = Reg(Bool())
  val storeWordOff = Reg(UInt(p.WORD_CNT_W.W))
  val storeWdata   = Reg(UInt(p.DATA_WIDTH.W))
  val storeWmask   = Reg(UInt(p.WMASK_BITS.W))
  val wordCnt   = Reg(UInt(p.WORD_CNT_W.W))
  val lineBuf   = Reg(Vec(p.LINE_WORDS, UInt(p.DATA_WIDTH.W)))

  val lastWord = wordCnt === (p.LINE_WORDS - 1).U

  switch(state) {
    is(sIdle)       { when(io.missValid) { nextState := sCheck } }
    is(sCheck)      { nextState := Mux(dirty, sWbReq, sRefillReq) }
    is(sWbReq)      { when(io.mem.req.fire) { nextState := sWbResp } }
    is(sWbResp)     { when(io.mem.resp.fire) { nextState := sRefillReq } }
    is(sRefillReq)  { when(io.mem.req.fire) { nextState := sRefillResp } }
    is(sRefillResp) { when(io.mem.resp.fire) { nextState := sRefillWrite } }
    is(sRefillWrite) { when(lastWord) { nextState := sDone } }
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
    isPrefetch := io.missIsPrefetch
    storeWordOff := io.missWordOff
    storeWdata   := io.missWdata
    storeWmask   := io.missWmask
    wordCnt  := 0.U
    lineBuf  := io.evictLine
  }.elsewhen(state === sRefillResp && io.mem.resp.fire) {
    lineBuf := io.mem.resp.bits.rline
    wordCnt := 0.U
  }.elsewhen(state === sRefillWrite) {
    wordCnt := Mux(lastWord, 0.U, wordCnt + 1.U)
  }

  val wbAddr     = Cat(evictTag, idx, 0.U(p.OFFSET_W.W))
  val refillAddr = Cat(missTag, idx, 0.U(p.OFFSET_W.W))

  val isWb       = state === sWbReq
  val isRefill   = state === sRefillReq
  io.mem.req.valid      := isWb || isRefill
  io.mem.req.bits.addr  := Mux(isWb, wbAddr, refillAddr)
  io.mem.req.bits.wdata := 0.U
  io.mem.req.bits.wline := lineBuf
  io.mem.req.bits.wen   := isWb
  io.mem.req.bits.wmask := 0.U
  io.mem.req.bits.line  := true.B
  io.mem.resp.ready     := state === sWbResp || state === sRefillResp

  io.refillEn     := state === sRefillWrite
  io.refillWay    := way
  io.refillIdx    := idx
  io.refillWord   := wordCnt
  val storeByteMask = Cat((0 until p.WMASK_BITS).reverse.map(i => Fill(8, storeWmask(i))))
  val refillRawData = lineBuf(wordCnt)
  val refillStoreData = (storeWdata & storeByteMask) | (refillRawData & ~storeByteMask)
  io.refillData   := Mux(isStore && (wordCnt === storeWordOff), refillStoreData, refillRawData)
  io.refillTag    := missTag
  io.refillDone   := state === sDone
  io.refillIsStore := isStore
  io.stall        := state =/= sIdle && !isPrefetch
  io.isIdle       := state === sIdle
}
