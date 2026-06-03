package icache

import chisel3._
import chisel3.util._
import parameterized_cache.{CacheParams, MemBusIO}

class ICacheMissFSM(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val missValid = Input(Bool())
    val missTag   = Input(UInt(p.TAG_W.W))
    val missIdx   = Input(UInt(p.INDEX_W.W))
    val evictWay  = Input(UInt(p.WAY_W.W))

    val pfReqValid = Input(Bool())
    val pfReqReady = Output(Bool())
    val pfReqTag   = Input(UInt(p.TAG_W.W))
    val pfReqIdx   = Input(UInt(p.INDEX_W.W))
    val pfEvictWay = Input(UInt(p.WAY_W.W))

    val mem = new MemBusIO(p)

    val refillEn    = Output(Bool())
    val refillWay   = Output(UInt(p.WAY_W.W))
    val refillIdx   = Output(UInt(p.INDEX_W.W))
    val refillWord  = Output(UInt(p.WORD_CNT_W.W))
    val refillData  = Output(UInt(p.DATA_WIDTH.W))
    val refillTag   = Output(UInt(p.TAG_W.W))
    val refillDone  = Output(Bool())
    val prefillDone = Output(Bool())

    val stall  = Output(Bool())
    val isIdle = Output(Bool())
  })

  val sIdle :: sRefillReq :: sRefillResp :: sDone :: sPrefill :: sPrefillResp :: sPrefillDone :: Nil = Enum(7)

  val state = RegInit(sIdle)
  val nextState = WireDefault(state)
  val wTag = Reg(UInt(p.TAG_W.W))
  val wIdx = Reg(UInt(p.INDEX_W.W))
  val wWay = Reg(UInt(p.WAY_W.W))
  val wordCnt = RegInit(0.U(p.WORD_CNT_W.W))
  val lastWord = wordCnt === (p.LINE_WORDS - 1).U

  switch(state) {
    is(sIdle) {
      when(io.missValid) {
        nextState := sRefillReq
      }.elsewhen(io.pfReqValid) {
        nextState := sPrefill
      }
    }
    is(sRefillReq)  { when(io.mem.req.fire)  { nextState := sRefillResp } }
    is(sRefillResp) { when(io.mem.resp.fire) { nextState := Mux(lastWord, sDone, sRefillReq) } }
    is(sDone)       { nextState := sIdle }
    is(sPrefill)    { when(io.mem.req.fire)  { nextState := sPrefillResp } }
    is(sPrefillResp) {
      when(io.mem.resp.fire) { nextState := Mux(lastWord, sPrefillDone, sPrefill) }
    }
    is(sPrefillDone) { nextState := sIdle }
  }
  state := nextState

  when(state === sIdle && io.missValid) {
    wTag := io.missTag
    wIdx := io.missIdx
    wWay := io.evictWay
    wordCnt := 0.U
  }.elsewhen(state === sIdle && !io.missValid && io.pfReqValid) {
    wTag := io.pfReqTag
    wIdx := io.pfReqIdx
    wWay := io.pfEvictWay
    wordCnt := 0.U
  }.elsewhen((state === sRefillResp || state === sPrefillResp) && io.mem.resp.fire) {
    wordCnt := Mux(lastWord, 0.U, wordCnt + 1.U)
  }

  val byteOffW = p.OFFSET_W - p.WORD_CNT_W
  val fetchAddr = Cat(wTag, wIdx, wordCnt, 0.U(byteOffW.W))
  val isRequesting = state === sRefillReq || state === sPrefill
  val isWaiting = state === sRefillResp || state === sPrefillResp

  io.mem.req.valid := isRequesting
  io.mem.req.bits.addr := fetchAddr
  io.mem.req.bits.wdata := 0.U
  io.mem.req.bits.wen := false.B
  io.mem.req.bits.wmask := 0.U
  io.mem.resp.ready := isWaiting

  io.refillEn := isWaiting && io.mem.resp.fire
  io.refillWay := wWay
  io.refillIdx := wIdx
  io.refillWord := wordCnt
  io.refillData := io.mem.resp.bits.rdata
  io.refillTag := wTag
  io.refillDone := state === sDone
  io.prefillDone := state === sPrefillDone
  io.stall := state === sRefillReq || state === sRefillResp || state === sDone
  io.isIdle := state === sIdle
  io.pfReqReady := state === sIdle && !io.missValid
}
