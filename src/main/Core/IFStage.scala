package riscv

import chisel3._
import chisel3.util._
import icache.ICacheTop
import parameterized_cache.{CacheParams, ICachePerfEvents, MemBusIO}

class IFStage(p: CacheParams = CacheParams.default) extends Module {
  val issueWidth = 2

  val io = IO(new Bundle {
    val stallIf = Input(Bool())
    val flushIf = Input(Bool())

    val exRedirectValid = Input(Bool())
    val exRedirectPc    = Input(UInt(32.W))
    val idRedirectValid = Input(Bool())
    val idRedirectPc    = Input(UInt(32.W))

    val bpuQueryPc    = Output(UInt(32.W))
    val bpuPredTaken  = Input(Bool())
    val bpuPredTarget = Input(UInt(32.W))
    val rasPredValid  = Input(Bool())
    val rasPredTarget = Input(UInt(32.W))

    val nextLinePrefetchEn = Input(Bool())
    val stridePrefetchEn   = Input(Bool())
    val streamPrefetchEn   = Input(Bool())

    val out = Output(Vec(issueWidth, new IFIDSlot))
    val icacheStall = Output(Bool())
    val icachePerf = Output(new ICachePerfEvents)
    val imem = new MemBusIO(p)
    val debugPc = Output(UInt(32.W))
  })

  val pcGen = Module(new PcGen(N = issueWidth))
  val icache = Module(new ICacheTop(p))
  val nextLinePrefetcher = Module(new NextLinePrefetcher)
  val stridePrefetcher = Module(new StridePrefetcher)
  val streamPrefetcher = Module(new StreamPrefetcher)

  private def isJal(inst: UInt): Bool = inst(6, 0) === "b1101111".U
  private def isRet(inst: UInt): Bool = inst === "h00008067".U
  private def isBranch(inst: UInt): Bool = inst(6, 0) === "b1100011".U
  private def jalImm(inst: UInt): UInt = {
    Cat(Fill(11, inst(31)), inst(31), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W))
  }

  val slotPc0 = pcGen.io.pcFetch
  val slotPc1 = pcGen.io.pcFetch + 4.U
  val slot0Jal = icache.io.instValids(0) && isJal(icache.io.insts(0))
  val slot1Jal = icache.io.instValids(1) && isJal(icache.io.insts(1))
  val slot0Ret = icache.io.instValids(0) && isRet(icache.io.insts(0)) && io.rasPredValid
  val slot1Ret = icache.io.instValids(1) && isRet(icache.io.insts(1)) && io.rasPredValid
  val slot0Branch = icache.io.instValids(0) && isBranch(icache.io.insts(0))
  val bpuTaken = io.bpuPredTaken && slot0Branch
  val slot0JalTarget = slotPc0 + jalImm(icache.io.insts(0))
  val slot1JalTarget = slotPc1 + jalImm(icache.io.insts(1))
  val ifPredTaken = slot0Jal || slot0Ret || bpuTaken || slot1Jal || slot1Ret
  val ifPredTarget = Mux(slot0Jal, slot0JalTarget,
    Mux(slot0Ret, io.rasPredTarget,
      Mux(bpuTaken, io.bpuPredTarget,
        Mux(slot1Jal, slot1JalTarget, io.rasPredTarget))))

  pcGen.io.exRedirectValid := io.exRedirectValid
  pcGen.io.exRedirectPc    := io.exRedirectPc
  pcGen.io.idRedirectValid := io.idRedirectValid
  pcGen.io.idRedirectPc    := io.idRedirectPc
  pcGen.io.bpuPredTaken    := ifPredTaken
  pcGen.io.bpuPredTarget   := ifPredTarget
  pcGen.io.stallIf         := io.stallIf
  pcGen.io.fetchSlot1Valid := icache.io.instValids(1)

  io.bpuQueryPc := pcGen.io.pcFetch
  io.debugPc := pcGen.io.currPc

  icache.io.addr  := pcGen.io.pcFetch
  icache.io.valid := true.B
  icache.io.flush := false.B
  io.imem <> icache.io.mem

  val pfObserve = icache.io.respValid && !icache.io.missOut && !io.stallIf && !io.flushIf

  nextLinePrefetcher.io.currAddr   := pcGen.io.currPc
  nextLinePrefetcher.io.cacheHit   := icache.io.respValid
  nextLinePrefetcher.io.cacheStall := icache.io.missOut
  nextLinePrefetcher.io.prefetchEn := io.nextLinePrefetchEn

  stridePrefetcher.io.observeValid := pfObserve
  stridePrefetcher.io.observeAddr  := pcGen.io.currPc
  stridePrefetcher.io.prefetchEn   := io.stridePrefetchEn

  streamPrefetcher.io.observeValid := pfObserve
  streamPrefetcher.io.observeAddr  := pcGen.io.currPc
  streamPrefetcher.io.prefetchEn   := io.streamPrefetchEn

  val pfSelStream = streamPrefetcher.io.pfReqValid
  val pfSelStride = !pfSelStream && stridePrefetcher.io.pfReqValid
  icache.io.pfReqValid := pfSelStream || pfSelStride || nextLinePrefetcher.io.pfReqValid
  icache.io.pfReqAddr  := Mux(pfSelStream, streamPrefetcher.io.pfReqAddr,
    Mux(pfSelStride, stridePrefetcher.io.pfReqAddr, nextLinePrefetcher.io.pfReqAddr))

  streamPrefetcher.io.pfReqReady := icache.io.pfReqReady && pfSelStream
  stridePrefetcher.io.pfReqReady := icache.io.pfReqReady && pfSelStride
  nextLinePrefetcher.io.pfReqReady := icache.io.pfReqReady && !pfSelStream && !pfSelStride

  val seqStep = Mux(icache.io.instValids(1), (issueWidth * 4).U(32.W), 4.U(32.W))
  val seqNextPc = pcGen.io.pcFetch + seqStep
  for (i <- 0 until issueWidth) {
    val slotPc = pcGen.io.pcFetch + (i * 4).U
    val slotJal = if (i == 0) slot0Jal else slot1Jal
    val slotRet = if (i == 0) slot0Ret else slot1Ret
    val slotJalTarget = if (i == 0) slot0JalTarget else slot1JalTarget
    val slotPredNextPc = Mux(slotJal, slotJalTarget,
      Mux(slotRet, io.rasPredTarget,
      Mux(bpuTaken, io.bpuPredTarget, slotPc + 4.U))
    )

    io.out(i) := 0.U.asTypeOf(new IFIDSlot)
    io.out(i).pc := slotPc
    io.out(i).inst := icache.io.insts(i)
    io.out(i).slotIdx := i.U
    io.out(i).fetchPc := pcGen.io.pcFetch
    io.out(i).predTaken := slotJal || slotRet || bpuTaken
    io.out(i).predTarget := Mux(slotJal, slotJalTarget,
      Mux(slotRet, io.rasPredTarget, io.bpuPredTarget))
    io.out(i).predNextPc := slotPredNextPc
    io.out(i).seqNextPc := seqNextPc
    io.out(i).rasPred := slotRet
    io.out(i).icacheHit := icache.io.respValid
    io.out(i).ctrl.valid := icache.io.instValids(i) && !io.flushIf
    io.out(i).ctrl.kill := io.flushIf
    io.out(i).ctrl.allowIn := true.B
  }

  io.icacheStall := icache.io.missOut
  io.icachePerf := icache.io.perf
}
