package riscv

import chisel3._
import chisel3.util._
import icache.ICacheTop
import parameterized_cache.{CacheParams, MemBusIO}

class IFStage extends Module {
  val issueWidth = 2
  private val p = ICacheTop.defaultParams

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

    val nextLinePrefetchEn = Input(Bool())

    val out = Output(Vec(issueWidth, new IFIDSlot))
    val icacheStall = Output(Bool())
    val imem = new MemBusIO(p)
    val debugPc = Output(UInt(32.W))
  })

  val pcGen = Module(new PcGen(N = issueWidth))
  val icache = Module(new ICacheTop(p))
  val prefetcher = Module(new NextLinePrefetcher)

  pcGen.io.exRedirectValid := io.exRedirectValid
  pcGen.io.exRedirectPc    := io.exRedirectPc
  pcGen.io.idRedirectValid := io.idRedirectValid
  pcGen.io.idRedirectPc    := io.idRedirectPc
  pcGen.io.bpuPredTaken    := io.bpuPredTaken
  pcGen.io.bpuPredTarget   := io.bpuPredTarget
  pcGen.io.stallIf         := io.stallIf

  io.bpuQueryPc := pcGen.io.pcFetch
  io.debugPc := pcGen.io.currPc

  icache.io.addr  := pcGen.io.pcFetch
  icache.io.valid := true.B
  icache.io.flush := false.B
  io.imem <> icache.io.mem

  prefetcher.io.currAddr   := pcGen.io.currPc
  prefetcher.io.cacheHit   := icache.io.respValid
  prefetcher.io.cacheStall := icache.io.missOut
  prefetcher.io.prefetchEn := io.nextLinePrefetchEn

  icache.io.pfReqValid := prefetcher.io.pfReqValid
  icache.io.pfReqAddr  := prefetcher.io.pfReqAddr
  prefetcher.io.pfReqReady := icache.io.pfReqReady

  val seqNextPc = pcGen.io.pcFetch + (issueWidth * 4).U
  for (i <- 0 until issueWidth) {
    val slotPc = pcGen.io.pcFetch + (i * 4).U
    val slotPredNextPc = Mux(io.bpuPredTaken, io.bpuPredTarget, slotPc + 4.U)

    io.out(i) := 0.U.asTypeOf(new IFIDSlot)
    io.out(i).pc := slotPc
    io.out(i).inst := icache.io.insts(i)
    io.out(i).slotIdx := i.U
    io.out(i).fetchPc := pcGen.io.pcFetch
    io.out(i).predTaken := io.bpuPredTaken
    io.out(i).predTarget := io.bpuPredTarget
    io.out(i).predNextPc := slotPredNextPc
    io.out(i).seqNextPc := seqNextPc
    io.out(i).icacheHit := icache.io.respValid
    io.out(i).ctrl.valid := icache.io.instValids(i) && !io.flushIf
    io.out(i).ctrl.kill := io.flushIf
    io.out(i).ctrl.allowIn := true.B
  }

  io.icacheStall := icache.io.missOut
}
