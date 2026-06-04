package riscv

import chisel3._
import chisel3.util._
import parameterized_cache.{CacheParams, MemBusIO}

class InOrderCore(enableRV32M: Boolean = false) extends Module {
  val issueWidth = 2
  private val p = CacheParams(32, 32, 8 * 1024, 4, 64)

  val io = IO(new Bundle {
    val imem      = new MemBusIO(p)
    val dmem      = new MemBusIO(p)
    val printChar = Output(Valid(UInt(8.W)))
    val success   = Output(Bool())
    val debugPc   = Output(UInt(32.W))
  })

  val ifStage  = Module(new IFStage)
  val idStage  = Module(new IDStage(enableRV32M))
  val exStage  = Module(new EXStage(enableRV32M))
  val memStage = Module(new MEMStage)
  val wbStage  = Module(new WBStage)
  val regFile  = Module(new RegFile(issueWidth))
  val csrFile  = Module(new CSRFile(32, issueWidth, enableRV32M))
  val bpu      = Module(new BPU)
  val bypass   = Module(new PipelineBypassUnit(issueWidth))
  val hazard   = Module(new BypassHazardUnit(issueWidth))

  val ifidReg  = RegInit(VecInit(Seq.fill(issueWidth)(0.U.asTypeOf(new IFIDSlot))))
  val idexReg  = RegInit(VecInit(Seq.fill(issueWidth)(0.U.asTypeOf(new IDEXBundle))))
  val exmemReg = RegInit(VecInit(Seq.fill(issueWidth)(0.U.asTypeOf(new EXMEMBundle))))
  val memwbReg = RegInit(VecInit(Seq.fill(issueWidth)(0.U.asTypeOf(new MEMWBBundle))))

  io.imem     <> ifStage.io.imem
  io.dmem     <> memStage.io.dmem
  io.printChar := memStage.io.printChar
  io.debugPc  := ifStage.io.debugPc
  // P0 fix: wire real success signal from DCacheTop (via MEMStage)
  io.success  := memStage.io.success
  // ── BPU ───────────────────────────────────────────────────────────────
  bpu.io.queryPc      := ifStage.io.bpuQueryPc
  ifStage.io.bpuPredTaken  := bpu.io.predTaken
  ifStage.io.bpuPredTarget := bpu.io.predTarget
  bpu.io.updateValid  := exStage.io.bpuUpdateValid
  bpu.io.updatePc     := exStage.io.bpuUpdatePc
  bpu.io.updateTaken  := exStage.io.bpuUpdateTaken
  bpu.io.updateTarget := exStage.io.bpuUpdateTarget

  // ── IF Stage ──────────────────────────────────────────────────────────
  ifStage.io.exRedirectValid     := exStage.io.exRedirectValid
  ifStage.io.exRedirectPc        := exStage.io.exRedirectPc
  ifStage.io.idRedirectValid     := idStage.io.idRedirectValid
  ifStage.io.idRedirectPc        := idStage.io.idRedirectPc
  ifStage.io.flushIf             := hazard.io.flushIF
  ifStage.io.stallIf             := hazard.io.stallIF || idStage.io.holdIfId
  ifStage.io.nextLinePrefetchEn  := csrFile.io.prefetchCtrl(0)

  // ── ID Stage ──────────────────────────────────────────────────────────
  idStage.io.in         := ifidReg
  idStage.io.stallId    := hazard.io.stallID
  idStage.io.flushId    := hazard.io.flushID
  idStage.io.regRs1Data := regFile.io.rs1Data
  idStage.io.regRs2Data := regFile.io.rs2Data
  idStage.io.csrRdata   := csrFile.io.rdata

  val wbRegWen = Wire(Vec(issueWidth, Bool()))
  regFile.io.rs1Addr := idStage.io.regRs1Addr
  regFile.io.rs2Addr := idStage.io.regRs2Addr
  regFile.io.wen     := wbRegWen
  regFile.io.waddr   := wbStage.io.regWaddr
  regFile.io.wdata   := wbStage.io.regWdata

  // ── CSR File ──────────────────────────────────────────────────────────
  csrFile.io.raddr   := idStage.io.csrRaddr
  csrFile.io.opValid := exStage.io.csrOpValid
  csrFile.io.opType  := exStage.io.csrOpType
  csrFile.io.waddr   := exStage.io.csrWaddr
  csrFile.io.wdata   := exStage.io.csrWdata
  csrFile.io.cycleTick := true.B

  // P1 fix: minstret must count each instruction exactly once.
  // During any structural stall (D-/I-Cache miss), memwbReg is frozen, so
  // wbStage.io.instRetire stays high every stall cycle → over-counts.
  // Solution: only count in the *first* cycle of a stall (when the WB
  // instruction genuinely completes) and suppress all subsequent stall cycles.
  val prevStallWB    = RegNext(hazard.io.stallWB, false.B)
  val firstStallCycle = hazard.io.stallWB && !prevStallWB
  // retireEnable = true when pipeline is advancing OR it is the very first
  // cycle of a stall (the instruction that just "stopped" still retires once).
  val retireEnable = !hazard.io.stallWB || firstStallCycle
  wbRegWen := VecInit(wbStage.io.regWen.map(_ && retireEnable))
  csrFile.io.instRetire := VecInit(wbStage.io.instRetire.map(_ && retireEnable))

  // ── Bypass Network ────────────────────────────────────────────────────
  bypass.io.idex      := idexReg
  bypass.io.idexValid := VecInit(idexReg.map(x => x.ctrl.valid && !x.ctrl.kill))
  bypass.io.exResult  := exStage.io.exResult
  bypass.io.exmem     := exmemReg
  bypass.io.exmemValid := VecInit(exmemReg.map(x => x.ctrl.valid && !x.ctrl.kill))
  bypass.io.memwb     := memwbReg
  bypass.io.memwbValid := VecInit(memwbReg.map(x => x.ctrl.valid && !x.ctrl.kill))
  bypass.io.wbValid   := VecInit(wbStage.io.wbValid.map(_ && retireEnable))
  bypass.io.wbRfWen   := wbRegWen
  bypass.io.wbRdAddr  := wbStage.io.wbRdAddr
  bypass.io.wbData    := wbStage.io.wbData
  bypass.io.rs1Use    := VecInit(idexReg.map(_.rs1Use))
  bypass.io.rs2Use    := VecInit(idexReg.map(_.rs2Use))

  // ── EX Stage ──────────────────────────────────────────────────────────
  exStage.io.in        := idexReg
  exStage.io.stallEx   := hazard.io.stallEX
  exStage.io.flushEx   := false.B
  exStage.io.rs1Data   := bypass.io.rs1Data
  exStage.io.rs2Data   := bypass.io.rs2Data
  exStage.io.op1Data   := bypass.io.op1Data
  exStage.io.op2Data   := bypass.io.op2Data
  exStage.io.storeData := bypass.io.storeData
  exStage.io.csrOldData := csrFile.io.oldData

  // ── MEM Stage ─────────────────────────────────────────────────────────
  memStage.io.in         := exmemReg
  memStage.io.flushMem   := false.B
  memStage.io.mtimeLo    := csrFile.io.mtimeLo
  memStage.io.mtimeHi    := csrFile.io.mtimeHi
  memStage.io.dcacheFlush := false.B

  // ── WB Stage ──────────────────────────────────────────────────────────
  wbStage.io.in := memwbReg

  // ── Hazard Unit ───────────────────────────────────────────────────────
  hazard.io.idValid   := idStage.io.hazardIdValid
  hazard.io.idRs1Addr := idStage.io.hazardRs1Addr
  hazard.io.idRs2Addr := idStage.io.hazardRs2Addr
  hazard.io.idRs1Use  := idStage.io.hazardRs1Use
  hazard.io.idRs2Use  := idStage.io.hazardRs2Use
  hazard.io.idRdAddr  := idStage.io.hazardRdAddr
  hazard.io.idRfWen   := idStage.io.hazardRfWen
  hazard.io.idCanBypassToYounger := idStage.io.hazardCanBypassToYounger
  hazard.io.exValid   := VecInit(idexReg.map(x => x.ctrl.valid && !x.ctrl.kill))
  hazard.io.exMemRen  := VecInit(idexReg.map(_.memRen))
  hazard.io.exRdAddr  := VecInit(idexReg.map(_.rdAddr))
  hazard.io.exRfWen   := VecInit(idexReg.map(_.rfWen))
  hazard.io.memValid  := VecInit(exmemReg.map(x => x.ctrl.valid && !x.ctrl.kill))
  hazard.io.memMemRen := VecInit(exmemReg.map(_.memRen))
  hazard.io.memRdAddr := VecInit(exmemReg.map(_.rdAddr))
  hazard.io.memRfWen  := VecInit(exmemReg.map(_.rfWen))
  hazard.io.pendingWaw := VecInit(Seq.fill(4)(0.U.asTypeOf(new PendingRegWriteInfo)))
  // brTaken is merged into exRedirect (branch mispred → exRedirectValid).
  hazard.io.brTaken     := false.B
  hazard.io.exRedirect  := exStage.io.exRedirectValid
  hazard.io.idRedirect  := idStage.io.idRedirectValid
  hazard.io.icacheStall := ifStage.io.icacheStall
  hazard.io.dcacheStall := memStage.io.dcacheStall
  hazard.io.backendStall := false.B

  // ── Pipeline register update logic ────────────────────────────────────
  when(hazard.io.flushIF) {
    ifidReg := VecInit(Seq.fill(issueWidth)(0.U.asTypeOf(new IFIDSlot)))
  }.elsewhen(!(hazard.io.stallIF || idStage.io.holdIfId)) {
    ifidReg := ifStage.io.out
  }

  when(hazard.io.flushID || hazard.io.flushEX) {
    idexReg := VecInit(Seq.fill(issueWidth)(0.U.asTypeOf(new IDEXBundle)))
  }.elsewhen(!hazard.io.stallID) {
    idexReg := idStage.io.out
  }

  when(!hazard.io.stallEX) {
    exmemReg := exStage.io.out
  }

  when(!hazard.io.stallMEM) {
    memwbReg := memStage.io.out
  }
}
