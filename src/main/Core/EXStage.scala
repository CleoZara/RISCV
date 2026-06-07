package riscv

import chisel3._
import chisel3.util._

class EXStage(enableRV32M: Boolean = false) extends Module {
  val issueWidth = 2

  val io = IO(new Bundle {
    val in        = Input(Vec(issueWidth, new IDEXBundle))
    val stallEx   = Input(Bool())
    val flushEx   = Input(Bool())

    val rs1Data   = Input(Vec(issueWidth, UInt(32.W)))
    val rs2Data   = Input(Vec(issueWidth, UInt(32.W)))
    val op1Data   = Input(Vec(issueWidth, UInt(32.W)))
    val op2Data   = Input(Vec(issueWidth, UInt(32.W)))
    val storeData = Input(Vec(issueWidth, UInt(32.W)))

    val csrOpValid = Output(Bool())
    val csrOpType  = Output(UInt(2.W))
    val csrWaddr   = Output(UInt(12.W))
    val csrWdata   = Output(UInt(32.W))
    val csrOldData = Input(UInt(32.W))

    val exRedirectValid = Output(Bool())
    val exRedirectPc    = Output(UInt(32.W))

    val branchInst       = Output(Vec(issueWidth, Bool()))
    val branchPred       = Output(Vec(issueWidth, Bool()))
    val branchCorrect    = Output(Vec(issueWidth, Bool()))
    val branchMispredict = Output(Vec(issueWidth, Bool()))
    val branchDirectionMispredict = Output(Vec(issueWidth, Bool()))
    val branchTargetMispredict    = Output(Vec(issueWidth, Bool()))
    val jalrInst         = Output(Vec(issueWidth, Bool()))
    val rasPred          = Output(Vec(issueWidth, Bool()))
    val rasCorrect       = Output(Vec(issueWidth, Bool()))

    val bpuUpdateValid  = Output(Bool())
    val bpuUpdatePc     = Output(UInt(32.W))
    val bpuUpdateTaken  = Output(Bool())
    val bpuUpdateTarget = Output(UInt(32.W))

    val exValid  = Output(Vec(issueWidth, Bool()))
    val exMemRen = Output(Vec(issueWidth, Bool()))
    val exRdAddr = Output(Vec(issueWidth, UInt(5.W)))
    val exRfWen  = Output(Vec(issueWidth, Bool()))
    val exResult = Output(Vec(issueWidth, UInt(32.W)))
    val mulDivStall = Output(Bool())

    val out = Output(Vec(issueWidth, new EXMEMBundle))
  })

  val alus         = Seq.fill(issueWidth)(Module(new ParamALU(32, enableRV32M)))
  val mulDivs      = Seq.fill(issueWidth)(Module(new MulDivALU(32)))
  val slotValid    = Wire(Vec(issueWidth, Bool()))
  val mulDivWaiting = Wire(Vec(issueWidth, Bool()))
  val branchTaken  = Wire(Vec(issueWidth, Bool()))
  val branchTarget = Wire(Vec(issueWidth, UInt(32.W)))  // PC + imm (B/J type)
  val jalrTarget   = Wire(Vec(issueWidth, UInt(32.W)))  // rs1 + imm, lsb cleared
  val actualNextPc = Wire(Vec(issueWidth, UInt(32.W)))
  val redirect     = Wire(Vec(issueWidth, Bool()))

  for (i <- 0 until issueWidth) {
    slotValid(i) := io.in(i).ctrl.valid && !io.in(i).ctrl.kill

    alus(i).io.op1   := io.op1Data(i)
    alus(i).io.op2   := io.op2Data(i)
    alus(i).io.aluOp := io.in(i).aluOp
    mulDivs(i).io.req.bits.op1   := io.op1Data(i)
    mulDivs(i).io.req.bits.op2   := io.op2Data(i)
    mulDivs(i).io.req.bits.aluOp := io.in(i).aluOp

    val isMulDivOp = enableRV32M.B && AluOp.isMulDiv(io.in(i).aluOp)
    val execResult = Mux(isMulDivOp, mulDivs(i).io.resp.bits, alus(i).io.result)

    branchTaken(i) := MuxLookup(io.in(i).brType, false.B, Seq(
      BrType.BR_EQ  -> (io.rs1Data(i) === io.rs2Data(i)),
      BrType.BR_NE  -> (io.rs1Data(i) =/= io.rs2Data(i)),
      BrType.BR_LT  -> (io.rs1Data(i).asSInt < io.rs2Data(i).asSInt),
      BrType.BR_GE  -> (io.rs1Data(i).asSInt >= io.rs2Data(i).asSInt),
      BrType.BR_LTU -> (io.rs1Data(i) < io.rs2Data(i)),
      BrType.BR_GEU -> (io.rs1Data(i) >= io.rs2Data(i))
    ))

    branchTarget(i) := io.in(i).pc + io.in(i).imm
    jalrTarget(i)   := (io.rs1Data(i) + io.in(i).imm) & "hfffffffe".U(32.W)

    val fallThrough = io.in(i).pc + 4.U
    val isBranch    = io.in(i).brType =/= BrType.BR_NONE
    val isControl   = isBranch || io.in(i).isJump || io.in(i).isJalr
    val forwardData = MuxLookup(io.in(i).wbSel, execResult, Seq(
      WbSel.WB_ALU -> execResult,
      WbSel.WB_PC4 -> fallThrough,
      WbSel.WB_CSR -> io.csrOldData,
      WbSel.WB_MEM -> execResult
    ))

    actualNextPc(i) := MuxCase(fallThrough, Seq(
      io.in(i).isJalr          -> jalrTarget(i),
      (isBranch && branchTaken(i)) -> branchTarget(i)
    ))
    val actualControlNextPc = MuxCase(fallThrough, Seq(
      io.in(i).isJalr -> jalrTarget(i),
      (io.in(i).isJump && !io.in(i).isJalr) -> branchTarget(i),
      (isBranch && branchTaken(i)) -> branchTarget(i)
    ))
    val predCorrect = actualControlNextPc === io.in(i).predNextPc
    val predRedirect = io.in(i).predNextPc =/= fallThrough
    val actualRedirect = actualControlNextPc =/= fallThrough
    val directionMiss = predRedirect =/= actualRedirect
    val targetMiss = !directionMiss && predRedirect && actualRedirect && !predCorrect

    val killedByOlderRedirect = if (i == 0) false.B else redirect(0)
    val slotLive = slotValid(i) && !killedByOlderRedirect
    val mulDivCancel = io.flushEx || killedByOlderRedirect
    mulDivs(i).io.cancel := mulDivCancel
    mulDivs(i).io.req.valid := slotLive && isMulDivOp && !io.flushEx
    mulDivs(i).io.resp.ready := !io.stallEx || mulDivCancel
    mulDivWaiting(i) := slotValid(i) && isMulDivOp && !io.flushEx && !mulDivs(i).io.resp.valid

    redirect(i) := slotLive &&
                   (isBranch || io.in(i).isJalr) &&
                   (actualNextPc(i) =/= io.in(i).predNextPc)

    io.branchInst(i)       := slotLive && isControl
    io.branchPred(i)       := slotLive && isControl
    io.branchCorrect(i)    := slotLive && isControl && predCorrect
    io.branchMispredict(i) := slotLive && isControl && !predCorrect
    io.branchDirectionMispredict(i) := slotLive && isControl && directionMiss
    io.branchTargetMispredict(i)    := slotLive && isControl && targetMiss
    io.jalrInst(i)         := slotLive && io.in(i).isJalr
    io.rasPred(i)          := slotLive && io.in(i).rasPred
    io.rasCorrect(i)       := slotLive && io.in(i).rasPred && predCorrect

    io.exValid(i)  := slotLive
    io.exMemRen(i) := slotLive && io.in(i).memRen
    io.exRdAddr(i) := io.in(i).rdAddr
    io.exRfWen(i)  := slotLive && io.in(i).rfWen
    io.exResult(i) := forwardData

    io.out(i) := 0.U.asTypeOf(new EXMEMBundle)
    io.out(i).pc        := io.in(i).pc
    io.out(i).inst      := io.in(i).inst
    io.out(i).slotIdx   := io.in(i).slotIdx
    io.out(i).aluOut    := execResult
    io.out(i).rs2Data   := io.storeData(i)
    io.out(i).rdAddr    := io.in(i).rdAddr
    io.out(i).wbSel     := io.in(i).wbSel
    io.out(i).rfWen     := slotLive && io.in(i).rfWen
    io.out(i).memRen    := slotLive && io.in(i).memRen
    io.out(i).memWen    := slotLive && io.in(i).memWen
    io.out(i).memWd     := io.in(i).memWd
    io.out(i).memSigned := io.in(i).memSigned
    io.out(i).csrRdata  := io.csrOldData
    io.out(i).csrOp     := Mux(slotLive, io.in(i).csrOp, CSROp.NONE)
    io.out(i).brType    := Mux(slotLive, io.in(i).brType, BrType.BR_NONE)
    io.out(i).isJump    := slotLive && io.in(i).isJump
    io.out(i).ctrl.valid   := slotLive
    io.out(i).ctrl.kill    := io.flushEx || io.in(i).ctrl.kill || killedByOlderRedirect
    io.out(i).ctrl.allowIn := true.B
  }
  io.mulDivStall := mulDivWaiting.asUInt.orR

  // ── CSR operation ─────────────────────────────────────────────────────
  val slot0Live = slotValid(0)
  val slot1Live = slotValid(1) && !redirect(0)

  val csrSlot0 = slot0Live && io.in(0).csrOp =/= CSROp.NONE
  val csrSlot1 = slot1Live && io.in(1).csrOp =/= CSROp.NONE
  val csrIdx   = Mux(csrSlot0, 0.U, 1.U)
  io.csrOpValid := csrSlot0 || csrSlot1
  io.csrOpType  := Mux(csrSlot0, io.in(0).csrOp,  io.in(1).csrOp)
  io.csrWaddr   := Mux(csrSlot0, io.in(0).csrAddr, io.in(1).csrAddr)
  io.csrWdata   := io.rs1Data(csrIdx)

  // ── Redirect ──────────────────────────────────────────────────────────
  io.exRedirectValid := redirect(0) || redirect(1)
  io.exRedirectPc    := Mux(redirect(0), actualNextPc(0), actualNextPc(1))

  // ── BPU update ────────────────────────────────────────────────────────
  // Update source priority: slot 0 > slot 1 (slot 0 is the older instruction).
  // Only conditional branches update the bi-mode BPU. JALR/ret targets are
  // indirect and need a RAS or per-context predictor; training them into the
  // simple BTB makes shared return sites such as _putchar.ret predict stale
  // call-site addresses.
  val br0   = slot0Live && io.in(0).brType =/= BrType.BR_NONE
  val br1   = slot1Live && io.in(1).brType =/= BrType.BR_NONE

  val isUpdate0 = br0
  val isUpdate1 = br1

  io.bpuUpdateValid := isUpdate0 || isUpdate1

  val bpuFromSlot0 = isUpdate0   // slot 0 wins when both want to update

  io.bpuUpdatePc := Mux(bpuFromSlot0, io.in(0).pc, io.in(1).pc)

  io.bpuUpdateTaken := Mux(bpuFromSlot0,
    branchTaken(0),
    branchTaken(1))

  // BTB target: always store the branch-target address (pc+imm) regardless of
  // taken/not-taken, so the BTB is correct when the branch is later taken.
  val updateTarget0 = branchTarget(0)
  val updateTarget1 = branchTarget(1)
  io.bpuUpdateTarget := Mux(bpuFromSlot0, updateTarget0, updateTarget1)

}
