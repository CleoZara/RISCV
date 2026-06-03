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

    val bpuUpdateValid  = Output(Bool())
    val bpuUpdatePc     = Output(UInt(32.W))
    val bpuUpdateTaken  = Output(Bool())
    val bpuUpdateTarget = Output(UInt(32.W))

    val exValid  = Output(Vec(issueWidth, Bool()))
    val exMemRen = Output(Vec(issueWidth, Bool()))
    val exRdAddr = Output(Vec(issueWidth, UInt(5.W)))
    val exRfWen  = Output(Vec(issueWidth, Bool()))
    val exResult = Output(Vec(issueWidth, UInt(32.W)))

    val out = Output(Vec(issueWidth, new EXMEMBundle))
  })

  val alus         = Seq.fill(issueWidth)(Module(new ParamALU(32, enableRV32M)))
  val slotValid    = Wire(Vec(issueWidth, Bool()))
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

    actualNextPc(i) := MuxCase(fallThrough, Seq(
      io.in(i).isJalr          -> jalrTarget(i),
      (isBranch && branchTaken(i)) -> branchTarget(i)
    ))

    redirect(i) := slotValid(i) &&
                   (isBranch || io.in(i).isJalr) &&
                   (actualNextPc(i) =/= io.in(i).predNextPc)

    io.exValid(i)  := slotValid(i)
    io.exMemRen(i) := slotValid(i) && io.in(i).memRen
    io.exRdAddr(i) := io.in(i).rdAddr
    io.exRfWen(i)  := slotValid(i) && io.in(i).rfWen
    io.exResult(i) := alus(i).io.result

    io.out(i) := 0.U.asTypeOf(new EXMEMBundle)
    io.out(i).pc        := io.in(i).pc
    io.out(i).inst      := io.in(i).inst
    io.out(i).slotIdx   := io.in(i).slotIdx
    io.out(i).aluOut    := alus(i).io.result
    io.out(i).rs2Data   := io.storeData(i)
    io.out(i).rdAddr    := io.in(i).rdAddr
    io.out(i).wbSel     := io.in(i).wbSel
    io.out(i).rfWen     := slotValid(i) && io.in(i).rfWen
    io.out(i).memRen    := slotValid(i) && io.in(i).memRen
    io.out(i).memWen    := slotValid(i) && io.in(i).memWen
    io.out(i).memWd     := io.in(i).memWd
    io.out(i).memSigned := io.in(i).memSigned
    io.out(i).csrRdata  := io.csrOldData
    io.out(i).csrOp     := Mux(slotValid(i), io.in(i).csrOp, CSROp.NONE)
    io.out(i).brType    := Mux(slotValid(i), io.in(i).brType, BrType.BR_NONE)
    io.out(i).isJump    := slotValid(i) && io.in(i).isJump
    io.out(i).ctrl.valid   := slotValid(i)
    io.out(i).ctrl.kill    := io.flushEx || io.in(i).ctrl.kill
    io.out(i).ctrl.allowIn := true.B
  }

  // ── CSR operation ─────────────────────────────────────────────────────
  val csrSlot0 = slotValid(0) && io.in(0).csrOp =/= CSROp.NONE
  val csrSlot1 = slotValid(1) && io.in(1).csrOp =/= CSROp.NONE
  val csrIdx   = Mux(csrSlot0, 0.U, 1.U)
  io.csrOpValid := csrSlot0 || csrSlot1
  io.csrOpType  := Mux(csrSlot0, io.in(0).csrOp,  io.in(1).csrOp)
  io.csrWaddr   := Mux(csrSlot0, io.in(0).csrAddr, io.in(1).csrAddr)
  io.csrWdata   := io.rs1Data(csrIdx)

  // ── Redirect ──────────────────────────────────────────────────────────
  io.exRedirectValid := redirect(0) || redirect(1)
  io.exRedirectPc    := Mux(redirect(0), actualNextPc(0), actualNextPc(1))

  // ── BPU update ────────────────────────────────────────────────────────
  // P1 fix: JALR must also update the BPU/BTB so the predictor learns the
  // indirect-call target.  Without this the BTB never converges and every
  // JALR (function call/return) pays the full 2-cycle flush penalty forever.
  //
  // Update source priority: slot 0 > slot 1 (slot 0 is the older instruction).
  // If both slots trigger an update in the same cycle (extremely rare after
  // the IDStage slot0Ctrl fix), slot 0 is recorded and slot 1 is silently
  // dropped.  The dropped update will be re-trained on the next occurrence.
  val br0   = slotValid(0) && io.in(0).brType =/= BrType.BR_NONE
  val br1   = slotValid(1) && io.in(1).brType =/= BrType.BR_NONE
  val jalr0 = slotValid(0) && io.in(0).isJalr
  val jalr1 = slotValid(1) && io.in(1).isJalr

  val isUpdate0 = br0 || jalr0
  val isUpdate1 = br1 || jalr1

  io.bpuUpdateValid := isUpdate0 || isUpdate1

  val bpuFromSlot0 = isUpdate0   // slot 0 wins when both want to update

  io.bpuUpdatePc := Mux(bpuFromSlot0, io.in(0).pc, io.in(1).pc)

  // For JALR: always "taken" (unconditional jump).
  // For branches: use the resolved branch outcome.
  io.bpuUpdateTaken := Mux(bpuFromSlot0,
    Mux(jalr0, true.B, branchTaken(0)),
    Mux(jalr1, true.B, branchTaken(1)))

  // BTB target: for branches, always store the branch-target address (pc+imm)
  // regardless of taken/not-taken, so the BTB is correct when the branch IS
  // later taken.  For JALR, store the resolved indirect target.
  val updateTarget0 = Mux(jalr0, jalrTarget(0), branchTarget(0))
  val updateTarget1 = Mux(jalr1, jalrTarget(1), branchTarget(1))
  io.bpuUpdateTarget := Mux(bpuFromSlot0, updateTarget0, updateTarget1)

}
