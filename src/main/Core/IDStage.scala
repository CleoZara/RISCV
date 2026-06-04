package riscv

import chisel3._
import chisel3.util._

class IDStage(enableRV32M: Boolean = false) extends Module {
  val issueWidth = 2

  val io = IO(new Bundle {
    val in      = Input(Vec(issueWidth, new IFIDSlot))
    val stallId = Input(Bool())
    val flushId = Input(Bool())

    val regRs1Addr = Output(Vec(issueWidth, UInt(5.W)))
    val regRs2Addr = Output(Vec(issueWidth, UInt(5.W)))
    val regRs1Data = Input(Vec(issueWidth, UInt(32.W)))
    val regRs2Data = Input(Vec(issueWidth, UInt(32.W)))

    val csrRaddr = Output(UInt(12.W))
    val csrRdata = Input(UInt(32.W))

    val idRedirectValid = Output(Bool())
    val idRedirectPc    = Output(UInt(32.W))

    val hazardIdValid               = Output(Vec(issueWidth, Bool()))
    val hazardRs1Addr               = Output(Vec(issueWidth, UInt(5.W)))
    val hazardRs2Addr               = Output(Vec(issueWidth, UInt(5.W)))
    val hazardRs1Use                = Output(Vec(issueWidth, Bool()))
    val hazardRs2Use                = Output(Vec(issueWidth, Bool()))
    val hazardRdAddr                = Output(Vec(issueWidth, UInt(5.W)))
    val hazardRfWen                 = Output(Vec(issueWidth, Bool()))
    val hazardCanBypassToYounger    = Output(Vec(issueWidth, Bool()))

    val holdIfId = Output(Bool())
    val out      = Output(Vec(issueWidth, new IDEXBundle))
  })

  // ── Pending-slot register for slot-1 deferral ─────────────────────────
  val pendingValid = RegInit(false.B)
  val pendingSlot  = RegInit(0.U.asTypeOf(new IFIDSlot))

  val eff = Wire(Vec(issueWidth, new IFIDSlot))
  eff(0) := Mux(pendingValid, pendingSlot, io.in(0))
  eff(1) := Mux(pendingValid, 0.U.asTypeOf(new IFIDSlot), io.in(1))
  io.holdIfId := pendingValid && !io.flushId

  for (i <- 0 until issueWidth) {
    io.regRs1Addr(i) := eff(i).inst(19, 15)
    io.regRs2Addr(i) := eff(i).inst(24, 20)
  }

  val dec = Seq.fill(issueWidth)(Module(new Decoder(enableRV32M)))
  for (i <- 0 until issueWidth) {
    dec(i).io.inst    := eff(i).inst
    dec(i).io.pc      := eff(i).pc
    dec(i).io.rs1Data := io.regRs1Data(i)
    dec(i).io.rs2Data := io.regRs2Data(i)
  }

  val slotValid = Wire(Vec(issueWidth, Bool()))
  for (i <- 0 until issueWidth) {
    slotValid(i) := eff(i).ctrl.valid && !eff(i).ctrl.kill && dec(i).io.out.ctrl.valid
  }

  // ── P2 fix: slot 0 can forward its ALU result to slot 1 in the same EX
  // cycle (F5 bypass path) when it is a non-Load register-writing instruction.
  // In that case we must NOT block slot 1 on an intra-slot RAW.
  val slot0Mem = slotValid(0) && (dec(0).io.out.memRen || dec(0).io.out.memWen)
  val slot1Mem = slotValid(1) && (dec(1).io.out.memRen || dec(1).io.out.memWen)

  val slot0CanBypass =
    slotValid(0) &&
    dec(0).io.out.rfWen &&
    !dec(0).io.out.memRen &&                           // Load result not available in EX
    (dec(0).io.out.wbSel === WbSel.WB_ALU) &&         // only ALU results are available to F5
    (dec(0).io.out.rdAddr =/= 0.U)

  // ── Slot-1 stall conditions ────────────────────────────────────────────
  // P2 fix: intra-slot RAW is resolved by the intra-EX bypass (F5) when
  // slot 0 can forward.  Only truly block slot 1 when slot 0 is a Load.
  val slotRaw01 =
    (!slot0CanBypass || slot1Mem) &&
    slotValid(0) && slotValid(1) &&
    dec(0).io.out.rfWen &&
    (dec(0).io.out.rdAddr =/= 0.U) &&
    ((dec(1).io.out.rs1Use && (dec(0).io.out.rdAddr === dec(1).io.out.rs1Addr)) ||
     (dec(1).io.out.rs2Use && (dec(0).io.out.rdAddr === dec(1).io.out.rs2Addr)))

  val slot0Csr = slotValid(0) && dec(0).io.out.csrOp =/= CSROp.NONE
  val slot1Csr = slotValid(1) && dec(1).io.out.csrOp =/= CSROp.NONE
  val slot1Ctrl = slotValid(1) &&
                  (dec(1).io.out.isJump || (dec(1).io.out.brType =/= BrType.BR_NONE))

  // P1 fix: slot0Ctrl must block slot 1 only when slot 1 would be on the
  // wrong execution path, i.e.:
  //   a) slot 0 is JAL      → redirect happens in ID, slot 1 is wrong-path
  //   b) slot 0 is JALR     → target unknown until EX, slot 1 is wrong-path
  //   c) slot 0 is a branch predicted-TAKEN → slot 1 (PC+4) is wrong-path
  // Branches predicted NOT-TAKEN: slot 1 (fallthrough) is on the right path;
  // if the branch is later found mispredicted, EX flush handles both slots.
  val slot0IsJal         = slotValid(0) && dec(0).io.out.isJump && !dec(0).io.out.isJalr
  val slot0IsJalr        = slotValid(0) && dec(0).io.out.isJalr
  val slot0BranchTaken   = slotValid(0) &&
                           (dec(0).io.out.brType =/= BrType.BR_NONE) &&
                           eff(0).predTaken        // BPU predicted this branch as taken
  val slot0Ctrl = slot0IsJal || slot0IsJalr || slot0BranchTaken

  val slot1DataOrResourceBlocked = slotRaw01 || (slot0Mem && slot1Mem) || (slot0Csr && slot1Csr)
  val slot1ReplayBlocked = slot1DataOrResourceBlocked || slot1Ctrl
  val slot1Blocked = slot1ReplayBlocked || slot0Ctrl

  val canIssue0 = slotValid(0) && !io.stallId && !io.flushId
  val canIssue1 = canIssue0 && slotValid(1) && !slot1Blocked

  // Capture slot 1 only for replayable data/resource conflicts.  If slot 0 is
  // a control-flow instruction, slot 1 is wrong-path and must be squashed.
  val capturePending = canIssue0 && slotValid(1) && !slot0Ctrl && slot1ReplayBlocked && !pendingValid
  when(io.flushId) {
    pendingValid := false.B
  }.elsewhen(capturePending) {
    pendingSlot  := eff(1)
    pendingValid := true.B
  }.elsewhen(pendingValid && !io.stallId) {
    pendingValid := false.B
  }

  // ── CSR read address MUX ──────────────────────────────────────────────
  val csrSel1 = !slot0Csr && slot1Csr
  io.csrRaddr := Mux(csrSel1, dec(1).io.out.csrAddr, dec(0).io.out.csrAddr)

  // ── JAL redirect (ID-level redirect, 1-cycle flush) ───────────────────
  val jalTarget0 = dec(0).io.out.pc + dec(0).io.out.imm
  val jalTarget1 = dec(1).io.out.pc + dec(1).io.out.imm
  val jal0 = canIssue0 &&
             dec(0).io.out.isJump &&
             !dec(0).io.out.isJalr &&
             eff(0).predNextPc =/= jalTarget0
  val jal1 = canIssue1 &&
             dec(1).io.out.isJump &&
             !dec(1).io.out.isJalr &&
             eff(1).predNextPc =/= jalTarget1
  io.idRedirectValid := jal0 || jal1
  io.idRedirectPc    := Mux(jal0,
    jalTarget0,
    jalTarget1)

  // ── Pipeline register outputs ─────────────────────────────────────────
  for (i <- 0 until issueWidth) {
    val issue = if (i == 0) canIssue0 else canIssue1
    val d = dec(i).io.out

    io.out(i) := 0.U.asTypeOf(new IDEXBundle)
    io.out(i).pc        := d.pc
    io.out(i).inst      := d.inst
    io.out(i).slotIdx   := eff(i).slotIdx
    io.out(i).fetchPc   := eff(i).fetchPc
    io.out(i).rs1Addr   := d.rs1Addr
    io.out(i).rs2Addr   := d.rs2Addr
    io.out(i).rdAddr    := d.rdAddr
    io.out(i).rs1Data   := d.rs1Data
    io.out(i).rs2Data   := d.rs2Data
    io.out(i).rs1Use    := d.rs1Use
    io.out(i).rs2Use    := d.rs2Use
    io.out(i).imm       := d.imm
    io.out(i).csrAddr   := d.csrAddr
    io.out(i).csrOp     := Mux(issue, d.csrOp, CSROp.NONE)
    io.out(i).aluOp     := d.aluOp
    io.out(i).op1Sel    := d.op1Sel
    io.out(i).op2Sel    := d.op2Sel
    io.out(i).wbSel     := d.wbSel
    io.out(i).rfWen     := issue && d.rfWen
    io.out(i).memRen    := issue && d.memRen
    io.out(i).memWen    := issue && d.memWen
    io.out(i).memWd     := d.memWd
    io.out(i).memSigned := d.memSigned
    io.out(i).brType    := Mux(issue, d.brType, BrType.BR_NONE)
    io.out(i).isJump    := issue && d.isJump
    io.out(i).isJalr    := issue && d.isJalr
    io.out(i).isSysInst := issue && d.isSysInst
    io.out(i).predTaken  := eff(i).predTaken
    io.out(i).predTarget := eff(i).predTarget
    io.out(i).predNextPc := eff(i).predNextPc
    io.out(i).seqNextPc  := eff(i).seqNextPc
    io.out(i).csrRdata   := io.csrRdata
    io.out(i).ctrl.valid   := issue
    io.out(i).ctrl.kill    := io.flushId || eff(i).ctrl.kill
    io.out(i).ctrl.allowIn := true.B

    // Hazard unit connections
    io.hazardIdValid(i)   := slotValid(i)
    io.hazardRs1Addr(i)   := d.rs1Addr
    io.hazardRs2Addr(i)   := d.rs2Addr
    io.hazardRs1Use(i)    := d.rs1Use
    io.hazardRs2Use(i)    := d.rs2Use
    io.hazardRdAddr(i)    := d.rdAddr
    io.hazardRfWen(i)     := d.rfWen
    // P2 fix: tell hazard unit that slot 0 can forward to younger slots
    // so the hazard unit's BypassNetwork can activate the F5 intra-EX path.
    io.hazardCanBypassToYounger(i) := (i == 0).B && slot0CanBypass
  }
}
