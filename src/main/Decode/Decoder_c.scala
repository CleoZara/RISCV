package riscv

import chisel3._
import chisel3.util._
import Instructions._
import AluOp._
import Op1Sel._
import Op2Sel._
import WbSel._
import MemWidth._
import BrType._

object ImmSel {
  val I = 0.U(3.W)
  val S = 1.U(3.W)
  val B = 2.U(3.W)
  val U = 3.U(3.W)
  val J = 4.U(3.W)
}

class Decoder(enableRV32M: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val inst    = Input(UInt(32.W))
    val pc      = Input(UInt(32.W))
    val rs1Data = Input(UInt(32.W))
    val rs2Data = Input(UInt(32.W))
    val out     = Output(new DecodedSlot)
  })

  val inst    = io.inst
  val rs1Addr = inst(19, 15)
  val rs2Addr = inst(24, 20)
  val rdAddr  = inst(11, 7)
  val csrAddr = inst(31, 20)

  val immI = Cat(Fill(20, inst(31)), inst(31, 20))
  val immS = Cat(Fill(20, inst(31)), inst(31, 25), inst(11, 7))
  val immB = Cat(Fill(19, inst(31)), inst(31), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W))
  val immU = Cat(inst(31, 12), 0.U(12.W))
  val immJ = Cat(Fill(11, inst(31)), inst(31), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W))

  val default = List(
    ALU_ADD, OP1_RS1, OP2_IMM, WB_ALU,
    false.B, false.B, false.B, MW_WORD, true.B,
    BR_NONE, false.B, false.B, false.B, CSROp.NONE,
    false.B, false.B, ImmSel.I, false.B
  )

  val table = Seq(
    ADD   -> List(ALU_ADD,  OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),
    SUB   -> List(ALU_SUB,  OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),
    AND   -> List(ALU_AND,  OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),
    OR    -> List(ALU_OR,   OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),
    XOR   -> List(ALU_XOR,  OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),
    SLL   -> List(ALU_SLL,  OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),
    SRL   -> List(ALU_SRL,  OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),
    SRA   -> List(ALU_SRA,  OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),
    SLT   -> List(ALU_SLT,  OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),
    SLTU  -> List(ALU_SLTU, OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),

    ADDI  -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    ANDI  -> List(ALU_AND,  OP1_RS1, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    ORI   -> List(ALU_OR,   OP1_RS1, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    XORI  -> List(ALU_XOR,  OP1_RS1, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    SLLI  -> List(ALU_SLL,  OP1_RS1, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    SRLI  -> List(ALU_SRL,  OP1_RS1, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    SRAI  -> List(ALU_SRA,  OP1_RS1, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    SLTI  -> List(ALU_SLT,  OP1_RS1, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    SLTIU -> List(ALU_SLTU, OP1_RS1, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),

    LW    -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_MEM, true.B,  true.B,  false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    LH    -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_MEM, true.B,  true.B,  false.B, MW_HALF, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    LB    -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_MEM, true.B,  true.B,  false.B, MW_BYTE, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    LHU   -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_MEM, true.B,  true.B,  false.B, MW_HALF, false.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    LBU   -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_MEM, true.B,  true.B,  false.B, MW_BYTE, false.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),

    SW    -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_ALU, false.B, false.B, true.B,  MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.S, true.B),
    SH    -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_ALU, false.B, false.B, true.B,  MW_HALF, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.S, true.B),
    SB    -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_ALU, false.B, false.B, true.B,  MW_BYTE, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.S, true.B),

    BEQ   -> List(ALU_SUB,  OP1_RS1, OP2_RS2, WB_ALU, false.B, false.B, false.B, MW_WORD, true.B,  BR_EQ,   false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.B, true.B),
    BNE   -> List(ALU_SUB,  OP1_RS1, OP2_RS2, WB_ALU, false.B, false.B, false.B, MW_WORD, true.B,  BR_NE,   false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.B, true.B),
    BLT   -> List(ALU_SLT,  OP1_RS1, OP2_RS2, WB_ALU, false.B, false.B, false.B, MW_WORD, true.B,  BR_LT,   false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.B, true.B),
    BGE   -> List(ALU_SLT,  OP1_RS1, OP2_RS2, WB_ALU, false.B, false.B, false.B, MW_WORD, true.B,  BR_GE,   false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.B, true.B),
    BLTU  -> List(ALU_SLTU, OP1_RS1, OP2_RS2, WB_ALU, false.B, false.B, false.B, MW_WORD, true.B,  BR_LTU,  false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.B, true.B),
    BGEU  -> List(ALU_SLTU, OP1_RS1, OP2_RS2, WB_ALU, false.B, false.B, false.B, MW_WORD, true.B,  BR_GEU,  false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.B, true.B),

    JAL   -> List(ALU_ADD,  OP1_PC,  OP2_IMM, WB_PC4, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, true.B,  false.B, false.B, CSROp.NONE, false.B, false.B, ImmSel.J, true.B),
    JALR  -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_PC4, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, true.B,  true.B,  false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    LUI   -> List(ALU_LUI,  OP1_IMM, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, false.B, false.B, ImmSel.U, true.B),
    AUIPC -> List(ALU_ADD,  OP1_PC,  OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, false.B, false.B, ImmSel.U, true.B),

    CSRRW -> List(ALU_COPY1, OP1_RS1, OP2_IMM, WB_CSR, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.WRITE, true.B, false.B, ImmSel.I, true.B),
    CSRRS -> List(ALU_COPY1, OP1_RS1, OP2_IMM, WB_CSR, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.SET,   true.B, false.B, ImmSel.I, true.B),
    CSRRC -> List(ALU_COPY1, OP1_RS1, OP2_IMM, WB_CSR, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.CLEAR, true.B, false.B, ImmSel.I, true.B),

    ECALL  -> List(ALU_ADD, OP1_RS1, OP2_IMM, WB_ALU, false.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, true.B, CSROp.NONE, false.B, false.B, ImmSel.I, true.B),
    EBREAK -> List(ALU_ADD, OP1_RS1, OP2_IMM, WB_ALU, false.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, true.B, CSROp.NONE, false.B, false.B, ImmSel.I, true.B),
    FENCE  -> List(ALU_ADD, OP1_RS1, OP2_IMM, WB_ALU, false.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, true.B, CSROp.NONE, false.B, false.B, ImmSel.I, true.B)
  )

  val mTable = if (enableRV32M) Seq(
    MUL    -> List(ALU_MUL,    OP1_RS1, OP2_RS2, WB_ALU, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B, true.B, ImmSel.I, true.B),
    MULH   -> List(ALU_MULH,   OP1_RS1, OP2_RS2, WB_ALU, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B, true.B, ImmSel.I, true.B),
    MULHSU -> List(ALU_MULHSU, OP1_RS1, OP2_RS2, WB_ALU, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B, true.B, ImmSel.I, true.B),
    MULHU  -> List(ALU_MULHU,  OP1_RS1, OP2_RS2, WB_ALU, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B, true.B, ImmSel.I, true.B),
    DIV    -> List(ALU_DIV,    OP1_RS1, OP2_RS2, WB_ALU, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B, true.B, ImmSel.I, true.B),
    DIVU   -> List(ALU_DIVU,   OP1_RS1, OP2_RS2, WB_ALU, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B, true.B, ImmSel.I, true.B),
    REM    -> List(ALU_REM,    OP1_RS1, OP2_RS2, WB_ALU, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B, true.B, ImmSel.I, true.B),
    REMU   -> List(ALU_REMU,   OP1_RS1, OP2_RS2, WB_ALU, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B, true.B, ImmSel.I, true.B)
  ) else Seq.empty

  val decoded = ListLookup(inst, default, (table ++ mTable).toArray)
  val immSel = decoded(16)

  io.out := 0.U.asTypeOf(new DecodedSlot)
  io.out.pc        := io.pc
  io.out.inst      := inst
  io.out.rs1Addr   := rs1Addr
  io.out.rs2Addr   := rs2Addr
  io.out.rdAddr    := rdAddr
  io.out.rs1Data   := io.rs1Data
  io.out.rs2Data   := io.rs2Data
  io.out.aluOp     := decoded(0)
  io.out.op1Sel    := decoded(1)
  io.out.op2Sel    := decoded(2)
  io.out.wbSel     := decoded(3)
  io.out.rfWen     := decoded(4).asBool
  io.out.memRen    := decoded(5).asBool
  io.out.memWen    := decoded(6).asBool
  io.out.memWd     := decoded(7)
  io.out.memSigned := decoded(8).asBool
  io.out.brType    := decoded(9)
  io.out.isJump    := decoded(10).asBool
  io.out.isJalr    := decoded(11).asBool
  io.out.isSysInst := decoded(12).asBool
  io.out.csrOp     := decoded(13)
  io.out.rs1Use    := decoded(14).asBool
  io.out.rs2Use    := decoded(15).asBool
  io.out.imm       := MuxLookup(immSel, immI, Seq(
    ImmSel.I -> immI,
    ImmSel.S -> immS,
    ImmSel.B -> immB,
    ImmSel.U -> immU,
    ImmSel.J -> immJ
  ))
  io.out.csrAddr := csrAddr
  io.out.ctrl.valid   := decoded(17).asBool
  io.out.ctrl.kill    := false.B
  io.out.ctrl.allowIn := true.B
}
