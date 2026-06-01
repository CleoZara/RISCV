// ===== 译码器 =====
// 输入一条32位指令，解析出所有控制信号（ALU操作、访存、写回、分支等）。
// 识别 RV32I（含可选 RV32M）指令，输出 DecodedSlot（含 valid 信号）。
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

  // 立即数生成
  val immI = inst(31, 20).asSInt
  val immS = Cat(inst(31, 25), inst(11, 7)).asSInt
  val immB = Cat(inst(31), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W)).asSInt
  val immU = Cat(inst(31, 12), 0.U(12.W)).asSInt
  val immJ = Cat(inst(31), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W)).asSInt

  val csrAddr = inst(31, 20)

  // 默认值
  val aluOp     = WireDefault(ALU_ADD)
  val op1Sel    = WireDefault(OP1_RS1)
  val op2Sel    = WireDefault(OP2_IMM)
  val wbSel     = WireDefault(WB_ALU)
  val rfWen     = WireDefault(false.B)
  val memRen    = WireDefault(false.B)
  val memWen    = WireDefault(false.B)
  val memWd     = WireDefault(MW_WORD)
  val memSigned = WireDefault(true.B)
  val brType    = WireDefault(BR_NONE)
  val isJump    = WireDefault(false.B)
  val isJalr    = WireDefault(false.B)
  val isSysInst = WireDefault(false.B)
  val csrOp     = WireDefault(CSROp.NONE)
  val imm       = WireDefault(immI)

  when(inst === Instructions.ADD) { aluOp := ALU_ADD; op2Sel := OP2_RS2; rfWen := true.B
  }.elsewhen(inst === Instructions.SUB) { aluOp := ALU_SUB; op2Sel := OP2_RS2; rfWen := true.B
  }.elsewhen(inst === Instructions.AND) { aluOp := ALU_AND; op2Sel := OP2_RS2; rfWen := true.B
  }.elsewhen(inst === Instructions.OR)  { aluOp := ALU_OR;  op2Sel := OP2_RS2; rfWen := true.B
  }.elsewhen(inst === Instructions.XOR) { aluOp := ALU_XOR; op2Sel := OP2_RS2; rfWen := true.B
  }.elsewhen(inst === Instructions.SLL) { aluOp := ALU_SLL; op2Sel := OP2_RS2; rfWen := true.B
  }.elsewhen(inst === Instructions.SRL) { aluOp := ALU_SRL; op2Sel := OP2_RS2; rfWen := true.B
  }.elsewhen(inst === Instructions.SRA) { aluOp := ALU_SRA; op2Sel := OP2_RS2; rfWen := true.B
  }.elsewhen(inst === Instructions.SLT) { aluOp := ALU_SLT; op2Sel := OP2_RS2; rfWen := true.B
  }.elsewhen(inst === Instructions.SLTU){ aluOp := ALU_SLTU;op2Sel := OP2_RS2; rfWen := true.B

  }.elsewhen(inst === ADDI)  { aluOp := ALU_ADD; rfWen := true.B
  }.elsewhen(inst === ANDI)  { aluOp := ALU_AND; rfWen := true.B
  }.elsewhen(inst === ORI)   { aluOp := ALU_OR;  rfWen := true.B
  }.elsewhen(inst === XORI)  { aluOp := ALU_XOR; rfWen := true.B
  }.elsewhen(inst === SLLI)  { aluOp := ALU_SLL; rfWen := true.B
  }.elsewhen(inst === SRLI)  { aluOp := ALU_SRL; rfWen := true.B
  }.elsewhen(inst === SRAI)  { aluOp := ALU_SRA; rfWen := true.B
  }.elsewhen(inst === SLTI)  { aluOp := ALU_SLT; rfWen := true.B
  }.elsewhen(inst === SLTIU) { aluOp := ALU_SLTU;rfWen := true.B

  }.elsewhen(inst === LW)  { aluOp := ALU_ADD; rfWen := true.B; memRen := true.B; wbSel := WB_MEM; memWd := MW_WORD
  }.elsewhen(inst === LH)  { aluOp := ALU_ADD; rfWen := true.B; memRen := true.B; wbSel := WB_MEM; memWd := MW_HALF
  }.elsewhen(inst === LB)  { aluOp := ALU_ADD; rfWen := true.B; memRen := true.B; wbSel := WB_MEM; memWd := MW_BYTE
  }.elsewhen(inst === LHU) { aluOp := ALU_ADD; rfWen := true.B; memRen := true.B; wbSel := WB_MEM; memWd := MW_HALF; memSigned := false.B
  }.elsewhen(inst === LBU) { aluOp := ALU_ADD; rfWen := true.B; memRen := true.B; wbSel := WB_MEM; memWd := MW_BYTE; memSigned := false.B

  }.elsewhen(inst === SW) { aluOp := ALU_ADD; memWen := true.B; imm := immS; memWd := MW_WORD
  }.elsewhen(inst === SH) { aluOp := ALU_ADD; memWen := true.B; imm := immS; memWd := MW_HALF
  }.elsewhen(inst === SB) { aluOp := ALU_ADD; memWen := true.B; imm := immS; memWd := MW_BYTE

  }.elsewhen(inst === BEQ)  { brType := BR_EQ;  imm := immB
  }.elsewhen(inst === BNE)  { brType := BR_NE;  imm := immB
  }.elsewhen(inst === BLT)  { brType := BR_LT;  imm := immB
  }.elsewhen(inst === BGE)  { brType := BR_GE;  imm := immB
  }.elsewhen(inst === BLTU) { brType := BR_LTU; imm := immB
  }.elsewhen(inst === BGEU) { brType := BR_GEU; imm := immB

  }.elsewhen(inst === JAL) {
    aluOp := ALU_ADD; op1Sel := OP1_PC; imm := immJ; rfWen := true.B; wbSel := WB_PC4; isJump := true.B
  }.elsewhen(inst === JALR) {
    aluOp := ALU_ADD; op1Sel := OP1_RS1; rfWen := true.B; wbSel := WB_PC4; isJump := true.B; isJalr := true.B

  }.elsewhen(inst === Instructions.LUI) {
    aluOp := ALU_LUI; op1Sel := OP1_IMM; imm := immU; rfWen := true.B
  }.elsewhen(inst === AUIPC) {
    aluOp := ALU_ADD; op1Sel := OP1_PC; imm := immU; rfWen := true.B

  }.elsewhen(inst === CSRRW) { rfWen := true.B; wbSel := WB_CSR; csrOp := CSROp.WRITE; op1Sel := OP1_RS1
  }.elsewhen(inst === CSRRS) { rfWen := true.B; wbSel := WB_CSR; csrOp := CSROp.SET;   op1Sel := OP1_RS1
  }.elsewhen(inst === CSRRC) { rfWen := true.B; wbSel := WB_CSR; csrOp := CSROp.CLEAR; op1Sel := OP1_RS1

  }.elsewhen(inst === ECALL)  { isSysInst := true.B
  }.elsewhen(inst === EBREAK) { isSysInst := true.B
  }.elsewhen(inst === MRET)   { isSysInst := true.B; isJump := true.B
  }.elsewhen(inst === FENCE)  { isSysInst := true.B
  }.otherwise {
    // RV32M（加分项）：仅在 enableRV32M 时识别
    if (enableRV32M) {
      when(inst === MUL)    { aluOp := ALU_MUL;    op2Sel := OP2_RS2; rfWen := true.B
      }.elsewhen(inst === MULH)   { aluOp := ALU_MULH;   op2Sel := OP2_RS2; rfWen := true.B
      }.elsewhen(inst === MULHSU) { aluOp := ALU_MULHSU; op2Sel := OP2_RS2; rfWen := true.B
      }.elsewhen(inst === MULHU)  { aluOp := ALU_MULHU;  op2Sel := OP2_RS2; rfWen := true.B
      }.elsewhen(inst === DIV)    { aluOp := ALU_DIV;    op2Sel := OP2_RS2; rfWen := true.B
      }.elsewhen(inst === DIVU)   { aluOp := ALU_DIVU;   op2Sel := OP2_RS2; rfWen := true.B
      }.elsewhen(inst === REM)    { aluOp := ALU_REM;    op2Sel := OP2_RS2; rfWen := true.B
      }.elsewhen(inst === REMU)   { aluOp := ALU_REMU;   op2Sel := OP2_RS2; rfWen := true.B
      }
    }
  }

  val isKnownInst = rfWen || memRen || memWen || (brType =/= BR_NONE) || isJump || isSysInst

  io.out.pc        := io.pc
  io.out.inst      := inst
  io.out.rs1Addr   := rs1Addr
  io.out.rs2Addr   := rs2Addr
  io.out.rdAddr    := rdAddr
  io.out.rs1Data   := io.rs1Data
  io.out.rs2Data   := io.rs2Data
  io.out.imm       := imm.asUInt
  io.out.csrAddr   := csrAddr
  io.out.csrOp     := csrOp
  io.out.aluOp     := aluOp
  io.out.op1Sel    := op1Sel
  io.out.op2Sel    := op2Sel
  io.out.wbSel     := wbSel
  io.out.rfWen     := rfWen
  io.out.memRen    := memRen
  io.out.memWen    := memWen
  io.out.memWd     := memWd
  io.out.memSigned := memSigned
  io.out.brType    := brType
  io.out.isJump    := isJump
  io.out.isJalr    := isJalr
  io.out.isSysInst := isSysInst
  io.out.ctrl.valid   := isKnownInst
  io.out.ctrl.kill    := false.B
  io.out.ctrl.allowIn := true.B
}
