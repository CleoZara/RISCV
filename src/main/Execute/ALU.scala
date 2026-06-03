package riscv

import chisel3._
import chisel3.util._
import AluOp._

class ParamALU(val xlen: Int = 32, val enableRV32M: Boolean = false) extends Module {
  require(xlen >= 32, "ParamALU currently expects xlen >= 32")

  private val shamtWidth = log2Ceil(xlen)

  val io = IO(new Bundle {
    val op1    = Input(UInt(xlen.W))
    val op2    = Input(UInt(xlen.W))
    val aluOp  = Input(UInt(AluOp.W.W))
    val result = Output(UInt(xlen.W))

    // Compare flags are convenient for branch units.
    val cmpEq  = Output(Bool())
    val cmpLt  = Output(Bool())
    val cmpLtu = Output(Bool())
  })

  val shamt = io.op2(shamtWidth - 1, 0)

  io.cmpEq  := io.op1 === io.op2
  io.cmpLt  := io.op1.asSInt < io.op2.asSInt
  io.cmpLtu := io.op1 < io.op2

  // 基础 RV32I ALU 映射
  val baseAluMapping = Seq(
    ALU_ADD   -> (io.op1 + io.op2),
    ALU_SUB   -> (io.op1 - io.op2),
    ALU_AND   -> (io.op1 & io.op2),
    ALU_OR    -> (io.op1 | io.op2),
    ALU_XOR   -> (io.op1 ^ io.op2),
    ALU_SLL   -> (io.op1 << shamt)(xlen - 1, 0),
    ALU_SRL   -> (io.op1 >> shamt),
    ALU_SRA   -> (io.op1.asSInt >> shamt).asUInt,
    ALU_SLT   -> io.cmpLt.asUInt,
    ALU_SLTU  -> io.cmpLtu.asUInt,
    ALU_LUI   -> io.op2,
    ALU_COPY1 -> io.op1
  )

  // 动态生成 RV32M ALU 指令映射
  val rv32mMapping: Seq[(UInt, UInt)] = if (enableRV32M) {
    // --- 乘法 (Multiplication) ---
    val mul    = io.op1 * io.op2                                  // 低 32 位相同
    val mulh   = (io.op1.asSInt * io.op2.asSInt).asUInt           // 有符号 × 有符号
    val mulhsu = (io.op1.asSInt * Cat(0.U(1.W), io.op2).asSInt).asUInt // 有符号 × 无符号
    val mulhu  = io.op1 * io.op2                                  // 无符号 × 无符号

    // --- 除法边界条件 ---
    val divByZero   = io.op2 === 0.U
    val isIntMin    = io.op1 === Cat(1.U(1.W), 0.U((xlen - 1).W)) // -2^31
    val isMinusOne  = io.op2.andR                                 // -1
    val divOverflow = isIntMin && isMinusOne

    val divResult = Mux(divByZero, (-1.S(xlen.W)).asUInt,
                    Mux(divOverflow, io.op1,
                    (io.op1.asSInt / io.op2.asSInt).asUInt))(xlen - 1, 0)

    val divuResult = Mux(divByZero, (-1.S(xlen.W)).asUInt,
                     (io.op1 / io.op2))(xlen - 1, 0)

    val remResult = Mux(divByZero, io.op1,
                    Mux(divOverflow, 0.U(xlen.W),
                    (io.op1.asSInt % io.op2.asSInt).asUInt))(xlen - 1, 0)

    val remuResult = Mux(divByZero, io.op1,
                     (io.op1 % io.op2))(xlen - 1, 0)

    Seq(
      ALU_MUL    -> mul(xlen - 1, 0),
      ALU_MULH   -> mulh(xlen * 2 - 1, xlen),
      ALU_MULHSU -> mulhsu(xlen * 2 - 1, xlen),
      ALU_MULHU  -> mulhu(xlen * 2 - 1, xlen),
      ALU_DIV    -> divResult,
      ALU_DIVU   -> divuResult,
      ALU_REM    -> remResult,
      ALU_REMU   -> remuResult
    )
  } else {
    Seq()
  }

  io.result := MuxLookup(io.aluOp, 0.U(xlen.W), baseAluMapping ++ rv32mMapping)
}
