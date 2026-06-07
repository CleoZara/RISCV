package riscv

import chisel3._
import chisel3.util._
import AluOp._

class MulDivReq(val xlen: Int = 32) extends Bundle {
  val op1   = UInt(xlen.W)
  val op2   = UInt(xlen.W)
  val aluOp = UInt(AluOp.W.W)
}

class MulDivALU(val xlen: Int = 32, val mulLatency: Int = 3, val divLatency: Int = 32)
    extends Module {
  require(xlen == 32, "MulDivALU implements RV32M and expects xlen == 32")
  require(mulLatency >= 1, "mulLatency must be positive")
  require(divLatency >= 1, "divLatency must be positive")

  private val latencyWidth = log2Ceil(math.max(mulLatency, divLatency) + 1).max(1)

  val io = IO(new Bundle {
    val req    = Flipped(Decoupled(new MulDivReq(xlen)))
    val resp   = Decoupled(UInt(xlen.W))
    val cancel = Input(Bool())
    val busy   = Output(Bool())
  })

  val sIdle :: sBusy :: sDone :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val remaining = RegInit(0.U(latencyWidth.W))
  val resultReg = RegInit(0.U(xlen.W))

  val divActive = RegInit(false.B)
  val divWantRem = RegInit(false.B)
  val divQuotNeg = RegInit(false.B)
  val divRemNeg = RegInit(false.B)
  val divDividend = RegInit(0.U(xlen.W))
  val divDivisor = RegInit(0.U(xlen.W))
  val divQuotient = RegInit(0.U(xlen.W))
  val divRemainder = RegInit(0.U((xlen + 1).W))

  private def twos(x: UInt): UInt = (~x).asUInt + 1.U

  private def abs32(x: UInt, signed: Bool): UInt = {
    Mux(signed && x(31), twos(x), x)
  }

  private def high32(x: UInt): UInt = x(63, 32)

  private def divStep(
      dividend: UInt,
      quotient: UInt,
      remainder: UInt,
      divisor: UInt): (UInt, UInt, UInt) = {
    val shiftedRem = Cat(remainder(31, 0), dividend(31))
    val divisor33 = Cat(0.U(1.W), divisor)
    val ge = shiftedRem >= divisor33
    val nextRem = Mux(ge, shiftedRem - divisor33, shiftedRem)
    val nextQuot = Cat(quotient(30, 0), ge)
    val nextDividend = Cat(dividend(30, 0), 0.U(1.W))
    (nextDividend, nextQuot, nextRem)
  }

  private def mulResult(op1: UInt, op2: UInt, aluOp: UInt): UInt = {
    val op1S33 = Cat(op1(31), op1).asSInt
    val op2S33 = Cat(op2(31), op2).asSInt
    val op1U33 = Cat(0.U(1.W), op1)
    val op2U33 = Cat(0.U(1.W), op2)

    val mulSS = (op1S33 * op2S33).asUInt
    val mulSU = (op1S33 * op2U33.asSInt).asUInt
    val mulUU = op1 * op2

    MuxLookup(aluOp, mulUU(31, 0), Seq(
      ALU_MUL    -> mulUU(31, 0),
      ALU_MULH   -> high32(mulSS),
      ALU_MULHSU -> high32(mulSU),
      ALU_MULHU  -> high32(mulUU)
    ))
  }

  private def divSpecialResult(op1: UInt, op2: UInt, aluOp: UInt): UInt = {
    val divByZero = op2 === 0.U
    val divOverflow = op1 === "h80000000".U && op2 === "hffffffff".U
    MuxLookup(aluOp, 0.U(xlen.W), Seq(
      ALU_DIV  -> Mux(divByZero, "hffffffff".U, Mux(divOverflow, op1, 0.U)),
      ALU_DIVU -> Mux(divByZero, "hffffffff".U, 0.U),
      ALU_REM  -> Mux(divByZero, op1, Mux(divOverflow, 0.U, 0.U)),
      ALU_REMU -> Mux(divByZero, op1, 0.U)
    ))
  }

  io.req.ready := state === sIdle
  io.resp.valid := state === sDone
  io.resp.bits := resultReg
  io.busy := state =/= sIdle

  val reqOp = io.req.bits.aluOp
  val reqIsDivRem = reqOp >= ALU_DIV
  val reqIsRem = reqOp === ALU_REM || reqOp === ALU_REMU
  val reqIsSignedDiv = reqOp === ALU_DIV || reqOp === ALU_REM
  val reqDivByZero = io.req.bits.op2 === 0.U
  val reqDivOverflow = io.req.bits.op1 === "h80000000".U && io.req.bits.op2 === "hffffffff".U
  val reqDivSpecial = reqDivByZero || reqDivOverflow
  val reqAbsDividend = abs32(io.req.bits.op1, reqIsSignedDiv)
  val reqAbsDivisor = abs32(io.req.bits.op2, reqIsSignedDiv)
  val reqDivQuotNeg = reqIsSignedDiv && (io.req.bits.op1(31) ^ io.req.bits.op2(31))
  val reqDivRemNeg = reqIsSignedDiv && io.req.bits.op1(31)
  val firstDiv = divStep(reqAbsDividend, 0.U(xlen.W), 0.U((xlen + 1).W), reqAbsDivisor)

  when(io.cancel) {
    state := sIdle
    remaining := 0.U
    divActive := false.B
  }.otherwise {
    switch(state) {
      is(sIdle) {
        when(io.req.fire) {
          when(reqIsDivRem) {
            resultReg := divSpecialResult(io.req.bits.op1, io.req.bits.op2, reqOp)
            divActive := !reqDivSpecial
            divWantRem := reqIsRem
            divQuotNeg := reqDivQuotNeg
            divRemNeg := reqDivRemNeg
            divDividend := firstDiv._1
            divQuotient := firstDiv._2
            divRemainder := firstDiv._3
            divDivisor := reqAbsDivisor
            when(divLatency.U === 1.U) {
              state := sDone
            }.otherwise {
              remaining := (divLatency - 1).U
              state := sBusy
            }
          }.otherwise {
            resultReg := mulResult(io.req.bits.op1, io.req.bits.op2, reqOp)
            divActive := false.B
            when(mulLatency.U === 1.U) {
              state := sDone
            }.otherwise {
              remaining := (mulLatency - 1).U
              state := sBusy
            }
          }
        }
      }
      is(sBusy) {
        val nextDiv = divStep(divDividend, divQuotient, divRemainder, divDivisor)
        when(divActive) {
          divDividend := nextDiv._1
          divQuotient := nextDiv._2
          divRemainder := nextDiv._3
        }

        when(remaining === 1.U) {
          when(divActive) {
            val finalQuot = Mux(divQuotNeg, twos(nextDiv._2), nextDiv._2)
            val finalRemRaw = nextDiv._3(31, 0)
            val finalRem = Mux(divRemNeg, twos(finalRemRaw), finalRemRaw)
            resultReg := Mux(divWantRem, finalRem, finalQuot)
          }
          state := sDone
          divActive := false.B
        }.otherwise {
          remaining := remaining - 1.U
        }
      }
      is(sDone) {
        when(io.resp.fire) {
          state := sIdle
        }
      }
    }
  }
}
