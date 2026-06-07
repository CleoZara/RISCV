package riscv

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import AluOp._

class MulDivALUSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  private val Mask32 = (BigInt(1) << 32) - 1

  private def u32(x: BigInt): BigInt = x & Mask32
  private def s32(x: BigInt): BigInt = {
    val v = u32(x)
    if ((v & (BigInt(1) << 31)) != 0) v - (BigInt(1) << 32) else v
  }

  private def high32(x: BigInt): BigInt = u32(x >> 32)

  private def expected(op: UInt, a: BigInt, b: BigInt): BigInt = {
    val au = u32(a)
    val bu = u32(b)
    val as = s32(a)
    val bs = s32(b)
    val divByZero = bu == 0
    val overflow = au == BigInt("80000000", 16) && bu == Mask32

    op.litValue match {
      case v if v == ALU_MUL.litValue    => u32(au * bu)
      case v if v == ALU_MULH.litValue   => high32(as * bs)
      case v if v == ALU_MULHSU.litValue => high32(as * bu)
      case v if v == ALU_MULHU.litValue  => high32(au * bu)
      case v if v == ALU_DIV.litValue =>
        if (divByZero) Mask32 else if (overflow) au else u32(as / bs)
      case v if v == ALU_DIVU.litValue =>
        if (divByZero) Mask32 else u32(au / bu)
      case v if v == ALU_REM.litValue =>
        if (divByZero) au else if (overflow) 0 else u32(as % bs)
      case v if v == ALU_REMU.litValue =>
        if (divByZero) au else u32(au % bu)
      case _ => 0
    }
  }

  private def runOne(c: MulDivALU, op: UInt, a: BigInt, b: BigInt, latency: Int): Unit = {
    c.io.cancel.poke(false.B)
    c.io.resp.ready.poke(false.B)
    c.io.req.valid.poke(true.B)
    c.io.req.bits.op1.poke(u32(a).U)
    c.io.req.bits.op2.poke(u32(b).U)
    c.io.req.bits.aluOp.poke(op)
    c.io.req.ready.expect(true.B)
    c.clock.step()

    c.io.req.valid.poke(false.B)
    for (_ <- 1 until latency) {
      c.io.resp.valid.expect(false.B)
      c.clock.step()
    }

    c.io.resp.valid.expect(true.B)
    c.io.resp.bits.expect(expected(op, a, b).U)
    c.io.resp.ready.poke(true.B)
    c.clock.step()
    c.io.resp.ready.poke(false.B)
    c.io.req.ready.expect(true.B)
  }

  behavior of "MulDivALU"

  it should "compute RV32M multiply operations after 3 cycles" in {
    test(new MulDivALU()) { c =>
      runOne(c, ALU_MUL, -3, 7, 3)
      runOne(c, ALU_MULH, -1, 2, 3)
      runOne(c, ALU_MULHSU, -2, BigInt("80000000", 16), 3)
      runOne(c, ALU_MULHU, Mask32, Mask32, 3)
    }
  }

  it should "compute RV32M divide and remainder operations after 32 cycles" in {
    test(new MulDivALU()) { c =>
      runOne(c, ALU_DIV, -7, 3, 32)
      runOne(c, ALU_DIVU, BigInt("fffffffe", 16), 2, 32)
      runOne(c, ALU_REM, -7, 3, 32)
      runOne(c, ALU_REMU, BigInt("fffffffe", 16), 3, 32)
      runOne(c, ALU_DIV, 123, 0, 32)
      runOne(c, ALU_DIVU, 123, 0, 32)
      runOne(c, ALU_REM, 123, 0, 32)
      runOne(c, ALU_REMU, 123, 0, 32)
      runOne(c, ALU_DIV, BigInt("80000000", 16), Mask32, 32)
      runOne(c, ALU_REM, BigInt("80000000", 16), Mask32, 32)
    }
  }
}
