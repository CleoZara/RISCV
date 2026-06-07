package riscv

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import AluOp._
import SyntheticAsm._

class RV32MSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
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

  private def li(rd: Int, value: BigInt): Seq[Long] = {
    val v = u32(value)
    val upper = ((v + 0x800) >> 12) & 0xFFFFF
    val lowerRaw = (v & 0xFFF).toInt
    val lower = if (lowerRaw >= 0x800) lowerRaw - 0x1000 else lowerRaw
    if (upper == 0) Seq(addi(rd, X0, lower)) else Seq(lui(rd, upper.toLong), addi(rd, rd, lower))
  }

  private def buildProgram: Seq[Long] = {
    val p = collection.mutable.ArrayBuffer[Long]()

    def emit(xs: Seq[Long]): Unit = p ++= xs
    def alignPair(): Unit = if (p.length % 2 != 0) p += addi(X0, X0, 0)
    def checkReg(actual: Int, expectedReg: Int): Unit = {
      p += beq(actual, expectedReg, 8)
      p += addi(T4, T4, 1)
    }
    def check(op: UInt, inst: (Int, Int, Int) => Long, a: BigInt, b: BigInt): Unit = {
      emit(li(T0, a))
      emit(li(T1, b))
      p += inst(T2, T0, T1)
      emit(li(T3, expected(op, a, b)))
      checkReg(T2, T3)
    }

    p += addi(T4, X0, 0)

    check(ALU_MUL, mul, -3, 7)
    check(ALU_MULH, mulh, -1, 2)
    check(ALU_MULHSU, mulhsu, -2, BigInt("80000000", 16))
    check(ALU_MULHU, mulhu, Mask32, Mask32)
    check(ALU_DIV, div, -7, 3)
    check(ALU_DIVU, divu, BigInt("fffffffe", 16), 2)
    check(ALU_REM, rem, -7, 3)
    check(ALU_REMU, remu, BigInt("fffffffe", 16), 3)
    check(ALU_DIV, div, 123, 0)
    check(ALU_DIVU, divu, 123, 0)
    check(ALU_REM, rem, 123, 0)
    check(ALU_REMU, remu, 123, 0)
    check(ALU_DIV, div, BigInt("80000000", 16), Mask32)
    check(ALU_REM, rem, BigInt("80000000", 16), Mask32)

    emit(li(T0, 6))
    emit(li(T1, 7))
    alignPair()
    p += mul(T2, T0, T1)
    p += add(T3, T2, T1)
    emit(li(S0, 49))
    checkReg(T3, S0)

    emit(li(T0, -81))
    emit(li(T1, 9))
    p += div(T2, T0, T1)
    p += rem(T3, T0, T1)
    emit(li(S0, -9))
    checkReg(T2, S0)
    emit(li(S0, 0))
    checkReg(T3, S0)

    emitFinish(p)
    p.toSeq
  }

  private def emitFinish(p: collection.mutable.ArrayBuffer[Long]): Unit = {
    p += beq(T4, X0, 8)
    p += jal(X0, 0)
    p ++= successEpilogue
  }

  private def buildSmokeProgram: Seq[Long] = {
    val p = collection.mutable.ArrayBuffer[Long]()
    p += addi(T4, X0, 0)
    p ++= li(T0, 6)
    p ++= li(T1, 7)
    p += mul(T2, T0, T1)
    p ++= li(T3, 42)
    p += beq(T2, T3, 8)
    p += addi(T4, T4, 1)
    emitFinish(p)
    p.toSeq
  }

  private def runProgram(name: String, program: Seq[Long], maxCycles: Int): Unit = {
    val dir = new File(s"rv32m_${name}_run_dir_${System.currentTimeMillis()}")
    val initFile = SyntheticAsm.writeHex(dir, s"$name.hex", program, minWords = 512)
    val targetDir = s"rv32m_${name}_test_dir_${System.currentTimeMillis()}"

    test(new SimTop(initFile, enableRV32M = true))
      .withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation(targetDir))) { c =>
        c.clock.setTimeout(maxCycles + 100)
        var cycles = 0
        while (!c.io.success.peek().litToBoolean && cycles < maxCycles) {
          c.clock.step()
          cycles += 1
        }
        val success = c.io.success.peek().litToBoolean
        val perf = PerfSnapshot.from(c.io.perf)
        println(PerfPrinter.line("perf-rv32m", PerfPrinter.common(if (success) "OK" else "TIMEOUT", name, perf)))
        withClue(s"program=$name cycles=$cycles") {
          success shouldBe true
        }
      }
  }

  behavior of "RV32M"

  it should "complete an RV32M smoke multiply program" in {
    runProgram("rv32m_smoke", buildSmokeProgram, maxCycles = 2000)
  }

  it should "run a synthetic RV32M program with multi-cycle MulDivALU" in {
    runProgram("rv32m", buildProgram, maxCycles = 20000)
  }
}
