package riscv

import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File

class BranchPredictorDhrystoneSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "BranchPredictorDhrystone"

  private case class Mode(name: String, value: Int)

  private def runMode(mode: Mode): Unit = {
    val f = new File("tests/dhrystone.hex")
    assume(f.exists(), "tests/dhrystone.hex not found")

    var success = false
    var cycles = 0
    val output = new StringBuilder
    val maxCycles = 500000
    val runDir = s"dhrystone_bpred_${mode.name}_${System.currentTimeMillis()}"
    var perf = PerfSnapshot.zero

    test(new SimTop(f.getAbsolutePath, branchPredInit = mode.value))
      .withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation(runDir))) { dut =>
        dut.clock.setTimeout(maxCycles + 100)
        while (!success && cycles < maxCycles) {
          dut.clock.step()
          cycles += 1
          if (dut.io.success.peekBoolean()) {
            success = true
          }
          if (dut.io.printChar.valid.peekBoolean()) {
            output.append(dut.io.printChar.bits.peekInt().toChar)
          }
        }

        perf = PerfSnapshot.from(dut.io.perf)
      }

    val out = output.toString
    val (dhryCycles, dhryInstRetired) = PerfPrinter.dhrystoneMetrics(out).getOrElse((BigInt(0), BigInt(0)))
    val dhryIpc = PerfPrinter.ratio(dhryInstRetired, dhryCycles)
    println(PerfPrinter.line(
      "perf-bpred",
      PerfPrinter.commonDhrystone(if (success) "OK" else "TIMEOUT", perf, out) ++ Seq(
        "mode" -> mode.name,
        "dhryCycles" -> dhryCycles,
        "dhryInstRetired" -> dhryInstRetired,
        "dhryIPC" -> dhryIpc)))
    println(s"[bpred-${mode.name}-dhry] cycles=$dhryCycles instret=$dhryInstRetired ipc=$dhryIpc")

    withClue(out) {
      success shouldBe true
      out should include ("Execution ends")
    }
  }

  Seq(
    Mode("bpu", 0),
    Mode("bpu_ras", 1),
    Mode("tage", 2)
  ).foreach { mode =>
    it should s"run Dhrystone with ${mode.name}" in {
      runMode(mode)
    }
  }
}
