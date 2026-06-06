package riscv

import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File

class PrefetchDhrystoneSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "PrefetchDhrystone"

  private case class Mode(name: String, value: Int)

  private val modes = Seq(
    Mode("none", 0),
    Mode("next-line", 1),
    Mode("stride", 2),
    Mode("stream", 4)
  )

  private def runMode(mode: Mode): Unit = {
    val f = new File("tests/dhrystone.hex")
    assume(f.exists(), "tests/dhrystone.hex not found")

    var success = false
    var cycles = 0
    val output = new StringBuilder
    val maxCycles = 500000
    val runDir = s"target/dhrystone_prefetch_${mode.name.replace("-", "_")}_${System.currentTimeMillis()}"
    var perf = PerfSnapshot.zero

    test(new SimTop(f.getAbsolutePath, prefetchInit = mode.value))
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
      "perf-prefetch",
      PerfPrinter.commonDhrystone(if (success) "OK" else "TIMEOUT", perf, out) ++ Seq(
        "mode" -> mode.name,
        "dhryCycles" -> dhryCycles,
        "dhryInstRetired" -> dhryInstRetired,
        "dhryIPC" -> dhryIpc)))
    println(s"[prefetch-${mode.name}-dhry] cycles=$dhryCycles instret=$dhryInstRetired ipc=$dhryIpc")

    withClue(out) {
      success shouldBe true
      out should include ("Execution ends")
    }
  }

  modes.foreach { mode =>
    it should s"run Dhrystone with ${mode.name} prefetch" in {
      runMode(mode)
    }
  }
}
