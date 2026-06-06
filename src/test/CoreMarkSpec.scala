package riscv

import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File

class CoreMarkSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "CoreMark"

  it should "run coremark.hex" in {
    val f = new File("tests/coremark.hex")
    assume(f.exists(), "tests/coremark.hex not found")

    var success = false
    var cycles = 0
    val output = new StringBuilder
    val maxCycles = 2000000
    val runDir = s"coremark_run_dir_${System.currentTimeMillis()}"
    var perf = PerfSnapshot.zero

    test(new SimTop(f.getAbsolutePath))
      .withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation(runDir))) { dut =>
        dut.clock.setTimeout(maxCycles + 100)
        while (!success && cycles < maxCycles) {
          dut.clock.step()
          cycles += 1
          if (dut.io.success.peekBoolean()) success = true
          if (dut.io.printChar.valid.peekBoolean()) {
            val ch = dut.io.printChar.bits.peekInt().toChar
            output.append(ch)
            print(ch)
          }
        }
        perf = PerfSnapshot.from(dut.io.perf)
      }

    val out = output.toString
    println(s"\n[coremark] ${if (success) "OK" else "TIMEOUT"} in $cycles cycles")
    println(PerfPrinter.line("perf-coremark", PerfPrinter.common(if (success) "OK" else "TIMEOUT", "coremark", perf)))
    withClue(out) {
      success shouldBe true
      out should include ("Correct operation validated")
      out should include ("CoreMark Size")
    }
  }
}
