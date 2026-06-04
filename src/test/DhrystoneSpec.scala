package riscv

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File

class DhrystoneSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Dhrystone"

  it should "run dhrystone.hex" in {
    val f = new File("tests/dhrystone.hex")
    assume(f.exists(), "tests/dhrystone.hex not found")

    var success = false
    var cycles = 0
    val output = new StringBuilder
    val maxCycles = 500000
    val runDir = s"dhrystone_run_dir_${System.currentTimeMillis()}"

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
      }

    val out = output.toString
    println(s"\n[dhry] ${if (success) "OK" else "TIMEOUT"} in $cycles cycles")
    withClue(out) {
      success shouldBe true
      out should include ("Execution ends")
      out should include ("Int_1_Loc:           5")
      out should include ("Str_1_Loc:           DHRYSTONE PROGRAM, 1'ST STRING")
    }
  }
}
