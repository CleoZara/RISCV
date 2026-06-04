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
    var perfCycles = BigInt(0)
    var perfInstRetired = BigInt(0)
    var retire0 = BigInt(0)
    var retire1 = BigInt(0)
    var retire2 = BigInt(0)
    var iStall = BigInt(0)
    var dStall = BigInt(0)
    var loadUse = BigInt(0)
    var idRedirect = BigInt(0)
    var exRedirect = BigInt(0)
    var rasPush = BigInt(0)
    var rasPop = BigInt(0)

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
        perfCycles = dut.io.perf.cycles.peekInt()
        perfInstRetired = dut.io.perf.instRetired.peekInt()
        retire0 = dut.io.perf.retire0Cycles.peekInt()
        retire1 = dut.io.perf.retire1Cycles.peekInt()
        retire2 = dut.io.perf.retire2Cycles.peekInt()
        iStall = dut.io.perf.icacheStallCycles.peekInt()
        dStall = dut.io.perf.dcacheStallCycles.peekInt()
        loadUse = dut.io.perf.loadUseStalls.peekInt()
        idRedirect = dut.io.perf.idRedirects.peekInt()
        exRedirect = dut.io.perf.exRedirects.peekInt()
        rasPush = dut.io.perf.rasPushes.peekInt()
        rasPop = dut.io.perf.rasPops.peekInt()
      }

    val out = output.toString
    println(s"\n[dhry] ${if (success) "OK" else "TIMEOUT"} in $cycles cycles")
    println(s"[perf] cycles=$perfCycles instret=$perfInstRetired retire0=$retire0 retire1=$retire1 retire2=$retire2")
    println(s"[perf] istall=$iStall dstall=$dStall loadUse=$loadUse idRedirect=$idRedirect exRedirect=$exRedirect rasPush=$rasPush rasPop=$rasPop")
    withClue(out) {
      success shouldBe true
      out should include ("Execution ends")
      out should include ("Int_1_Loc:           5")
      out should include ("Str_1_Loc:           DHRYSTONE PROGRAM, 1'ST STRING")
    }
  }
}
