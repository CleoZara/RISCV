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
    var perfCycles = BigInt(0)
    var perfInstRetired = BigInt(0)
    var iStall = BigInt(0)
    var dStall = BigInt(0)
    var loadUse = BigInt(0)
    var idRedirect = BigInt(0)
    var exRedirect = BigInt(0)
    var rasPush = BigInt(0)
    var rasPop = BigInt(0)

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

        perfCycles = dut.io.perf.cycles.peekInt()
        perfInstRetired = dut.io.perf.instRetired.peekInt()
        iStall = dut.io.perf.icacheStallCycles.peekInt()
        dStall = dut.io.perf.dcacheStallCycles.peekInt()
        loadUse = dut.io.perf.loadUseStalls.peekInt()
        idRedirect = dut.io.perf.idRedirects.peekInt()
        exRedirect = dut.io.perf.exRedirects.peekInt()
        rasPush = dut.io.perf.rasPushes.peekInt()
        rasPop = dut.io.perf.rasPops.peekInt()
      }

    val cpi = BigDecimal(perfCycles) / BigDecimal(perfInstRetired)
    val out = output.toString
    val dhryCycles = "Cycles spent for 10 iterations dhrystone:\\s*(\\d+)".r
      .findFirstMatchIn(out).map(_.group(1)).getOrElse("0")
    val dhryInstRetired = "Instructions retired for 10 iterations dhrystone:\\s*(\\d+)".r
      .findFirstMatchIn(out).map(_.group(1)).getOrElse("0")
    val dhryIpc =
      if (BigInt(dhryCycles) != 0) BigDecimal(BigInt(dhryInstRetired)) / BigDecimal(BigInt(dhryCycles))
      else BigDecimal(0)
    println(
      f"[bpred-${mode.name}] cycles=$perfCycles instret=$perfInstRetired cpi=$cpi%.4f " +
      s"istall=$iStall dstall=$dStall loadUse=$loadUse idRedirect=$idRedirect exRedirect=$exRedirect rasPush=$rasPush rasPop=$rasPop")
    println(f"[bpred-${mode.name}-dhry] cycles=$dhryCycles instret=$dhryInstRetired ipc=$dhryIpc%.4f")

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
