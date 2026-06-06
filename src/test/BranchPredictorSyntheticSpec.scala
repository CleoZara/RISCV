package riscv

import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File

class BranchPredictorSyntheticSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "BranchPredictorSynthetic"

  private case class Mode(name: String, value: Int)
  private case class Bench(name: String, words: Seq[Long])
  private case class SimResult(success: Boolean, cycles: Int, output: String, perf: PerfSnapshot)

  private val modes = Seq(
    Mode("bpu", 0),
    Mode("bpu_ras", 1),
    Mode("tage", 2)
  )

  private val hexDir: File = {
    val d = new File("test-tmp-hex")
    d.mkdirs()
    d
  }

  import SyntheticAsm._

  private def loopTakenBench: Seq[Long] = Seq(
    addi(T0, X0, 512),
    addi(T2, X0, 0),
    addi(T2, T2, 1),
    addi(T0, T0, -1),
    bne(T0, X0, -8)
  ) ++ successEpilogue

  private def alternatingBench: Seq[Long] = Seq(
    addi(T0, X0, 512),
    addi(T1, X0, 0),
    bne(T1, X0, 12),
    addi(T1, X0, 1),
    beq(X0, X0, 8),
    addi(T1, X0, 0),
    addi(T0, T0, -1),
    bne(T0, X0, -20)
  ) ++ successEpilogue

  private def returnChainBench: Seq[Long] = Seq(
    addi(T0, X0, 256),
    addi(T2, X0, 0),
    jal(RA, 32),
    addi(T0, T0, -1),
    bne(T0, X0, -8)
  ) ++ successEpilogue ++
    Seq.fill(16)(addi(T2, T2, 1)) ++ Seq(
    jalr(X0, RA, 0)
  )

  private val benches = Seq(
    Bench("loop_taken", loopTakenBench),
    Bench("alternating", alternatingBench),
    Bench("return_chain", returnChainBench)
  )

  private def runSim(initFile: String, mode: Mode, bench: Bench): SimResult = {
    var success = false
    var cycles = 0
    val output = new StringBuilder
    val runDir = s"branch_synth_${bench.name}_${mode.name}_${System.currentTimeMillis()}"
    var perf = PerfSnapshot.zero

    test(new SimTop(initFile, branchPredInit = mode.value))
      .withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation(runDir))) { dut =>
        dut.clock.setTimeout(200000)
        while (!success && cycles < 200000) {
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

    SimResult(success, cycles, output.toString, perf)
  }

  private def checkBranchSanity(p: PerfSnapshot): Unit = {
    p.cycles should be > BigInt(0)
    p.instRetired should be > BigInt(0)
    (p.branchCorrect + p.branchMispredicts) should be <= p.branchInsts
    (p.branchDirectionMispredicts + p.branchTargetMispredicts) should be <= p.branchMispredicts
  }

  benches.foreach { bench =>
    it should s"compare branch predictors on ${bench.name}" in {
      modes.foreach { mode =>
        val hex = writeHex(hexDir, s"branch_${bench.name}_${mode.name}.hex", bench.words)
        val r = runSim(hex, mode, bench)
        withClue(s"bench=${bench.name} mode=${mode.name} output='${r.output}' cycles=${r.cycles}") {
          r.success shouldBe true
          checkBranchSanity(r.perf)
        }
        println(PerfPrinter.line(
          "perf-bpred-synth",
          PerfPrinter.common(if (r.success) "OK" else "TIMEOUT", "branch_synth", r.perf) ++ Seq(
            "mode" -> mode.name,
            "bench" -> bench.name,
            "simCycles" -> r.cycles)))
      }
      succeed
    }
  }
}
