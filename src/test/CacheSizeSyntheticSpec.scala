package riscv

import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parameterized_cache.CacheParams
import java.io.File

class CacheSizeSyntheticSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "CacheSizeSynthetic"

  private case class CacheMode(name: String, iCacheKb: Int, dCacheKb: Int)
  private case class Bench(name: String, words: Seq[Long], maxCycles: Int = 500000)
  private case class SimResult(success: Boolean, cycles: Int, output: String, perf: PerfSnapshot)

  private val modes = Seq(
    CacheMode("4KB", 4, 4),
    CacheMode("8KB", 8, 8),
    CacheMode("16KB", 16, 16),
    CacheMode("32KB", 32, 32),
    CacheMode("I32_D16", 32, 16),
    CacheMode("I16_D32", 16, 32)
  )

  private val hexDir: File = {
    val d = new File("test-tmp-hex")
    d.mkdirs()
    d
  }

  import SyntheticAsm._

  private def loadLoop(countWords: Int, strideBytes: Int): Seq[Long] = {
    val countInit =
      if (countWords <= 2047) Seq(addi(T1, X0, countWords))
      else Seq(lui(T1, 1), addi(T1, T1, countWords - 4096))

    Seq(lui(T0, 0x80002L)) ++ countInit ++ Seq(
      addi(T2, X0, 0),
      lw(T4, T0, 0),
      add(T2, T2, T4),
      addi(T0, T0, strideBytes),
      addi(T1, T1, -1),
      bne(T1, X0, -16)
    )
  }

  private def seq8KbBench: Seq[Long] =
    loadLoop(2048, 4) ++ successEpilogue

  private def stride64Bench: Seq[Long] =
    loadLoop(1024, 64) ++ successEpilogue

  private def capacity24KbBench: Seq[Long] = {
    val onePass = Seq(
      lui(T0, 0x80002L),
      addi(T1, X0, 384),
      lw(T4, T0, 0),
      add(T2, T2, T4),
      addi(T0, T0, 64),
      addi(T1, T1, -1),
      bne(T1, X0, -16)
    )

    Seq(addi(T2, X0, 0)) ++ onePass ++ onePass ++ successEpilogue
  }

  private def icacheStreamBench: Seq[Long] = {
    val bodyLen = 6144
    val body = Seq.fill(bodyLen)(addi(T2, T2, 0))
    Seq(
      addi(T0, X0, 3),
      addi(T2, X0, 0)
    ) ++ body ++ Seq(
      addi(T0, T0, -1),
      beq(T0, X0, 8),
      jal(X0, -((bodyLen + 2) * 4))
    ) ++ successEpilogue
  }

  private val benches = Seq(
    Bench("seq_8kb", seq8KbBench),
    Bench("stride_64b", stride64Bench),
    Bench("capacity_24kb", capacity24KbBench),
    Bench("icache_stream", icacheStreamBench, maxCycles = 800000)
  )

  private def runSim(initFile: String, mode: CacheMode, bench: Bench): SimResult = {
    var success = false
    var cycles = 0
    val output = new StringBuilder
    val iParams = CacheParams(32, 32, mode.iCacheKb * 1024, 4, 64)
    val dParams = CacheParams(32, 32, mode.dCacheKb * 1024, 4, 64)
    val runDir = s"cache_synth_${bench.name}_${mode.name}_${System.currentTimeMillis()}"
    var perf = PerfSnapshot.zero

    test(new SimTop(initFile, cacheParams = iParams, dCacheParams = Some(dParams)))
      .withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation(runDir))) { dut =>
        dut.clock.setTimeout(bench.maxCycles + 100)
        while (!success && cycles < bench.maxCycles) {
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

  private def checkCacheSanity(p: PerfSnapshot): Unit = {
    p.cycles should be > BigInt(0)
    p.instRetired should be > BigInt(0)
    (p.icacheHits + p.icacheMisses) shouldBe p.icacheAccesses
    (p.dcacheHits + p.dcacheMisses) should be <= (p.dcacheLoads + p.dcacheStores)
  }

  benches.foreach { bench =>
    it should s"compare cache sizes on ${bench.name}" in {
      modes.foreach { mode =>
        val hex = writeHex(hexDir, s"cache_${bench.name}_${mode.name}.hex", bench.words)
        val r = runSim(hex, mode, bench)
        withClue(s"bench=${bench.name} mode=${mode.name} output='${r.output}' cycles=${r.cycles}") {
          r.success shouldBe true
          checkCacheSanity(r.perf)
        }
        println(PerfPrinter.line(
          "perf-cache-synth",
          PerfPrinter.common(if (r.success) "OK" else "TIMEOUT", "cache_synth", r.perf) ++ Seq(
            "mode" -> mode.name,
            "bench" -> bench.name,
            "iCacheKB" -> mode.iCacheKb,
            "dCacheKB" -> mode.dCacheKb,
            "iWay" -> 4,
            "dWay" -> 4,
            "simCycles" -> r.cycles)))
      }
      succeed
    }
  }
}
