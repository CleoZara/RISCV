package riscv

import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parameterized_cache.CacheParams
import java.io.File

class CacheSizeDhrystoneSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "ParameterizedCacheDhrystone"

  private def runDhry(cacheKb: Int, dCacheKb: Option[Int] = None): Unit = {
    val f = new File("tests/dhrystone.hex")
    assume(f.exists(), "tests/dhrystone.hex not found")

    var success = false
    var cycles = 0
    val output = new StringBuilder
    val maxCycles = 500000
    val p = CacheParams(32, 32, cacheKb * 1024, 4, 64)
    val dp = dCacheKb.map(kb => CacheParams(32, 32, kb * 1024, 4, 64))
    val label = dCacheKb.map(kb => s"I${cacheKb}KB_D${kb}KB").getOrElse(s"${cacheKb}KB")
    val runDir = dCacheKb
      .map(kb => s"dhrystone_cache_i${cacheKb}kb_d${kb}kb_${System.currentTimeMillis()}")
      .getOrElse(s"dhrystone_cache_${cacheKb}kb_${System.currentTimeMillis()}")

    test(new SimTop(f.getAbsolutePath, cacheParams = p, dCacheParams = dp))
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
        println(s"[cache-$label] ${if (success) "OK" else "TIMEOUT"} in $cycles cycles")
        println(s"[cache-$label] outputTail='${output.toString.takeRight(80)}'")
        println(s"[cache-$label] perf cycles=${dut.io.perf.cycles.peekInt()} instret=${dut.io.perf.instRetired.peekInt()} istall=${dut.io.perf.icacheStallCycles.peekInt()} dstall=${dut.io.perf.dcacheStallCycles.peekInt()} loadUse=${dut.io.perf.loadUseStalls.peekInt()}")
      }

    withClue(output.toString) {
      success shouldBe true
      output.toString should include ("Execution ends")
    }
  }

  for (kb <- Seq(4, 8, 16, 32)) {
    it should s"run Dhrystone with ${kb}KB I/D caches" in {
      runDhry(kb)
    }
  }

  it should "run Dhrystone I32_D16" in {
    runDhry(32, Some(16))
  }

  it should "run Dhrystone I16_D32" in {
    runDhry(16, Some(32))
  }
}
