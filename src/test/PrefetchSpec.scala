package riscv

import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.{File, PrintWriter}

class PrefetchSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Prefetch controller CSR 0x7C0"

  private case class SimResult(success: Boolean, output: String, cycles: Int, perf: PerfSnapshot)
  private case class BenchResult(name: String, mode: Int, cycles: Int, perf: PerfSnapshot)

  private val modes = Seq(
    (0, "none"),
    (1, "next-line"),
    (2, "stride"),
    (4, "stream")
  )

  private val X0 = 0
  private val T0 = 5
  private val T1 = 6
  private val T2 = 7
  private val T4 = 29

  private val hexDir: File = {
    val d = new File("test-tmp-hex")
    d.mkdirs()
    d
  }

  private def writeHex(name: String, words: Seq[Long]): String = {
    val f = new File(hexDir, name)
    val pw = new PrintWriter(f)
    try {
      words.padTo(512, 0x00000013L).foreach { w =>
        pw.println(f"${w & 0xFFFFFFFFL}%08x")
      }
    } finally {
      pw.close()
    }
    f.getAbsolutePath
  }

  private def runSim(initFile: String, maxCycles: Int = 500000): SimResult = {
    var success = false
    var cycles = 0
    val output = new StringBuilder
    val label = new File(initFile).getName.stripSuffix(".hex")
    val runDir = s"prefetch_${label}_${System.currentTimeMillis()}"
    var perf = PerfSnapshot.zero

    test(new SimTop(initFile))
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

    SimResult(success, output.toString, cycles, perf)
  }

  private def addi(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xFFF).toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x13L

  private def lui(rd: Int, imm20: Long): Long =
    ((imm20 & 0xFFFFF) << 12) | (rd.toLong << 7) | 0x37L

  private def csrrw(csr: Int, rs1: Int): Long =
    ((csr.toLong & 0xFFF) << 20) | (rs1.toLong << 15) | (1L << 12) | 0x73L

  private def sw(rs1: Int, rs2: Int, imm: Int): Long = {
    val i = imm & 0xFFF
    ((i >> 5).toLong << 25) | (rs2.toLong << 20) | (rs1.toLong << 15) |
      (2L << 12) | ((i & 0x1F).toLong << 7) | 0x23L
  }

  private def lw(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xFFF).toLong << 20) | (rs1.toLong << 15) | (2L << 12) |
      (rd.toLong << 7) | 0x03L

  private def add(rd: Int, rs1: Int, rs2: Int): Long =
    (rs2.toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x33L

  private def bne(rs1: Int, rs2: Int, offset: Int): Long = {
    val imm12 = (offset >> 12) & 1
    val imm11 = (offset >> 11) & 1
    val imm10_5 = (offset >> 5) & 0x3F
    val imm4_1 = (offset >> 1) & 0xF
    (imm12.toLong << 31) | (imm10_5.toLong << 25) | (rs2.toLong << 20) |
      (rs1.toLong << 15) | (1L << 12) |
      (imm4_1.toLong << 8) | (imm11.toLong << 7) | 0x63L
  }

  private val epilogue: Seq[Long] = Seq(
    lui(T0, 0x10002L),
    addi(T0, T0, -16),
    addi(T1, X0, 1),
    sw(T0, T1, 0),
    0x0000006FL
  )

  private def seqBench(mode: Int): Seq[Long] = Seq(
    addi(T0, X0, mode),
    csrrw(0x7C0, T0),
    lui(T0, 0x80002L),
    lui(T1, 2),
    addi(T2, X0, 0),
    lw(T4, T0, 0),
    add(T2, T2, T4),
    addi(T0, T0, 4),
    addi(T1, T1, -1),
    bne(T1, X0, -16)
  ) ++ epilogue

  private def strideBench(mode: Int): Seq[Long] = Seq(
    addi(T0, X0, mode),
    csrrw(0x7C0, T0),
    lui(T0, 0x80002L),
    addi(T1, X0, 1024),
    addi(T2, X0, 0),
    lw(T4, T0, 0),
    add(T2, T2, T4),
    addi(T0, T0, 64),
    addi(T1, T1, -1),
    bne(T1, X0, -16)
  ) ++ epilogue

  private def stride128Bench(mode: Int): Seq[Long] = Seq(
    addi(T0, X0, mode),
    csrrw(0x7C0, T0),
    lui(T0, 0x80002L),
    addi(T1, X0, 512),
    addi(T2, X0, 0),
    lw(T4, T0, 0),
    add(T2, T2, T4),
    addi(T0, T0, 128),
    addi(T1, T1, -1),
    bne(T1, X0, -16)
  ) ++ epilogue

  private def runBench(
      label: String,
      benchFn: Int => Seq[Long],
      prefix: String,
      maxCycles: Int = 500000
  ): Seq[BenchResult] = {
    info(s"")
    info(s"=== $label ===")
    val results = modes.map { case (mode, name) =>
      val hex = writeHex(s"${prefix}_m$mode.hex", benchFn(mode))
      val r = runSim(hex, maxCycles)
      withClue(s"$label mode=$mode ($name) output='${r.output}' cycles=${r.cycles}") {
        r.success shouldBe true
      }
      val res = BenchResult(name, mode, r.cycles, r.perf)
      println(PerfPrinter.line(
        "perf-prefetch",
        PerfPrinter.common(if (r.success) "OK" else "TIMEOUT", prefix, r.perf) ++ Seq(
          "mode" -> name,
          "bench" -> label,
          "simCycles" -> r.cycles)))
      info(f"  mode=$mode%-2d ($name%-9s) cycles=${r.cycles}%9d")
      res
    }

    val baseline = results.find(_.mode == 0).get.cycles
    val best = results.minBy(_.cycles)
    val speedup = baseline.toDouble / best.cycles.toDouble
    info(f"  best: mode=${best.mode} (${best.name}), speedup=${speedup}%.4fx vs none")
    results
  }

  it should "sequential 32KB prefetch comparison" in {
    runBench("sequential 32KB", seqBench, "seq")
    succeed
  }

  it should "stride 64B prefetch comparison" in {
    runBench("stride 64B", strideBench, "stride64")
    succeed
  }

  it should "stride 128B prefetch comparison" in {
    runBench("stride 128B", stride128Bench, "stride128")
    succeed
  }

  it should "matrix prefetch comparison" in {
    val seq = runBench("matrix sequential 32KB", seqBench, "matrix_seq")
    val str64 = runBench("matrix stride 64B", strideBench, "matrix_stride64")
    val str128 = runBench("matrix stride 128B", stride128Bench, "matrix_stride128")

    info("")
    info("mode       seq32       stride64    stride128")
    modes.foreach { case (mode, name) =>
      val s1 = seq.find(_.mode == mode).get.cycles
      val s2 = str64.find(_.mode == mode).get.cycles
      val s3 = str128.find(_.mode == mode).get.cycles
      info(f"$mode%-2d $name%-9s $s1%10d $s2%10d $s3%10d")
    }
    succeed
  }
}
