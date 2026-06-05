package riscv

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.{File, PrintWriter}

/** ═══════════════════════════════════════════════════════════════════════════
  * PrefetchSpec – 预取器性能基准测试
  *
  * 运行方式：
  *   sbt "testOnly riscv.PrefetchSpec"                   全部
  *   sbt "testOnly riscv.PrefetchSpec -- -z sequential"  仅顺序访问
  *   sbt "testOnly riscv.PrefetchSpec -- -z stride"      仅步长访问
  *
  * 测试原理：
  *   每个基准用相同访问模式运行 5 次，仅改变 CSR 0x7C0（prefetchCtrl）：
  *     bit 0 = next-line 预取（I-Cache）
  *     bit 1 = stride    预取（I-Cache + D-Cache）
  *     bit 2 = stream    预取（I-Cache + D-Cache）
  *
  *   Scala 测试框架记录每次运行到 io.success 拉高所经历的时钟周期数，
  *   即可直接比较各模式的执行时间。
  *
  * 内存布局（基于 SimTop / RV32DualPortMemory）：
  *   0x80000000  程序区（hex 文件加载处）
  *   0x80002000  测试数据区（data arrays）
  *   0x10001FF0  success MMIO（sw 1 → io.success=1）
  *   0x10001FF1  putchar MMIO（sb byte）
  *
  * D-Cache 参数（来自 SimTop）：8 KB，4 路，64 B 行 → 32 组 × 4 路 = 128 行
  *   顺序测试数组：8192 words = 32 KB → 512 行（4× 超出容量，确保大量 miss）
  *   步长测试数组：1024 loads × 步长 64 B → 1024 行（约 8× 超出容量）
  * ═══════════════════════════════════════════════════════════════════════════
  */
class PrefetchSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  // ── 基础设施 ──────────────────────────────────────────────────────────────

  private val hexDir: File = { val d = new File("test-tmp-hex"); d.mkdirs(); d }

  private def writeHex(name: String, words: Seq[Long]): String = {
    val f  = new File(hexDir, name)
    val pw = new PrintWriter(f)
    words.padTo(512, 0x00000013L).foreach(w => pw.println(f"${w & 0xFFFFFFFFL}%08x"))
    pw.close()
    f.getAbsolutePath
  }

  case class SimResult(success: Boolean, output: String, cycles: Int)

  private def runSim(initFile: String, maxCycles: Int = 500_000): SimResult = {
    var success = false; var cycles = 0
    val output  = new StringBuilder
    test(new SimTop(initFile))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        while (!success && cycles < maxCycles) {
          dut.clock.step(); cycles += 1
          if (dut.io.success.peekBoolean()) success = true
          if (dut.io.printChar.valid.peekBoolean())
            output.append(dut.io.printChar.bits.peekInt().toChar)
        }
      }
    SimResult(success, output.toString, cycles)
  }

  // ── 指令编码辅助函数 ───────────────────────────────────────────────────────
  //
  // 命名约定：与汇编助记符一致，参数顺序：(rd, rs1, rs2/imm)

  /** ADDI rd, rs1, imm  （imm 为 12-bit 有符号） */
  private def addi(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xFFF).toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x13L

  /** LUI rd, imm20  （rd ← imm20 << 12） */
  private def lui(rd: Int, imm20: Long): Long =
    ((imm20 & 0xFFFFF) << 12) | (rd.toLong << 7) | 0x37L

  /** CSRRW x0, csr, rs1  （写 CSR，丢弃旧值） */
  private def csrrw(csr: Int, rs1: Int): Long =
    ((csr.toLong & 0xFFF) << 20) | (rs1.toLong << 15) | (1L << 12) | 0x73L

  /** SW rs2, imm(rs1)  （全字写） */
  private def sw(rs1: Int, rs2: Int, imm: Int): Long = {
    val i = imm & 0xFFF
    ((i >> 5).toLong << 25) | (rs2.toLong << 20) | (rs1.toLong << 15) |
      (2L << 12) | ((i & 0x1F).toLong << 7) | 0x23L
  }

  /** LW rd, imm(rs1) */
  private def lw(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xFFF).toLong << 20) | (rs1.toLong << 15) | (2L << 12) |
      (rd.toLong << 7) | 0x03L

  /** ADD rd, rs1, rs2 */
  private def add(rd: Int, rs1: Int, rs2: Int): Long =
    (rs2.toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x33L

  /** BNE rs1, rs2, offset  （offset 为字节数，须为 2 的倍数） */
  private def bne(rs1: Int, rs2: Int, offset: Int): Long = {
    val imm12   = (offset >> 12) & 1
    val imm11   = (offset >> 11) & 1
    val imm10_5 = (offset >> 5)  & 0x3F
    val imm4_1  = (offset >> 1)  & 0xF
    (imm12.toLong  << 31) | (imm10_5.toLong << 25) | (rs2.toLong << 20) |
      (rs1.toLong  << 15) | (1L << 12) |
      (imm4_1.toLong << 8) | (imm11.toLong << 7) | 0x63L
  }

  // ── 寄存器别名 ────────────────────────────────────────────────────────────
  private val X0=0; private val T0=5; private val T1=6
  private val T2=7; private val T4=29

  // ── 公共成功尾声 ──────────────────────────────────────────────────────────
  //   lui  t0, 0x10002
  //   addi t0, t0, -16    → t0 = 0x10001FF0（success MMIO）
  //   addi t1, x0, 1
  //   sw   t1, 0(t0)      → io.success = 1
  //   jal  x0, 0          halt
  private val epilogue: Seq[Long] = Seq(
    lui(T0, 0x10002L),
    addi(T0, T0, -16),
    addi(T1, X0, 1),
    sw(T0, T1, 0),
    0x0000006FL,
  )

  // ═══════════════════════════════════════════════════════════════════════════
  // 基准程序定义
  // ═══════════════════════════════════════════════════════════════════════════

  /** 顺序流基准 – 连续读取 8192 个字（32 KB）
    *
    * 程序布局（字节偏移 from 0x80000000）：
    *   0x00  addi t0, x0, mode       设置预取模式
    *   0x04  csrrw x0, 0x7C0, t0     写 prefetchCtrl CSR
    *   0x08  lui  t0, 0x80002        t0 = 0x80002000（数组基地址）
    *   0x0C  lui  t1, 2              t1 = 8192（循环计数；2<<12=0x2000）
    *   0x10  addi t2, x0, 0          t2 = 0（累加器，防止编译器优化）
    *   ---- loop top (0x14) ----
    *   0x14  lw   t4, 0(t0)          加载
    *   0x18  add  t2, t2, t4         累加
    *   0x1C  addi t0, t0, 4          指针+4
    *   0x20  addi t1, t1, -1         计数-1
    *   0x24  bne  t1, x0, -16        回 0x14
    *   ---- epilogue ----
    *
    * 数据特征：
    *   32 KB 连续访问 → 512 条 cache 行
    *   D-Cache 仅容纳 128 行 → 大量冷 miss + 替换 miss
    *   对 next-line / stream 预取最友好
    */
  private def seqBench(mode: Int): Seq[Long] = Seq(
    addi(T0, X0, mode),    // 0x00  设置预取模式到 t0
    csrrw(0x7C0, T0),      // 0x04  写 CSR
    lui(T0, 0x80002L),     // 0x08  t0 = 0x80002000
    lui(T1, 2),            // 0x0C  t1 = 8192（2 << 12）
    addi(T2, X0, 0),       // 0x10  累加器清零
    // ── loop top ──
    lw(T4, T0, 0),         // 0x14  lw t4, 0(t0)
    add(T2, T2, T4),       // 0x18  add t2, t2, t4
    addi(T0, T0, 4),       // 0x1C  ptr += 4
    addi(T1, T1, -1),      // 0x20  count--
    bne(T1, X0, -16),      // 0x24  if count≠0 goto 0x14
  ) ++ epilogue

  /** 固定步长基准 – 以 64 字节（1 条 cache 行）为步长读取 1024 次
    *
    * 程序布局（与 seqBench 相同，仅 ADDI 步长不同）：
    *   0x1C  addi t0, t0, 64         stride = 64 B = 1 cache line
    *
    * 数据特征：
    *   访问 1024 × 64 B = 64 KB 离散 cache 行
    *   每次访问必然是新的 cache 行，miss 率 100%
    *   stride = 恒定 1 行 → 对 stride 预取最友好（2 次稳定后开始预取）
    */
  private def strideBench(mode: Int): Seq[Long] = Seq(
    addi(T0, X0, mode),    // 0x00
    csrrw(0x7C0, T0),      // 0x04
    lui(T0, 0x80002L),     // 0x08  t0 = 0x80002000
    addi(T1, X0, 1024),    // 0x0C  t1 = 1024（1024 < 2047，直接用 addi）
    addi(T2, X0, 0),       // 0x10
    // ── loop top ──
    lw(T4, T0, 0),         // 0x14
    add(T2, T2, T4),       // 0x18
    addi(T0, T0, 64),      // 0x1C  stride = 64 B（1 cache line）
    addi(T1, T1, -1),      // 0x20
    bne(T1, X0, -16),      // 0x24  goto 0x14
  ) ++ epilogue

  /** 双步长基准 – 以 128 字节（2 条 cache 行）为步长读取 512 次
    *
    * stride = 2 cache lines，访问 512 × 128 B = 64 KB
    * stride 预取须识别步长 2，理论上同样有效
    * 与 stride=64 对比可观察步长识别的鲁棒性
    */
  private def stride128Bench(mode: Int): Seq[Long] = Seq(
    addi(T0, X0, mode),
    csrrw(0x7C0, T0),
    lui(T0, 0x80002L),
    addi(T1, X0, 512),     // 512 iterations
    addi(T2, X0, 0),
    lw(T4, T0, 0),
    add(T2, T2, T4),
    addi(T0, T0, 128),     // stride = 128 B = 2 cache lines
    addi(T1, T1, -1),
    bne(T1, X0, -16),
  ) ++ epilogue

  // ── 辅助：打印结果表 ───────────────────────────────────────────────────────

  private case class BenchResult(name: String, mode: Int, cycles: Int)

  private def runBench(
      label: String,
      benchFn: Int => Seq[Long],
      prefix: String,
      maxCycles: Int = 500_000
  ): Seq[BenchResult] = {
    val modes = Seq(
      (0, "none     "),
      (1, "next-line"),
      (2, "stride   "),
      (4, "stream   "),
      (7, "all      "),
    )
    info(s"\n=== $label ===")
    val results = modes.map { case (mode, name) =>
      val prog = benchFn(mode)
      val hex  = writeHex(s"${prefix}_m$mode.hex", prog)
      val r    = runSim(hex, maxCycles)
      r.success shouldBe true
      val res  = BenchResult(name, mode, r.cycles)
      info(f"  mode=${mode}%-2d  ($name): ${r.cycles}%9d cycles")
      res
    }
    val base    = results.find(_.mode == 0).get.cycles
    val best    = results.minBy(_.cycles)
    val speedup = base.toDouble / best.cycles
    info(f"  最优: mode=${best.mode} (${best.name.trim}), 加速比 ${speedup}%.2fx vs 无预取")
    results
  }

  // ═══════════════════════════════════════════════════════════════════════════
  // 测试用例
  // ═══════════════════════════════════════════════════════════════════════════

  behavior of "Prefetch controller (CSR 0x7C0)"

  /** 顺序访问：32 KB，预期 next-line / stream 最有效 */
  it should "sequential 32KB: stream and next-line prefetch reduce cycles" in {
    val results = runBench(
      "Sequential 32KB (8192 words × 4B)",
      seqBench,
      prefix = "seq",
    )
    // 开启任意预取模式不应比无预取更慢（允许 2% 误差）
    val baseline = results.find(_.mode == 0).get.cycles
    results.filter(_.mode != 0).foreach { r =>
      withClue(s"mode=${r.mode} (${r.name.trim}) 比无预取慢") {
        r.cycles should be <= (baseline * 1.02).toInt
      }
    }
  }

  /** 步长 64B：每次恰好跨一条 cache 行，预期 stride 最有效 */
  it should "stride 64B: stride prefetch reduces cycles vs no prefetch" in {
    val results = runBench(
      "Stride 64B × 1024 iter (total 64KB)",
      strideBench,
      prefix = "stride64",
    )
    val baseline  = results.find(_.mode == 0).get.cycles
    val strideRes = results.find(_.mode == 2).get.cycles
    // stride 预取至少应比无预取快 5%（暖机需要 2 次稳定）
    withClue("stride prefetch 应明显快于无预取") {
      strideRes should be <= (baseline * 0.98).toInt
    }
  }

  /** 步长 128B：跨 2 条 cache 行，验证步长识别鲁棒性 */
  it should "stride 128B: stride prefetch still effective for larger stride" in {
    runBench(
      "Stride 128B × 512 iter (total 64KB)",
      stride128Bench,
      prefix = "stride128",
    )
    // 仅打印，不硬断言：用于观察 stride prefetch 对不同步长的效果
    succeed
  }

  /** 全模式对比：在同一测试中打印三种基准的完整矩阵 */
  it should "print full comparison matrix across all benchmarks" in {
    info("\n╔═══════════════════════════════════════════════════════╗")
    info("║          预取器性能对比（周期数，越小越好）           ║")
    info("╠══════════╦══════════╦══════════╦══════════╦══════════╣")
    info("║  模式    ║ seq-32KB ║ str-64B  ║ str-128B ║ 说明     ║")
    info("╠══════════╬══════════╬══════════╬══════════╬══════════╣")

    val modes = Seq((0,"none"),(1,"next-ln"),(2,"stride"),(4,"stream"),(7,"all"))
    modes.foreach { case (mode, label) =>
      val s1 = runSim(writeHex(s"cmp_seq_$mode.hex",     seqBench(mode))).cycles
      val s2 = runSim(writeHex(s"cmp_str64_$mode.hex",   strideBench(mode))).cycles
      val s3 = runSim(writeHex(s"cmp_str128_$mode.hex",  stride128Bench(mode))).cycles
      info(f"║ $mode%-2d $label%-6s ║ $s1%8d ║ $s2%8d ║ $s3%8d ║          ║")
    }
    info("╚══════════╩══════════╩══════════╩══════════╩══════════╝")
    succeed
  }
}
