package riscv

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.{File, PrintWriter}

/** ═══════════════════════════════════════════════════════════════════════════
  * TopSpec – sbt test / Verilator simulation for the RV32I superscalar core
  *
  * Run all:         sbt test
  * Run one:         sbt "testOnly riscv.TopSpec -- -z smoke"
  * Generate waves:  add WriteVcdAnnotation to the Seq below
  *
  * Requires Verilator on PATH.
  *   Ubuntu: sudo apt install verilator
  *   macOS:  brew install verilator
  *
  * Memory map:
  *   0x80000000  program (loaded from hex file)
  *   0x80001000  data scratch area
  *   0x10001FF0  success MMIO  (sw 1 → io.success=1)
  *   0x10001FF1  putchar MMIO  (sb char)
  *   0x0000BFF8  mtime MMIO    (lw)
  * ═══════════════════════════════════════════════════════════════════════════
  */
class TopSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  // ── Infrastructure ───────────────────────────────────────────────────────

  private val hexDir: File = { val d = new File("test-tmp-hex"); d.mkdirs(); d }

  /** Write words as a plain hex file (one 32-bit word per line, no @address).
    * Padded to 512 words with NOP (0x00000013) to keep Verilator happy.
    * Returns the absolute path so $readmemh can find it.
    */
  private def writeHex(name: String, words: Seq[Long]): String = {
    val f  = new File(hexDir, name)
    val pw = new PrintWriter(f)
    words.padTo(512, 0x00000013L)
         .foreach(w => pw.println(f"${w & 0xFFFFFFFFL}%08x"))
    pw.close()
    f.getAbsolutePath
  }

  case class SimResult(success: Boolean, output: String, cycles: Int)

  /** Step the Verilator simulation until io.success rises or maxCycles expires. */
  private def runSim(initFile: String,
                     maxCycles: Int = 5000,
                     verbose: Boolean = false): SimResult = {
    var success = false; var cycles = 0
    val output  = new StringBuilder
    var outputTruncated = false
    val runDir = s"test_run_dir_${System.currentTimeMillis()}_${math.abs(initFile.hashCode)}"
    test(new SimTop(initFile))
      .withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation(runDir))) { dut =>
        dut.clock.setTimeout(maxCycles + 100)
        while (!success && cycles < maxCycles) {
          dut.clock.step(); cycles += 1
          if (dut.io.success.peekBoolean()) success = true
          if (dut.io.printChar.valid.peekBoolean()) {
            val ch = dut.io.printChar.bits.peekInt().toChar
            if (output.length < 65536) {
              output.append(ch)
              if (verbose) print(ch)
            } else if (!outputTruncated) {
              output.append("\n[output truncated]\n")
              if (verbose) print("\n[output truncated]\n")
              outputTruncated = true
              cycles = maxCycles
            }
          }
        }
        if (verbose) println(s"\n[sim] ${if (success) "OK" else "TIMEOUT"} in $cycles cycles")
      }
    SimResult(success, output.toString, cycles)
  }

  // ── Machine-code programs ────────────────────────────────────────────────
  //
  // All encodings computed for RV32I little-endian.
  // Register aliases: t0=x5  t1=x6  t2=x7  t3=x28  ra=x1
  //
  // Common success epilogue (reused by every program):
  //   lui  t0, 0x10002       → t0 = 0x10002000
  //   addi t0, t0, -16       → t0 = 0x10001FF0  (success MMIO)
  //   addi t1, x0, 1
  //   sw   t1, 0(t0)         → io.success = 1
  //   jal  x0, 0             (halt – self loop)
  //
  // The same FAIL label is always a jal x0,0 (infinite loop) that the
  // test harness will detect as a timeout.

  // ── T1: Smoke (ALU + branch + success MMIO) ──────────────────────────────
  // 5 + 3 = 8, compare, assert success
  //   0x00 addi t0, x0, 5
  //   0x04 addi t1, x0, 3
  //   0x08 add  t2, t0, t1         t2=8
  //   0x0C addi t3, x0, 8
  //   0x10 bne  t2, t3, +24        → 0x28 (fail) if t2≠8
  //   0x14..0x20  success epilogue
  //   0x24 halt
  //   0x28 halt (fail)
  private val smokeProg = Seq(
    0x00500293L, // addi t0, x0, 5
    0x00300313L, // addi t1, x0, 3
    0x006283B3L, // add  t2, t0, t1
    0x00800E13L, // addi t3, x0, 8
    0x01C39C63L, // bne  t2, t3, +24
    0x100022B7L, // lui  t0, 0x10002
    0xFF028293L, // addi t0, t0, -16       t0=0x10001FF0
    0x00100313L, // addi t1, x0, 1
    0x0062A023L, // sw   t1, 0(t0)         SUCCESS
    0x0000006FL, // halt
    0x0000006FL, // fail-halt
  )

  // ── T2: Printf putchar ────────────────────────────────────────────────────
  // Print "Hi\n" via MMIO at 0x10001FF1 (SB), then assert success.
  //   0x10001FF1 = 0x10002000 - 15  →  lui 0x10002; addi -15
  //   0x10001FF0 = 0x10002000 - 16
  private val putcharProg = Seq(
    0x100022B7L, // lui  t0, 0x10002
    0xFF128293L, // addi t0, t0, -15       t0=0x10001FF1 (putchar)
    0x04800313L, // addi t1, x0, 72        'H'
    0x00628023L, // sb   t1, 0(t0)
    0x06900313L, // addi t1, x0, 105       'i'
    0x00628023L, // sb   t1, 0(t0)
    0x00A00313L, // addi t1, x0, 10        '\n'
    0x00628023L, // sb   t1, 0(t0)
    0x100022B7L, // lui  t0, 0x10002
    0xFF028293L, // addi t0, t0, -16       t0=0x10001FF0
    0x00100313L, // addi t1, x0, 1
    0x0062A023L, // sw   t1, 0(t0)         SUCCESS
    0x0000006FL, // halt
  )

  // ── T3: Load / Store  ─────────────────────────────────────────────────────
  // Data at 0x80001000 (inside 4 MB RAM, word index 0x400).
  // Tests: SW/LW round-trip, SB/LB (sign extension), BNE guards.
  //
  //   0x00 lui  t0, 0x80001          t0=0x80001000
  //   0x04 addi t1, x0, 42
  //   0x08 sw   t1, 0(t0)
  //   0x0C lw   t2, 0(t0)
  //   0x10 addi t3, x0, 42
  //   0x14 bne  t2, t3, +40          → 0x3C fail
  //   0x18 addi t1, x0, 90
  //   0x1C sb   t1, 4(t0)
  //   0x20 lb   t2, 4(t0)            signed byte: 90 = 0x5A < 128 → no sign-ext
  //   0x24 bne  t2, t1, +24          → 0x3C fail
  //   0x28..0x34  success epilogue
  //   0x38 halt
  //   0x3C halt (fail)
  private val loadStoreProg = Seq(
    0x800012B7L, // lui  t0, 0x80001
    0x02A00313L, // addi t1, x0, 42
    0x0062A023L, // sw   t1, 0(t0)
    0x0002A383L, // lw   t2, 0(t0)
    0x02A00E13L, // addi t3, x0, 42
    0x03C39463L, // bne  t2, t3, +40
    0x05A00313L, // addi t1, x0, 90
    0x00628223L, // sb   t1, 4(t0)
    0x00428383L, // lb   t2, 4(t0)
    0x00639C63L, // bne  t2, t1, +24
    0x100022B7L, // lui  t0, 0x10002
    0xFF028293L, // addi t0, t0, -16
    0x00100313L, // addi t1, x0, 1
    0x0062A023L, // sw   t1, 0(t0)       SUCCESS
    0x0000006FL, // halt
    0x0000006FL, // fail-halt
  )

  // ── T4: Forwarding, load-use stall, and tight branch loop ────────────────
  //
  // Phase 1 – EX→EX forwarding chain:
  //   li t0, 1; addi t0,t0,1 ×6 → t0 must equal 7
  //   fail branch if t0≠7 (encoding: bne x5,x28, offset=68 → 0x5C29263)
  //
  // Phase 2 – Load-use stall:
  //   sw 42 to data addr; lw t1; immediately addi t1,t1,1
  //   → HW must insert 1-cycle stall; t1 must equal 43
  //   fail branch if t1≠43 (encoding: bne x6,x28, offset=40 → 0x3C31463)
  //
  // Phase 3 – Countdown loop (8→0):
  //   li t1, 8; loop: addi t1,t1,-1; bnez t1, -4
  //   (tight taken branch ×8, warms BTB)
  //
  // Layout:
  //   0x00–0x18  phase 1 chain
  //   0x1C       addi t3, x0, 7
  //   0x20       bne t0, t3, +68  → 0x64 fail
  //   0x24       lui  t0, 0x80001  (data area)
  //   0x28       addi t1, x0, 42
  //   0x2C       sw   t1, 0(t0)
  //   0x30       lw   t1, 0(t0)   ← lw
  //   0x34       addi t1, t1, 1   ← LOAD-USE dep
  //   0x38       addi t3, x0, 43
  //   0x3C       bne  t1, t3, +40 → 0x64 fail
  //   0x40       addi t1, x0, 8   loop counter
  //   0x44       addi t1, t1, -1  ← loop top
  //   0x48       bnez t1, -4      branch back to 0x44
  //   0x4C–0x58  success epilogue
  //   0x5C       halt
  //   0x60       (pad)
  //   0x64       halt (fail)
  //
  // Key encodings:
  //   bne x5,x28, +68  : 0x05C29263
  //   bne x6,x28, +40  : 0x03C31463  (corrected below from manual calc)
  //   lw  t1, 0(t0)    : 0x0002A303
  //   bnez t1, -4      : 0xFE031EE3
  private val hazardProg = Seq(
    0x00100293L, // 0x00 addi t0, x0, 1
    0x00128293L, // 0x04 addi t0, t0, 1       t0=2
    0x00128293L, // 0x08 addi t0, t0, 1       t0=3
    0x00128293L, // 0x0C addi t0, t0, 1       t0=4
    0x00128293L, // 0x10 addi t0, t0, 1       t0=5
    0x00128293L, // 0x14 addi t0, t0, 1       t0=6
    0x00128293L, // 0x18 addi t0, t0, 1       t0=7  (EX→EX chain)
    0x00700E13L, // 0x1C addi t3, x0, 7
    0x05C29263L, // 0x20 bne  t0, t3, +68     → 0x64 fail if t0≠7
    0x800012B7L, // 0x24 lui  t0, 0x80001     t0=0x80001000
    0x02A00313L, // 0x28 addi t1, x0, 42
    0x0062A023L, // 0x2C sw   t1, 0(t0)
    0x0002A303L, // 0x30 lw   t1, 0(t0)       ← LOAD
    0x00130313L, // 0x34 addi t1, t1, 1       ← LOAD-USE: HW stalls 1 cycle
    0x02B00E13L, // 0x38 addi t3, x0, 43
    0x03C31463L, // 0x3C bne  t1, t3, +40     → 0x64 fail if t1≠43
    0x00800313L, // 0x40 addi t1, x0, 8       loop counter
    0xFFF30313L, // 0x44 addi t1, t1, -1      ← loop top (BTB warm-up)
    0xFE031EE3L, // 0x48 bnez t1, -4          branch back to 0x44
    0x100022B7L, // 0x4C lui  t0, 0x10002
    0xFF028293L, // 0x50 addi t0, t0, -16     t0=0x10001FF0
    0x00100313L, // 0x54 addi t1, x0, 1
    0x0062A023L, // 0x58 sw   t1, 0(t0)       SUCCESS
    0x0000006FL, // 0x5C halt
    0x0000006FL, // 0x60 (pad)
    0x0000006FL, // 0x64 fail-halt
  )

  // ── T5: CSR and mtime MMIO ────────────────────────────────────────────────
  // Read mcycle twice (CSRR = CSRRS rd, csr, x0):
  //   CSR 0xB00 (mcycle):  funct3=010, opcode=0x73
  //   csrr t0 (x5): (0xB00<<20)|(0b010<<12)|(5<<7)|0x73 = 0xB00022F3
  //   csrr t1 (x6): (0xB00<<20)|(0b010<<12)|(6<<7)|0x73 = 0xB0002373
  //   bgeu t0, t1, fail  (fail if second reading ≤ first)
  //
  // Read mtime MMIO at 0xBFF8 twice:
  //   0xBFF8 = lui 0xC (t0=0xC000) + addi -8 → t0=0xBFF8
  //   lui t0, 0xC: (0xC<<12)|(5<<7)|0x37 = 0xC000|0x280|0x37 = 0xC2B7
  //     Wait: (0xC<<12) = 0xC000. (5<<7)=0x280. 0xC000|0x280|0x37=0xC2B7? 
  //     But 0xC000 | 0x280 = 0xC280; 0xC280 | 0x37 = 0xC2B7.
  //     This is a 16-bit number, padded to 32-bit: 0x0000C2B7. ✓
  //   addi t0, t0, -8: imm=-8=0xFF8; (0xFF8<<20)|(5<<15)|(5<<7)|0x13 = 0xFF828293
  //   bgeu t0, t1, fail / bgeu t1, t2, fail encodings below.
  //
  // Instruction layout:
  //   0x00 csrr t0, mcycle
  //   0x04–0x0C  nop ×3
  //   0x10 csrr t1, mcycle
  //   0x14 bgeu t0, t1, +56   → 0x4C fail if t0>=t1
  //   0x18 lui  t0, 0xC        t0=0xC000
  //   0x1C addi t0, t0, -8     t0=0xBFF8
  //   0x20 lw   t1, 0(t0)      first mtime
  //   0x24–0x2C  nop ×3
  //   0x30 lw   t2, 0(t0)      second mtime
  //   0x34 bgeu t1, t2, +24    → 0x4C fail if t1>=t2
  //   0x38 lui  t0, 0x10002
  //   0x3C addi t0, t0, -16
  //   0x40 addi t1, x0, 1
  //   0x44 sw   t1, 0(t0)      SUCCESS
  //   0x48 halt
  //   0x4C halt (fail)
  //
  //   bgeu t0(x5), t1(x6), +56:
  //     56=0b111000: imm[4:1]=0b1100, imm[10:5]=0b000001
  //     rs1=5,rs2=6,funct3=0b111(BGEU),opcode=0x63
  //     (0<<31)|(0b000001<<25)|(6<<20)|(5<<15)|(0b111<<12)|(0b1100<<8)|(0<<7)|0x63
  //     = 0x02000000|0x600000|0x28000|0x7000|0xC00|0x63 = 0x0262FC63
  //
  //   bgeu t1(x6), t2(x7), +24:
  //     24=0b011000: imm[4:1]=0b1100, imm[10:5]=0b000000
  //     rs1=6,rs2=7,funct3=0b111(BGEU),opcode=0x63
  //     (0<<31)|(0b000000<<25)|(7<<20)|(6<<15)|(0b111<<12)|(0b1100<<8)|(0<<7)|0x63
  //     = 0|0x700000|0x30000|0x7000|0xC00|0x63 = 0x00737C63
  //
  //   lw t2, 0(t0): rd=x7=7, rs1=x5=5, funct3=2, opcode=3
  //     (0<<20)|(5<<15)|(2<<12)|(7<<7)|0x03 = 0x28000|0x2000|0x380|3 = 0x0002A383
  private val csrProg = Seq(
    0xB00022F3L, // 0x00 csrr t0, mcycle         (first read)
    0x00000013L, // 0x04 nop
    0x00000013L, // 0x08 nop
    0x00000013L, // 0x0C nop
    0xB0002373L, // 0x10 csrr t1, mcycle         (second read)
    0x00000013L, // 0x14 nop                     avoid CSR->branch same packet
    0x0262FC63L, // 0x18 bgeu t0, t1, +56        fail if first >= second
    0x0000C2B7L, // 0x1C lui  t0, 0xC             t0=0xC000
    0xFF828293L, // 0x20 addi t0, t0, -8          t0=0xBFF8
    0x0002A303L, // 0x24 lw   t1, 0(t0)           first mtime read
    0x00000013L, // 0x28 nop
    0x00000013L, // 0x2C nop
    0x00000013L, // 0x30 nop
    0x0002A383L, // 0x34 lw   t2, 0(t0)           second mtime read
    0x00737C63L, // 0x38 bgeu t1, t2, +24         fail if first >= second
    0x100022B7L, // 0x3C lui  t0, 0x10002
    0xFF028293L, // 0x40 addi t0, t0, -16
    0x00100313L, // 0x44 addi t1, x0, 1
    0x0062A023L, // 0x48 sw   t1, 0(t0)           SUCCESS
    0x0000006FL, // 0x4C halt
    0x0000006FL, // 0x50 fail-halt
  )

  // ── Tests ─────────────────────────────────────────────────────────────────

  behavior of "InOrderCore"

  it should "complete the smoke test (ALU + branch + success MMIO)" in {
    val result = runSim(writeHex("smoke.hex", smokeProg), verbose = true)
    withClue(s"cycles=${result.cycles}") { result.success shouldBe true }
  }

  it should "output 'Hi\\n' via the putchar MMIO" in {
    val result = runSim(writeHex("putchar.hex", putcharProg), verbose = true)
    result.success shouldBe true
    result.output  shouldBe "Hi\n"
  }

  it should "correctly store and load bytes and words (D-Cache round-trip)" in {
    val result = runSim(writeHex("load_store.hex", loadStoreProg), maxCycles = 8000, verbose = true)
    withClue(s"output='${result.output}' cycles=${result.cycles}") {
      result.success shouldBe true
    }
  }

  it should "pass the forwarding chain, load-use stall, and branch loop" in {
    val result = runSim(writeHex("hazard.hex", hazardProg), maxCycles = 8000, verbose = true)
    withClue(s"output='${result.output}' cycles=${result.cycles}") {
      result.success shouldBe true
    }
  }

  it should "show mcycle and mtime increasing (CSR + MMIO counters)" in {
    val result = runSim(writeHex("csr.hex", csrProg), maxCycles = 8000, verbose = true)
    withClue(s"output='${result.output}' cycles=${result.cycles}") {
      result.success shouldBe true
    }
  }

  /** Full assembly suite – only runs if tests/test.hex was built.
    * Build: cd tests && make
    */
  it should "pass the full assembly test suite (tests/test.hex)" in {
    val f = new File("tests/test.hex")
    assume(f.exists(), "tests/test.hex not found – run 'cd tests && make' first")
    val result = runSim(f.getAbsolutePath, maxCycles = 2_000_000, verbose = true)
    result.success shouldBe true
    result.output  should include ("PASS")
  }
}
