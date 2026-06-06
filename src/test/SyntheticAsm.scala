package riscv

import java.io.{File, PrintWriter}

object SyntheticAsm {
  val X0 = 0
  val RA = 1
  val T0 = 5
  val T1 = 6
  val T2 = 7
  val T3 = 28
  val T4 = 29

  def writeHex(dir: File, name: String, words: Seq[Long], minWords: Int = 512): String = {
    dir.mkdirs()
    val f = new File(dir, name)
    val pw = new PrintWriter(f)
    try {
      words.padTo(minWords, 0x00000013L).foreach { w =>
        pw.println(f"${w & 0xFFFFFFFFL}%08x")
      }
    } finally {
      pw.close()
    }
    f.getAbsolutePath
  }

  def addi(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xFFF).toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x13L

  def lui(rd: Int, imm20: Long): Long =
    ((imm20 & 0xFFFFF) << 12) | (rd.toLong << 7) | 0x37L

  def csrrw(csr: Int, rs1: Int): Long =
    ((csr.toLong & 0xFFF) << 20) | (rs1.toLong << 15) | (1L << 12) | 0x73L

  def sw(rs1: Int, rs2: Int, imm: Int): Long = {
    val i = imm & 0xFFF
    ((i >> 5).toLong << 25) | (rs2.toLong << 20) | (rs1.toLong << 15) |
      (2L << 12) | ((i & 0x1F).toLong << 7) | 0x23L
  }

  def lw(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xFFF).toLong << 20) | (rs1.toLong << 15) | (2L << 12) |
      (rd.toLong << 7) | 0x03L

  def add(rd: Int, rs1: Int, rs2: Int): Long =
    (rs2.toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x33L

  def beq(rs1: Int, rs2: Int, offset: Int): Long =
    branch(rs1, rs2, offset, 0)

  def bne(rs1: Int, rs2: Int, offset: Int): Long =
    branch(rs1, rs2, offset, 1)

  private def branch(rs1: Int, rs2: Int, offset: Int, funct3: Int): Long = {
    val imm12 = (offset >> 12) & 1
    val imm11 = (offset >> 11) & 1
    val imm10_5 = (offset >> 5) & 0x3F
    val imm4_1 = (offset >> 1) & 0xF
    (imm12.toLong << 31) | (imm10_5.toLong << 25) | (rs2.toLong << 20) |
      (rs1.toLong << 15) | (funct3.toLong << 12) |
      (imm4_1.toLong << 8) | (imm11.toLong << 7) | 0x63L
  }

  def jal(rd: Int, offset: Int): Long = {
    val imm20 = (offset >> 20) & 1
    val imm10_1 = (offset >> 1) & 0x3FF
    val imm11 = (offset >> 11) & 1
    val imm19_12 = (offset >> 12) & 0xFF
    (imm20.toLong << 31) | (imm10_1.toLong << 21) |
      (imm11.toLong << 20) | (imm19_12.toLong << 12) |
      (rd.toLong << 7) | 0x6FL
  }

  def jalr(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xFFF).toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x67L

  val successEpilogue: Seq[Long] = Seq(
    lui(T0, 0x10002L),
    addi(T0, T0, -16),
    addi(T1, X0, 1),
    sw(T0, T1, 0),
    jal(X0, 0)
  )
}
