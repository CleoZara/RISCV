package riscv

import chisel3._
import chisel3.util._

class RegFile(issueWidth: Int = 2) extends Module {
  require(issueWidth == 2, "This in-order core currently fixes issueWidth at 2")

  val io = IO(new Bundle {
    val rs1Addr = Input(Vec(issueWidth, UInt(5.W)))
    val rs2Addr = Input(Vec(issueWidth, UInt(5.W)))
    val rs1Data = Output(Vec(issueWidth, UInt(32.W)))
    val rs2Data = Output(Vec(issueWidth, UInt(32.W)))

    val wen   = Input(Vec(issueWidth, Bool()))
    val waddr = Input(Vec(issueWidth, UInt(5.W)))
    val wdata = Input(Vec(issueWidth, UInt(32.W)))
  })

  val regs = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))

  private def read(addr: UInt): UInt = {
    val raw = Mux(addr === 0.U, 0.U, regs(addr))
    val bypass0 = io.wen(0) && io.waddr(0) =/= 0.U && io.waddr(0) === addr
    val bypass1 = io.wen(1) && io.waddr(1) =/= 0.U && io.waddr(1) === addr
    Mux(bypass1, io.wdata(1), Mux(bypass0, io.wdata(0), raw))
  }

  for (i <- 0 until issueWidth) {
    io.rs1Data(i) := read(io.rs1Addr(i))
    io.rs2Data(i) := read(io.rs2Addr(i))
  }

  when(io.wen(0) && io.waddr(0) =/= 0.U) {
    regs(io.waddr(0)) := io.wdata(0)
  }
  when(io.wen(1) && io.waddr(1) =/= 0.U) {
    regs(io.waddr(1)) := io.wdata(1)
  }
}
