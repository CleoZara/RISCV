package riscv

import chisel3._
import chisel3.util._

class WBStage extends Module {
  val issueWidth = 2

  val io = IO(new Bundle {
    val in = Input(Vec(issueWidth, new MEMWBBundle))

    val regWen = Output(Vec(issueWidth, Bool()))
    val regWaddr = Output(Vec(issueWidth, UInt(5.W)))
    val regWdata = Output(Vec(issueWidth, UInt(32.W)))

    val instRetire = Output(Vec(issueWidth, Bool()))

    val wbValid = Output(Vec(issueWidth, Bool()))
    val wbRfWen = Output(Vec(issueWidth, Bool()))
    val wbRdAddr = Output(Vec(issueWidth, UInt(5.W)))
    val wbData = Output(Vec(issueWidth, UInt(32.W)))
  })

  for (i <- 0 until issueWidth) {
    val valid = io.in(i).ctrl.valid && !io.in(i).ctrl.kill
    val data = MuxLookup(io.in(i).wbSel, io.in(i).aluOut, Seq(
      WbSel.WB_ALU -> io.in(i).aluOut,
      WbSel.WB_MEM -> io.in(i).memData,
      WbSel.WB_PC4 -> (io.in(i).pc + 4.U),
      WbSel.WB_CSR -> io.in(i).csrRdata
    ))

    io.regWen(i) := valid && io.in(i).rfWen && io.in(i).rdAddr =/= 0.U
    io.regWaddr(i) := io.in(i).rdAddr
    io.regWdata(i) := data

    io.instRetire(i) := valid
    io.wbValid(i) := valid
    io.wbRfWen(i) := io.regWen(i)
    io.wbRdAddr(i) := io.in(i).rdAddr
    io.wbData(i) := data
  }
}
