package parameterized_cache

import chisel3._
import chisel3.util._

class LoadExtend(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val hitWay   = Input(UInt(p.WAY_W.W))
    val byteoff  = Input(UInt(log2Ceil(p.WORD_BYTES).W))
    val memWd    = Input(UInt(2.W))
    val signed   = Input(Bool())
    val rawData  = Input(Vec(p.WAY_NUM, UInt(p.DATA_WIDTH.W)))
    val rdata    = Output(UInt(p.DATA_WIDTH.W))
  })

  val data = io.rawData(io.hitWay)

  val byte  = (data >> (io.byteoff << 3.U))(7, 0)
  val hword = (data >> ((io.byteoff(1) << 1.U) << 3.U))(15, 0)

  val extByte  = Mux(io.signed, Fill(p.DATA_WIDTH - 8, byte(7)), 0.U)
  val extHword = Mux(io.signed, Fill(p.DATA_WIDTH - 16, hword(15)), 0.U)

  io.rdata := MuxLookup(io.memWd, data, Array(
    1.U -> (extHword ## hword),
    2.U -> (extByte ## byte)
  ))
}
