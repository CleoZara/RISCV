package riscv

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline
import firrtl.annotations.MemoryLoadFileType
import parameterized_cache.{CacheParams, MemBusIO}

class RV32DualPortMemory(
    p: CacheParams,
    words: Int = 1 << 20,
    latency: Int = 10,
    initFile: String = "")
    extends Module {
  val io = IO(new Bundle {
    val imem = Flipped(new MemBusIO(p))
    val dmem = Flipped(new MemBusIO(p))
  })

  require(p.ADDR_WIDTH == 32, "RV32DualPortMemory expects 32-bit addresses")
  require(p.DATA_WIDTH == 32, "RV32DualPortMemory expects 32-bit data")
  require(p.WMASK_BITS == 4, "RV32DualPortMemory expects 4 byte write mask bits")
  require(latency >= 1, "memory latency must be at least 1 cycle")

  val mem = Mem(words, UInt(32.W))
  if (initFile.nonEmpty) {
    loadMemoryFromFileInline(mem, initFile, MemoryLoadFileType.Hex)
  }

  val busy = RegInit(false.B)
  val sourceIsD = RegInit(false.B)
  val count = RegInit(0.U(log2Ceil(latency + 1).W))
  val rdata = RegInit(0.U(32.W))

  io.dmem.req.ready := !busy
  io.imem.req.ready := !busy && !io.dmem.req.valid

  io.dmem.resp.valid := busy && sourceIsD && count === 0.U
  io.imem.resp.valid := busy && !sourceIsD && count === 0.U
  io.dmem.resp.bits.rdata := rdata
  io.imem.resp.bits.rdata := rdata

  val acceptD = io.dmem.req.fire
  val acceptI = io.imem.req.fire
  val accept = acceptD || acceptI
  val req = Mux(acceptD, io.dmem.req.bits, io.imem.req.bits)
  val wordAddr = req.addr(log2Ceil(words) + 1, 2)

  when(accept) {
    busy := true.B
    sourceIsD := acceptD
    count := latency.U
    rdata := mem(wordAddr)

    when(req.wen) {
      val old = mem(wordAddr)
      val byteMask = Cat((0 until 4).reverse.map(i => Fill(8, req.wmask(i))))
      mem(wordAddr) := (req.wdata & byteMask) | (old & ~byteMask)
    }
  }.elsewhen(busy && count =/= 0.U) {
    count := count - 1.U
  }.elsewhen(io.dmem.resp.fire || io.imem.resp.fire) {
    busy := false.B
  }
}
