package riscv

import chisel3._
import chisel3.util._
import parameterized_cache.{CacheParams, MemBusIO}

class Top(
    enableRV32M: Boolean = false,
    cacheParams: CacheParams = CacheParams.default,
    dCacheParams: Option[CacheParams] = None) extends Module {
  private val ip = cacheParams
  private val dp = dCacheParams.getOrElse(cacheParams)

  val io = IO(new Bundle {
    val status = Output(Bool())
    val success = Output(Bool())
    val printChar = Output(Valid(UInt(8.W)))
    val debugPc = Output(UInt(32.W))
    val perf = Output(new CorePerfCounters)
  })

  val core = Module(new InOrderCore(enableRV32M, ip, Some(dp)))
  val memory = Module(new RV32DualPortMemory(ip))

  memory.io.imem <> core.io.imem
  memory.io.dmem <> core.io.dmem

  io.status := false.B
  io.success := core.io.success
  io.printChar := core.io.printChar
  io.debugPc := core.io.debugPc
  io.perf := core.io.perf
}

object Elaborate extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new Top)
}
