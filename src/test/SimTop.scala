package riscv

import chisel3._
import chisel3.util._
import parameterized_cache.CacheParams

/** Test-only top-level wrapper.
  * Lives in src/test/ so it does NOT affect Elaborate.main.
  *
  * Wires an InOrderCore to a pre-initialized RV32DualPortMemory,
  * exposing only the signals that the test needs to observe.
  */
class SimTop(
    initFile: String,
    enableRV32M: Boolean = false,
    cacheParams: CacheParams = CacheParams.default,
    dCacheParams: Option[CacheParams] = None,
    branchPredInit: Int = 1
) extends Module {
  private val ip = cacheParams
  private val dp = dCacheParams.getOrElse(cacheParams)

  val io = IO(new Bundle {
    val success   = Output(Bool())
    val printChar = Output(Valid(UInt(8.W)))
    val debugPc   = Output(UInt(32.W))
    val perf      = Output(new CorePerfCounters)
  })

  val core   = Module(new InOrderCore(enableRV32M, ip, Some(dp), branchPredInit))
  val memory = Module(new RV32DualPortMemory(ip, initFile = initFile))

  memory.io.imem <> core.io.imem
  memory.io.dmem <> core.io.dmem

  io.success   := core.io.success
  io.printChar := core.io.printChar
  io.debugPc   := core.io.debugPc
  io.perf      := core.io.perf
}
