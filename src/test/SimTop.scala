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
class SimTop(initFile: String, enableRV32M: Boolean = false) extends Module {
  private val p = CacheParams(32, 32, 8 * 1024, 4, 64)

  val io = IO(new Bundle {
    val success   = Output(Bool())
    val printChar = Output(Valid(UInt(8.W)))
  })

  val core   = Module(new InOrderCore(enableRV32M))
  val memory = Module(new RV32DualPortMemory(p, initFile = initFile))

  memory.io.imem <> core.io.imem
  memory.io.dmem <> core.io.dmem

  io.success   := core.io.success
  io.printChar := core.io.printChar
}
