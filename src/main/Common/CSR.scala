package riscv

import chisel3._
import chisel3.util._

object CSRAddr {
  val cycle         = "hC00".U(12.W)
  val time          = "hC01".U(12.W)
  val instret       = "hC02".U(12.W)
  val cycleh        = "hC80".U(12.W)
  val timeh         = "hC81".U(12.W)
  val instreth      = "hC82".U(12.W)
  val mcycle        = "hB00".U(12.W)
  val mcycleh       = "hB80".U(12.W)
  val minstret      = "hB02".U(12.W)
  val minstreth     = "hB82".U(12.W)
  val mcountinhibit = "h320".U(12.W)
  val misa          = "h301".U(12.W)
  val prefetchCtrl  = "h7C0".U(12.W) // bit0=ICache next-line, bit1=stride, bit2=stream
  val branchPredCtrl = "h7C1".U(12.W) // bits[1:0]: 0=BPU, 1=BPU+RAS, 2=TAGE+RAS
}

// CSROp 现统一定义在 Defines_c.scala（object CSROp），此处不再重复定义。

class CSRFile(
    val xlen: Int = 32,
    val issueWidth: Int = 2,
    val enableRV32M: Boolean = false,
    val branchPredInit: Int = 1,
    val prefetchInit: Int = 0) extends Module {
  require(xlen == 32, "Current CSRFile implementation targets RV32")
  require(issueWidth >= 1, "issueWidth must be >= 1")
  require(branchPredInit >= 0 && branchPredInit <= 3, "branchPredInit must fit in branchPredCtrl[1:0]")
  require(prefetchInit >= 0 && prefetchInit <= 7, "prefetchInit must fit in prefetchCtrl[2:0]")

  val io = IO(new Bundle {
    // Query/read port (for decode/execute preview).
    val raddr = Input(UInt(12.W))
    val rdata = Output(UInt(xlen.W))

    // Execute-stage CSR op request.
    val opValid = Input(Bool())
    val opType  = Input(UInt(2.W))
    val waddr   = Input(UInt(12.W))
    val wdata   = Input(UInt(xlen.W))
    val oldData = Output(UInt(xlen.W)) // value before write, for rd writeback

    // Counter update controls.
    val cycleTick  = Input(Bool())
    val instRetire = Input(Vec(issueWidth, Bool()))

    // Counter outputs for convenient top-level wiring.
    val mcycleLo   = Output(UInt(xlen.W))
    val mcycleHi   = Output(UInt(xlen.W))
    val minstretLo = Output(UInt(xlen.W))

    // mtime（MMIO 实时计数器，每周期自增）：供顶层送往 D-Cache 的 mtimeLo/mtimeHi。
    val mtimeLo = Output(UInt(xlen.W))
    val mtimeHi = Output(UInt(xlen.W))
    val branchPredCtrl = Output(UInt(xlen.W))

    // 预取开关：bit0 仅控制 ICache next-line；bit1/bit2 控制 stride/stream。
    val prefetchCtrl = Output(UInt(xlen.W))
  })

  val mcycle        = RegInit(0.U(64.W))
  val minstret      = RegInit(0.U(64.W))
  val mtime         = RegInit(0.U(64.W))
  val mcountinhibit = RegInit(0.U(xlen.W))
  val prefetchCtrl  = RegInit(prefetchInit.U(xlen.W))
  val branchPredCtrl = RegInit(branchPredInit.U(xlen.W))

  // misa：只读。MXL=01（RV32）置于 bit[31:30]，'I'=bit8；含 M 时再置 'M'=bit12。
  val misaVal = {
    val base = ("h40000000".U(32.W) | (1.U << 8)) // RV32 + I
    if (enableRV32M) base | (1.U << 12) else base
  }

  // 计数器自增（mcountinhibit 可关闭）。mtime 始终自增。
  when(io.cycleTick && !mcountinhibit(0)) { mcycle := mcycle + 1.U }
  when(!mcountinhibit(2)) { minstret := minstret + PopCount(io.instRetire) }
  mtime := mtime + 1.U

  private def csrRead(addr: UInt): UInt = {
    MuxLookup(addr, 0.U(xlen.W), Seq(
      CSRAddr.cycle         -> mcycle(31, 0),
      CSRAddr.cycleh        -> mcycle(63, 32),
      CSRAddr.time          -> mtime(31, 0),
      CSRAddr.timeh         -> mtime(63, 32),
      CSRAddr.instret       -> minstret(31, 0),
      CSRAddr.instreth      -> minstret(63, 32),
      CSRAddr.mcycle        -> mcycle(31, 0),
      CSRAddr.mcycleh       -> mcycle(63, 32),
      CSRAddr.minstret      -> minstret(31, 0),
      CSRAddr.minstreth     -> minstret(63, 32),
      CSRAddr.mcountinhibit -> mcountinhibit,
      CSRAddr.misa          -> misaVal,
      CSRAddr.prefetchCtrl  -> prefetchCtrl,
      CSRAddr.branchPredCtrl -> branchPredCtrl
    ))
  }

  val oldVal = csrRead(io.waddr)
  io.oldData := oldVal

  val writeVal = WireDefault(oldVal)
  switch(io.opType) {
    is(CSROp.WRITE) { writeVal := io.wdata }
    is(CSROp.SET)   { writeVal := oldVal | io.wdata }
    is(CSROp.CLEAR) { writeVal := oldVal & (~io.wdata).asUInt }
  }

  val csrWriteForward =
    io.opValid &&
    (io.waddr === io.raddr) &&
    (io.waddr =/= CSRAddr.misa)
  io.rdata := Mux(csrWriteForward, writeVal, csrRead(io.raddr))

  when(io.opValid) {
    switch(io.waddr) {
      is(CSRAddr.mcycle)        { mcycle := Cat(mcycle(63, 32), writeVal) }
      is(CSRAddr.mcycleh)       { mcycle := Cat(writeVal, mcycle(31, 0)) }
      is(CSRAddr.minstret)      { minstret := Cat(minstret(63, 32), writeVal) }
      is(CSRAddr.minstreth)     { minstret := Cat(writeVal, minstret(31, 0)) }
      is(CSRAddr.mcountinhibit) { mcountinhibit := writeVal }
      is(CSRAddr.prefetchCtrl)  { prefetchCtrl := writeVal }
      is(CSRAddr.branchPredCtrl) { branchPredCtrl := writeVal }
      // misa 只读：忽略写入
    }
  }

  io.mcycleLo   := mcycle(31, 0)
  io.mcycleHi   := mcycle(63, 32)
  io.minstretLo := minstret(31, 0)
  io.mtimeLo    := mtime(31, 0)
  io.mtimeHi    := mtime(63, 32)
  io.prefetchCtrl := prefetchCtrl
  io.branchPredCtrl := branchPredCtrl
}
