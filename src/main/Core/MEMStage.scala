package riscv

import chisel3._
import chisel3.util._
import parameterized_cache.{CacheParams, DCacheTop, MemBusIO}

class MEMStage extends Module {
  val issueWidth = 2
  private val p = CacheParams(32, 32, 8 * 1024, 4, 64)

  val io = IO(new Bundle {
    val in = Input(Vec(issueWidth, new EXMEMBundle))
    val flushMem = Input(Bool())

    val mtimeLo = Input(UInt(32.W))
    val mtimeHi = Input(UInt(32.W))
    val dcacheFlush = Input(Bool())
    val dcacheStall = Output(Bool())
    val printChar   = Output(Valid(UInt(8.W)))
    // P0 fix: expose success signal for test-completion detection
    val success     = Output(Bool())
    val dmem = new MemBusIO(p)

    val out = Output(Vec(issueWidth, new MEMWBBundle))
  })

  val dcache = Module(new DCacheTop(p))

  val slotValid = Wire(Vec(issueWidth, Bool()))
  val slotMem   = Wire(Vec(issueWidth, Bool()))
  for (i <- 0 until issueWidth) {
    slotValid(i) := io.in(i).ctrl.valid && !io.in(i).ctrl.kill && !io.flushMem
    slotMem(i)   := slotValid(i) && (io.in(i).memRen || io.in(i).memWen)
  }

  // At most one slot reaches D-Cache per cycle (IDStage blocks dual-slot memory ops).
  val useSlot1 = !slotMem(0) && slotMem(1)
  val memIdx   = Mux(useSlot1, 1.U, 0.U)
  val memReqValid = slotMem(0) || slotMem(1)
  val memAddr  = io.in(memIdx).aluOut
  val byteOff  = memAddr(1, 0)
  val isPrintfWrite =
    memReqValid &&
    io.in(memIdx).memWen &&
    (memAddr === "h10001FF1".U(32.W)) &&
    (io.in(memIdx).memWd === MemWidth.MW_BYTE)

  val byteMask = (1.U(4.W) << byteOff).asUInt
  val halfMask = Mux(memAddr(1), "b1100".U(4.W), "b0011".U(4.W))
  val wmask = MuxLookup(io.in(memIdx).memWd, "b1111".U(4.W), Seq(
    MemWidth.MW_WORD -> "b1111".U(4.W),
    MemWidth.MW_HALF -> halfMask,
    MemWidth.MW_BYTE -> byteMask
  ))
  val shift        = Cat(byteOff, 0.U(3.W))
  val shiftedWdata = (io.in(memIdx).rs2Data << shift)(31, 0)

  dcache.io.addr    := memAddr
  dcache.io.flush   := io.dcacheFlush
  dcache.io.wen     := memReqValid && io.in(memIdx).memWen
  dcache.io.wmask   := wmask
  dcache.io.wdata   := shiftedWdata
  dcache.io.memRen  := memReqValid && io.in(memIdx).memRen
  dcache.io.memWd   := io.in(memIdx).memWd
  dcache.io.signed  := io.in(memIdx).memSigned
  dcache.io.mtimeLo := io.mtimeLo
  dcache.io.mtimeHi := io.mtimeHi
  io.dmem <> dcache.io.mem

  val printValidReg = RegNext(isPrintfWrite, false.B)
  val printBitsReg  = RegEnable(io.in(memIdx).rs2Data(7, 0), 0.U(8.W), isPrintfWrite)

  io.dcacheStall := dcache.io.stall || dcache.io.missOut
  io.printChar.valid := printValidReg
  io.printChar.bits  := printBitsReg
  io.success     := dcache.io.success   // P0 fix

  for (i <- 0 until issueWidth) {
    io.out(i) := 0.U.asTypeOf(new MEMWBBundle)
    io.out(i).pc       := io.in(i).pc
    io.out(i).inst     := io.in(i).inst
    io.out(i).slotIdx  := io.in(i).slotIdx
    io.out(i).aluOut   := io.in(i).aluOut
    io.out(i).memData  := Mux(slotMem(i) && io.in(i).memRen, dcache.io.rdata, 0.U)
    io.out(i).rdAddr   := io.in(i).rdAddr
    io.out(i).wbSel    := io.in(i).wbSel
    io.out(i).rfWen    := slotValid(i) && io.in(i).rfWen
    io.out(i).csrRdata := io.in(i).csrRdata
    io.out(i).ctrl.valid   := slotValid(i)
    io.out(i).ctrl.kill    := io.flushMem || io.in(i).ctrl.kill
    io.out(i).ctrl.allowIn := true.B
  }
}
