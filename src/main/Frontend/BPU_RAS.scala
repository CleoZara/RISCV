package riscv

import chisel3._
import chisel3.util._

class RASInterface extends Bundle {
  val push       = Input(Bool())
  val pushAddr   = Input(UInt(32.W))
  val pop        = Input(Bool())
  val topAddr    = Output(UInt(32.W))
  val flush      = Input(Bool())
  val checkpoint = Input(UInt(4.W))
}

// BTBEntry 定义在 BPU.scala，同属 riscv 包，直接复用。

class BPU_RAS extends Module {
  val io = IO(new Bundle {
    val queryPc      = Input(UInt(32.W))
    val predTaken    = Output(Bool())
    val predTarget   = Output(UInt(32.W))

    val updateValid  = Input(Bool())
    val updatePc     = Input(UInt(32.W))
    val updateTaken  = Input(Bool())
    val updateTarget = Input(UInt(32.W))

    val ras = new RASInterface()
  })

  // spec §8.3：所有 2-bit 饱和计数器复位为弱不跳转 2'b01。
  val choiceTable = RegInit(VecInit(Seq.fill(512)(1.U(2.W))))
  val takenTable  = RegInit(VecInit(Seq.fill(512)(1.U(2.W))))
  val ntTable     = RegInit(VecInit(Seq.fill(512)(1.U(2.W))))

  val btb = RegInit(VecInit(Seq.fill(256)(0.U.asTypeOf(new BTBEntry))))

  def updateCounter(cnt: UInt, taken: Bool): UInt = {
    val next = WireDefault(cnt)
    when(taken && cnt =/= 3.U)  { next := cnt + 1.U }
    .elsewhen(!taken && cnt =/= 0.U) { next := cnt - 1.U }
    next
  }

  val qDirIdx = io.queryPc(10, 2)
  val qBtbIdx = io.queryPc(9, 2)
  val uDirIdx = io.updatePc(10, 2)
  val uBtbIdx = io.updatePc(9, 2)

  val uCurChoice = choiceTable(uDirIdx)
  val uCurTaken  = takenTable(uDirIdx)
  val uCurNt     = ntTable(uDirIdx)

  val uNewTaken  = Mux(uCurChoice(1),
    updateCounter(uCurTaken, io.updateTaken), uCurTaken)
  val uNewNt     = Mux(!uCurChoice(1),
    updateCounter(uCurNt, io.updateTaken), uCurNt)
  val uNewChoice = Mux(uCurTaken(1) =/= uCurNt(1),
    updateCounter(uCurChoice, io.updateTaken), uCurChoice)

  val uNewBtb = Wire(new BTBEntry)
  uNewBtb.valid  := true.B
  uNewBtb.tag    := io.updatePc(31, 10)
  uNewBtb.target := io.updateTarget

  val dirBypass = io.updateValid && (uDirIdx === qDirIdx)
  val btbBypass = io.updateValid && (uBtbIdx === qBtbIdx)

  val choiceVal = Mux(dirBypass, uNewChoice, choiceTable(qDirIdx))
  val takenVal  = Mux(dirBypass, uNewTaken,  takenTable(qDirIdx))
  val ntVal     = Mux(dirBypass, uNewNt,     ntTable(qDirIdx))
  val btbEntry  = Mux(btbBypass, uNewBtb,    btb(qBtbIdx))

  val useTaken = choiceVal(1)
  val predDir  = Mux(useTaken, takenVal(1), ntVal(1))
  val btbHit   = btbEntry.valid && (btbEntry.tag === io.queryPc(31, 10))

  io.predTaken  := btbHit && predDir
  io.predTarget := btbEntry.target

  when(io.updateValid) {
    val uidx = io.updatePc(10, 2)
    val ubtb = io.updatePc(9, 2)
    val act  = io.updateTaken

    btb(ubtb).valid  := true.B
    btb(ubtb).tag    := io.updatePc(31, 10)
    btb(ubtb).target := io.updateTarget

    val ct = choiceTable(uidx)
    val tt = takenTable(uidx)
    val nt = ntTable(uidx)

    when(ct(1)) {
      takenTable(uidx) := updateCounter(tt, act)
    } .otherwise {
      ntTable(uidx)    := updateCounter(nt, act)
    }

    when(tt(1) =/= nt(1)) {
      choiceTable(uidx) := updateCounter(ct, act)
    }
  }

  // RAS
  val rasStack = RegInit(VecInit(Seq.fill(16)(0.U(32.W))))
  val rasPtr   = RegInit(0.U(4.W))

  io.ras.topAddr := rasStack(rasPtr)

  when(io.ras.flush) {
    rasPtr := io.ras.checkpoint
  } .elsewhen(io.ras.push) {
    val nextPtr = (rasPtr + 1.U)(3, 0)
    rasStack(nextPtr) := io.ras.pushAddr
    rasPtr            := nextPtr
  } .elsewhen(io.ras.pop) {
    rasPtr := (rasPtr - 1.U)(3, 0)
  }
}
