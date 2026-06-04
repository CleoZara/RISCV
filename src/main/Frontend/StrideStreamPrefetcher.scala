package riscv

import chisel3._
import chisel3.util._

class StridePrefetcher extends Module {
  val io = IO(new Bundle {
    val observeValid = Input(Bool())
    val observeAddr  = Input(UInt(32.W))
    val prefetchEn   = Input(Bool())

    val pfReqValid = Output(Bool())
    val pfReqReady = Input(Bool())
    val pfReqAddr  = Output(UInt(32.W))
  })

  val lastValid = RegInit(false.B)
  val lastLine  = RegInit(0.S(27.W))
  val stride    = RegInit(0.S(27.W))
  val conf      = RegInit(0.U(2.W))
  val lastObs   = RegInit(0.U(32.W))

  val currLine = io.observeAddr(31, 6).asSInt
  val newObs = io.observeValid && (!lastValid || (io.observeAddr =/= lastObs))
  val delta = currLine - lastLine
  val strideHit = lastValid && (delta === stride) && (delta =/= 0.S(27.W))
  val targetLine = currLine + stride
  val targetBits = targetLine.asUInt
  val targetAddr = Cat(targetBits(25, 0), 0.U(6.W))

  io.pfReqValid := io.prefetchEn && newObs && strideHit && (conf >= 2.U)
  io.pfReqAddr  := targetAddr

  when(newObs) {
    lastObs := io.observeAddr
    when(strideHit) {
      when(conf =/= 3.U) { conf := conf + 1.U }
    }.otherwise {
      stride := delta
      conf := 0.U
    }
    lastLine := currLine
    lastValid := true.B
  }
}

class StreamPrefetcher extends Module {
  val io = IO(new Bundle {
    val observeValid = Input(Bool())
    val observeAddr  = Input(UInt(32.W))
    val prefetchEn   = Input(Bool())

    val pfReqValid = Output(Bool())
    val pfReqReady = Input(Bool())
    val pfReqAddr  = Output(UInt(32.W))
  })

  val lastValid = RegInit(false.B)
  val lastLine  = RegInit(0.S(27.W))
  val direction = RegInit(1.S(2.W))
  val conf      = RegInit(0.U(2.W))
  val lastObs   = RegInit(0.U(32.W))

  val currLine = io.observeAddr(31, 6).asSInt
  val newObs = io.observeValid && (!lastValid || (io.observeAddr =/= lastObs))
  val delta = currLine - lastLine
  val streamStep = (delta === 1.S(27.W)) || (delta === -1.S(27.W))
  val direction27 = Mux(direction === -1.S(2.W), -1.S(27.W), 1.S(27.W))
  val sameDir = streamStep && (delta === direction27)
  val nextDir = Mux(delta === -1.S(27.W), -1.S(2.W), 1.S(2.W))
  val targetStep = Mux(direction === -1.S(2.W), -2.S(27.W), 2.S(27.W))
  val targetLine = currLine + targetStep
  val targetBits = targetLine.asUInt
  val targetAddr = Cat(targetBits(25, 0), 0.U(6.W))

  io.pfReqValid := io.prefetchEn && newObs && sameDir && (conf >= 1.U)
  io.pfReqAddr  := targetAddr

  when(newObs) {
    lastObs := io.observeAddr
    when(sameDir) {
      when(conf =/= 3.U) { conf := conf + 1.U }
    }.elsewhen(streamStep) {
      direction := nextDir
      conf := 0.U
    }.otherwise {
      conf := 0.U
    }
    lastLine := currLine
    lastValid := true.B
  }
}
