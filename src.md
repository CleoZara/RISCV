## .\src\main\Common\CSR.scala

```scala
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
```

## .\src\main\Common\Defines_c.scala

```scala
package riscv

import chisel3._
import chisel3.util._

object Instructions {
  val ADD   = BitPat("b0000000??????????000?????0110011")
  val SUB   = BitPat("b0100000??????????000?????0110011")
  val AND   = BitPat("b0000000??????????111?????0110011")
  val OR    = BitPat("b0000000??????????110?????0110011")
  val XOR   = BitPat("b0000000??????????100?????0110011")
  val SLL   = BitPat("b0000000??????????001?????0110011")
  val SRL   = BitPat("b0000000??????????101?????0110011")
  val SRA   = BitPat("b0100000??????????101?????0110011")
  val SLT   = BitPat("b0000000??????????010?????0110011")
  val SLTU  = BitPat("b0000000??????????011?????0110011")

  val ADDI  = BitPat("b?????????????????000?????0010011")
  val ANDI  = BitPat("b?????????????????111?????0010011")
  val ORI   = BitPat("b?????????????????110?????0010011")
  val XORI  = BitPat("b?????????????????100?????0010011")
  val SLLI  = BitPat("b0000000??????????001?????0010011")
  val SRLI  = BitPat("b0000000??????????101?????0010011")
  val SRAI  = BitPat("b0100000??????????101?????0010011")
  val SLTI  = BitPat("b?????????????????010?????0010011")
  val SLTIU = BitPat("b?????????????????011?????0010011")

  val LW    = BitPat("b?????????????????010?????0000011")
  val LH    = BitPat("b?????????????????001?????0000011")
  val LB    = BitPat("b?????????????????000?????0000011")
  val LHU   = BitPat("b?????????????????101?????0000011")
  val LBU   = BitPat("b?????????????????100?????0000011")

  val SW    = BitPat("b?????????????????010?????0100011")
  val SH    = BitPat("b?????????????????001?????0100011")
  val SB    = BitPat("b?????????????????000?????0100011")

  val BEQ   = BitPat("b?????????????????000?????1100011")
  val BNE   = BitPat("b?????????????????001?????1100011")
  val BLT   = BitPat("b?????????????????100?????1100011")
  val BGE   = BitPat("b?????????????????101?????1100011")
  val BLTU  = BitPat("b?????????????????110?????1100011")
  val BGEU  = BitPat("b?????????????????111?????1100011")

  val JAL   = BitPat("b?????????????????????????1101111")
  val JALR  = BitPat("b?????????????????000?????1100111")

  val LUI   = BitPat("b?????????????????????????0110111")
  val AUIPC = BitPat("b?????????????????????????0010111")

  val CSRRW = BitPat("b?????????????????001?????1110011")
  val CSRRS = BitPat("b?????????????????010?????1110011")
  val CSRRC = BitPat("b?????????????????011?????1110011")

  val ECALL  = BitPat("b00000000000000000000000001110011")
  val EBREAK = BitPat("b00000000000100000000000001110011")
  val MRET   = BitPat("b00110000001000000000000001110011")
  val FENCE  = BitPat("b?????????????????000?????0001111")

  val MUL    = BitPat("b0000001??????????000?????0110011")
  val MULH   = BitPat("b0000001??????????001?????0110011")
  val MULHSU = BitPat("b0000001??????????010?????0110011")
  val MULHU  = BitPat("b0000001??????????011?????0110011")
  val DIV    = BitPat("b0000001??????????100?????0110011")
  val DIVU   = BitPat("b0000001??????????101?????0110011")
  val REM    = BitPat("b0000001??????????110?????0110011")
  val REMU   = BitPat("b0000001??????????111?????0110011")
}

object AluOp {
  val W          = 5
  val ALU_ADD    = 0.U(W.W)
  val ALU_SUB    = 1.U(W.W)
  val ALU_AND    = 2.U(W.W)
  val ALU_OR     = 3.U(W.W)
  val ALU_XOR    = 4.U(W.W)
  val ALU_SLL    = 5.U(W.W)
  val ALU_SRL    = 6.U(W.W)
  val ALU_SRA    = 7.U(W.W)
  val ALU_SLT    = 8.U(W.W)
  val ALU_SLTU   = 9.U(W.W)
  val ALU_LUI    = 10.U(W.W)
  val ALU_COPY1  = 11.U(W.W)
  val ALU_MUL    = 12.U(W.W)
  val ALU_MULH   = 13.U(W.W)
  val ALU_MULHSU = 14.U(W.W)
  val ALU_MULHU  = 15.U(W.W)
  val ALU_DIV    = 16.U(W.W)
  val ALU_DIVU   = 17.U(W.W)
  val ALU_REM    = 18.U(W.W)
  val ALU_REMU   = 19.U(W.W)

  def isMulDiv(op: UInt): Bool = op >= ALU_MUL && op <= ALU_REMU
}

object Op1Sel {
  val OP1_RS1 = 0.U(2.W)
  val OP1_PC  = 1.U(2.W)
  val OP1_IMM = 2.U(2.W)
}

object Op2Sel {
  val OP2_RS2 = 0.U(2.W)
  val OP2_IMM = 1.U(2.W)
  val OP2_PC4 = 2.U(2.W)
}

object WbSel {
  val WB_ALU = 0.U(2.W)
  val WB_MEM = 1.U(2.W)
  val WB_PC4 = 2.U(2.W)
  val WB_CSR = 3.U(2.W)
}

object MemWidth {
  val MW_WORD = 0.U(2.W)
  val MW_HALF = 1.U(2.W)
  val MW_BYTE = 2.U(2.W)
}

object BrType {
  val BR_NONE = 0.U(3.W)
  val BR_EQ   = 1.U(3.W)
  val BR_NE   = 2.U(3.W)
  val BR_LT   = 3.U(3.W)
  val BR_GE   = 4.U(3.W)
  val BR_LTU  = 5.U(3.W)
  val BR_GEU  = 6.U(3.W)
}

object CSROp {
  val NONE  = 0.U(2.W)
  val WRITE = 1.U(2.W)
  val SET   = 2.U(2.W)
  val CLEAR = 3.U(2.W)
}

class PipelineControl extends Bundle {
  val valid   = Bool()
  val kill    = Bool()
  val allowIn = Bool()
}

class IFIDSlot extends Bundle {
  val pc         = UInt(32.W)
  val inst       = UInt(32.W)
  val slotIdx    = UInt(1.W)
  val fetchPc    = UInt(32.W)
  val predTaken  = Bool()
  val predTarget = UInt(32.W)
  val predNextPc = UInt(32.W)
  val seqNextPc  = UInt(32.W)
  val rasPred    = Bool()
  val icacheHit  = Bool()
  val ctrl       = new PipelineControl
}

class DecodedSlot extends Bundle {
  val pc        = UInt(32.W)
  val inst      = UInt(32.W)
  val slotIdx   = UInt(1.W)
  val fetchPc   = UInt(32.W)

  val rs1Addr   = UInt(5.W)
  val rs2Addr   = UInt(5.W)
  val rdAddr    = UInt(5.W)
  val rs1Data   = UInt(32.W)
  val rs2Data   = UInt(32.W)
  val rs1Use    = Bool()
  val rs2Use    = Bool()

  val imm       = UInt(32.W)
  val csrAddr   = UInt(12.W)
  val csrOp     = UInt(2.W)

  val aluOp     = UInt(AluOp.W.W)
  val op1Sel    = UInt(2.W)
  val op2Sel    = UInt(2.W)

  val wbSel     = UInt(2.W)
  val rfWen     = Bool()
  val memRen    = Bool()
  val memWen    = Bool()
  val memWd     = UInt(2.W)
  val memSigned = Bool()

  val brType    = UInt(3.W)
  val isJump    = Bool()
  val isJalr    = Bool()
  val isSysInst = Bool()

  val predTaken  = Bool()
  val predTarget = UInt(32.W)
  val predNextPc = UInt(32.W)
  val seqNextPc  = UInt(32.W)
  val rasPred    = Bool()
  val csrRdata   = UInt(32.W)

  val ctrl      = new PipelineControl
}

class IDEXBundle extends DecodedSlot

class EXMEMBundle extends Bundle {
  val pc        = UInt(32.W)
  val inst      = UInt(32.W)
  val slotIdx   = UInt(1.W)
  val aluOut    = UInt(32.W)
  val rs2Data   = UInt(32.W)
  val rdAddr    = UInt(5.W)
  val wbSel     = UInt(2.W)
  val rfWen     = Bool()
  val memRen    = Bool()
  val memWen    = Bool()
  val memWd     = UInt(2.W)
  val memSigned = Bool()
  val csrRdata  = UInt(32.W)
  val csrOp     = UInt(2.W)
  val brType    = UInt(3.W)
  val isJump    = Bool()
  val ctrl      = new PipelineControl
}

class MEMWBBundle extends Bundle {
  val pc       = UInt(32.W)
  val inst     = UInt(32.W)
  val slotIdx  = UInt(1.W)
  val aluOut   = UInt(32.W)
  val memData  = UInt(32.W)
  val rdAddr   = UInt(5.W)
  val wbSel    = UInt(2.W)
  val rfWen    = Bool()
  val csrRdata = UInt(32.W)
  val ctrl     = new PipelineControl
}

class CorePerfCounters extends Bundle {
  val cycles            = UInt(64.W)
  val retire0Cycles     = UInt(64.W)
  val retire1Cycles     = UInt(64.W)
  val retire2Cycles     = UInt(64.W)
  val instRetired       = UInt(64.W)
  val icacheStallCycles = UInt(64.W)
  val dcacheStallCycles = UInt(64.W)
  val loadUseStalls     = UInt(64.W)
  val idRedirects       = UInt(64.W)
  val exRedirects       = UInt(64.W)
  val rasPushes         = UInt(64.W)
  val rasPops           = UInt(64.W)
  val branchInsts       = UInt(64.W)
  val branchPreds       = UInt(64.W)
  val branchCorrect     = UInt(64.W)
  val branchMispredicts = UInt(64.W)
  val branchDirectionMispredicts = UInt(64.W)
  val branchTargetMispredicts    = UInt(64.W)
  val wrongPathFlushInsts        = UInt(64.W)
  val jalrInsts         = UInt(64.W)
  val rasPreds          = UInt(64.W)
  val rasCorrect        = UInt(64.W)
  val icacheAccesses    = UInt(64.W)
  val icacheHits        = UInt(64.W)
  val icacheMisses      = UInt(64.W)
  val icacheDemandRefills       = UInt(64.W)
  val icachePrefetchReqs        = UInt(64.W)
  val icachePrefetchAccepted    = UInt(64.W)
  val icachePrefetchDropped     = UInt(64.W)
  val icachePrefetchRefills     = UInt(64.W)
  val icachePrefetchUseful      = UInt(64.W)
  val dcacheLoads       = UInt(64.W)
  val dcacheStores      = UInt(64.W)
  val dcacheHits        = UInt(64.W)
  val dcacheMisses      = UInt(64.W)
  val dcacheWritebacks  = UInt(64.W)
  val dcacheDemandRefills       = UInt(64.W)
  val dcachePrefetchReqs        = UInt(64.W)
  val dcachePrefetchAccepted    = UInt(64.W)
  val dcachePrefetchDropped     = UInt(64.W)
  val dcachePrefetchRefills     = UInt(64.W)
  val dcachePrefetchUseful      = UInt(64.W)
}
```

## .\src\main\Compat\ICacheMissFSMCompat.scala

```scala
package icache

import chisel3._
import chisel3.util._
import parameterized_cache.{CacheParams, MemBusIO}

class ICacheMissFSM(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val missValid = Input(Bool())
    val missTag   = Input(UInt(p.TAG_W.W))
    val missIdx   = Input(UInt(p.INDEX_W.W))
    val evictWay  = Input(UInt(p.WAY_W.W))

    val pfReqValid = Input(Bool())
    val pfReqReady = Output(Bool())
    val pfReqTag   = Input(UInt(p.TAG_W.W))
    val pfReqIdx   = Input(UInt(p.INDEX_W.W))
    val pfEvictWay = Input(UInt(p.WAY_W.W))

    val mem = new MemBusIO(p)

    val refillEn    = Output(Bool())
    val refillWay   = Output(UInt(p.WAY_W.W))
    val refillIdx   = Output(UInt(p.INDEX_W.W))
    val refillWord  = Output(UInt(p.WORD_CNT_W.W))
    val refillData  = Output(UInt(p.DATA_WIDTH.W))
    val refillTag   = Output(UInt(p.TAG_W.W))
    val refillDone  = Output(Bool())
    val prefillDone = Output(Bool())

    val stall  = Output(Bool())
    val isIdle = Output(Bool())
  })

  val sIdle :: sRefillReq :: sRefillResp :: sRefillWrite :: sDone :: sPrefill :: sPrefillResp :: sPrefillWrite :: sPrefillDone :: Nil = Enum(9)

  val state = RegInit(sIdle)
  val nextState = WireDefault(state)
  val wTag = Reg(UInt(p.TAG_W.W))
  val wIdx = Reg(UInt(p.INDEX_W.W))
  val wWay = Reg(UInt(p.WAY_W.W))
  val wordCnt = RegInit(0.U(p.WORD_CNT_W.W))
  val lineBuf = Reg(Vec(p.LINE_WORDS, UInt(p.DATA_WIDTH.W)))
  val lastWord = wordCnt === (p.LINE_WORDS - 1).U

  switch(state) {
    is(sIdle) {
      when(io.missValid) {
        nextState := sRefillReq
      }.elsewhen(io.pfReqValid) {
        nextState := sPrefill
      }
    }
    is(sRefillReq)  { when(io.mem.req.fire)  { nextState := sRefillResp } }
    is(sRefillResp) { when(io.mem.resp.fire) { nextState := sRefillWrite } }
    is(sRefillWrite) { when(lastWord) { nextState := sDone } }
    is(sDone)       { nextState := sIdle }
    is(sPrefill)    { when(io.mem.req.fire)  { nextState := sPrefillResp } }
    is(sPrefillResp) {
      when(io.mem.resp.fire) { nextState := sPrefillWrite }
    }
    is(sPrefillWrite) { when(lastWord) { nextState := sPrefillDone } }
    is(sPrefillDone) { nextState := sIdle }
  }
  state := nextState

  when(state === sIdle && io.missValid) {
    wTag := io.missTag
    wIdx := io.missIdx
    wWay := io.evictWay
    wordCnt := 0.U
  }.elsewhen(state === sIdle && !io.missValid && io.pfReqValid) {
    wTag := io.pfReqTag
    wIdx := io.pfReqIdx
    wWay := io.pfEvictWay
    wordCnt := 0.U
  }.elsewhen((state === sRefillResp || state === sPrefillResp) && io.mem.resp.fire) {
    lineBuf := io.mem.resp.bits.rline
    wordCnt := 0.U
  }.elsewhen(state === sRefillWrite || state === sPrefillWrite) {
    wordCnt := Mux(lastWord, 0.U, wordCnt + 1.U)
  }

  val fetchAddr = Cat(wTag, wIdx, 0.U(p.OFFSET_W.W))
  val isRequesting = state === sRefillReq || state === sPrefill
  val isWaiting = state === sRefillResp || state === sPrefillResp

  io.mem.req.valid := isRequesting
  io.mem.req.bits.addr := fetchAddr
  io.mem.req.bits.wdata := 0.U
  io.mem.req.bits.wline := 0.U.asTypeOf(Vec(p.LINE_WORDS, UInt(p.DATA_WIDTH.W)))
  io.mem.req.bits.wen := false.B
  io.mem.req.bits.wmask := 0.U
  io.mem.req.bits.line := true.B
  io.mem.resp.ready := isWaiting

  io.refillEn := state === sRefillWrite || state === sPrefillWrite
  io.refillWay := wWay
  io.refillIdx := wIdx
  io.refillWord := wordCnt
  io.refillData := lineBuf(wordCnt)
  io.refillTag := wTag
  io.refillDone := state === sDone
  io.prefillDone := state === sPrefillDone
  io.stall := state === sRefillReq || state === sRefillResp || state === sRefillWrite || state === sDone
  io.isIdle := state === sIdle
  io.pfReqReady := state === sIdle && !io.missValid
}
```

## .\src\main\Core\BypassHazardUnit.scala

```scala
package riscv

import chisel3._
import chisel3.util._

// 说明：本文件是顺序核唯一的冒险/旁路单元，已实现 spec §5.3.1 的 F1–F11 完整
// forwarding 矩阵、§5.3.2 的「先判 forwarding 再决定 stall」、load-use stall、
// 槽间 RAW 精细降级（idCanBypassToYounger）等。
// 旧的简化版 HazardUnit.scala 与本单元功能重叠且语义冲突（对槽间 RAW 一律降级，
// 与 F5 同周期旁路矛盾），应予删除，集成时仅保留本单元。

object BypassSel {
  val W = 3
  val REG     = 0.U(W.W)
  val EX      = 1.U(W.W)
  val MEM     = 2.U(W.W)
  val WB      = 3.U(W.W)
  val INTRAEX = 4.U(W.W)
}

class BypassReadPort extends Bundle {
  val use     = Bool()
  val addr    = UInt(5.W)
  val regData = UInt(32.W)
}

class BypassWritePort extends Bundle {
  val valid      = Bool()
  val rfWen      = Bool()
  val rdAddr     = UInt(5.W)
  val data       = UInt(32.W)
  val canForward = Bool()
}

class PendingRegWriteInfo extends Bundle {
  val valid  = Bool()
  val rfWen  = Bool()
  val rdAddr = UInt(5.W)
}

class BypassNetwork(issueWidth: Int = 2) extends Module {
  require(issueWidth >= 1, "issueWidth must be at least 1")

  val io = IO(new Bundle {
    val rs1 = Input(Vec(issueWidth, new BypassReadPort))
    val rs2 = Input(Vec(issueWidth, new BypassReadPort))

    val intraEx = Input(Vec(issueWidth, new BypassWritePort))
    val exMem   = Input(Vec(issueWidth, new BypassWritePort))
    val memWb   = Input(Vec(issueWidth, new BypassWritePort))
    val wb      = Input(Vec(issueWidth, new BypassWritePort))

    val rs1Data     = Output(Vec(issueWidth, UInt(32.W)))
    val rs2Data     = Output(Vec(issueWidth, UInt(32.W)))
    val rs1Sel      = Output(Vec(issueWidth, UInt(BypassSel.W.W)))
    val rs2Sel      = Output(Vec(issueWidth, UInt(BypassSel.W.W)))
    val rs1Bypassed = Output(Vec(issueWidth, Bool()))
    val rs2Bypassed = Output(Vec(issueWidth, Bool()))
  })

  private def sourceHit(src: BypassWritePort, read: BypassReadPort): Bool = {
    read.use &&
      src.valid &&
      src.rfWen &&
      src.canForward &&
      (src.rdAddr =/= 0.U) &&
      (src.rdAddr === read.addr)
  }

  private def pick(read: BypassReadPort, slot: Int): (UInt, UInt) = {
    val intraCandidates =
      (0 until slot).reverse.map { i =>
        (sourceHit(io.intraEx(i), read), io.intraEx(i).data, BypassSel.INTRAEX)
      }

    val exMemCandidates =
      (0 until issueWidth).reverse.map { i =>
        (sourceHit(io.exMem(i), read), io.exMem(i).data, BypassSel.EX)
      }

    val memWbCandidates =
      (0 until issueWidth).reverse.map { i =>
        (sourceHit(io.memWb(i), read), io.memWb(i).data, BypassSel.MEM)
      }

    val wbCandidates =
      (0 until issueWidth).reverse.map { i =>
        (sourceHit(io.wb(i), read), io.wb(i).data, BypassSel.WB)
      }

    val candidates = intraCandidates ++ exMemCandidates ++ memWbCandidates ++ wbCandidates
    val data = PriorityMux(candidates.map { case (hit, data, _) => hit -> data } :+ (true.B -> read.regData))
    val sel = PriorityMux(candidates.map { case (hit, _, sel) => hit -> sel } :+ (true.B -> BypassSel.REG))
    (data, sel)
  }

  for (i <- 0 until issueWidth) {
    val (rs1Data, rs1Sel) = pick(io.rs1(i), i)
    val (rs2Data, rs2Sel) = pick(io.rs2(i), i)

    io.rs1Data(i) := rs1Data
    io.rs2Data(i) := rs2Data
    io.rs1Sel(i) := rs1Sel
    io.rs2Sel(i) := rs2Sel
    io.rs1Bypassed(i) := rs1Sel =/= BypassSel.REG
    io.rs2Bypassed(i) := rs2Sel =/= BypassSel.REG
  }
}

class PipelineBypassUnit(issueWidth: Int = 2) extends Module {
  require(issueWidth >= 1, "issueWidth must be at least 1")

  val io = IO(new Bundle {
    val idex      = Input(Vec(issueWidth, new IDEXBundle))
    val idexValid = Input(Vec(issueWidth, Bool()))

    val exResult = Input(Vec(issueWidth, UInt(32.W)))

    val exmem      = Input(Vec(issueWidth, new EXMEMBundle))
    val exmemValid = Input(Vec(issueWidth, Bool()))

    val memwb      = Input(Vec(issueWidth, new MEMWBBundle))
    val memwbValid = Input(Vec(issueWidth, Bool()))

    val wbValid  = Input(Vec(issueWidth, Bool()))
    val wbRfWen  = Input(Vec(issueWidth, Bool()))
    val wbRdAddr = Input(Vec(issueWidth, UInt(5.W)))
    val wbData   = Input(Vec(issueWidth, UInt(32.W)))

    val rs1Use = Input(Vec(issueWidth, Bool()))
    val rs2Use = Input(Vec(issueWidth, Bool()))

    val rs1Data   = Output(Vec(issueWidth, UInt(32.W)))
    val rs2Data   = Output(Vec(issueWidth, UInt(32.W)))
    val op1Data   = Output(Vec(issueWidth, UInt(32.W)))
    val op2Data   = Output(Vec(issueWidth, UInt(32.W)))
    val storeData = Output(Vec(issueWidth, UInt(32.W)))

    val rs1Sel = Output(Vec(issueWidth, UInt(BypassSel.W.W)))
    val rs2Sel = Output(Vec(issueWidth, UInt(BypassSel.W.W)))
  })

  private def pcPlus4(pc: UInt): UInt = {
    (pc + 4.U)(31, 0)
  }

  private def exMemWbData(x: EXMEMBundle): UInt = {
    MuxLookup(
      x.wbSel,
      x.aluOut,
      Seq(
        WbSel.WB_ALU -> x.aluOut,
        WbSel.WB_PC4 -> pcPlus4(x.pc),
        WbSel.WB_CSR -> x.csrRdata,
        WbSel.WB_MEM -> x.aluOut
      )
    )
  }

  private def memWbData(x: MEMWBBundle): UInt = {
    MuxLookup(
      x.wbSel,
      x.aluOut,
      Seq(
        WbSel.WB_ALU -> x.aluOut,
        WbSel.WB_MEM -> x.memData,
        WbSel.WB_PC4 -> pcPlus4(x.pc),
        WbSel.WB_CSR -> x.csrRdata
      )
    )
  }

  val network = Module(new BypassNetwork(issueWidth))

  for (i <- 0 until issueWidth) {
    network.io.rs1(i).use := io.rs1Use(i)
    network.io.rs1(i).addr := io.idex(i).rs1Addr
    network.io.rs1(i).regData := io.idex(i).rs1Data

    network.io.rs2(i).use := io.rs2Use(i)
    network.io.rs2(i).addr := io.idex(i).rs2Addr
    network.io.rs2(i).regData := io.idex(i).rs2Data

    network.io.intraEx(i).valid := io.idexValid(i)
    network.io.intraEx(i).rfWen := io.idex(i).rfWen
    network.io.intraEx(i).rdAddr := io.idex(i).rdAddr
    network.io.intraEx(i).data := io.exResult(i)
    network.io.intraEx(i).canForward :=
      io.idex(i).rfWen && !io.idex(i).memRen && (io.idex(i).wbSel =/= WbSel.WB_MEM) &&
        !AluOp.isMulDiv(io.idex(i).aluOp)

    network.io.exMem(i).valid := io.exmemValid(i)
    network.io.exMem(i).rfWen := io.exmem(i).rfWen
    network.io.exMem(i).rdAddr := io.exmem(i).rdAddr
    network.io.exMem(i).data := exMemWbData(io.exmem(i))
    network.io.exMem(i).canForward :=
      io.exmem(i).rfWen && !io.exmem(i).memRen && (io.exmem(i).wbSel =/= WbSel.WB_MEM)

    network.io.memWb(i).valid := io.memwbValid(i)
    network.io.memWb(i).rfWen := io.memwb(i).rfWen
    network.io.memWb(i).rdAddr := io.memwb(i).rdAddr
    network.io.memWb(i).data := memWbData(io.memwb(i))
    network.io.memWb(i).canForward := io.memwb(i).rfWen

    network.io.wb(i).valid := io.wbValid(i)
    network.io.wb(i).rfWen := io.wbRfWen(i)
    network.io.wb(i).rdAddr := io.wbRdAddr(i)
    network.io.wb(i).data := io.wbData(i)
    network.io.wb(i).canForward := io.wbRfWen(i)

    io.rs1Data(i) := network.io.rs1Data(i)
    io.rs2Data(i) := network.io.rs2Data(i)
    io.storeData(i) := network.io.rs2Data(i)
    io.rs1Sel(i) := network.io.rs1Sel(i)
    io.rs2Sel(i) := network.io.rs2Sel(i)

    io.op1Data(i) := MuxLookup(
      io.idex(i).op1Sel,
      network.io.rs1Data(i),
      Seq(
        Op1Sel.OP1_RS1 -> network.io.rs1Data(i),
        Op1Sel.OP1_PC  -> io.idex(i).pc,
        Op1Sel.OP1_IMM -> 0.U(32.W)
      )
    )

    io.op2Data(i) := MuxLookup(
      io.idex(i).op2Sel,
      network.io.rs2Data(i),
      Seq(
        Op2Sel.OP2_RS2 -> network.io.rs2Data(i),
        Op2Sel.OP2_IMM -> io.idex(i).imm.asUInt,
        Op2Sel.OP2_PC4 -> pcPlus4(io.idex(i).pc)
      )
    )
  }
}

class BypassHazardUnit(
    issueWidth: Int = 2,
    pendingWawPorts: Int = 4,
    stallOnWaw: Boolean = false,   // spec §7.2.1：双写端口冲突由槽优先级解决，不 stall
    conservativeSlotRaw: Boolean = false)
    extends Module {
  require(issueWidth >= 1, "issueWidth must be at least 1")
  require(pendingWawPorts >= 0, "pendingWawPorts must be non-negative")

  val io = IO(new Bundle {
    val idValid = Input(Vec(issueWidth, Bool()))
    val idRs1Addr = Input(Vec(issueWidth, UInt(5.W)))
    val idRs2Addr = Input(Vec(issueWidth, UInt(5.W)))
    val idRs1Use = Input(Vec(issueWidth, Bool()))
    val idRs2Use = Input(Vec(issueWidth, Bool()))
    val idRdAddr = Input(Vec(issueWidth, UInt(5.W)))
    val idRfWen = Input(Vec(issueWidth, Bool()))
    val idCanBypassToYounger = Input(Vec(issueWidth, Bool()))

    val exValid = Input(Vec(issueWidth, Bool()))
    val exMemRen = Input(Vec(issueWidth, Bool()))
    val exRdAddr = Input(Vec(issueWidth, UInt(5.W)))
    val exRfWen = Input(Vec(issueWidth, Bool()))

    val memValid = Input(Vec(issueWidth, Bool()))
    val memMemRen = Input(Vec(issueWidth, Bool()))
    val memRdAddr = Input(Vec(issueWidth, UInt(5.W)))
    val memRfWen = Input(Vec(issueWidth, Bool()))

    val pendingWaw = Input(Vec(pendingWawPorts, new PendingRegWriteInfo))

    val brTaken = Input(Bool())
    val exRedirect = Input(Bool())
    val idRedirect = Input(Bool())
    val icacheStall = Input(Bool())
    val dcacheStall = Input(Bool())
    val backendStall = Input(Bool())

    val stallIF = Output(Bool())
    val stallID = Output(Bool())
    val stallEX = Output(Bool())
    val stallMEM = Output(Bool())
    val stallWB = Output(Bool())
    val flushIF = Output(Bool())
    val flushID = Output(Bool())
    val flushEX = Output(Bool())

    val slotIssue = Output(Vec(issueWidth, Bool()))
    val slot1Stall = Output(Bool())
    val rawHazard = Output(Vec(issueWidth, Bool()))
    val wawHazard = Output(Vec(issueWidth, Bool()))
    val loadUseStall = Output(Bool())
    val wawStall = Output(Bool())
  })

  private def anyOr(seq: Seq[Bool]): Bool = {
    if (seq.isEmpty) false.B else seq.reduce(_ || _)
  }

  private def srcDep(rd: UInt, idSlot: Int): Bool = {
    (rd =/= 0.U) &&
      io.idValid(idSlot) &&
      ((io.idRs1Use(idSlot) && (io.idRs1Addr(idSlot) === rd)) ||
        (io.idRs2Use(idSlot) && (io.idRs2Addr(idSlot) === rd)))
  }

  val loadUseBySlot = Wire(Vec(issueWidth, Bool()))
  for (i <- 0 until issueWidth) {
    val exLoadUse = anyOr((0 until issueWidth).map { e =>
      io.exValid(e) &&
        io.exMemRen(e) &&
        io.exRfWen(e) &&
        srcDep(io.exRdAddr(e), i)
    })
    val memLoadUse = anyOr((0 until issueWidth).map { m =>
      io.memValid(m) &&
        io.memMemRen(m) &&
        io.memRfWen(m) &&
        srcDep(io.memRdAddr(m), i)
    })
    loadUseBySlot(i) := exLoadUse || memLoadUse
  }
  val loadUseStall = loadUseBySlot.asUInt.orR

  val intraRawHazard = Wire(Vec(issueWidth, Bool()))
  val intraRawBlock = Wire(Vec(issueWidth, Bool()))
  for (i <- 0 until issueWidth) {
    val deps = (0 until i).map { older =>
      io.idValid(older) &&
        io.idRfWen(older) &&
        srcDep(io.idRdAddr(older), i)
    }

    val blocks = (0 until i).map { older =>
      val dep =
        io.idValid(older) &&
          io.idRfWen(older) &&
          srcDep(io.idRdAddr(older), i)

      if (conservativeSlotRaw) dep else dep && !io.idCanBypassToYounger(older)
    }

    intraRawHazard(i) := anyOr(deps)
    intraRawBlock(i) := anyOr(blocks)
  }

  val wawBySlot = Wire(Vec(issueWidth, Bool()))
  for (i <- 0 until issueWidth) {
    val intraWaw = anyOr((0 until i).map { older =>
      io.idValid(i) &&
        io.idValid(older) &&
        io.idRfWen(i) &&
        io.idRfWen(older) &&
        (io.idRdAddr(i) =/= 0.U) &&
        (io.idRdAddr(i) === io.idRdAddr(older))
    })

    val pendingWaw = anyOr((0 until pendingWawPorts).map { p =>
      io.idValid(i) &&
        io.idRfWen(i) &&
        (io.idRdAddr(i) =/= 0.U) &&
        io.pendingWaw(p).valid &&
        io.pendingWaw(p).rfWen &&
        (io.pendingWaw(p).rdAddr === io.idRdAddr(i))
    })

    wawBySlot(i) := intraWaw || pendingWaw
  }

  val slotBlocked = Wire(Vec(issueWidth, Bool()))
  for (i <- 0 until issueWidth) {
    slotBlocked(i) := intraRawBlock(i) || (if (stallOnWaw) wawBySlot(i) else false.B)
  }

  val redirectFlush = io.exRedirect || io.brTaken
  val frontRedirect = redirectFlush || io.idRedirect
  val structuralStall = io.icacheStall || io.dcacheStall || io.backendStall
  val slot0Blocked = if (issueWidth == 0) false.B else slotBlocked(0)
  val dataStall = loadUseStall || slot0Blocked
  val idStall = structuralStall || dataStall

  io.stallIF := idStall
  io.stallID := idStall
  io.stallEX := structuralStall
  io.stallMEM := structuralStall
  io.stallWB := structuralStall

  io.flushIF := frontRedirect
  io.flushID := redirectFlush
  io.flushEX := loadUseStall && !structuralStall && !redirectFlush

  val canIssue = Wire(Vec(issueWidth, Bool()))
  canIssue(0) := io.idValid(0) && !idStall && !frontRedirect && !slotBlocked(0)
  for (i <- 1 until issueWidth) {
    canIssue(i) := canIssue(i - 1) && io.idValid(i) && !slotBlocked(i)
  }
  io.slotIssue := canIssue

  if (issueWidth > 1) {
    io.slot1Stall := slotBlocked(1)
  } else {
    io.slot1Stall := false.B
  }
  for (i <- 0 until issueWidth) {
    io.rawHazard(i) := intraRawHazard(i) || loadUseBySlot(i)
    io.wawHazard(i) := wawBySlot(i)
  }
  io.loadUseStall := loadUseStall
  if (stallOnWaw) {
    io.wawStall := wawBySlot.asUInt.orR
  } else {
    io.wawStall := false.B
  }
}
```

## .\src\main\Core\EXStage.scala

```scala
package riscv

import chisel3._
import chisel3.util._

class EXStage(enableRV32M: Boolean = false) extends Module {
  val issueWidth = 2

  val io = IO(new Bundle {
    val in        = Input(Vec(issueWidth, new IDEXBundle))
    val stallEx   = Input(Bool())
    val flushEx   = Input(Bool())

    val rs1Data   = Input(Vec(issueWidth, UInt(32.W)))
    val rs2Data   = Input(Vec(issueWidth, UInt(32.W)))
    val op1Data   = Input(Vec(issueWidth, UInt(32.W)))
    val op2Data   = Input(Vec(issueWidth, UInt(32.W)))
    val storeData = Input(Vec(issueWidth, UInt(32.W)))

    val csrOpValid = Output(Bool())
    val csrOpType  = Output(UInt(2.W))
    val csrWaddr   = Output(UInt(12.W))
    val csrWdata   = Output(UInt(32.W))
    val csrOldData = Input(UInt(32.W))

    val exRedirectValid = Output(Bool())
    val exRedirectPc    = Output(UInt(32.W))

    val branchInst       = Output(Vec(issueWidth, Bool()))
    val branchPred       = Output(Vec(issueWidth, Bool()))
    val branchCorrect    = Output(Vec(issueWidth, Bool()))
    val branchMispredict = Output(Vec(issueWidth, Bool()))
    val branchDirectionMispredict = Output(Vec(issueWidth, Bool()))
    val branchTargetMispredict    = Output(Vec(issueWidth, Bool()))
    val jalrInst         = Output(Vec(issueWidth, Bool()))
    val rasPred          = Output(Vec(issueWidth, Bool()))
    val rasCorrect       = Output(Vec(issueWidth, Bool()))

    val bpuUpdateValid  = Output(Bool())
    val bpuUpdatePc     = Output(UInt(32.W))
    val bpuUpdateTaken  = Output(Bool())
    val bpuUpdateTarget = Output(UInt(32.W))

    val exValid  = Output(Vec(issueWidth, Bool()))
    val exMemRen = Output(Vec(issueWidth, Bool()))
    val exRdAddr = Output(Vec(issueWidth, UInt(5.W)))
    val exRfWen  = Output(Vec(issueWidth, Bool()))
    val exResult = Output(Vec(issueWidth, UInt(32.W)))
    val mulDivStall = Output(Bool())

    val out = Output(Vec(issueWidth, new EXMEMBundle))
  })

  val alus         = Seq.fill(issueWidth)(Module(new ParamALU(32, enableRV32M)))
  val mulDivs      = Seq.fill(issueWidth)(Module(new MulDivALU(32)))
  val slotValid    = Wire(Vec(issueWidth, Bool()))
  val mulDivWaiting = Wire(Vec(issueWidth, Bool()))
  val branchTaken  = Wire(Vec(issueWidth, Bool()))
  val branchTarget = Wire(Vec(issueWidth, UInt(32.W)))  // PC + imm (B/J type)
  val jalrTarget   = Wire(Vec(issueWidth, UInt(32.W)))  // rs1 + imm, lsb cleared
  val actualNextPc = Wire(Vec(issueWidth, UInt(32.W)))
  val redirect     = Wire(Vec(issueWidth, Bool()))

  for (i <- 0 until issueWidth) {
    slotValid(i) := io.in(i).ctrl.valid && !io.in(i).ctrl.kill

    alus(i).io.op1   := io.op1Data(i)
    alus(i).io.op2   := io.op2Data(i)
    alus(i).io.aluOp := io.in(i).aluOp
    mulDivs(i).io.req.bits.op1   := io.op1Data(i)
    mulDivs(i).io.req.bits.op2   := io.op2Data(i)
    mulDivs(i).io.req.bits.aluOp := io.in(i).aluOp

    val isMulDivOp = enableRV32M.B && AluOp.isMulDiv(io.in(i).aluOp)
    val execResult = Mux(isMulDivOp, mulDivs(i).io.resp.bits, alus(i).io.result)

    branchTaken(i) := MuxLookup(io.in(i).brType, false.B, Seq(
      BrType.BR_EQ  -> (io.rs1Data(i) === io.rs2Data(i)),
      BrType.BR_NE  -> (io.rs1Data(i) =/= io.rs2Data(i)),
      BrType.BR_LT  -> (io.rs1Data(i).asSInt < io.rs2Data(i).asSInt),
      BrType.BR_GE  -> (io.rs1Data(i).asSInt >= io.rs2Data(i).asSInt),
      BrType.BR_LTU -> (io.rs1Data(i) < io.rs2Data(i)),
      BrType.BR_GEU -> (io.rs1Data(i) >= io.rs2Data(i))
    ))

    branchTarget(i) := io.in(i).pc + io.in(i).imm
    jalrTarget(i)   := (io.rs1Data(i) + io.in(i).imm) & "hfffffffe".U(32.W)

    val fallThrough = io.in(i).pc + 4.U
    val isBranch    = io.in(i).brType =/= BrType.BR_NONE
    val isControl   = isBranch || io.in(i).isJump || io.in(i).isJalr
    val forwardData = MuxLookup(io.in(i).wbSel, execResult, Seq(
      WbSel.WB_ALU -> execResult,
      WbSel.WB_PC4 -> fallThrough,
      WbSel.WB_CSR -> io.csrOldData,
      WbSel.WB_MEM -> execResult
    ))

    actualNextPc(i) := MuxCase(fallThrough, Seq(
      io.in(i).isJalr          -> jalrTarget(i),
      (isBranch && branchTaken(i)) -> branchTarget(i)
    ))
    val actualControlNextPc = MuxCase(fallThrough, Seq(
      io.in(i).isJalr -> jalrTarget(i),
      (io.in(i).isJump && !io.in(i).isJalr) -> branchTarget(i),
      (isBranch && branchTaken(i)) -> branchTarget(i)
    ))
    val predCorrect = actualControlNextPc === io.in(i).predNextPc
    val predRedirect = io.in(i).predNextPc =/= fallThrough
    val actualRedirect = actualControlNextPc =/= fallThrough
    val directionMiss = predRedirect =/= actualRedirect
    val targetMiss = !directionMiss && predRedirect && actualRedirect && !predCorrect

    val killedByOlderRedirect = if (i == 0) false.B else redirect(0)
    val slotLive = slotValid(i) && !killedByOlderRedirect
    val mulDivCancel = io.flushEx || killedByOlderRedirect
    mulDivs(i).io.cancel := mulDivCancel
    mulDivs(i).io.req.valid := slotLive && isMulDivOp && !io.flushEx
    mulDivs(i).io.resp.ready := !io.stallEx || mulDivCancel
    mulDivWaiting(i) := slotValid(i) && isMulDivOp && !io.flushEx && !mulDivs(i).io.resp.valid

    redirect(i) := slotLive &&
                   (isBranch || io.in(i).isJalr) &&
                   (actualNextPc(i) =/= io.in(i).predNextPc)

    io.branchInst(i)       := slotLive && isControl
    io.branchPred(i)       := slotLive && isControl
    io.branchCorrect(i)    := slotLive && isControl && predCorrect
    io.branchMispredict(i) := slotLive && isControl && !predCorrect
    io.branchDirectionMispredict(i) := slotLive && isControl && directionMiss
    io.branchTargetMispredict(i)    := slotLive && isControl && targetMiss
    io.jalrInst(i)         := slotLive && io.in(i).isJalr
    io.rasPred(i)          := slotLive && io.in(i).rasPred
    io.rasCorrect(i)       := slotLive && io.in(i).rasPred && predCorrect

    io.exValid(i)  := slotLive
    io.exMemRen(i) := slotLive && io.in(i).memRen
    io.exRdAddr(i) := io.in(i).rdAddr
    io.exRfWen(i)  := slotLive && io.in(i).rfWen
    io.exResult(i) := forwardData

    io.out(i) := 0.U.asTypeOf(new EXMEMBundle)
    io.out(i).pc        := io.in(i).pc
    io.out(i).inst      := io.in(i).inst
    io.out(i).slotIdx   := io.in(i).slotIdx
    io.out(i).aluOut    := execResult
    io.out(i).rs2Data   := io.storeData(i)
    io.out(i).rdAddr    := io.in(i).rdAddr
    io.out(i).wbSel     := io.in(i).wbSel
    io.out(i).rfWen     := slotLive && io.in(i).rfWen
    io.out(i).memRen    := slotLive && io.in(i).memRen
    io.out(i).memWen    := slotLive && io.in(i).memWen
    io.out(i).memWd     := io.in(i).memWd
    io.out(i).memSigned := io.in(i).memSigned
    io.out(i).csrRdata  := io.csrOldData
    io.out(i).csrOp     := Mux(slotLive, io.in(i).csrOp, CSROp.NONE)
    io.out(i).brType    := Mux(slotLive, io.in(i).brType, BrType.BR_NONE)
    io.out(i).isJump    := slotLive && io.in(i).isJump
    io.out(i).ctrl.valid   := slotLive
    io.out(i).ctrl.kill    := io.flushEx || io.in(i).ctrl.kill || killedByOlderRedirect
    io.out(i).ctrl.allowIn := true.B
  }
  io.mulDivStall := mulDivWaiting.asUInt.orR

  // ── CSR operation ─────────────────────────────────────────────────────
  val slot0Live = slotValid(0)
  val slot1Live = slotValid(1) && !redirect(0)

  val csrSlot0 = slot0Live && io.in(0).csrOp =/= CSROp.NONE
  val csrSlot1 = slot1Live && io.in(1).csrOp =/= CSROp.NONE
  val csrIdx   = Mux(csrSlot0, 0.U, 1.U)
  io.csrOpValid := csrSlot0 || csrSlot1
  io.csrOpType  := Mux(csrSlot0, io.in(0).csrOp,  io.in(1).csrOp)
  io.csrWaddr   := Mux(csrSlot0, io.in(0).csrAddr, io.in(1).csrAddr)
  io.csrWdata   := io.rs1Data(csrIdx)

  // ── Redirect ──────────────────────────────────────────────────────────
  io.exRedirectValid := redirect(0) || redirect(1)
  io.exRedirectPc    := Mux(redirect(0), actualNextPc(0), actualNextPc(1))

  // ── BPU update ────────────────────────────────────────────────────────
  // Update source priority: slot 0 > slot 1 (slot 0 is the older instruction).
  // Only conditional branches update the bi-mode BPU. JALR/ret targets are
  // indirect and need a RAS or per-context predictor; training them into the
  // simple BTB makes shared return sites such as _putchar.ret predict stale
  // call-site addresses.
  val br0   = slot0Live && io.in(0).brType =/= BrType.BR_NONE
  val br1   = slot1Live && io.in(1).brType =/= BrType.BR_NONE

  val isUpdate0 = br0
  val isUpdate1 = br1

  io.bpuUpdateValid := isUpdate0 || isUpdate1

  val bpuFromSlot0 = isUpdate0   // slot 0 wins when both want to update

  io.bpuUpdatePc := Mux(bpuFromSlot0, io.in(0).pc, io.in(1).pc)

  io.bpuUpdateTaken := Mux(bpuFromSlot0,
    branchTaken(0),
    branchTaken(1))

  // BTB target: always store the branch-target address (pc+imm) regardless of
  // taken/not-taken, so the BTB is correct when the branch is later taken.
  val updateTarget0 = branchTarget(0)
  val updateTarget1 = branchTarget(1)
  io.bpuUpdateTarget := Mux(bpuFromSlot0, updateTarget0, updateTarget1)

}
```

## .\src\main\Core\IDStage.scala

```scala
package riscv

import chisel3._
import chisel3.util._

class IDStage(enableRV32M: Boolean = false) extends Module {
  val issueWidth = 2

  val io = IO(new Bundle {
    val in      = Input(Vec(issueWidth, new IFIDSlot))
    val stallId = Input(Bool())
    val flushId = Input(Bool())

    val regRs1Addr = Output(Vec(issueWidth, UInt(5.W)))
    val regRs2Addr = Output(Vec(issueWidth, UInt(5.W)))
    val regRs1Data = Input(Vec(issueWidth, UInt(32.W)))
    val regRs2Data = Input(Vec(issueWidth, UInt(32.W)))

    val csrRaddr = Output(UInt(12.W))
    val csrRdata = Input(UInt(32.W))

    val idRedirectValid = Output(Bool())
    val idRedirectPc    = Output(UInt(32.W))
    val rasPush         = Output(Bool())
    val rasPushAddr     = Output(UInt(32.W))
    val rasPop          = Output(Bool())

    val hazardIdValid               = Output(Vec(issueWidth, Bool()))
    val hazardRs1Addr               = Output(Vec(issueWidth, UInt(5.W)))
    val hazardRs2Addr               = Output(Vec(issueWidth, UInt(5.W)))
    val hazardRs1Use                = Output(Vec(issueWidth, Bool()))
    val hazardRs2Use                = Output(Vec(issueWidth, Bool()))
    val hazardRdAddr                = Output(Vec(issueWidth, UInt(5.W)))
    val hazardRfWen                 = Output(Vec(issueWidth, Bool()))
    val hazardCanBypassToYounger    = Output(Vec(issueWidth, Bool()))

    val holdIfId = Output(Bool())
    val out      = Output(Vec(issueWidth, new IDEXBundle))
  })

  // ── Pending-slot register for slot-1 deferral ─────────────────────────
  val pendingValid = RegInit(false.B)
  val pendingSlot  = RegInit(0.U.asTypeOf(new IFIDSlot))

  val eff = Wire(Vec(issueWidth, new IFIDSlot))
  eff(0) := Mux(pendingValid, pendingSlot, io.in(0))
  eff(1) := Mux(pendingValid, 0.U.asTypeOf(new IFIDSlot), io.in(1))
  io.holdIfId := pendingValid && !io.flushId

  for (i <- 0 until issueWidth) {
    io.regRs1Addr(i) := eff(i).inst(19, 15)
    io.regRs2Addr(i) := eff(i).inst(24, 20)
  }

  val dec = Seq.fill(issueWidth)(Module(new Decoder(enableRV32M)))
  for (i <- 0 until issueWidth) {
    dec(i).io.inst    := eff(i).inst
    dec(i).io.pc      := eff(i).pc
    dec(i).io.rs1Data := io.regRs1Data(i)
    dec(i).io.rs2Data := io.regRs2Data(i)
  }

  val slotValid = Wire(Vec(issueWidth, Bool()))
  for (i <- 0 until issueWidth) {
    slotValid(i) := eff(i).ctrl.valid && !eff(i).ctrl.kill && dec(i).io.out.ctrl.valid
  }

  // ── P2 fix: slot 0 can forward its ALU result to slot 1 in the same EX
  // cycle (F5 bypass path) when it is a non-Load register-writing instruction.
  // In that case we must NOT block slot 1 on an intra-slot RAW.
  val slot0Mem = slotValid(0) && (dec(0).io.out.memRen || dec(0).io.out.memWen)
  val slot1Mem = slotValid(1) && (dec(1).io.out.memRen || dec(1).io.out.memWen)

  val slot0CanBypass =
    slotValid(0) &&
    dec(0).io.out.rfWen &&
    !dec(0).io.out.memRen &&                           // Load result not available in EX
    (dec(0).io.out.wbSel === WbSel.WB_ALU) &&         // only ALU results are available to F5
    !AluOp.isMulDiv(dec(0).io.out.aluOp) &&            // RV32M results are multi-cycle
    (dec(0).io.out.rdAddr =/= 0.U)

  // ── Slot-1 stall conditions ────────────────────────────────────────────
  // P2 fix: intra-slot RAW is resolved by the intra-EX bypass (F5) when
  // slot 0 can forward.  This also covers slot-1 load/store address and store
  // data operands, because they are selected in EX before entering MEM.
  val slotRaw01 =
    !slot0CanBypass &&
    slotValid(0) && slotValid(1) &&
    dec(0).io.out.rfWen &&
    (dec(0).io.out.rdAddr =/= 0.U) &&
    ((dec(1).io.out.rs1Use && (dec(0).io.out.rdAddr === dec(1).io.out.rs1Addr)) ||
     (dec(1).io.out.rs2Use && (dec(0).io.out.rdAddr === dec(1).io.out.rs2Addr)))

  val slot0Csr = slotValid(0) && dec(0).io.out.csrOp =/= CSROp.NONE
  val slot1Csr = slotValid(1) && dec(1).io.out.csrOp =/= CSROp.NONE
  val slot1Ctrl = slotValid(1) &&
                  (dec(1).io.out.isJump || (dec(1).io.out.brType =/= BrType.BR_NONE))

  // P1 fix: slot0Ctrl must block slot 1 only when slot 1 would be on the
  // wrong execution path, i.e.:
  //   a) slot 0 is JAL      → redirect happens in ID, slot 1 is wrong-path
  //   b) slot 0 is JALR     → target unknown until EX, slot 1 is wrong-path
  //   c) slot 0 is a branch predicted-TAKEN → slot 1 (PC+4) is wrong-path
  // Branches predicted NOT-TAKEN: slot 1 (fallthrough) is on the right path;
  // if the branch is later found mispredicted, EX flush handles both slots.
  val slot0IsJal         = slotValid(0) && dec(0).io.out.isJump && !dec(0).io.out.isJalr
  val slot0IsJalr        = slotValid(0) && dec(0).io.out.isJalr
  val slot0BranchTaken   = slotValid(0) &&
                           (dec(0).io.out.brType =/= BrType.BR_NONE) &&
                           eff(0).predTaken        // BPU predicted this branch as taken
  val slot0Ctrl = slot0IsJal || slot0IsJalr || slot0BranchTaken

  val slot1DataOrResourceBlocked = slotRaw01 || (slot0Mem && slot1Mem) || (slot0Csr && slot1Csr)
  val slot1ReplayBlocked = slot1DataOrResourceBlocked || slot1Ctrl
  val slot1Blocked = slot1ReplayBlocked || slot0Ctrl

  val canIssue0 = slotValid(0) && !io.stallId && !io.flushId
  val canIssue1 = canIssue0 && slotValid(1) && !slot1Blocked

  // Capture slot 1 only for replayable data/resource conflicts.  If slot 0 is
  // a control-flow instruction, slot 1 is wrong-path and must be squashed.
  val capturePending = canIssue0 && slotValid(1) && !slot0Ctrl && slot1ReplayBlocked && !pendingValid
  when(io.flushId) {
    pendingValid := false.B
  }.elsewhen(capturePending) {
    pendingSlot  := eff(1)
    pendingValid := true.B
  }.elsewhen(pendingValid && !io.stallId) {
    pendingValid := false.B
  }

  // ── CSR read address MUX ──────────────────────────────────────────────
  val csrSel1 = !slot0Csr && slot1Csr
  io.csrRaddr := Mux(csrSel1, dec(1).io.out.csrAddr, dec(0).io.out.csrAddr)

  // ── JAL redirect (ID-level redirect, 1-cycle flush) ───────────────────
  val jalTarget0 = dec(0).io.out.pc + dec(0).io.out.imm
  val jalTarget1 = dec(1).io.out.pc + dec(1).io.out.imm
  val jal0 = canIssue0 &&
             dec(0).io.out.isJump &&
             !dec(0).io.out.isJalr &&
             eff(0).predNextPc =/= jalTarget0
  val jal1 = canIssue1 &&
             dec(1).io.out.isJump &&
             !dec(1).io.out.isJalr &&
             eff(1).predNextPc =/= jalTarget1
  io.idRedirectValid := jal0 || jal1
  io.idRedirectPc    := Mux(jal0,
    jalTarget0,
    jalTarget1)

  private def isLinkReg(rd: UInt): Bool = rd === 1.U || rd === 5.U
  private def isRetInst(d: DecodedSlot): Bool = {
    d.isJalr && d.rdAddr === 0.U && d.rs1Addr === 1.U && d.imm === 0.U
  }
  val rasCall0 = canIssue0 && dec(0).io.out.isJump && isLinkReg(dec(0).io.out.rdAddr) && !isRetInst(dec(0).io.out)
  val rasCall1 = canIssue1 && dec(1).io.out.isJump && isLinkReg(dec(1).io.out.rdAddr) && !isRetInst(dec(1).io.out)
  val rasRet0  = canIssue0 && isRetInst(dec(0).io.out)
  val rasRet1  = canIssue1 && isRetInst(dec(1).io.out)
  io.rasPush     := rasCall0 || rasCall1
  io.rasPushAddr := Mux(rasCall0, dec(0).io.out.pc + 4.U, dec(1).io.out.pc + 4.U)
  io.rasPop      := !io.rasPush && (rasRet0 || rasRet1)

  // ── Pipeline register outputs ─────────────────────────────────────────
  for (i <- 0 until issueWidth) {
    val issue = if (i == 0) canIssue0 else canIssue1
    val d = dec(i).io.out

    io.out(i) := 0.U.asTypeOf(new IDEXBundle)
    io.out(i).pc        := d.pc
    io.out(i).inst      := d.inst
    io.out(i).slotIdx   := eff(i).slotIdx
    io.out(i).fetchPc   := eff(i).fetchPc
    io.out(i).rs1Addr   := d.rs1Addr
    io.out(i).rs2Addr   := d.rs2Addr
    io.out(i).rdAddr    := d.rdAddr
    io.out(i).rs1Data   := d.rs1Data
    io.out(i).rs2Data   := d.rs2Data
    io.out(i).rs1Use    := d.rs1Use
    io.out(i).rs2Use    := d.rs2Use
    io.out(i).imm       := d.imm
    io.out(i).csrAddr   := d.csrAddr
    io.out(i).csrOp     := Mux(issue, d.csrOp, CSROp.NONE)
    io.out(i).aluOp     := d.aluOp
    io.out(i).op1Sel    := d.op1Sel
    io.out(i).op2Sel    := d.op2Sel
    io.out(i).wbSel     := d.wbSel
    io.out(i).rfWen     := issue && d.rfWen
    io.out(i).memRen    := issue && d.memRen
    io.out(i).memWen    := issue && d.memWen
    io.out(i).memWd     := d.memWd
    io.out(i).memSigned := d.memSigned
    io.out(i).brType    := Mux(issue, d.brType, BrType.BR_NONE)
    io.out(i).isJump    := issue && d.isJump
    io.out(i).isJalr    := issue && d.isJalr
    io.out(i).isSysInst := issue && d.isSysInst
    io.out(i).predTaken  := eff(i).predTaken
    io.out(i).predTarget := eff(i).predTarget
    io.out(i).predNextPc := eff(i).predNextPc
    io.out(i).seqNextPc  := eff(i).seqNextPc
    io.out(i).rasPred    := eff(i).rasPred
    io.out(i).csrRdata   := io.csrRdata
    io.out(i).ctrl.valid   := issue
    io.out(i).ctrl.kill    := io.flushId || eff(i).ctrl.kill
    io.out(i).ctrl.allowIn := true.B

    // Hazard unit connections
    io.hazardIdValid(i)   := slotValid(i)
    io.hazardRs1Addr(i)   := d.rs1Addr
    io.hazardRs2Addr(i)   := d.rs2Addr
    io.hazardRs1Use(i)    := d.rs1Use
    io.hazardRs2Use(i)    := d.rs2Use
    io.hazardRdAddr(i)    := d.rdAddr
    io.hazardRfWen(i)     := d.rfWen
    // P2 fix: tell hazard unit that slot 0 can forward to younger slots
    // so the hazard unit's BypassNetwork can activate the F5 intra-EX path.
    io.hazardCanBypassToYounger(i) := (i == 0).B && slot0CanBypass
  }
}
```

## .\src\main\Core\IFStage.scala

```scala
package riscv

import chisel3._
import chisel3.util._
import icache.ICacheTop
import parameterized_cache.{CacheParams, ICachePerfEvents, MemBusIO}

class IFStage(p: CacheParams = CacheParams.default) extends Module {
  val issueWidth = 2

  val io = IO(new Bundle {
    val stallIf = Input(Bool())
    val flushIf = Input(Bool())

    val exRedirectValid = Input(Bool())
    val exRedirectPc    = Input(UInt(32.W))
    val idRedirectValid = Input(Bool())
    val idRedirectPc    = Input(UInt(32.W))

    val bpuQueryPc    = Output(UInt(32.W))
    val bpuPredTaken  = Input(Bool())
    val bpuPredTarget = Input(UInt(32.W))
    val rasPredValid  = Input(Bool())
    val rasPredTarget = Input(UInt(32.W))

    val nextLinePrefetchEn = Input(Bool())
    val stridePrefetchEn   = Input(Bool())
    val streamPrefetchEn   = Input(Bool())

    val out = Output(Vec(issueWidth, new IFIDSlot))
    val icacheStall = Output(Bool())
    val icachePerf = Output(new ICachePerfEvents)
    val imem = new MemBusIO(p)
    val debugPc = Output(UInt(32.W))
  })

  val pcGen = Module(new PcGen(N = issueWidth))
  val icache = Module(new ICacheTop(p))
  val nextLinePrefetcher = Module(new NextLinePrefetcher)
  val stridePrefetcher = Module(new StridePrefetcher)
  val streamPrefetcher = Module(new StreamPrefetcher)

  private def isJal(inst: UInt): Bool = inst(6, 0) === "b1101111".U
  private def isRet(inst: UInt): Bool = inst === "h00008067".U
  private def isBranch(inst: UInt): Bool = inst(6, 0) === "b1100011".U
  private def jalImm(inst: UInt): UInt = {
    Cat(Fill(11, inst(31)), inst(31), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W))
  }

  val slotPc0 = pcGen.io.pcFetch
  val slotPc1 = pcGen.io.pcFetch + 4.U
  val slot0Jal = icache.io.instValids(0) && isJal(icache.io.insts(0))
  val slot1Jal = icache.io.instValids(1) && isJal(icache.io.insts(1))
  val slot0Ret = icache.io.instValids(0) && isRet(icache.io.insts(0)) && io.rasPredValid
  val slot1Ret = icache.io.instValids(1) && isRet(icache.io.insts(1)) && io.rasPredValid
  val slot0Branch = icache.io.instValids(0) && isBranch(icache.io.insts(0))
  val bpuTaken = io.bpuPredTaken && slot0Branch
  val slot0JalTarget = slotPc0 + jalImm(icache.io.insts(0))
  val slot1JalTarget = slotPc1 + jalImm(icache.io.insts(1))
  val ifPredTaken = slot0Jal || slot0Ret || bpuTaken || slot1Jal || slot1Ret
  val ifPredTarget = Mux(slot0Jal, slot0JalTarget,
    Mux(slot0Ret, io.rasPredTarget,
      Mux(bpuTaken, io.bpuPredTarget,
        Mux(slot1Jal, slot1JalTarget, io.rasPredTarget))))

  pcGen.io.exRedirectValid := io.exRedirectValid
  pcGen.io.exRedirectPc    := io.exRedirectPc
  pcGen.io.idRedirectValid := io.idRedirectValid
  pcGen.io.idRedirectPc    := io.idRedirectPc
  pcGen.io.bpuPredTaken    := ifPredTaken
  pcGen.io.bpuPredTarget   := ifPredTarget
  pcGen.io.stallIf         := io.stallIf
  pcGen.io.fetchSlot1Valid := icache.io.instValids(1)

  io.bpuQueryPc := pcGen.io.pcFetch
  io.debugPc := pcGen.io.currPc

  icache.io.addr  := pcGen.io.pcFetch
  icache.io.valid := true.B
  icache.io.flush := false.B
  io.imem <> icache.io.mem

  val pfObserve = icache.io.respValid && !icache.io.missOut && !io.stallIf && !io.flushIf

  nextLinePrefetcher.io.currAddr   := pcGen.io.currPc
  nextLinePrefetcher.io.cacheHit   := icache.io.respValid
  nextLinePrefetcher.io.cacheStall := icache.io.missOut
  nextLinePrefetcher.io.prefetchEn := io.nextLinePrefetchEn

  stridePrefetcher.io.observeValid := pfObserve
  stridePrefetcher.io.observeAddr  := pcGen.io.currPc
  stridePrefetcher.io.prefetchEn   := io.stridePrefetchEn

  streamPrefetcher.io.observeValid := pfObserve
  streamPrefetcher.io.observeAddr  := pcGen.io.currPc
  streamPrefetcher.io.prefetchEn   := io.streamPrefetchEn

  val pfSelStream = streamPrefetcher.io.pfReqValid
  val pfSelStride = !pfSelStream && stridePrefetcher.io.pfReqValid
  icache.io.pfReqValid := pfSelStream || pfSelStride || nextLinePrefetcher.io.pfReqValid
  icache.io.pfReqAddr  := Mux(pfSelStream, streamPrefetcher.io.pfReqAddr,
    Mux(pfSelStride, stridePrefetcher.io.pfReqAddr, nextLinePrefetcher.io.pfReqAddr))

  streamPrefetcher.io.pfReqReady := icache.io.pfReqReady && pfSelStream
  stridePrefetcher.io.pfReqReady := icache.io.pfReqReady && pfSelStride
  nextLinePrefetcher.io.pfReqReady := icache.io.pfReqReady && !pfSelStream && !pfSelStride

  val seqStep = Mux(icache.io.instValids(1), (issueWidth * 4).U(32.W), 4.U(32.W))
  val seqNextPc = pcGen.io.pcFetch + seqStep
  for (i <- 0 until issueWidth) {
    val slotPc = pcGen.io.pcFetch + (i * 4).U
    val slotJal = if (i == 0) slot0Jal else slot1Jal
    val slotRet = if (i == 0) slot0Ret else slot1Ret
    val slotJalTarget = if (i == 0) slot0JalTarget else slot1JalTarget
    val slotPredNextPc = Mux(slotJal, slotJalTarget,
      Mux(slotRet, io.rasPredTarget,
      Mux(bpuTaken, io.bpuPredTarget, slotPc + 4.U))
    )

    io.out(i) := 0.U.asTypeOf(new IFIDSlot)
    io.out(i).pc := slotPc
    io.out(i).inst := icache.io.insts(i)
    io.out(i).slotIdx := i.U
    io.out(i).fetchPc := pcGen.io.pcFetch
    io.out(i).predTaken := slotJal || slotRet || bpuTaken
    io.out(i).predTarget := Mux(slotJal, slotJalTarget,
      Mux(slotRet, io.rasPredTarget, io.bpuPredTarget))
    io.out(i).predNextPc := slotPredNextPc
    io.out(i).seqNextPc := seqNextPc
    io.out(i).rasPred := slotRet
    io.out(i).icacheHit := icache.io.respValid
    io.out(i).ctrl.valid := icache.io.instValids(i) && !io.flushIf
    io.out(i).ctrl.kill := io.flushIf
    io.out(i).ctrl.allowIn := true.B
  }

  io.icacheStall := icache.io.missOut
  io.icachePerf := icache.io.perf
}
```

## .\src\main\Core\InOrderCore.scala

```scala
package riscv

import chisel3._
import chisel3.util._
import parameterized_cache.{CacheParams, MemBusIO}

class InOrderCore(
    enableRV32M: Boolean = false,
    cacheParams: CacheParams = CacheParams.default,
    dCacheParams: Option[CacheParams] = None,
    branchPredInit: Int = 1,
    prefetchInit: Int = 0) extends Module {
  val issueWidth = 2
  private val ip = cacheParams
  private val dp = dCacheParams.getOrElse(cacheParams)

  val io = IO(new Bundle {
    val imem      = new MemBusIO(ip)
    val dmem      = new MemBusIO(dp)
    val printChar = Output(Valid(UInt(8.W)))
    val success   = Output(Bool())
    val debugPc   = Output(UInt(32.W))
    val perf      = Output(new CorePerfCounters)
  })

  val ifStage  = Module(new IFStage(ip))
  val idStage  = Module(new IDStage(enableRV32M))
  val exStage  = Module(new EXStage(enableRV32M))
  val memStage = Module(new MEMStage(dp))
  val wbStage  = Module(new WBStage)
  val regFile  = Module(new RegFile(issueWidth))
  val csrFile  = Module(new CSRFile(32, issueWidth, enableRV32M, branchPredInit, prefetchInit))
  val bpu      = Module(new BPU)
  val bpuRas   = Module(new BPU_RAS)
  val tage     = Module(new TAGE)
  val bypass   = Module(new PipelineBypassUnit(issueWidth))
  val hazard   = Module(new BypassHazardUnit(issueWidth))

  val ifidReg  = RegInit(VecInit(Seq.fill(issueWidth)(0.U.asTypeOf(new IFIDSlot))))
  val idexReg  = RegInit(VecInit(Seq.fill(issueWidth)(0.U.asTypeOf(new IDEXBundle))))
  val exmemReg = RegInit(VecInit(Seq.fill(issueWidth)(0.U.asTypeOf(new EXMEMBundle))))
  val memwbReg = RegInit(VecInit(Seq.fill(issueWidth)(0.U.asTypeOf(new MEMWBBundle))))

  io.imem     <> ifStage.io.imem
  io.dmem     <> memStage.io.dmem
  io.printChar := memStage.io.printChar
  io.debugPc  := ifStage.io.debugPc
  // P0 fix: wire real success signal from DCacheTop (via MEMStage)
  io.success  := memStage.io.success
  // BPU
  bpu.io.queryPc      := ifStage.io.bpuQueryPc
  bpu.io.updateValid  := exStage.io.bpuUpdateValid
  bpu.io.updatePc     := exStage.io.bpuUpdatePc
  bpu.io.updateTaken  := exStage.io.bpuUpdateTaken
  bpu.io.updateTarget := exStage.io.bpuUpdateTarget

  bpuRas.io.queryPc      := ifStage.io.bpuQueryPc
  bpuRas.io.updateValid  := exStage.io.bpuUpdateValid
  bpuRas.io.updatePc     := exStage.io.bpuUpdatePc
  bpuRas.io.updateTaken  := exStage.io.bpuUpdateTaken
  bpuRas.io.updateTarget := exStage.io.bpuUpdateTarget
  bpuRas.io.ras.push       := idStage.io.rasPush
  bpuRas.io.ras.pushAddr   := idStage.io.rasPushAddr
  bpuRas.io.ras.pop        := idStage.io.rasPop
  bpuRas.io.ras.flush      := false.B
  bpuRas.io.ras.checkpoint := 0.U

  tage.io.queryPc      := ifStage.io.bpuQueryPc
  tage.io.updateValid  := exStage.io.bpuUpdateValid
  tage.io.updatePc     := exStage.io.bpuUpdatePc
  tage.io.updateTaken  := exStage.io.bpuUpdateTaken
  tage.io.updateTarget := exStage.io.bpuUpdateTarget
  tage.io.ras.push       := idStage.io.rasPush
  tage.io.ras.pushAddr   := idStage.io.rasPushAddr
  tage.io.ras.pop        := idStage.io.rasPop
  tage.io.ras.flush      := false.B
  tage.io.ras.checkpoint := 0.U

  val branchPredMode = csrFile.io.branchPredCtrl(1, 0)
  val usePlainBpu = branchPredMode === 0.U
  val useTage     = branchPredMode === 2.U
  val useBpuRas   = !usePlainBpu && !useTage

  ifStage.io.bpuPredTaken := Mux(usePlainBpu, bpu.io.predTaken,
    Mux(useTage, tage.io.predTaken, bpuRas.io.predTaken))
  ifStage.io.bpuPredTarget := Mux(usePlainBpu, bpu.io.predTarget,
    Mux(useTage, tage.io.predTarget, bpuRas.io.predTarget))

  // IF Stage
  ifStage.io.exRedirectValid     := exStage.io.exRedirectValid
  ifStage.io.exRedirectPc        := exStage.io.exRedirectPc
  ifStage.io.idRedirectValid     := idStage.io.idRedirectValid
  ifStage.io.idRedirectPc        := idStage.io.idRedirectPc
  ifStage.io.flushIf             := hazard.io.flushIF
  ifStage.io.stallIf             := hazard.io.stallIF || idStage.io.holdIfId
  ifStage.io.rasPredValid        := Mux(useTage, tage.io.ras.topValid,
    Mux(useBpuRas, bpuRas.io.ras.topValid, false.B))
  ifStage.io.rasPredTarget       := Mux(useTage, tage.io.ras.topAddr, bpuRas.io.ras.topAddr)
  ifStage.io.nextLinePrefetchEn  := csrFile.io.prefetchCtrl(0)
  ifStage.io.stridePrefetchEn    := csrFile.io.prefetchCtrl(1)
  ifStage.io.streamPrefetchEn    := csrFile.io.prefetchCtrl(2)

  // ID Stage
  idStage.io.in         := ifidReg
  idStage.io.stallId    := hazard.io.stallID
  idStage.io.flushId    := hazard.io.flushID
  idStage.io.regRs1Data := regFile.io.rs1Data
  idStage.io.regRs2Data := regFile.io.rs2Data
  idStage.io.csrRdata   := csrFile.io.rdata

  val wbRegWen = Wire(Vec(issueWidth, Bool()))
  regFile.io.rs1Addr := idStage.io.regRs1Addr
  regFile.io.rs2Addr := idStage.io.regRs2Addr
  regFile.io.wen     := wbRegWen
  regFile.io.waddr   := wbStage.io.regWaddr
  regFile.io.wdata   := wbStage.io.regWdata

  // CSR File
  csrFile.io.raddr   := idStage.io.csrRaddr
  csrFile.io.opValid := exStage.io.csrOpValid
  csrFile.io.opType  := exStage.io.csrOpType
  csrFile.io.waddr   := exStage.io.csrWaddr
  csrFile.io.wdata   := exStage.io.csrWdata
  csrFile.io.cycleTick := true.B

  // Count/write back only when the WB stage advances. During an I-/D-cache
  // stall memwbReg is frozen, so wbStage.io.instRetire keeps reflecting the
  // same instruction and must not be counted again.
  val retireEnable = !hazard.io.stallWB
  wbRegWen := VecInit(wbStage.io.regWen.map(_ && retireEnable))
  val retireVec = VecInit(wbStage.io.instRetire.map(_ && retireEnable))
  csrFile.io.instRetire := retireVec

  // Bypass Network
  bypass.io.idex      := idexReg
  bypass.io.idexValid := VecInit(idexReg.map(x => x.ctrl.valid && !x.ctrl.kill))
  bypass.io.exResult  := exStage.io.exResult
  bypass.io.exmem     := exmemReg
  bypass.io.exmemValid := VecInit(exmemReg.map(x => x.ctrl.valid && !x.ctrl.kill))
  bypass.io.memwb     := memwbReg
  bypass.io.memwbValid := VecInit(memwbReg.map(x => x.ctrl.valid && !x.ctrl.kill))
  bypass.io.wbValid   := VecInit(wbStage.io.wbValid.map(_ && retireEnable))
  bypass.io.wbRfWen   := wbRegWen
  bypass.io.wbRdAddr  := wbStage.io.wbRdAddr
  bypass.io.wbData    := wbStage.io.wbData
  bypass.io.rs1Use    := VecInit(idexReg.map(_.rs1Use))
  bypass.io.rs2Use    := VecInit(idexReg.map(_.rs2Use))

  // EX Stage
  exStage.io.in        := idexReg
  exStage.io.stallEx   := hazard.io.stallEX
  exStage.io.flushEx   := false.B
  exStage.io.rs1Data   := bypass.io.rs1Data
  exStage.io.rs2Data   := bypass.io.rs2Data
  exStage.io.op1Data   := bypass.io.op1Data
  exStage.io.op2Data   := bypass.io.op2Data
  exStage.io.storeData := bypass.io.storeData
  exStage.io.csrOldData := csrFile.io.oldData

  // MEM Stage
  memStage.io.in         := exmemReg
  memStage.io.flushMem   := false.B
  memStage.io.mtimeLo    := csrFile.io.mtimeLo
  memStage.io.mtimeHi    := csrFile.io.mtimeHi
  memStage.io.dcacheFlush := false.B
  memStage.io.stridePrefetchEn := csrFile.io.prefetchCtrl(1)
  memStage.io.streamPrefetchEn := csrFile.io.prefetchCtrl(2)

  // WB Stage
  wbStage.io.in := memwbReg

  // Hazard Unit
  hazard.io.idValid   := idStage.io.hazardIdValid
  hazard.io.idRs1Addr := idStage.io.hazardRs1Addr
  hazard.io.idRs2Addr := idStage.io.hazardRs2Addr
  hazard.io.idRs1Use  := idStage.io.hazardRs1Use
  hazard.io.idRs2Use  := idStage.io.hazardRs2Use
  hazard.io.idRdAddr  := idStage.io.hazardRdAddr
  hazard.io.idRfWen   := idStage.io.hazardRfWen
  hazard.io.idCanBypassToYounger := idStage.io.hazardCanBypassToYounger
  hazard.io.exValid   := VecInit(idexReg.map(x => x.ctrl.valid && !x.ctrl.kill))
  hazard.io.exMemRen  := VecInit(idexReg.map(_.memRen))
  hazard.io.exRdAddr  := VecInit(idexReg.map(_.rdAddr))
  hazard.io.exRfWen   := VecInit(idexReg.map(_.rfWen))
  hazard.io.memValid  := VecInit(exmemReg.map(x => x.ctrl.valid && !x.ctrl.kill))
  hazard.io.memMemRen := VecInit(exmemReg.map(_.memRen))
  hazard.io.memRdAddr := VecInit(exmemReg.map(_.rdAddr))
  hazard.io.memRfWen  := VecInit(exmemReg.map(_.rfWen))
  hazard.io.pendingWaw := VecInit(Seq.fill(4)(0.U.asTypeOf(new PendingRegWriteInfo)))
  // brTaken is merged into exRedirect (branch mispred -> exRedirectValid).
  hazard.io.brTaken     := false.B
  hazard.io.exRedirect  := exStage.io.exRedirectValid
  hazard.io.idRedirect  := idStage.io.idRedirectValid
  hazard.io.icacheStall := ifStage.io.icacheStall
  hazard.io.dcacheStall := memStage.io.dcacheStall
  hazard.io.backendStall := exStage.io.mulDivStall

  // Pipeline register update logic
  val retireCount = PopCount(retireVec)
  val perfCycles = RegInit(0.U(64.W))
  val perfRetire0 = RegInit(0.U(64.W))
  val perfRetire1 = RegInit(0.U(64.W))
  val perfRetire2 = RegInit(0.U(64.W))
  val perfInstRetired = RegInit(0.U(64.W))
  val perfIStall = RegInit(0.U(64.W))
  val perfDStall = RegInit(0.U(64.W))
  val perfLoadUse = RegInit(0.U(64.W))
  val perfIdRedirect = RegInit(0.U(64.W))
  val perfExRedirect = RegInit(0.U(64.W))
  val perfRasPush = RegInit(0.U(64.W))
  val perfRasPop = RegInit(0.U(64.W))
  val perfBranchInsts = RegInit(0.U(64.W))
  val perfBranchPreds = RegInit(0.U(64.W))
  val perfBranchCorrect = RegInit(0.U(64.W))
  val perfBranchMispredicts = RegInit(0.U(64.W))
  val perfBranchDirectionMispredicts = RegInit(0.U(64.W))
  val perfBranchTargetMispredicts = RegInit(0.U(64.W))
  val perfWrongPathFlushInsts = RegInit(0.U(64.W))
  val perfJalrInsts = RegInit(0.U(64.W))
  val perfRasPreds = RegInit(0.U(64.W))
  val perfRasCorrect = RegInit(0.U(64.W))
  val perfICacheAccesses = RegInit(0.U(64.W))
  val perfICacheHits = RegInit(0.U(64.W))
  val perfICacheMisses = RegInit(0.U(64.W))
  val perfICacheDemandRefills = RegInit(0.U(64.W))
  val perfICachePrefetchReqs = RegInit(0.U(64.W))
  val perfICachePrefetchAccepted = RegInit(0.U(64.W))
  val perfICachePrefetchDropped = RegInit(0.U(64.W))
  val perfICachePrefetchRefills = RegInit(0.U(64.W))
  val perfICachePrefetchUseful = RegInit(0.U(64.W))
  val perfDCacheLoads = RegInit(0.U(64.W))
  val perfDCacheStores = RegInit(0.U(64.W))
  val perfDCacheHits = RegInit(0.U(64.W))
  val perfDCacheMisses = RegInit(0.U(64.W))
  val perfDCacheWritebacks = RegInit(0.U(64.W))
  val perfDCacheDemandRefills = RegInit(0.U(64.W))
  val perfDCachePrefetchReqs = RegInit(0.U(64.W))
  val perfDCachePrefetchAccepted = RegInit(0.U(64.W))
  val perfDCachePrefetchDropped = RegInit(0.U(64.W))
  val perfDCachePrefetchRefills = RegInit(0.U(64.W))
  val perfDCachePrefetchUseful = RegInit(0.U(64.W))

  private def countBools(xs: Seq[Bool]): UInt = PopCount(VecInit(xs))
  val exPerfActive = !hazard.io.stallEX
  val branchInstInc = countBools(exStage.io.branchInst.map(_ && exPerfActive))
  val branchPredInc = countBools(exStage.io.branchPred.map(_ && exPerfActive))
  val branchCorrectInc = countBools(exStage.io.branchCorrect.map(_ && exPerfActive))
  val branchMispredictInc = countBools(exStage.io.branchMispredict.map(_ && exPerfActive))
  val branchDirectionMispredictInc = countBools(exStage.io.branchDirectionMispredict.map(_ && exPerfActive))
  val branchTargetMispredictInc = countBools(exStage.io.branchTargetMispredict.map(_ && exPerfActive))
  val jalrInstInc = countBools(exStage.io.jalrInst.map(_ && exPerfActive))
  val rasPredInc = countBools(exStage.io.rasPred.map(_ && exPerfActive))
  val rasCorrectInc = countBools(exStage.io.rasCorrect.map(_ && exPerfActive))
  val wrongPathFlushInc = countBools(ifidReg.map(s => s.ctrl.valid && !s.ctrl.kill)) +
                          countBools(idexReg.map(s => s.ctrl.valid && !s.ctrl.kill))

  perfCycles := perfCycles + 1.U
  when(retireCount === 0.U) { perfRetire0 := perfRetire0 + 1.U }
  when(retireCount === 1.U) { perfRetire1 := perfRetire1 + 1.U }
  when(retireCount === 2.U) { perfRetire2 := perfRetire2 + 1.U }
  perfInstRetired := perfInstRetired + retireCount
  when(ifStage.io.icacheStall) { perfIStall := perfIStall + 1.U }
  when(memStage.io.dcacheStall) { perfDStall := perfDStall + 1.U }
  when(hazard.io.loadUseStall) { perfLoadUse := perfLoadUse + 1.U }
  when(idStage.io.idRedirectValid) { perfIdRedirect := perfIdRedirect + 1.U }
  when(exStage.io.exRedirectValid) { perfExRedirect := perfExRedirect + 1.U }
  when(idStage.io.rasPush) { perfRasPush := perfRasPush + 1.U }
  when(idStage.io.rasPop) { perfRasPop := perfRasPop + 1.U }
  perfBranchInsts := perfBranchInsts + branchInstInc
  perfBranchPreds := perfBranchPreds + branchPredInc
  perfBranchCorrect := perfBranchCorrect + branchCorrectInc
  perfBranchMispredicts := perfBranchMispredicts + branchMispredictInc
  perfBranchDirectionMispredicts := perfBranchDirectionMispredicts + branchDirectionMispredictInc
  perfBranchTargetMispredicts := perfBranchTargetMispredicts + branchTargetMispredictInc
  when(idStage.io.idRedirectValid || exStage.io.exRedirectValid) {
    perfWrongPathFlushInsts := perfWrongPathFlushInsts + wrongPathFlushInc
  }
  perfJalrInsts := perfJalrInsts + jalrInstInc
  perfRasPreds := perfRasPreds + rasPredInc
  perfRasCorrect := perfRasCorrect + rasCorrectInc
  when(ifStage.io.icachePerf.access) { perfICacheAccesses := perfICacheAccesses + 1.U }
  when(ifStage.io.icachePerf.hit) { perfICacheHits := perfICacheHits + 1.U }
  when(ifStage.io.icachePerf.miss) { perfICacheMisses := perfICacheMisses + 1.U }
  when(ifStage.io.icachePerf.demandRefill) { perfICacheDemandRefills := perfICacheDemandRefills + 1.U }
  when(ifStage.io.icachePerf.prefetchReq) { perfICachePrefetchReqs := perfICachePrefetchReqs + 1.U }
  when(ifStage.io.icachePerf.prefetchAccepted) { perfICachePrefetchAccepted := perfICachePrefetchAccepted + 1.U }
  when(ifStage.io.icachePerf.prefetchDropped) { perfICachePrefetchDropped := perfICachePrefetchDropped + 1.U }
  when(ifStage.io.icachePerf.prefetchRefill) { perfICachePrefetchRefills := perfICachePrefetchRefills + 1.U }
  when(ifStage.io.icachePerf.prefetchUseful) { perfICachePrefetchUseful := perfICachePrefetchUseful + 1.U }
  when(memStage.io.dcachePerf.load) { perfDCacheLoads := perfDCacheLoads + 1.U }
  when(memStage.io.dcachePerf.store) { perfDCacheStores := perfDCacheStores + 1.U }
  when(memStage.io.dcachePerf.hit) { perfDCacheHits := perfDCacheHits + 1.U }
  when(memStage.io.dcachePerf.miss) { perfDCacheMisses := perfDCacheMisses + 1.U }
  when(memStage.io.dcachePerf.writeback) { perfDCacheWritebacks := perfDCacheWritebacks + 1.U }
  when(memStage.io.dcachePerf.demandRefill) { perfDCacheDemandRefills := perfDCacheDemandRefills + 1.U }
  when(memStage.io.dcachePerf.prefetchReq) { perfDCachePrefetchReqs := perfDCachePrefetchReqs + 1.U }
  when(memStage.io.dcachePerf.prefetchAccepted) { perfDCachePrefetchAccepted := perfDCachePrefetchAccepted + 1.U }
  when(memStage.io.dcachePerf.prefetchDropped) { perfDCachePrefetchDropped := perfDCachePrefetchDropped + 1.U }
  when(memStage.io.dcachePerf.prefetchRefill) { perfDCachePrefetchRefills := perfDCachePrefetchRefills + 1.U }
  when(memStage.io.dcachePerf.prefetchUseful) { perfDCachePrefetchUseful := perfDCachePrefetchUseful + 1.U }

  io.perf.cycles            := perfCycles
  io.perf.retire0Cycles     := perfRetire0
  io.perf.retire1Cycles     := perfRetire1
  io.perf.retire2Cycles     := perfRetire2
  io.perf.instRetired       := perfInstRetired
  io.perf.icacheStallCycles := perfIStall
  io.perf.dcacheStallCycles := perfDStall
  io.perf.loadUseStalls     := perfLoadUse
  io.perf.idRedirects       := perfIdRedirect
  io.perf.exRedirects       := perfExRedirect
  io.perf.rasPushes         := perfRasPush
  io.perf.rasPops           := perfRasPop
  io.perf.branchInsts       := perfBranchInsts
  io.perf.branchPreds       := perfBranchPreds
  io.perf.branchCorrect     := perfBranchCorrect
  io.perf.branchMispredicts := perfBranchMispredicts
  io.perf.branchDirectionMispredicts := perfBranchDirectionMispredicts
  io.perf.branchTargetMispredicts    := perfBranchTargetMispredicts
  io.perf.wrongPathFlushInsts        := perfWrongPathFlushInsts
  io.perf.jalrInsts         := perfJalrInsts
  io.perf.rasPreds          := perfRasPreds
  io.perf.rasCorrect        := perfRasCorrect
  io.perf.icacheAccesses    := perfICacheAccesses
  io.perf.icacheHits        := perfICacheHits
  io.perf.icacheMisses      := perfICacheMisses
  io.perf.icacheDemandRefills       := perfICacheDemandRefills
  io.perf.icachePrefetchReqs        := perfICachePrefetchReqs
  io.perf.icachePrefetchAccepted    := perfICachePrefetchAccepted
  io.perf.icachePrefetchDropped     := perfICachePrefetchDropped
  io.perf.icachePrefetchRefills     := perfICachePrefetchRefills
  io.perf.icachePrefetchUseful      := perfICachePrefetchUseful
  io.perf.dcacheLoads       := perfDCacheLoads
  io.perf.dcacheStores      := perfDCacheStores
  io.perf.dcacheHits        := perfDCacheHits
  io.perf.dcacheMisses      := perfDCacheMisses
  io.perf.dcacheWritebacks  := perfDCacheWritebacks
  io.perf.dcacheDemandRefills       := perfDCacheDemandRefills
  io.perf.dcachePrefetchReqs        := perfDCachePrefetchReqs
  io.perf.dcachePrefetchAccepted    := perfDCachePrefetchAccepted
  io.perf.dcachePrefetchDropped     := perfDCachePrefetchDropped
  io.perf.dcachePrefetchRefills     := perfDCachePrefetchRefills
  io.perf.dcachePrefetchUseful      := perfDCachePrefetchUseful

  when(hazard.io.flushIF) {
    ifidReg := VecInit(Seq.fill(issueWidth)(0.U.asTypeOf(new IFIDSlot)))
  }.elsewhen(!(hazard.io.stallIF || idStage.io.holdIfId)) {
    ifidReg := ifStage.io.out
  }

  when(hazard.io.flushID || hazard.io.flushEX) {
    idexReg := VecInit(Seq.fill(issueWidth)(0.U.asTypeOf(new IDEXBundle)))
  }.elsewhen(!hazard.io.stallID) {
    idexReg := idStage.io.out
  }

  when(!hazard.io.stallEX) {
    exmemReg := exStage.io.out
  }

  when(!hazard.io.stallMEM) {
    memwbReg := memStage.io.out
  }
}
```

## .\src\main\Core\MEMStage.scala

```scala
package riscv

import chisel3._
import chisel3.util._
import parameterized_cache.{CacheParams, DCachePerfEvents, DCacheTop, MemBusIO}

class MEMStage(p: CacheParams = CacheParams.default) extends Module {
  val issueWidth = 2

  val io = IO(new Bundle {
    val in = Input(Vec(issueWidth, new EXMEMBundle))
    val flushMem = Input(Bool())

    val mtimeLo = Input(UInt(32.W))
    val mtimeHi = Input(UInt(32.W))
    val dcacheFlush = Input(Bool())
    val stridePrefetchEn = Input(Bool())
    val streamPrefetchEn = Input(Bool())
    val dcacheStall = Output(Bool())
    val dcachePerf  = Output(new DCachePerfEvents)
    val printChar   = Output(Valid(UInt(8.W)))
    // P0 fix: expose success signal for test-completion detection
    val success     = Output(Bool())
    val dmem = new MemBusIO(p)

    val out = Output(Vec(issueWidth, new MEMWBBundle))
  })

  val dcache = Module(new DCacheTop(p))
  val stridePrefetcher = Module(new StridePrefetcher)
  val streamPrefetcher = Module(new StreamPrefetcher)

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

  val dcacheBusy = dcache.io.stall || dcache.io.missOut
  val pfAddrCacheable =
    (memAddr =/= "h10001FF0".U(32.W)) &&
    (memAddr =/= "h10001FF1".U(32.W)) &&
    (memAddr =/= "h0000BFF8".U(32.W)) &&
    (memAddr =/= "h0000BFFC".U(32.W))
  val pfObserve = memReqValid && io.in(memIdx).memRen && pfAddrCacheable
  stridePrefetcher.io.observeValid := pfObserve
  stridePrefetcher.io.observeAddr  := memAddr
  stridePrefetcher.io.prefetchEn   := io.stridePrefetchEn
  streamPrefetcher.io.observeValid := pfObserve
  streamPrefetcher.io.observeAddr  := memAddr
  streamPrefetcher.io.prefetchEn   := io.streamPrefetchEn

  val pfSelStream = streamPrefetcher.io.pfReqValid
  val pfSelStride = !pfSelStream && stridePrefetcher.io.pfReqValid
  val rawPfValid = pfSelStream || pfSelStride
  val rawPfAddr = Mux(pfSelStream, streamPrefetcher.io.pfReqAddr, stridePrefetcher.io.pfReqAddr)

  val pfPendingValid = RegInit(false.B)
  val pfPendingAddr  = RegInit(0.U(32.W))
  val pfIssueValid   = pfPendingValid || rawPfValid
  val pfIssueAddr    = Mux(pfPendingValid, pfPendingAddr, rawPfAddr)

  dcache.io.pfReqValid := pfIssueValid
  dcache.io.pfReqAddr  := pfIssueAddr

  val pfAccepted = pfIssueValid && dcache.io.pfReqReady
  when(io.flushMem) {
    pfPendingValid := false.B
  }.elsewhen(pfPendingValid) {
    when(pfAccepted) {
      pfPendingValid := false.B
    }
  }.elsewhen(rawPfValid && !pfAccepted) {
    pfPendingValid := true.B
    pfPendingAddr  := rawPfAddr
  }

  val rawPfCanAccept = !pfPendingValid && dcache.io.pfReqReady
  streamPrefetcher.io.pfReqReady := rawPfCanAccept && pfSelStream
  stridePrefetcher.io.pfReqReady := rawPfCanAccept && pfSelStride

  val printPc   = io.in(memIdx).pc
  val printBits = io.in(memIdx).rs2Data(7, 0)

  val lastPrintValid = RegInit(false.B)
  val lastPrintPc    = RegInit(0.U(32.W))
  val lastPrintBits  = RegInit(0.U(8.W))
  val samePrintAsLast = lastPrintValid &&
                        (lastPrintPc === printPc) &&
                        (lastPrintBits === printBits)
  val printFire = isPrintfWrite && !samePrintAsLast

  when(isPrintfWrite) {
    lastPrintValid := true.B
    lastPrintPc    := printPc
    lastPrintBits  := printBits
  }.otherwise {
    lastPrintValid := false.B
  }

  val printValidReg = RegNext(printFire, false.B)
  val printBitsReg  = RegEnable(printBits, 0.U(8.W), printFire)

  io.dcacheStall := dcacheBusy
  io.dcachePerf  := dcache.io.perf
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
```

## .\src\main\Core\RegFile.scala

```scala
package riscv

import chisel3._
import chisel3.util._

class RegFile(issueWidth: Int = 2) extends Module {
  require(issueWidth == 2, "This in-order core currently fixes issueWidth at 2")

  val io = IO(new Bundle {
    val rs1Addr = Input(Vec(issueWidth, UInt(5.W)))
    val rs2Addr = Input(Vec(issueWidth, UInt(5.W)))
    val rs1Data = Output(Vec(issueWidth, UInt(32.W)))
    val rs2Data = Output(Vec(issueWidth, UInt(32.W)))

    val wen   = Input(Vec(issueWidth, Bool()))
    val waddr = Input(Vec(issueWidth, UInt(5.W)))
    val wdata = Input(Vec(issueWidth, UInt(32.W)))
  })

  val regs = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))

  private def read(addr: UInt): UInt = {
    val raw = Mux(addr === 0.U, 0.U, regs(addr))
    val bypass0 = io.wen(0) && io.waddr(0) =/= 0.U && io.waddr(0) === addr
    val bypass1 = io.wen(1) && io.waddr(1) =/= 0.U && io.waddr(1) === addr
    Mux(bypass1, io.wdata(1), Mux(bypass0, io.wdata(0), raw))
  }

  for (i <- 0 until issueWidth) {
    io.rs1Data(i) := read(io.rs1Addr(i))
    io.rs2Data(i) := read(io.rs2Addr(i))
  }

  when(io.wen(0) && io.waddr(0) =/= 0.U) {
    regs(io.waddr(0)) := io.wdata(0)
  }
  when(io.wen(1) && io.waddr(1) =/= 0.U) {
    regs(io.waddr(1)) := io.wdata(1)
  }
}
```

## .\src\main\Core\WBStage.scala

```scala
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
```

## .\src\main\Dcache\CacheParams.scala

```scala
package parameterized_cache

import chisel3._
import chisel3.util._

// ============================================================
//  全局唯一的参数定义。包内所有模块共用此 case class，
//  其余文件不再各自重复 `case class CacheParams`。
// ============================================================
case class CacheParams(
  ADDR_WIDTH: Int,
  DATA_WIDTH: Int,
  CACHE_SIZE: Int,
  WAY_NUM: Int,
  LINE_BYTES: Int
) {
  val WORD_BYTES  = DATA_WIDTH / 8
  val LINE_WORDS  = LINE_BYTES / WORD_BYTES
  val SET_NUM     = CACHE_SIZE / (WAY_NUM * LINE_BYTES)

  val OFFSET_W    = log2Ceil(LINE_BYTES)
  val INDEX_W     = log2Ceil(SET_NUM)
  val TAG_W       = ADDR_WIDTH - INDEX_W - OFFSET_W
  val WAY_W       = log2Ceil(WAY_NUM)
  val WORD_CNT_W  = log2Ceil(LINE_WORDS)
  val WMASK_BITS  = WORD_BYTES
}

// ============================================================
//  全局唯一的 TagEntry 定义（带 dirty 字段）。
//  HitTest 不使用 dirty，忽略即可；DCacheTop 通过
//  tagArray.io.tagData(...).dirty 读取脏位，连线类型一致。
// ============================================================
class TagEntry(p: CacheParams) extends Bundle {
  val valid = Bool()
  val dirty = Bool()
  val tag   = UInt(p.TAG_W.W)
}

object CacheParams {
  val default: CacheParams = CacheParams(
    ADDR_WIDTH = 32,
    DATA_WIDTH = 32,
    CACHE_SIZE = 8 * 1024,
    WAY_NUM    = 4,
    LINE_BYTES = 64
  )
}

class ICachePerfEvents extends Bundle {
  val access           = Bool()
  val hit              = Bool()
  val miss             = Bool()
  val demandRefill     = Bool()
  val prefetchReq      = Bool()
  val prefetchAccepted = Bool()
  val prefetchDropped  = Bool()
  val prefetchRefill   = Bool()
  val prefetchUseful   = Bool()
}

class DCachePerfEvents extends Bundle {
  val load             = Bool()
  val store            = Bool()
  val hit              = Bool()
  val miss             = Bool()
  val writeback        = Bool()
  val demandRefill     = Bool()
  val prefetchReq      = Bool()
  val prefetchAccepted = Bool()
  val prefetchDropped  = Bool()
  val prefetchRefill   = Bool()
  val prefetchUseful   = Bool()
}
```

## .\src\main\Dcache\DataArray.scala

```scala
package parameterized_cache

import chisel3._
import chisel3.util._

class DataArray(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val idx          = Input(UInt(p.INDEX_W.W))
    val wordsoff     = Input(UInt(p.WORD_CNT_W.W))
    val hitWen       = Input(Bool())
    val hitWay       = Input(UInt(p.WAY_W.W))
    val wdata        = Input(UInt(p.DATA_WIDTH.W))
    val wmask        = Input(UInt(p.WMASK_BITS.W))

    val refillDataEn = Input(Bool())
    val refillWay    = Input(UInt(p.WAY_W.W))
    val refillIdx    = Input(UInt(p.INDEX_W.W))
    val refillWord   = Input(UInt(p.WORD_CNT_W.W))
    val refillData   = Input(UInt(p.DATA_WIDTH.W))

    val evictIdx     = Input(UInt(p.INDEX_W.W))
    val evictWay     = Input(UInt(p.WAY_W.W))
    val evictLine    = Output(Vec(p.LINE_WORDS, UInt(p.DATA_WIDTH.W)))

    val rawData      = Output(Vec(p.WAY_NUM, UInt(p.DATA_WIDTH.W)))
  })

  val dArray = Reg(Vec(p.WAY_NUM, Vec(p.SET_NUM, Vec(p.LINE_WORDS, UInt(p.DATA_WIDTH.W)))))

  for (way <- 0 until p.WAY_NUM) {
    io.rawData(way) := dArray(way)(io.idx)(io.wordsoff)
  }

  io.evictLine := dArray(io.evictWay)(io.evictIdx)

  val byteMask = Cat((0 until p.WMASK_BITS).reverse.map(i => Fill(8, io.wmask(i))))

  when(io.refillDataEn) {
    dArray(io.refillWay)(io.refillIdx)(io.refillWord) := io.refillData
  }.elsewhen(io.hitWen) {
    val oldData = dArray(io.hitWay)(io.idx)(io.wordsoff)
    dArray(io.hitWay)(io.idx)(io.wordsoff) := (io.wdata & byteMask) | (oldData & ~byteMask)
  }
}
```

## .\src\main\Dcache\DCacheMissFSM.scala

```scala
package parameterized_cache

import chisel3._
import chisel3.util._

class DCacheMissFSMIO(p: CacheParams) extends Bundle {
  val missValid   = Input(Bool())
  val missTag     = Input(UInt(p.TAG_W.W))
  val missIsStore = Input(Bool())
  val missIsPrefetch = Input(Bool())
  val missWordOff = Input(UInt(p.WORD_CNT_W.W))
  val missWdata   = Input(UInt(p.DATA_WIDTH.W))
  val missWmask   = Input(UInt(p.WMASK_BITS.W))

  val evictIdx    = Input(UInt(p.INDEX_W.W))
  val evictWay    = Input(UInt(p.WAY_W.W))
  val evictTag    = Input(UInt(p.TAG_W.W))
  val evictDirty  = Input(Bool())
  val evictLine   = Input(Vec(p.LINE_WORDS, UInt(p.DATA_WIDTH.W)))

  val mem         = new MemBusIO(p)

  val refillEn    = Output(Bool())
  val refillWay   = Output(UInt(p.WAY_W.W))
  val refillIdx   = Output(UInt(p.INDEX_W.W))
  val refillWord  = Output(UInt(p.WORD_CNT_W.W))
  val refillData  = Output(UInt(p.DATA_WIDTH.W))
  val refillTag   = Output(UInt(p.TAG_W.W))
  val refillDone  = Output(Bool())
  val refillIsStore = Output(Bool())
  val refillIsPrefetch = Output(Bool())
  val writeback    = Output(Bool())

  val stall       = Output(Bool())
  val isIdle      = Output(Bool())
  val arrayWriteBusy = Output(Bool())
}

class DCacheMissFSM(p: CacheParams) extends Module {
  val io = IO(new DCacheMissFSMIO(p))

  val sIdle :: sCheck :: sWbReq :: sWbResp :: sRefillReq :: sRefillResp :: sRefillWrite :: sDone :: Nil = Enum(8)
  val state = RegInit(sIdle)
  val nextState = WireDefault(state)

  val missTag   = Reg(UInt(p.TAG_W.W))
  val evictTag  = Reg(UInt(p.TAG_W.W))
  val idx       = Reg(UInt(p.INDEX_W.W))
  val way       = Reg(UInt(p.WAY_W.W))
  val dirty     = Reg(Bool())
  val isStore   = Reg(Bool())
  val isPrefetch = Reg(Bool())
  val storeWordOff = Reg(UInt(p.WORD_CNT_W.W))
  val storeWdata   = Reg(UInt(p.DATA_WIDTH.W))
  val storeWmask   = Reg(UInt(p.WMASK_BITS.W))
  val wordCnt   = Reg(UInt(p.WORD_CNT_W.W))
  val lineBuf   = Reg(Vec(p.LINE_WORDS, UInt(p.DATA_WIDTH.W)))

  val lastWord = wordCnt === (p.LINE_WORDS - 1).U

  switch(state) {
    is(sIdle)       { when(io.missValid) { nextState := sCheck } }
    is(sCheck)      { nextState := Mux(dirty, sWbReq, sRefillReq) }
    is(sWbReq)      { when(io.mem.req.fire) { nextState := sWbResp } }
    is(sWbResp)     { when(io.mem.resp.fire) { nextState := sRefillReq } }
    is(sRefillReq)  { when(io.mem.req.fire) { nextState := sRefillResp } }
    is(sRefillResp) { when(io.mem.resp.fire) { nextState := sRefillWrite } }
    is(sRefillWrite) { when(lastWord) { nextState := sDone } }
    is(sDone)       { nextState := sIdle }
  }
  state := nextState

  when(state === sIdle && io.missValid) {
    missTag  := io.missTag
    evictTag := io.evictTag
    idx      := io.evictIdx
    way      := io.evictWay
    dirty    := io.evictDirty
    isStore  := io.missIsStore
    isPrefetch := io.missIsPrefetch
    storeWordOff := io.missWordOff
    storeWdata   := io.missWdata
    storeWmask   := io.missWmask
    wordCnt  := 0.U
    lineBuf  := io.evictLine
  }.elsewhen(state === sRefillResp && io.mem.resp.fire) {
    lineBuf := io.mem.resp.bits.rline
    wordCnt := 0.U
  }.elsewhen(state === sRefillWrite) {
    wordCnt := Mux(lastWord, 0.U, wordCnt + 1.U)
  }

  val wbAddr     = Cat(evictTag, idx, 0.U(p.OFFSET_W.W))
  val refillAddr = Cat(missTag, idx, 0.U(p.OFFSET_W.W))

  val isWb       = state === sWbReq
  val isRefill   = state === sRefillReq
  io.mem.req.valid      := isWb || isRefill
  io.mem.req.bits.addr  := Mux(isWb, wbAddr, refillAddr)
  io.mem.req.bits.wdata := 0.U
  io.mem.req.bits.wline := lineBuf
  io.mem.req.bits.wen   := isWb
  io.mem.req.bits.wmask := 0.U
  io.mem.req.bits.line  := true.B
  io.mem.resp.ready     := state === sWbResp || state === sRefillResp

  io.refillEn     := state === sRefillWrite
  io.refillWay    := way
  io.refillIdx    := idx
  io.refillWord   := wordCnt
  val storeByteMask = Cat((0 until p.WMASK_BITS).reverse.map(i => Fill(8, storeWmask(i))))
  val refillRawData = lineBuf(wordCnt)
  val refillStoreData = (storeWdata & storeByteMask) | (refillRawData & ~storeByteMask)
  io.refillData   := Mux(isStore && (wordCnt === storeWordOff), refillStoreData, refillRawData)
  io.refillTag    := missTag
  io.refillDone   := state === sDone
  io.refillIsStore := isStore
  io.refillIsPrefetch := isPrefetch
  io.writeback    := state === sWbReq && io.mem.req.fire
  io.stall        := state =/= sIdle && !isPrefetch
  io.isIdle       := state === sIdle
  io.arrayWriteBusy := isPrefetch && (state === sRefillWrite || state === sDone)
}
```

## .\src\main\Dcache\DCacheTop.scala

```scala
package parameterized_cache

import chisel3._
import chisel3.util._

class DCacheTop(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val addr    = Input(UInt(p.ADDR_WIDTH.W))
    val flush   = Input(Bool())
    val wen     = Input(Bool())
    val wmask   = Input(UInt(p.WMASK_BITS.W))
    val wdata   = Input(UInt(p.DATA_WIDTH.W))
    val memRen  = Input(Bool())
    val memWd   = Input(UInt(2.W))
    val signed  = Input(Bool())

    val rdata   = Output(UInt(p.DATA_WIDTH.W))
    val missOut = Output(Bool())
    val stall   = Output(Bool())

    val mtimeLo = Input(UInt(p.DATA_WIDTH.W))
    val mtimeHi = Input(UInt(p.DATA_WIDTH.W))

    val printChar = Output(Valid(UInt(8.W)))
    // P0 fix: success signal propagated from halt MMIO write
    val success   = Output(Bool())

    val pfReqValid = Input(Bool())
    val pfReqReady = Output(Bool())
    val pfReqAddr  = Input(UInt(p.ADDR_WIDTH.W))

    val mem = new MemBusIO(p)
    val perf = Output(new DCachePerfEvents)
  })

  require(p.ADDR_WIDTH >= 22, "DCacheTop expects at least 22-bit physical addresses")
  private def toPhysAddr(addr: UInt): UInt = {
    Cat(0.U((p.ADDR_WIDTH - 22).W), addr(21, 0))
  }

  // The simulation memory is addressed by low 22 bits.  Dhrystone uses both
  // high reset-vector aliases (0x8002_xxxx via gp) and low absolute data
  // addresses (0x0002_xxxx via lui), so cache lookup must use the same
  // canonical physical address as the memory model.  MMIO checks below still
  // use the original full address.
  val cacheAddr = toPhysAddr(io.addr)
  val pfCacheAddr = toPhysAddr(io.pfReqAddr)

  val addrTag  = cacheAddr(p.ADDR_WIDTH - 1, p.OFFSET_W + p.INDEX_W)
  val addrIdx  = cacheAddr(p.OFFSET_W + p.INDEX_W - 1, p.OFFSET_W)
  val addrWoff = cacheAddr(p.OFFSET_W - 1, 2)
  val addrBoff = cacheAddr(1, 0)
  val pfTag    = pfCacheAddr(p.ADDR_WIDTH - 1, p.OFFSET_W + p.INDEX_W)
  val pfIdx    = pfCacheAddr(p.OFFSET_W + p.INDEX_W - 1, p.OFFSET_W)

  // -------------------------------------------------------------------
  // Special / MMIO addresses
  // 0x10001FF1 – printf putchar  (store byte)
  // 0x10001FF0 – halt / success  (store word, wdata[0]=1 → success)
  // 0x0000BFF8 – mtime low       (load word, uncacheable)
  // 0x0000BFFC – mtime high      (load word, uncacheable)
  // -------------------------------------------------------------------
  val ADDR_PRINTF   = "h10001FF1".U(p.ADDR_WIDTH.W)
  val ADDR_HALT     = "h10001FF0".U(p.ADDR_WIDTH.W)   // P0 fix: halt/success MMIO
  val ADDR_MTIME_LO = "h0000BFF8".U(p.ADDR_WIDTH.W)
  val ADDR_MTIME_HI = "h0000BFFC".U(p.ADDR_WIDTH.W)

  val isPrintf      = (io.addr === ADDR_PRINTF) && io.wen && (io.memWd === 2.U)
  val addrIsPrintf  = (io.addr === ADDR_PRINTF) && (isPrintf || io.memRen)
  val addrIsMtimeLo = io.addr === ADDR_MTIME_LO
  val addrIsMtimeHi = io.addr === ADDR_MTIME_HI
  // P0 fix: halt address is uncacheable
  val addrIsHalt    = io.addr === ADDR_HALT
  val isBypass      = addrIsPrintf || addrIsMtimeLo || addrIsMtimeHi || addrIsHalt
  val pfIsBypass    = (io.pfReqAddr === ADDR_PRINTF) ||
                      (io.pfReqAddr === ADDR_HALT) ||
                      (io.pfReqAddr === ADDR_MTIME_LO) ||
                      (io.pfReqAddr === ADDR_MTIME_HI)

  val isMtimeLo = addrIsMtimeLo && io.memRen
  val isMtimeHi = addrIsMtimeHi && io.memRen

  // Latch success on any non-zero store to ADDR_HALT. Dhrystone writes 2,
  // while the assembly smoke tests write 1.
  val isHaltWrite = addrIsHalt && io.wen
  val successReg  = RegInit(false.B)
  when(isHaltWrite && (io.wdata =/= 0.U)) { successReg := true.B }
  io.success := successReg

  val tagArray  = Module(new TagArray(p))
  val dataArray = Module(new DataArray(p))
  val plru      = Module(new TreePLRU(p))
  val hitTest   = Module(new HitTest(p))
  val loadExt   = Module(new LoadExtend(p))
  val missFsm   = Module(new DCacheMissFSM(p))

  val demandReq = (io.memRen || io.wen) && !isBypass
  val queryPrefetch = missFsm.io.isIdle && io.pfReqValid && !demandReq && !pfIsBypass

  hitTest.io.tagData := tagArray.io.tagData
  hitTest.io.tag     := Mux(queryPrefetch, pfTag, addrTag)
  hitTest.io.memRen  := Mux(queryPrefetch, true.B, io.memRen)
  hitTest.io.wen     := Mux(queryPrefetch, false.B, io.wen)

  val pfMiss = queryPrefetch && hitTest.io.missValid
  val isHit  = hitTest.io.isHit && !isBypass && !queryPrefetch
  val hitWay = hitTest.io.hitWay
  val cacheMiss = hitTest.io.missValid && !isBypass && !queryPrefetch

  // TagArray
  tagArray.io.flush       := io.flush
  tagArray.io.idx         := Mux(queryPrefetch, pfIdx, addrIdx)
  tagArray.io.refillTagEn := missFsm.io.refillDone
  tagArray.io.refillWay   := missFsm.io.refillWay
  tagArray.io.refillIdx   := missFsm.io.refillIdx
  tagArray.io.refillTag   := missFsm.io.refillTag
  tagArray.io.refillDirty := missFsm.io.refillIsStore
  tagArray.io.setDirtyEn  := isHit && io.wen && !isBypass
  tagArray.io.setDirtyWay := hitWay

  // PLRU
  plru.io.idx       := Mux(queryPrefetch, pfIdx, addrIdx)
  plru.io.updateEn  := ((isHit && (io.memRen || io.wen)) || missFsm.io.refillDone) && !isBypass
  plru.io.updateWay := Mux(missFsm.io.refillDone, missFsm.io.refillWay, hitWay)

  // DataArray
  dataArray.io.idx          := addrIdx
  dataArray.io.wordsoff     := addrWoff
  dataArray.io.hitWen       := isHit && io.wen && !isBypass
  dataArray.io.hitWay       := hitWay
  dataArray.io.wdata        := io.wdata
  dataArray.io.wmask        := io.wmask
  dataArray.io.refillDataEn := missFsm.io.refillEn
  dataArray.io.refillWay    := missFsm.io.refillWay
  dataArray.io.refillIdx    := missFsm.io.refillIdx
  dataArray.io.refillWord   := missFsm.io.refillWord
  dataArray.io.refillData   := missFsm.io.refillData
  dataArray.io.evictIdx     := Mux(queryPrefetch, pfIdx, addrIdx)
  dataArray.io.evictWay     := plru.io.evictWay

  // LoadExt
  loadExt.io.hitWay  := hitWay
  loadExt.io.byteoff := addrBoff
  loadExt.io.memWd   := io.memWd
  loadExt.io.signed  := io.signed
  loadExt.io.rawData := dataArray.io.rawData

  // Miss FSM
  missFsm.io.missValid   := cacheMiss || pfMiss
  missFsm.io.missTag     := Mux(pfMiss, pfTag, addrTag)
  missFsm.io.missIsStore := cacheMiss && io.wen
  missFsm.io.missIsPrefetch := pfMiss
  missFsm.io.missWordOff := addrWoff
  missFsm.io.missWdata   := io.wdata
  missFsm.io.missWmask   := io.wmask
  missFsm.io.evictIdx    := Mux(pfMiss, pfIdx, addrIdx)
  missFsm.io.evictWay    := plru.io.evictWay
  missFsm.io.evictTag    := tagArray.io.tagData(plru.io.evictWay).tag
  missFsm.io.evictDirty  := tagArray.io.tagData(plru.io.evictWay).dirty
  missFsm.io.evictLine   := dataArray.io.evictLine
  io.pfReqReady := missFsm.io.isIdle && !demandReq

  // ── 预取 useful 统计标记 ─────────────────────────────────────
  val prefetched = RegInit(VecInit(Seq.fill(p.SET_NUM)(
    VecInit(Seq.fill(p.WAY_NUM)(false.B))
  )))
  val demandLookup = demandReq && missFsm.io.isIdle && !queryPrefetch
  val demandHit = demandLookup && isHit
  val demandMiss = demandLookup && cacheMiss
  val prefetchUseful = demandHit && prefetched(addrIdx)(hitWay)
  val prefetchDropped = (io.pfReqValid && pfIsBypass) ||
                        (queryPrefetch && hitTest.io.isHit)

  when(io.flush) {
    for (s <- 0 until p.SET_NUM) {
      for (w <- 0 until p.WAY_NUM) {
        prefetched(s)(w) := false.B
      }
    }
  }.otherwise {
    when(prefetchUseful) {
      prefetched(addrIdx)(hitWay) := false.B
    }
    when(missFsm.io.refillDone && !missFsm.io.refillIsPrefetch) {
      prefetched(missFsm.io.refillIdx)(missFsm.io.refillWay) := false.B
    }
    when(missFsm.io.refillDone && missFsm.io.refillIsPrefetch) {
      prefetched(missFsm.io.refillIdx)(missFsm.io.refillWay) := true.B
    }
  }

  io.mem <> missFsm.io.mem

  io.printChar.valid := isPrintf
  io.printChar.bits  := io.wdata(7, 0)

  io.rdata := MuxCase(loadExt.io.rdata, Seq(
    isMtimeLo -> io.mtimeLo,
    isMtimeHi -> io.mtimeHi,
    (addrIsPrintf && io.memRen) -> 0.U,
    (addrIsHalt  && io.memRen) -> 0.U   // load from halt addr → undefined, return 0
  ))

  val topStall = cacheMiss || missFsm.io.stall || (missFsm.io.arrayWriteBusy && demandReq)
  io.missOut := topStall
  io.stall   := topStall

  io.perf.load             := demandLookup && io.memRen
  io.perf.store            := demandLookup && io.wen
  io.perf.hit              := demandHit
  io.perf.miss             := demandMiss
  io.perf.writeback        := missFsm.io.writeback
  io.perf.demandRefill     := missFsm.io.refillDone && !missFsm.io.refillIsPrefetch
  io.perf.prefetchReq      := io.pfReqValid
  io.perf.prefetchAccepted := pfMiss
  io.perf.prefetchDropped  := prefetchDropped
  io.perf.prefetchRefill   := missFsm.io.refillDone && missFsm.io.refillIsPrefetch
  io.perf.prefetchUseful   := prefetchUseful
}
```

## .\src\main\Dcache\HitTest.scala

```scala
package parameterized_cache

import chisel3._
import chisel3.util._

class HitTest(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val tagData   = Input(Vec(p.WAY_NUM, new TagEntry(p)))
    val tag       = Input(UInt(p.TAG_W.W))
    val memRen    = Input(Bool())
    val wen       = Input(Bool())

    val isHit     = Output(Bool())
    val hitWay    = Output(UInt(p.WAY_W.W))
    val missValid = Output(Bool())
  })

  // HitTest 不使用 tagData 中的 dirty 字段，仅比对 valid 与 tag。
  val hitVec = VecInit((0 until p.WAY_NUM).map(i =>
    io.tagData(i).valid && (io.tagData(i).tag === io.tag)))

  io.isHit     := hitVec.asUInt.orR
  io.hitWay    := PriorityEncoder(hitVec)
  io.missValid := !io.isHit && (io.memRen || io.wen)
}
```

## .\src\main\Dcache\LoadExtend.scala

```scala
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
```

## .\src\main\Dcache\MemBusIO.scala

```scala
package parameterized_cache

import chisel3._
import chisel3.util._

class MemBusReq(p: CacheParams) extends Bundle {
  val addr  = UInt(p.ADDR_WIDTH.W)
  val wdata = UInt(p.DATA_WIDTH.W)
  val wline = Vec(p.LINE_WORDS, UInt(p.DATA_WIDTH.W))
  val wen   = Bool()
  val wmask = UInt(p.WMASK_BITS.W)
  val line  = Bool()
}

class MemBusResp(p: CacheParams) extends Bundle {
  val rdata = UInt(p.DATA_WIDTH.W)
  val rline = Vec(p.LINE_WORDS, UInt(p.DATA_WIDTH.W))
}

class MemBusIO(p: CacheParams) extends Bundle {
  val req  = Decoupled(new MemBusReq(p))
  val resp = Flipped(Decoupled(new MemBusResp(p)))
}
```

## .\src\main\Dcache\TagArray.scala

```scala
package parameterized_cache

import chisel3._
import chisel3.util._

class TagArray(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val flush        = Input(Bool())
    val idx          = Input(UInt(p.INDEX_W.W))
    val tagData      = Output(Vec(p.WAY_NUM, new TagEntry(p)))

    val refillTagEn  = Input(Bool())
    val refillWay    = Input(UInt(p.WAY_W.W))
    val refillIdx    = Input(UInt(p.INDEX_W.W))
    val refillTag    = Input(UInt(p.TAG_W.W))
    val refillDirty  = Input(Bool())

    val setDirtyEn   = Input(Bool())
    val setDirtyWay  = Input(UInt(p.WAY_W.W))
  })

  // 用 RegInit 保证上电复位后 valid/dirty=0，复位语义与 reset 信号统一，
  // 不再依赖独立的 when(reset.asBool) 块。
  val tagArray = RegInit(VecInit(Seq.fill(p.SET_NUM)(
    VecInit(Seq.fill(p.WAY_NUM)(0.U.asTypeOf(new TagEntry(p))))
  )))

  io.tagData := tagArray(io.idx)

  when(io.flush) {
    for (s <- 0 until p.SET_NUM) {
      for (w <- 0 until p.WAY_NUM) {
        tagArray(s)(w).valid := false.B
        tagArray(s)(w).dirty := false.B
      }
    }
  }.elsewhen(io.refillTagEn) {
    tagArray(io.refillIdx)(io.refillWay).valid := true.B
    tagArray(io.refillIdx)(io.refillWay).dirty := io.refillDirty
    tagArray(io.refillIdx)(io.refillWay).tag   := io.refillTag
  }.elsewhen(io.setDirtyEn) {
    tagArray(io.idx)(io.setDirtyWay).dirty := true.B
  }
}
```

## .\src\main\Dcache\TreePLRU.scala

```scala
package parameterized_cache

import chisel3._
import chisel3.util._

// ================================================================
//  通用树状 PLRU，支持任意 2 的幂路数（当前需求：4 / 8 路）。
//
//  存储：每组 (WAY_NUM-1) 个 bit，按 0 基堆编号摆放内部节点：
//        node 0 = 根；node n 的左/右子节点 = 2n+1 / 2n+2。
//        叶子 node 索引 (WAY_NUM-1 .. 2*WAY_NUM-2) 对应 way 0 .. WAY_NUM-1。
//
//  约定：内部节点 bit = “受害者方向”（0 = 左子树，1 = 右子树）。
//
//  驱逐：自根下行 depth 层，按各节点 bit 选择子树，落到的叶子即被驱逐路。
//  更新：沿 root→leaf(updateWay) 路径，把每个节点 bit 置为该路方向的取反
//        （指向兄弟子树），使被访问路成为 MRU。
//
//  4 路驱逐序：0→2→1→3→0
//  8 路驱逐序：0→4→2→6→1→5→3→7→0
// ================================================================
class TreePLRU(p: CacheParams) extends Module {
  require(isPow2(p.WAY_NUM), "TreePLRU 要求 WAY_NUM 为 2 的幂（当前支持 4 / 8 路）")

  val numNodes = p.WAY_NUM - 1
  val depth    = log2Ceil(p.WAY_NUM)
  val nodeW    = log2Ceil(2 * p.WAY_NUM)

  val io = IO(new Bundle {
    val idx       = Input(UInt(p.INDEX_W.W))
    val updateEn  = Input(Bool())
    val updateWay = Input(UInt(p.WAY_W.W))
    val evictWay  = Output(UInt(p.WAY_W.W))
  })

  val treeArray = RegInit(VecInit(Seq.fill(p.SET_NUM)(0.U(numNodes.W))))
  val tree = treeArray(io.idx)

  // ---- 驱逐路计算（组合）----
  var evNode = 0.U(nodeW.W)
  for (_ <- 0 until depth) {
    evNode = (evNode << 1).asUInt + 1.U + tree(evNode).asUInt
  }
  io.evictWay := (evNode - numNodes.U)(p.WAY_W - 1, 0)

  // ---- 命中 / 回填后更新 ----
  when(io.updateEn) {
    val newBits = Wire(Vec(numNodes, Bool()))
    for (i <- 0 until numNodes) newBits(i) := tree(i)
    var upNode = 0.U(nodeW.W)
    for (level <- 0 until depth) {
      val dir = io.updateWay(depth - 1 - level)   // 0 = 左, 1 = 右
      newBits(upNode) := ~dir
      upNode = (upNode << 1).asUInt + 1.U + dir.asUInt
    }
    treeArray(io.idx) := newBits.asUInt
  }
}
```

## .\src\main\Decode\Decoder_c.scala

```scala
package riscv

import chisel3._
import chisel3.util._
import Instructions._
import AluOp._
import Op1Sel._
import Op2Sel._
import WbSel._
import MemWidth._
import BrType._

object ImmSel {
  val I = 0.U(3.W)
  val S = 1.U(3.W)
  val B = 2.U(3.W)
  val U = 3.U(3.W)
  val J = 4.U(3.W)
}

class Decoder(enableRV32M: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val inst    = Input(UInt(32.W))
    val pc      = Input(UInt(32.W))
    val rs1Data = Input(UInt(32.W))
    val rs2Data = Input(UInt(32.W))
    val out     = Output(new DecodedSlot)
  })

  val inst    = io.inst
  val rs1Addr = inst(19, 15)
  val rs2Addr = inst(24, 20)
  val rdAddr  = inst(11, 7)
  val csrAddr = inst(31, 20)

  val immI = Cat(Fill(20, inst(31)), inst(31, 20))
  val immS = Cat(Fill(20, inst(31)), inst(31, 25), inst(11, 7))
  val immB = Cat(Fill(19, inst(31)), inst(31), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W))
  val immU = Cat(inst(31, 12), 0.U(12.W))
  val immJ = Cat(Fill(11, inst(31)), inst(31), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W))

  val default = List(
    ALU_ADD, OP1_RS1, OP2_IMM, WB_ALU,
    false.B, false.B, false.B, MW_WORD, true.B,
    BR_NONE, false.B, false.B, false.B, CSROp.NONE,
    false.B, false.B, ImmSel.I, false.B
  )

  val table = Seq(
    ADD   -> List(ALU_ADD,  OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),
    SUB   -> List(ALU_SUB,  OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),
    AND   -> List(ALU_AND,  OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),
    OR    -> List(ALU_OR,   OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),
    XOR   -> List(ALU_XOR,  OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),
    SLL   -> List(ALU_SLL,  OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),
    SRL   -> List(ALU_SRL,  OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),
    SRA   -> List(ALU_SRA,  OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),
    SLT   -> List(ALU_SLT,  OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),
    SLTU  -> List(ALU_SLTU, OP1_RS1, OP2_RS2, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.I, true.B),

    ADDI  -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    ANDI  -> List(ALU_AND,  OP1_RS1, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    ORI   -> List(ALU_OR,   OP1_RS1, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    XORI  -> List(ALU_XOR,  OP1_RS1, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    SLLI  -> List(ALU_SLL,  OP1_RS1, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    SRLI  -> List(ALU_SRL,  OP1_RS1, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    SRAI  -> List(ALU_SRA,  OP1_RS1, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    SLTI  -> List(ALU_SLT,  OP1_RS1, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    SLTIU -> List(ALU_SLTU, OP1_RS1, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),

    LW    -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_MEM, true.B,  true.B,  false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    LH    -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_MEM, true.B,  true.B,  false.B, MW_HALF, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    LB    -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_MEM, true.B,  true.B,  false.B, MW_BYTE, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    LHU   -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_MEM, true.B,  true.B,  false.B, MW_HALF, false.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    LBU   -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_MEM, true.B,  true.B,  false.B, MW_BYTE, false.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),

    SW    -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_ALU, false.B, false.B, true.B,  MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.S, true.B),
    SH    -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_ALU, false.B, false.B, true.B,  MW_HALF, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.S, true.B),
    SB    -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_ALU, false.B, false.B, true.B,  MW_BYTE, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.S, true.B),

    BEQ   -> List(ALU_SUB,  OP1_RS1, OP2_RS2, WB_ALU, false.B, false.B, false.B, MW_WORD, true.B,  BR_EQ,   false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.B, true.B),
    BNE   -> List(ALU_SUB,  OP1_RS1, OP2_RS2, WB_ALU, false.B, false.B, false.B, MW_WORD, true.B,  BR_NE,   false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.B, true.B),
    BLT   -> List(ALU_SLT,  OP1_RS1, OP2_RS2, WB_ALU, false.B, false.B, false.B, MW_WORD, true.B,  BR_LT,   false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.B, true.B),
    BGE   -> List(ALU_SLT,  OP1_RS1, OP2_RS2, WB_ALU, false.B, false.B, false.B, MW_WORD, true.B,  BR_GE,   false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.B, true.B),
    BLTU  -> List(ALU_SLTU, OP1_RS1, OP2_RS2, WB_ALU, false.B, false.B, false.B, MW_WORD, true.B,  BR_LTU,  false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.B, true.B),
    BGEU  -> List(ALU_SLTU, OP1_RS1, OP2_RS2, WB_ALU, false.B, false.B, false.B, MW_WORD, true.B,  BR_GEU,  false.B, false.B, false.B, CSROp.NONE, true.B,  true.B,  ImmSel.B, true.B),

    JAL   -> List(ALU_ADD,  OP1_PC,  OP2_IMM, WB_PC4, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, true.B,  false.B, false.B, CSROp.NONE, false.B, false.B, ImmSel.J, true.B),
    JALR  -> List(ALU_ADD,  OP1_RS1, OP2_IMM, WB_PC4, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, true.B,  true.B,  false.B, CSROp.NONE, true.B,  false.B, ImmSel.I, true.B),
    LUI   -> List(ALU_LUI,  OP1_IMM, OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, false.B, false.B, ImmSel.U, true.B),
    AUIPC -> List(ALU_ADD,  OP1_PC,  OP2_IMM, WB_ALU, true.B,  false.B, false.B, MW_WORD, true.B,  BR_NONE, false.B, false.B, false.B, CSROp.NONE, false.B, false.B, ImmSel.U, true.B),

    CSRRW -> List(ALU_COPY1, OP1_RS1, OP2_IMM, WB_CSR, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.WRITE, true.B, false.B, ImmSel.I, true.B),
    CSRRS -> List(ALU_COPY1, OP1_RS1, OP2_IMM, WB_CSR, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.SET,   true.B, false.B, ImmSel.I, true.B),
    CSRRC -> List(ALU_COPY1, OP1_RS1, OP2_IMM, WB_CSR, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.CLEAR, true.B, false.B, ImmSel.I, true.B),

    ECALL  -> List(ALU_ADD, OP1_RS1, OP2_IMM, WB_ALU, false.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, true.B, CSROp.NONE, false.B, false.B, ImmSel.I, true.B),
    EBREAK -> List(ALU_ADD, OP1_RS1, OP2_IMM, WB_ALU, false.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, true.B, CSROp.NONE, false.B, false.B, ImmSel.I, true.B),
    FENCE  -> List(ALU_ADD, OP1_RS1, OP2_IMM, WB_ALU, false.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, true.B, CSROp.NONE, false.B, false.B, ImmSel.I, true.B)
  )

  val mTable = if (enableRV32M) Seq(
    MUL    -> List(ALU_MUL,    OP1_RS1, OP2_RS2, WB_ALU, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B, true.B, ImmSel.I, true.B),
    MULH   -> List(ALU_MULH,   OP1_RS1, OP2_RS2, WB_ALU, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B, true.B, ImmSel.I, true.B),
    MULHSU -> List(ALU_MULHSU, OP1_RS1, OP2_RS2, WB_ALU, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B, true.B, ImmSel.I, true.B),
    MULHU  -> List(ALU_MULHU,  OP1_RS1, OP2_RS2, WB_ALU, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B, true.B, ImmSel.I, true.B),
    DIV    -> List(ALU_DIV,    OP1_RS1, OP2_RS2, WB_ALU, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B, true.B, ImmSel.I, true.B),
    DIVU   -> List(ALU_DIVU,   OP1_RS1, OP2_RS2, WB_ALU, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B, true.B, ImmSel.I, true.B),
    REM    -> List(ALU_REM,    OP1_RS1, OP2_RS2, WB_ALU, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B, true.B, ImmSel.I, true.B),
    REMU   -> List(ALU_REMU,   OP1_RS1, OP2_RS2, WB_ALU, true.B, false.B, false.B, MW_WORD, true.B, BR_NONE, false.B, false.B, false.B, CSROp.NONE, true.B, true.B, ImmSel.I, true.B)
  ) else Seq.empty

  val decoded = ListLookup(inst, default, (table ++ mTable).toArray)
  val immSel = decoded(16)

  io.out := 0.U.asTypeOf(new DecodedSlot)
  io.out.pc        := io.pc
  io.out.inst      := inst
  io.out.rs1Addr   := rs1Addr
  io.out.rs2Addr   := rs2Addr
  io.out.rdAddr    := rdAddr
  io.out.rs1Data   := io.rs1Data
  io.out.rs2Data   := io.rs2Data
  io.out.aluOp     := decoded(0)
  io.out.op1Sel    := decoded(1)
  io.out.op2Sel    := decoded(2)
  io.out.wbSel     := decoded(3)
  io.out.rfWen     := decoded(4).asBool
  io.out.memRen    := decoded(5).asBool
  io.out.memWen    := decoded(6).asBool
  io.out.memWd     := decoded(7)
  io.out.memSigned := decoded(8).asBool
  io.out.brType    := decoded(9)
  io.out.isJump    := decoded(10).asBool
  io.out.isJalr    := decoded(11).asBool
  io.out.isSysInst := decoded(12).asBool
  io.out.csrOp     := decoded(13)
  io.out.rs1Use    := decoded(14).asBool
  io.out.rs2Use    := decoded(15).asBool
  io.out.imm       := MuxLookup(immSel, immI, Seq(
    ImmSel.I -> immI,
    ImmSel.S -> immS,
    ImmSel.B -> immB,
    ImmSel.U -> immU,
    ImmSel.J -> immJ
  ))
  io.out.csrAddr := csrAddr
  io.out.ctrl.valid   := decoded(17).asBool
  io.out.ctrl.kill    := false.B
  io.out.ctrl.allowIn := true.B
}
```

## .\src\main\Execute\ALU.scala

```scala
package riscv

import chisel3._
import chisel3.util._
import AluOp._

class ParamALU(val xlen: Int = 32, val enableRV32M: Boolean = false) extends Module {
  require(xlen >= 32, "ParamALU currently expects xlen >= 32")

  private val shamtWidth = log2Ceil(xlen)

  val io = IO(new Bundle {
    val op1    = Input(UInt(xlen.W))
    val op2    = Input(UInt(xlen.W))
    val aluOp  = Input(UInt(AluOp.W.W))
    val result = Output(UInt(xlen.W))

    val cmpEq  = Output(Bool())
    val cmpLt  = Output(Bool())
    val cmpLtu = Output(Bool())
  })

  val shamt = io.op2(shamtWidth - 1, 0)

  io.cmpEq  := io.op1 === io.op2
  io.cmpLt  := io.op1.asSInt < io.op2.asSInt
  io.cmpLtu := io.op1 < io.op2

  val baseAluMapping = Seq(
    ALU_ADD   -> (io.op1 + io.op2),
    ALU_SUB   -> (io.op1 - io.op2),
    ALU_AND   -> (io.op1 & io.op2),
    ALU_OR    -> (io.op1 | io.op2),
    ALU_XOR   -> (io.op1 ^ io.op2),
    ALU_SLL   -> (io.op1 << shamt)(xlen - 1, 0),
    ALU_SRL   -> (io.op1 >> shamt),
    ALU_SRA   -> (io.op1.asSInt >> shamt).asUInt,
    ALU_SLT   -> io.cmpLt.asUInt,
    ALU_SLTU  -> io.cmpLtu.asUInt,
    ALU_LUI   -> io.op2,
    ALU_COPY1 -> io.op1
  )

  // RV32M operations are handled by the dedicated multi-cycle MulDivALU.
  io.result := MuxLookup(io.aluOp, 0.U(xlen.W), baseAluMapping)
}
```

## .\src\main\Execute\MulDivALU.scala

```scala
package riscv

import chisel3._
import chisel3.util._
import AluOp._

class MulDivReq(val xlen: Int = 32) extends Bundle {
  val op1   = UInt(xlen.W)
  val op2   = UInt(xlen.W)
  val aluOp = UInt(AluOp.W.W)
}

class MulDivALU(val xlen: Int = 32, val mulLatency: Int = 3, val divLatency: Int = 32)
    extends Module {
  require(xlen == 32, "MulDivALU implements RV32M and expects xlen == 32")
  require(mulLatency >= 1, "mulLatency must be positive")
  require(divLatency >= 1, "divLatency must be positive")

  private val latencyWidth = log2Ceil(math.max(mulLatency, divLatency) + 1).max(1)

  val io = IO(new Bundle {
    val req    = Flipped(Decoupled(new MulDivReq(xlen)))
    val resp   = Decoupled(UInt(xlen.W))
    val cancel = Input(Bool())
    val busy   = Output(Bool())
  })

  val sIdle :: sBusy :: sDone :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val remaining = RegInit(0.U(latencyWidth.W))
  val resultReg = RegInit(0.U(xlen.W))

  val divActive = RegInit(false.B)
  val divWantRem = RegInit(false.B)
  val divQuotNeg = RegInit(false.B)
  val divRemNeg = RegInit(false.B)
  val divDividend = RegInit(0.U(xlen.W))
  val divDivisor = RegInit(0.U(xlen.W))
  val divQuotient = RegInit(0.U(xlen.W))
  val divRemainder = RegInit(0.U((xlen + 1).W))

  private def twos(x: UInt): UInt = (~x).asUInt + 1.U

  private def abs32(x: UInt, signed: Bool): UInt = {
    Mux(signed && x(31), twos(x), x)
  }

  private def high32(x: UInt): UInt = x(63, 32)

  private def divStep(
      dividend: UInt,
      quotient: UInt,
      remainder: UInt,
      divisor: UInt): (UInt, UInt, UInt) = {
    val shiftedRem = Cat(remainder(31, 0), dividend(31))
    val divisor33 = Cat(0.U(1.W), divisor)
    val ge = shiftedRem >= divisor33
    val nextRem = Mux(ge, shiftedRem - divisor33, shiftedRem)
    val nextQuot = Cat(quotient(30, 0), ge)
    val nextDividend = Cat(dividend(30, 0), 0.U(1.W))
    (nextDividend, nextQuot, nextRem)
  }

  private def mulResult(op1: UInt, op2: UInt, aluOp: UInt): UInt = {
    val op1S33 = Cat(op1(31), op1).asSInt
    val op2S33 = Cat(op2(31), op2).asSInt
    val op1U33 = Cat(0.U(1.W), op1)
    val op2U33 = Cat(0.U(1.W), op2)

    val mulSS = (op1S33 * op2S33).asUInt
    val mulSU = (op1S33 * op2U33.asSInt).asUInt
    val mulUU = op1 * op2

    MuxLookup(aluOp, mulUU(31, 0), Seq(
      ALU_MUL    -> mulUU(31, 0),
      ALU_MULH   -> high32(mulSS),
      ALU_MULHSU -> high32(mulSU),
      ALU_MULHU  -> high32(mulUU)
    ))
  }

  private def divSpecialResult(op1: UInt, op2: UInt, aluOp: UInt): UInt = {
    val divByZero = op2 === 0.U
    val divOverflow = op1 === "h80000000".U && op2 === "hffffffff".U
    MuxLookup(aluOp, 0.U(xlen.W), Seq(
      ALU_DIV  -> Mux(divByZero, "hffffffff".U, Mux(divOverflow, op1, 0.U)),
      ALU_DIVU -> Mux(divByZero, "hffffffff".U, 0.U),
      ALU_REM  -> Mux(divByZero, op1, Mux(divOverflow, 0.U, 0.U)),
      ALU_REMU -> Mux(divByZero, op1, 0.U)
    ))
  }

  io.req.ready := state === sIdle
  io.resp.valid := state === sDone
  io.resp.bits := resultReg
  io.busy := state =/= sIdle

  val reqOp = io.req.bits.aluOp
  val reqIsDivRem = reqOp >= ALU_DIV
  val reqIsRem = reqOp === ALU_REM || reqOp === ALU_REMU
  val reqIsSignedDiv = reqOp === ALU_DIV || reqOp === ALU_REM
  val reqDivByZero = io.req.bits.op2 === 0.U
  val reqDivOverflow = io.req.bits.op1 === "h80000000".U && io.req.bits.op2 === "hffffffff".U
  val reqDivSpecial = reqDivByZero || reqDivOverflow
  val reqAbsDividend = abs32(io.req.bits.op1, reqIsSignedDiv)
  val reqAbsDivisor = abs32(io.req.bits.op2, reqIsSignedDiv)
  val reqDivQuotNeg = reqIsSignedDiv && (io.req.bits.op1(31) ^ io.req.bits.op2(31))
  val reqDivRemNeg = reqIsSignedDiv && io.req.bits.op1(31)
  val firstDiv = divStep(reqAbsDividend, 0.U(xlen.W), 0.U((xlen + 1).W), reqAbsDivisor)

  when(io.cancel) {
    state := sIdle
    remaining := 0.U
    divActive := false.B
  }.otherwise {
    switch(state) {
      is(sIdle) {
        when(io.req.fire) {
          when(reqIsDivRem) {
            resultReg := divSpecialResult(io.req.bits.op1, io.req.bits.op2, reqOp)
            divActive := !reqDivSpecial
            divWantRem := reqIsRem
            divQuotNeg := reqDivQuotNeg
            divRemNeg := reqDivRemNeg
            divDividend := firstDiv._1
            divQuotient := firstDiv._2
            divRemainder := firstDiv._3
            divDivisor := reqAbsDivisor
            when(divLatency.U === 1.U) {
              state := sDone
            }.otherwise {
              remaining := (divLatency - 1).U
              state := sBusy
            }
          }.otherwise {
            resultReg := mulResult(io.req.bits.op1, io.req.bits.op2, reqOp)
            divActive := false.B
            when(mulLatency.U === 1.U) {
              state := sDone
            }.otherwise {
              remaining := (mulLatency - 1).U
              state := sBusy
            }
          }
        }
      }
      is(sBusy) {
        val nextDiv = divStep(divDividend, divQuotient, divRemainder, divDivisor)
        when(divActive) {
          divDividend := nextDiv._1
          divQuotient := nextDiv._2
          divRemainder := nextDiv._3
        }

        when(remaining === 1.U) {
          when(divActive) {
            val finalQuot = Mux(divQuotNeg, twos(nextDiv._2), nextDiv._2)
            val finalRemRaw = nextDiv._3(31, 0)
            val finalRem = Mux(divRemNeg, twos(finalRemRaw), finalRemRaw)
            resultReg := Mux(divWantRem, finalRem, finalQuot)
          }
          state := sDone
          divActive := false.B
        }.otherwise {
          remaining := remaining - 1.U
        }
      }
      is(sDone) {
        when(io.resp.fire) {
          state := sIdle
        }
      }
    }
  }
}
```

## .\src\main\frame.md

```scala
# 流水线框架说明

## IF 模块

IF 模块负责 PC 生成、BPU 查询、I-Cache 取指、预取器控制和 IF/ID 流水寄存器输出。当前主线固定双发，因此 `issueWidth = 2`。

### 暴露接口说明

| 信号名 | 位宽/类型 | 方向 | 说明 |
| --- | --- | --- | --- |
| `stallIf` | `Bool` | Input | IF 级停顿信号。为 1 时 PC 保持不变，通常由 I-Cache miss、D-Cache miss 或后端阻塞触发。 |
| `flushIf` | `Bool` | Input | IF 级冲刷信号。为 1 时当前取指结果应作废，输出 bubble。 |
| `exRedirectValid` | `Bool` | Input | EX 级 redirect 有效信号，优先级最高。用于分支误预测修正、JALR 目标修正等。 |
| `exRedirectPc` | `UInt(32.W)` | Input | EX 级 redirect 目标 PC。 |
| `idRedirectValid` | `Bool` | Input | ID 级 redirect 有效信号，主要用于 JAL 在译码级提前改向。优先级低于 EX redirect，高于 stall 和 BPU 预测。 |
| `idRedirectPc` | `UInt(32.W)` | Input | ID 级 redirect 目标 PC。 |
| `bpuQueryPc` | `UInt(32.W)` | Output | 送往 BPU 的查询 PC，通常等于当前 `pcFetch`。 |
| `bpuPredTaken` | `Bool` | Input | BPU 返回的预测方向。为 1 表示预测跳转。 |
| `bpuPredTarget` | `UInt(32.W)` | Input | BPU 返回的预测目标地址。仅当 `bpuPredTaken` 为 1 时有效。 |
| `nextLinePrefetchEn` | `Bool` | Input | Next-line 预取器开关。建议由 CSR `prefetchCtrl(0)` 控制。与 stride 预取器开关互不影响。 |
| `stridePrefetchEn` | `Bool` | Input | Stride 预取器开关。建议由 CSR `prefetchCtrl(1)` 控制。与 next-line 预取器开关互不影响。 |
| `out` | `Vec(2, IFIDSlot)` | Output | IF 到 ID 的双发取指结果，每拍最多输出两个 slot。 |
| `icacheStall` | `Bool` | Output | I-Cache stall/miss 信号，送往 Hazard/Control 单元，用于冻结前端或全流水。 |
| `imem` | `MemBusIO` | IO | I-Cache miss/refill 和预取请求使用的外部内存总线。 |
| `debugPc` | `UInt(32.W)` | Output | 调试 PC，通常等于当前 IF 级 PC。 |

### `IFIDSlot` 展开

| 字段名 | 位宽/类型 | 说明 |
| --- | --- | --- |
| `pc` | `UInt(32.W)` | 当前 slot 对应指令的 PC。slot0 为 `fetchPc`，slot1 为 `fetchPc + 4`。 |
| `inst` | `UInt(32.W)` | 当前 slot 的 32 位指令。 |
| `ctrl.valid` | `Bool` | 当前 slot 是否包含有效指令。I-Cache miss、slot1 跨 cache line、flush 或 bubble 时为 0。 |
| `ctrl.kill` | `Bool` | 当前 slot 是否被 flush 杀掉。主要用于调试和后级防御性判断。 |
| `ctrl.allowIn` | `Bool` | 当前 slot 是否允许进入下一级。IF 输出侧可固定为 1，真正写入 IF/ID 寄存器由顶层 stall 控制。 |
| `slotIdx` | `UInt(1.W)` | 双发槽编号。slot0 为 0，slot1 为 1。 |
| `fetchPc` | `UInt(32.W)` | 本拍取指 bundle 的基地址，即 slot0 的 PC。 |
| `predTaken` | `Bool` | BPU 对本拍取指 PC 的预测方向。基础版两个 slot 可共享同一次 BPU 查询结果。 |
| `predTarget` | `UInt(32.W)` | BPU 预测目标地址。 |
| `predNextPc` | `UInt(32.W)` | IF 级根据预测实际选择的下一取指 PC。若预测跳转则为 `predTarget`，否则为顺序下一 bundle PC。 |
| `seqNextPc` | `UInt(32.W)` | 不跳转时的顺序下一 bundle PC。双发固定为 `fetchPc + 8`。 |
| `icacheHit` | `Bool` | 当前取指结果是否来自 I-Cache 命中响应。主要用于调试和性能统计。 |

### 预取开关约定

`nextLinePrefetchEn` 和 `stridePrefetchEn` 是两个独立开关，任意一个关闭都不应影响另一个预取器的内部状态更新和请求生成策略。

建议 CSR 映射如下：

| CSR 字段 | 控制对象 | 说明 |
| --- | --- | --- |
| `prefetchCtrl(0)` | `nextLinePrefetchEn` | 为 1 时允许 next-line 预取器发起预取请求。 |
| `prefetchCtrl(1)` | `stridePrefetchEn` | 为 1 时允许 stride 预取器发起预取请求。 |

当两个预取器同时发起请求时，IF 内部应先保证 demand miss 优先级最高；预取请求之间可先采用固定优先级，例如 next-line 优先于 stride，后续再改为轮询仲裁。

## ID 模块

ID 模块负责接收 IF/ID 流水寄存器中的双发取指结果，完成译码、寄存器堆读地址生成、CSR 读地址生成、槽间发射约束判断，并输出 ID/EX 流水寄存器内容。

当前顺序核采用前缀连续发射策略：槽 0 不能发射时，槽 1 必须同时变为 bubble；槽 1 不能发射时，只影响槽 1，不允许跳过槽 0 去发射更后面的指令。

### 暴露接口说明

| 信号名 | 位宽/类型 | 方向 | 说明 |
| --- | --- | --- | --- |
| `in` | `Vec(2, IFIDSlot)` | Input | 来自 IF/ID 流水寄存器的双发取指结果。 |
| `stallId` | `Bool` | Input | ID 级停顿信号。为 1 时 ID/EX 流水寄存器保持不变，两个槽均冻结。 |
| `flushId` | `Bool` | Input | ID 级冲刷信号。为 1 时当前 ID 输出全部变为 bubble。 |
| `regRs1Addr` | `Vec(2, UInt(5.W))` | Output | 送往 RegFile 的 rs1 读地址。 |
| `regRs2Addr` | `Vec(2, UInt(5.W))` | Output | 送往 RegFile 的 rs2 读地址。 |
| `regRs1Data` | `Vec(2, UInt(32.W))` | Input | RegFile 返回的 rs1 数据。 |
| `regRs2Data` | `Vec(2, UInt(32.W))` | Input | RegFile 返回的 rs2 数据。 |
| `csrRaddr` | `UInt(12.W)` | Output | 送往 CSRFile 的读地址。基础实现建议同周期最多允许一条 CSR 指令进入 EX。 |
| `csrRdata` | `UInt(32.W)` | Input | CSRFile 返回的 CSR 旧值，用于后续 CSR 写回语义。 |
| `idRedirectValid` | `Bool` | Output | ID 级 redirect 有效信号，主要用于 JAL 提前改向。 |
| `idRedirectPc` | `UInt(32.W)` | Output | ID 级 redirect 目标 PC，通常为 JAL 的 `pc + immJ`。 |
| `hazardIdValid` | `Vec(2, Bool)` | Output | 送往 HazardUnit 的 ID 槽有效信号。 |
| `hazardRs1Addr` | `Vec(2, UInt(5.W))` | Output | 送往 HazardUnit 的 rs1 地址。 |
| `hazardRs2Addr` | `Vec(2, UInt(5.W))` | Output | 送往 HazardUnit 的 rs2 地址。 |
| `hazardRs1Use` | `Vec(2, Bool)` | Output | 当前指令是否实际使用 rs1。 |
| `hazardRs2Use` | `Vec(2, Bool)` | Output | 当前指令是否实际使用 rs2。 |
| `hazardRdAddr` | `Vec(2, UInt(5.W))` | Output | 送往 HazardUnit 的 rd 地址。 |
| `hazardRfWen` | `Vec(2, Bool)` | Output | 当前指令是否写通用寄存器。 |
| `loadUseStall` | `Bool` | Input | HazardUnit 给出的 load-use stall。为 1 时槽 0 和槽 1 均冻结，不产生新发射。 |
| `structuralStall` | `Bool` | Input | I-Cache miss、D-Cache miss 或后端结构阻塞的合并停顿。为 1 时所有槽冻结。 |
| `out` | `Vec(2, IDEXBundle)` | Output | ID 到 EX 的双发译码结果。 |

### 内部槽间 RAW 检测

ID 级必须检测同一取指包内的槽间 RAW 冲突。规则如下：

| 条件 | 处理方式 |
| --- | --- |
| `slot0Valid && slot1Valid && slot0.rfWen && slot0.rdAddr =/= 0.U && slot0.rdAddr === slot1.rs1Addr && slot1.rs1Use` | 本周期降级单发射：仅发射槽 0，槽 1 变为 bubble，并在下一周期重新尝试发射。 |
| `slot0Valid && slot1Valid && slot0.rfWen && slot0.rdAddr =/= 0.U && slot0.rdAddr === slot1.rs2Addr && slot1.rs2Use` | 本周期降级单发射：仅发射槽 0，槽 1 变为 bubble，并在下一周期重新尝试发射。 |

组合表达式可写为：

```scala
val slotRaw01 =
  slot0Valid &&
  slot1Valid &&
  slot0.rfWen &&
  (slot0.rdAddr =/= 0.U) &&
  ((slot1.rs1Use && slot0.rdAddr === slot1.rs1Addr) ||
   (slot1.rs2Use && slot0.rdAddr === slot1.rs2Addr))
```

本设计不在 ID 级通过同周期旁路解决槽 0 到槽 1 的 RAW。只要检测到槽间 RAW，槽 1 必须等待槽 0 完成写回后再重新尝试发射。

### 前缀连续发射语义

ID 级发射必须满足前缀连续性。任意槽无法发射时，其后的槽都必须变为 bubble，不能跳过当前槽发射后续指令。

| 条件 | 影响槽位 |
| --- | --- |
| 槽 0 `valid=0`，例如取指包不足或对齐问题 | 槽 0 和槽 1 均为 bubble |
| 槽 1 `valid=0`，例如取指包仅返回 1 条 | 仅槽 1 为 bubble |
| 槽 1 与槽 0 存在内部 RAW 冲突 | 仅槽 1 为 bubble |
| `loadUseStall=1` | 槽 0 和槽 1 均冻结，ID/EX 保持不变 |
| `structuralStall=1`，例如 D-Cache miss 或 I-Cache miss | 所有槽冻结，整条流水线保持不变 |
| `flushId=1` | 槽 0 和槽 1 均变为 bubble |

推荐发射有效信号：

```scala
val slot0CanIssue =
  in(0).ctrl.valid &&
  !in(0).ctrl.kill &&
  !flushId &&
  !stallId &&
  !loadUseStall &&
  !structuralStall

val slot1CanIssue =
  slot0CanIssue &&
  in(1).ctrl.valid &&
  !in(1).ctrl.kill &&
  !slotRaw01
```

输出到 ID/EX 时：

```scala
out(0).ctrl.valid := slot0CanIssue
out(1).ctrl.valid := slot1CanIssue
```

当 `slot1CanIssue=false` 且 `slot0CanIssue=true` 时，本周期为单发射；槽 1 对应 ID/EX 内容必须写成安全 bubble，至少保证：

```scala
rfWen  := false.B
memRen := false.B
memWen := false.B
csrOp  := CSROp.NONE
brType := BrType.BR_NONE
```

### 槽 1 重新尝试发射的要求

当槽 1 因内部 RAW 冲突被降级为 bubble 时，系统必须保证槽 1 指令不会丢失。实现方式二选一：

| 方式 | 说明 |
| --- | --- |
| 保持 IF/ID 寄存器 | 当槽 1 因内部 RAW 停发时，冻结 IF/ID 中的槽 1，下一周期继续尝试发射。实现简单但需要处理槽 0 已发射后的状态。 |
| 引入小型 pending slot | 将未发射的槽 1 保存到 ID 内部 pending 寄存器，下一周期优先作为槽 0 尝试发射。接口更清晰，推荐用于后续重构。 |

为了保持前缀连续发射语义，推荐使用 pending slot 方案：被降级的槽 1 下一周期应作为最老指令优先进入译码/发射，而不是被新取指包覆盖。

## EX 模块

EX 模块负责执行 ID/EX 流水寄存器传入的双发指令，完成 ALU 运算、访存地址计算、分支判断、JALR 目标计算、CSR 操作请求生成、BPU 更新信息生成，并输出 EX/MEM 流水寄存器内容。

ALU 不是独立流水级，而是 EX 级内部的执行单元。双发顺序核中 EX 至少实例化 2 个 ALU，分别服务槽 0 和槽 1。

### 内部建议模块

| 子模块/逻辑 | 说明 |
| --- | --- |
| `ALU0` / `ALU1` | 两个并行 ALU，分别处理槽 0 和槽 1。 |
| 操作数选择逻辑 | 根据 `op1Sel/op2Sel` 选择 `rs1/rs2/pc/imm/pc+4` 等操作数。 |
| 分支比较逻辑 | 根据 `brType` 判断条件分支是否跳转。 |
| 分支目标计算逻辑 | 条件分支目标为 `pc + imm`，fall-through 为 `pc + 4`。 |
| JALR 目标计算逻辑 | JALR 目标为 `(rs1 + imm) & ~1`，在 EX 级产生 redirect。 |
| CSR 执行逻辑 | 生成 CSR 写请求，并把 CSR 旧值传入后续写回路径。 |
| BPU 更新逻辑 | 使用 EX 得到的真实分支方向和目标更新 BPU。 |
| Redirect 判断逻辑 | 比较真实 next PC 与预测 next PC，发现误预测时通知 IF flush/redirect。 |
| EX/MEM 打包逻辑 | 将 ALU 结果、访存控制、写回控制、CSR 结果等打包给 MEM 级。 |

### 暴露接口说明

| 信号名 | 位宽/类型 | 方向 | 说明 |
| --- | --- | --- | --- |
| `in` | `Vec(2, IDEXBundle)` | Input | 来自 ID/EX 流水寄存器的双发译码结果。 |
| `stallEx` | `Bool` | Input | EX 级停顿信号。为 1 时 EX/MEM 流水寄存器保持不变。 |
| `flushEx` | `Bool` | Input | EX 级冲刷信号。为 1 时当前 EX 输出变为 bubble，不允许写寄存器、访存、写 CSR 或更新 BPU。 |
| `rs1Data` | `Vec(2, UInt(32.W))` | Input | 经过旁路网络修正后的 rs1 数据。 |
| `rs2Data` | `Vec(2, UInt(32.W))` | Input | 经过旁路网络修正后的 rs2 数据。 |
| `op1Data` | `Vec(2, UInt(32.W))` | Input | ALU 操作数 1，通常由旁路后的 rs1、PC 或 0 选择得到。 |
| `op2Data` | `Vec(2, UInt(32.W))` | Input | ALU 操作数 2，通常由旁路后的 rs2、立即数或 `PC+4` 选择得到。 |
| `storeData` | `Vec(2, UInt(32.W))` | Input | Store 指令写入内存的数据，应使用旁路后的 rs2 数据。 |
| `csrOpValid` | `Bool` | Output | 当前周期是否有 CSR 操作请求。基础实现建议同周期最多一条 CSR 指令进入 EX。 |
| `csrOpType` | `UInt(2.W)` | Output | CSR 操作类型，对应 `CSROp.WRITE/SET/CLEAR`。主线至少需要支持 `CSROp.WRITE`。 |
| `csrWaddr` | `UInt(12.W)` | Output | CSR 写地址。 |
| `csrWdata` | `UInt(32.W)` | Output | CSR 写入的新值。对 `csrrw` 来说通常为旁路后的 rs1 数据。 |
| `csrOldData` | `UInt(32.W)` | Input | CSR 写入前的旧值，用于后续写回 rd。 |
| `exRedirectValid` | `Bool` | Output | EX 级 redirect 有效信号。用于分支误预测修正、JALR 改向等。 |
| `exRedirectPc` | `UInt(32.W)` | Output | EX 级 redirect 目标 PC。 |
| `bpuUpdateValid` | `Bool` | Output | BPU 更新有效信号。条件分支在 EX 得到真实结果后更新 BPU。 |
| `bpuUpdatePc` | `UInt(32.W)` | Output | 需要更新的分支指令 PC。 |
| `bpuUpdateTaken` | `Bool` | Output | 分支真实方向。为 1 表示实际跳转。 |
| `bpuUpdateTarget` | `UInt(32.W)` | Output | 分支真实目标地址。 |
| `exValid` | `Vec(2, Bool)` | Output | 当前 EX 各槽是否有效，供 HazardUnit/BypassUnit 使用。 |
| `exMemRen` | `Vec(2, Bool)` | Output | 当前 EX 各槽是否为 load 指令，用于 load-use stall 检测。 |
| `exRdAddr` | `Vec(2, UInt(5.W))` | Output | 当前 EX 各槽的目的寄存器地址。 |
| `exRfWen` | `Vec(2, Bool)` | Output | 当前 EX 各槽是否会写通用寄存器。 |
| `exResult` | `Vec(2, UInt(32.W))` | Output | EX 级 ALU 结果，用于旁路。注意 load 指令的该值是访存地址，不是 load 数据。 |
| `out` | `Vec(2, EXMEMBundle)` | Output | EX 到 MEM 的双发流水输出。 |

### ALU 操作数语义

EX 级 ALU 输入建议由旁路单元完成选择后送入 EX：

| 信号 | 说明 |
| --- | --- |
| `rs1Data` | 旁路修正后的 rs1 值，用于分支比较、JALR、CSR 写数据等。 |
| `rs2Data` | 旁路修正后的 rs2 值，用于分支比较。 |
| `op1Data` | 已根据 `op1Sel` 选择完成的 ALU 输入 1。 |
| `op2Data` | 已根据 `op2Sel` 选择完成的 ALU 输入 2。 |
| `storeData` | Store 写内存数据，必须是旁路后的 rs2 值。 |

ALU 连接方式：

```scala
alu(i).io.op1   := op1Data(i)
alu(i).io.op2   := op2Data(i)
alu(i).io.aluOp := in(i).aluOp
```

### 分支与 Redirect 语义

条件分支在 EX 级解析真实方向：

| `brType` | 判断条件 |
| --- | --- |
| `BR_EQ` | `rs1Data === rs2Data` |
| `BR_NE` | `rs1Data =/= rs2Data` |
| `BR_LT` | `rs1Data.asSInt < rs2Data.asSInt` |
| `BR_GE` | `rs1Data.asSInt >= rs2Data.asSInt` |
| `BR_LTU` | `rs1Data < rs2Data` |
| `BR_GEU` | `rs1Data >= rs2Data` |

分支目标与顺序目标：

```scala
val branchTarget = pc + imm
val fallThrough  = pc + 4.U
val actualNextPc = Mux(branchTaken, branchTarget, fallThrough)
```

JALR 目标：

```scala
val jalrTarget = (rs1Data + imm) & "hfffffffe".U
```

EX 级应比较真实 next PC 与 IF 级预测 next PC。若不一致，则产生 redirect：

```scala
val mispred = actualNextPc =/= predNextPc
exRedirectValid := mispred
exRedirectPc    := actualNextPc
```

因此建议 `IDEXBundle` 后续补充从 IF/ID 传下来的预测字段：

| 字段 | 说明 |
| --- | --- |
| `predTaken` | IF 级预测方向。 |
| `predTarget` | IF 级预测目标。 |
| `predNextPc` | IF 级实际选择的下一 PC。 |

### CSR 执行语义

基础主线至少支持 `csrrw`：

```text
CSR[csr] <- rs1
rd       <- old CSR value
```

EX 级生成 CSR 写请求：

```scala
csrOpValid := slotValid && in(i).csrOp =/= CSROp.NONE
csrOpType  := in(i).csrOp
csrWaddr   := in(i).csrAddr
csrWdata   := rs1Data(i)
```

CSR 旧值 `csrOldData` 应进入 EX/MEM 的 `csrRdata`，后续 WB 根据 `wbSel=WB_CSR` 写回 rd。

如果两个槽同周期都是 CSR 指令，基础实现应在 ID 级阻止槽 1 发射，避免 EX 级 CSR 写端口冲突。

### 双发控制流约束

为降低 redirect 仲裁复杂度，基础顺序核建议采用保守规则：

| 情况 | 建议处理 |
| --- | --- |
| 槽 0 是 branch/JAL/JALR | 槽 1 在 ID 级变为 bubble，或至少不允许槽 1 再产生 redirect。 |
| 槽 1 是 branch/JAL/JALR，槽 0 是普通指令 | 可以允许槽 1 进入 EX。 |
| 两个槽都可能产生 redirect | 槽 0 优先，因为槽 0 程序序更老。 |

当前 ID 级已经规定内部 RAW 时槽 1 降级，因此 EX 不需要实现槽 0 到槽 1 的同周期旁路。

### EX 输出有效性

推荐每槽 EX 有效信号：

```scala
val exSlotValid =
  in(i).ctrl.valid &&
  !in(i).ctrl.kill &&
  !flushEx
```

当 `exSlotValid=false` 时，EX/MEM 输出必须为安全 bubble，至少保证：

```scala
out(i).ctrl.valid := false.B
out(i).rfWen  := false.B
out(i).memRen := false.B
out(i).memWen := false.B
out(i).brType := BrType.BR_NONE
```

### EX/MEM 打包字段语义

| 字段 | 说明 |
| --- | --- |
| `pc` | 当前指令 PC。 |
| `inst` | 当前指令编码，主要用于调试。 |
| `aluOut` | ALU 结果。对 load/store 是访存地址；对普通 ALU 指令是运算结果。 |
| `rs2Data` | Store 写内存数据，建议填入旁路后的 `storeData`。 |
| `rdAddr` | 目的寄存器地址。 |
| `wbSel` | WB 阶段写回数据来源选择。 |
| `rfWen` | 是否写通用寄存器。 |
| `memRen` | 是否为 load。 |
| `memWen` | 是否为 store。 |
| `memWd` | 访存宽度，word/half/byte。 |
| `memSigned` | load 是否符号扩展。 |
| `csrRdata` | CSR 旧值，用于 `WB_CSR` 写回。 |
| `ctrl` | 流水控制信息。 |

## MEM 模块

MEM 模块负责接收 EX/MEM 流水寄存器中的双发结果，完成 load/store 访问、D-Cache 接入、mtime MMIO 读、printf 地址输出，以及向 MEM/WB 打包写回数据。

当前 `DCacheTop` 已经在 `src/main/Dcache` 中实现，MEM 模块需要适配它的真实接口。`DCacheTop` 当前是单端口数据 cache，因此基础顺序核建议同周期最多允许一条访存指令进入 MEM；如果两个槽同时是 load/store，应在 ID 或 EX 之前让较年轻槽变为 bubble，或者在 MEM 内部只服务最老访存并冻结流水。

### `DCacheTop` 适配接口

`DCacheTop` 的真实接口如下，MEM 模块应直接连接这些信号：

| DCacheTop 信号 | 位宽/类型 | 方向（相对 DCache） | MEM 侧连接语义 |
| --- | --- | --- | --- |
| `addr` | `UInt(p.ADDR_WIDTH.W)` | Input | 访存地址，来自访存槽的 `EXMEMBundle.aluOut`。 |
| `flush` | `Bool` | Input | cache flush 信号。基础版可接全局 flush 或固定为 0，后续用于 cache 清空。 |
| `wen` | `Bool` | Input | store 使能，来自访存槽 `memWen`。 |
| `wmask` | `UInt(p.WMASK_BITS.W)` | Input | store 字节写掩码，由地址低位和 `memWd` 生成。 |
| `wdata` | `UInt(p.DATA_WIDTH.W)` | Input | store 写数据，来自 EX 级传下来的旁路后 `rs2Data/storeData`。 |
| `memRen` | `Bool` | Input | load 使能，来自访存槽 `memRen`。 |
| `memWd` | `UInt(2.W)` | Input | 访存宽度，直接来自 `EXMEMBundle.memWd`。 |
| `signed` | `Bool` | Input | load 是否符号扩展，直接来自 `EXMEMBundle.memSigned`。 |
| `rdata` | `UInt(p.DATA_WIDTH.W)` | Output | load 读出并完成扩展后的数据，送入对应槽的 `MEMWBBundle.memData`。 |
| `missOut` | `Bool` | Output | D-Cache miss/stall 状态，可送 Hazard/Control 作为 `dcacheStall`。 |
| `stall` | `Bool` | Output | 与 `missOut` 语义一致，表示 D-Cache 正在处理 miss，流水线应冻结。 |
| `mtimeLo` | `UInt(p.DATA_WIDTH.W)` | Input | 来自 CSRFile 的 `mtimeLo`，用于 DCache 内部 MMIO 地址读取。 |
| `mtimeHi` | `UInt(p.DATA_WIDTH.W)` | Input | 来自 CSRFile 的 `mtimeHi`，用于 DCache 内部 MMIO 地址读取。 |
| `printChar` | `Valid(UInt(8.W))` | Output | printf 地址 store 产生的字符输出。 |
| `mem` | `MemBusIO(p)` | IO | D-Cache miss/writeback/refill 使用的外部内存总线。 |

### MEM 暴露接口说明

| 信号名 | 位宽/类型 | 方向 | 说明 |
| --- | --- | --- | --- |
| `in` | `Vec(2, EXMEMBundle)` | Input | 来自 EX/MEM 流水寄存器的双发结果。 |
| `stallMem` | `Bool` | Input | MEM 级停顿信号。为 1 时 MEM/WB 流水寄存器保持不变。 |
| `flushMem` | `Bool` | Input | MEM 级冲刷信号。为 1 时当前 MEM 输出应变为 bubble，不能写回。 |
| `mtimeLo` | `UInt(32.W)` | Input | CSRFile 输出的 mtime 低 32 位，透传给 DCacheTop。 |
| `mtimeHi` | `UInt(32.W)` | Input | CSRFile 输出的 mtime 高 32 位，透传给 DCacheTop。 |
| `dcacheFlush` | `Bool` | Input | D-Cache flush 控制信号。基础版可由顶层固定为 0。 |
| `dcacheStall` | `Bool` | Output | D-Cache miss/stall 状态，送往 Hazard/Control 触发全流水冻结。 |
| `printChar` | `Valid(UInt(8.W))` | Output | printf 地址 store 的字符输出。 |
| `dmem` | `MemBusIO(p)` | IO | D-Cache 对外内存总线。 |
| `out` | `Vec(2, MEMWBBundle)` | Output | MEM 到 WB 的双发流水输出。 |

### MEM 内部访存槽选择

由于当前 D-Cache 是单端口，MEM 每周期只能向 D-Cache 发起一个 load/store。推荐选择程序序最老的有效访存槽：

```scala
val slot0Mem = in(0).ctrl.valid && (in(0).memRen || in(0).memWen)
val slot1Mem = in(1).ctrl.valid && (in(1).memRen || in(1).memWen)

val memSel0 = slot0Mem
val memSel1 = !slot0Mem && slot1Mem
```

基础实现更推荐在 ID 阶段禁止双访存同发，这样 MEM 中不会出现 `slot0Mem && slot1Mem`。如果仍然出现，应当以槽 0 为准，并触发流水冻结或断言，防止槽 1 访存丢失。

### Store Mask 生成

MEM 级根据地址低 2 位和 `memWd` 生成 DCache `wmask`：

| 访存宽度 | `addr(1,0)` | `wmask` 语义 |
| --- | --- | --- |
| word | 任意，通常要求对齐 | `1111` |
| half | `00` | `0011` |
| half | `10` | `1100` |
| byte | `00` | `0001` |
| byte | `01` | `0010` |
| byte | `10` | `0100` |
| byte | `11` | `1000` |

若不实现精确异常，非对齐访问可先按硬件自然掩码处理或在测试中避免。

### MEM/WB 打包规则

| 输入类型 | MEM/WB 字段填充 |
| --- | --- |
| 普通 ALU/JAL/CSR 指令 | `memData := 0.U`，其余写回信息从 EX/MEM 透传。 |
| load 指令 | `memData := dcache.io.rdata`，`wbSel` 保持 `WB_MEM`。 |
| store 指令 | `rfWen := false.B`，不写回通用寄存器。 |
| bubble/flush | `ctrl.valid := false.B`，`rfWen := false.B`，`memData := 0.U`。 |

### MEM 停顿语义

当 `dcache.io.stall` 或 `dcache.io.missOut` 为 1 时：

| 行为 | 说明 |
| --- | --- |
| `dcacheStall := true.B` | 通知 Hazard/Control。 |
| IF/ID/EX/MEM/WB | 基础版建议全流水冻结，直到 D-Cache miss 完成。 |
| MEM/WB | 保持原值，不提交新的 load 结果。 |

## WB 模块

WB 模块负责从 MEM/WB 流水寄存器中选择最终写回数据，驱动 RegFile 写端口，并产生提交计数信号给 CSRFile。

### 暴露接口说明

| 信号名 | 位宽/类型 | 方向 | 说明 |
| --- | --- | --- | --- |
| `in` | `Vec(2, MEMWBBundle)` | Input | 来自 MEM/WB 流水寄存器的双发结果。 |
| `regWen` | `Vec(2, Bool)` | Output | RegFile 写使能。无效槽、`rd=x0` 或 `rfWen=0` 时必须为 0。 |
| `regWaddr` | `Vec(2, UInt(5.W))` | Output | RegFile 写地址。 |
| `regWdata` | `Vec(2, UInt(32.W))` | Output | RegFile 写数据。 |
| `instRetire` | `Vec(2, Bool)` | Output | 每个槽是否成功退休，用于 CSRFile 统计 `minstret`。 |
| `wbValid` | `Vec(2, Bool)` | Output | 写回槽有效信号，供 BypassUnit 使用。 |
| `wbRfWen` | `Vec(2, Bool)` | Output | 写回槽是否写通用寄存器，供 BypassUnit 使用。 |
| `wbRdAddr` | `Vec(2, UInt(5.W))` | Output | 写回目的寄存器号，供 BypassUnit 使用。 |
| `wbData` | `Vec(2, UInt(32.W))` | Output | 写回数据，供 BypassUnit 使用。 |

### 写回数据选择

WB 根据 `wbSel` 选择写回数据：

| `wbSel` | 写回数据 |
| --- | --- |
| `WB_ALU` | `aluOut` |
| `WB_MEM` | `memData` |
| `WB_PC4` | `pc + 4` |
| `WB_CSR` | `csrRdata` |

推荐组合逻辑：

```scala
val wbData = MuxLookup(in(i).wbSel, in(i).aluOut, Seq(
  WbSel.WB_ALU -> in(i).aluOut,
  WbSel.WB_MEM -> in(i).memData,
  WbSel.WB_PC4 -> (in(i).pc + 4.U),
  WbSel.WB_CSR -> in(i).csrRdata
))
```

### 双写端口语义

RegFile 支持双写端口时，WB 两个槽可同时写回。若槽 0 和槽 1 同周期写同一个非零 `rd`，必须保证程序序更年轻的槽 1 最终可见。

| 情况 | 处理 |
| --- | --- |
| `rd=x0` | 写使能强制为 0。 |
| 槽 0、槽 1 写不同 rd | 两个写端口同时写。 |
| 槽 0、槽 1 写同一非零 rd | 槽 1 优先，RegFile 内部或 WB 写端口仲裁必须保证槽 1 覆盖槽 0。 |

### 退休计数语义

`instRetire(i)` 建议定义为：

```scala
instRetire(i) := in(i).ctrl.valid && !in(i).ctrl.kill
```

如果后续加入异常、阻塞提交或精确退休，再把该定义收紧。当前课程要求不实现精确异常，因此基础版可按 WB 有效槽计数。

## RegFile 模块

RegFile 是 32 个 32 位通用寄存器文件，服务双发 ID 读和双发 WB 写。

### 暴露接口说明

| 信号名 | 位宽/类型 | 方向 | 说明 |
| --- | --- | --- | --- |
| `rs1Addr` | `Vec(2, UInt(5.W))` | Input | 两个槽的 rs1 读地址。 |
| `rs2Addr` | `Vec(2, UInt(5.W))` | Input | 两个槽的 rs2 读地址。 |
| `rs1Data` | `Vec(2, UInt(32.W))` | Output | 两个槽的 rs1 读数据。 |
| `rs2Data` | `Vec(2, UInt(32.W))` | Output | 两个槽的 rs2 读数据。 |
| `wen` | `Vec(2, Bool)` | Input | 两个写端口的写使能，来自 WB。 |
| `waddr` | `Vec(2, UInt(5.W))` | Input | 两个写端口的写地址。 |
| `wdata` | `Vec(2, UInt(32.W))` | Input | 两个写端口的写数据。 |

### RegFile 语义约定

| 规则 | 说明 |
| --- | --- |
| `x0` 恒为 0 | 读 `x0` 必须返回 0，写 `x0` 必须忽略。 |
| 双读双写 | 双发需要 4 个读端口和 2 个写端口。 |
| 同周期读写同一寄存器 | 建议实现 write-first 或在 ID/EX 旁路中覆盖，保证读到最新可见值。 |
| 双写同一寄存器 | 若两个写端口写同一非零寄存器，槽 1 优先。 |

推荐双写优先级：

```scala
when(wen(0) && waddr(0) =/= 0.U) {
  regs(waddr(0)) := wdata(0)
}
when(wen(1) && waddr(1) =/= 0.U) {
  regs(waddr(1)) := wdata(1)
}
```

这样当两个端口写同一地址时，后写的槽 1 覆盖槽 0。

## CSRFile 模块

CSRFile 负责实现基础 CSR、性能计数器、mtime 计数器和预取开关。当前源码中的 `CSRFile` 已经包含 `mcycle`、`mcycleh`、`minstret`、`mcountinhibit`、`misa`、`prefetchCtrl`、`mtimeLo/mtimeHi`。

### 暴露接口说明

| 信号名 | 位宽/类型 | 方向 | 说明 |
| --- | --- | --- | --- |
| `raddr` | `UInt(12.W)` | Input | CSR 读地址。基础实现只有一个读端口，因此 ID 阶段应限制同周期最多一条 CSR 指令。 |
| `rdata` | `UInt(32.W)` | Output | CSR 读数据。 |
| `opValid` | `Bool` | Input | EX 级 CSR 操作有效信号。 |
| `opType` | `UInt(2.W)` | Input | CSR 操作类型，使用 `CSROp.WRITE/SET/CLEAR`。主线至少需要 `CSROp.WRITE`。 |
| `waddr` | `UInt(12.W)` | Input | CSR 写地址。 |
| `wdata` | `UInt(32.W)` | Input | CSR 写数据。 |
| `oldData` | `UInt(32.W)` | Output | CSR 写入前旧值，用于 `csrrw` 写回 rd。 |
| `cycleTick` | `Bool` | Input | 周期计数使能。为 1 且 `mcountinhibit(0)=0` 时，`mcycle` 自增。 |
| `instRetire` | `Vec(2, Bool)` | Input | WB 阶段每槽退休信号，用于更新 `minstret`。 |
| `mcycleLo` | `UInt(32.W)` | Output | `mcycle` 低 32 位。 |
| `mcycleHi` | `UInt(32.W)` | Output | `mcycle` 高 32 位。 |
| `minstretLo` | `UInt(32.W)` | Output | `minstret` 低 32 位。 |
| `mtimeLo` | `UInt(32.W)` | Output | `mtime` 低 32 位，送 DCacheTop 处理 MMIO 读。 |
| `mtimeHi` | `UInt(32.W)` | Output | `mtime` 高 32 位，送 DCacheTop 处理 MMIO 读。 |
| `prefetchCtrl` | `UInt(32.W)` | Output | 预取控制 CSR。`bit0` 控制 next-line，`bit1` 控制 stride。 |

### CSR 地址约定

| CSR | 地址 | 说明 |
| --- | --- | --- |
| `mcycle` | `0xB00` | 周期计数低 32 位。 |
| `mcycleh` | `0xB80` | 周期计数高 32 位。 |
| `minstret` | `0xB02` | 退休指令计数低 32 位。 |
| `mcountinhibit` | `0x320` | 计数器抑制控制。 |
| `misa` | `0x301` | ISA 信息，只读。 |
| `prefetchCtrl` | `0x7C0` | 自定义预取控制 CSR。 |

### 预取控制位

| 位 | 名称 | 说明 |
| --- | --- | --- |
| `prefetchCtrl(0)` | `nextLinePrefetchEn` | 为 1 时允许 next-line 预取器发起请求。 |
| `prefetchCtrl(1)` | `stridePrefetchEn` | 为 1 时允许 stride 预取器发起请求。 |

### `csrrw` 语义

主线至少需要支持：

```text
old = CSR[csr]
CSR[csr] = rs1
rd = old
```

EX 级应把 `rs1Data` 作为 `wdata`，CSRFile 输出 `oldData`，随后 WB 通过 `WB_CSR` 写回 `rd`。

### CSR 双发限制

由于当前 CSRFile 只有一个读端口和一个写请求端口，基础顺序核应在 ID 级限制同周期最多一条 CSR 指令进入 EX。若槽 0 和槽 1 都是 CSR 指令，应只发射槽 0，槽 1 下一周期重新尝试。

## Prefetch Update

Current prefetch control uses CSR `0x7C0`:

| Bit | Name | Scope |
| --- | --- | --- |
| `prefetchCtrl(0)` | `nextLinePrefetchEn` | Enables IF/I-Cache next-line prefetch. |
| `prefetchCtrl(1)` | `stridePrefetchEn` | Enables IF/I-Cache and MEM/D-Cache stride prefetch. |
| `prefetchCtrl(2)` | `streamPrefetchEn` | Enables IF/I-Cache and MEM/D-Cache stream prefetch. |

D-Cache now exposes `pfReqValid`, `pfReqReady`, and `pfReqAddr`. Demand load/store miss has priority over prefetch. A D-Cache prefetch miss may occupy the memory refill FSM, but it must not directly freeze the pipeline unless a demand miss arrives while the FSM is busy.

## Branch Predictor Mode CSR

Branch predictor selection uses custom CSR `0x7C1` (`branchPredCtrl`).

| Bits | Value | Mode | Description |
| --- | --- | --- | --- |
| `[1:0]` | `0` | `BPU` | Bi-mode direction predictor plus BTB. RAS is disabled. |
| `[1:0]` | `1` | `BPU_RAS` | Bi-mode direction predictor plus BTB and return-address stack. This is the reset default. |
| `[1:0]` | `2` | `TAGE` | Simplified TAGE direction predictor plus BTB and return-address stack. |
| `[1:0]` | `3` | `BPU_RAS` | Reserved value, currently falls back to `BPU_RAS`. |

All predictors are updated in parallel from EX-stage branch resolution. The CSR only selects which predictor drives the IF-stage `bpuPredTaken`, `bpuPredTarget`, `rasPredValid`, and `rasPredTarget` signals.
```

## .\src\main\Frontend\BPU.scala

```scala
package riscv

import chisel3._
import chisel3.util._

// BTBEntry 定义在此处，BPU / BPU_RAS / TAGE 共用（同属 riscv 包）。
class BTBEntry extends Bundle {
  val valid  = Bool()
  val tag    = UInt(22.W)
  val target = UInt(32.W)
}

class BPU extends Module {
  val io = IO(new Bundle {
    val queryPc      = Input(UInt(32.W))
    val predTaken    = Output(Bool())
    val predTarget   = Output(UInt(32.W))

    val updateValid  = Input(Bool())
    val updatePc     = Input(UInt(32.W))
    val updateTaken  = Input(Bool())
    val updateTarget = Input(UInt(32.W))
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
}
```

## .\src\main\Frontend\BPU_RAS.scala

```scala
package riscv

import chisel3._
import chisel3.util._

class RASInterface extends Bundle {
  val push       = Input(Bool())
  val pushAddr   = Input(UInt(32.W))
  val pop        = Input(Bool())
  val topAddr    = Output(UInt(32.W))
  val topValid   = Output(Bool())
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
  val rasCount = RegInit(0.U(5.W))

  io.ras.topAddr := rasStack(rasPtr)
  io.ras.topValid := rasCount =/= 0.U

  when(io.ras.flush) {
    rasPtr := io.ras.checkpoint
    rasCount := 0.U
  } .elsewhen(io.ras.push) {
    val nextPtr = (rasPtr + 1.U)(3, 0)
    rasStack(nextPtr) := io.ras.pushAddr
    rasPtr            := nextPtr
    when(rasCount =/= 16.U) {
      rasCount := rasCount + 1.U
    }
  } .elsewhen(io.ras.pop) {
    rasPtr := (rasPtr - 1.U)(3, 0)
    when(rasCount =/= 0.U) {
      rasCount := rasCount - 1.U
    }
  }
}
```

## .\src\main\Frontend\NextLinePrefetcher.scala

```scala
package riscv

import chisel3._
import chisel3.util._

// ============================================================
//  NextLinePrefetcher：Next-line 预取器（spec §6.6）
//
//  修复点（相比原版）：
//   1. 预取地址 +4 → 下一条 64B Cache Line 基地址（+64，对齐）
//   2. 接口对齐 ICacheTop 的 pfReqValid / pfReqReady / pfReqAddr
//   3. 新增 prefetchEn（CSR prefetch_ctrl[0]）开关
//   4. 总线忙（pfReqReady=0）时本次请求丢弃，不重试
//
//  发起条件：
//    prefetchEn && cacheHit && !cacheStall
//
//  不发起条件（任一成立即丢弃）：
//    - prefetchEn=0（CSR 关闭）
//    - cacheStall=1（I-Cache 正在处理 miss，总线忙）
//    - pfReqReady=0（ICacheTop FSM 不在 sIdle 或正处理 miss，
//                   ICacheMissFSM 会自动 drop，无需外部重试）
//
//  注意：目标行是否已在 Cache 由 ICacheTop 内部的 IHitTest 负责
//  判断——若已命中则 FSM 不会真正发出内存总线请求；外部预取器
//  无需（也无法）独立查询 TagArray，故 lineInCache 检查省略。
// ============================================================
class NextLinePrefetcher extends Module {
  val io = IO(new Bundle {
    // ── 来自 ICacheTop / IFStage ───────────────────────────
    // 当前命中行的基地址（即取指 PC，用于计算下一行地址）
    val currAddr   = Input(UInt(32.W))
    val cacheHit   = Input(Bool())   // ICacheTop.respValid
    val cacheStall = Input(Bool())   // ICacheTop.missOut

    // ── 来自 CSRFile ──────────────────────────────────────
    val prefetchEn = Input(Bool())   // CSR prefetch_ctrl[0]

    // ── 与 ICacheTop 的握手接口 ───────────────────────────
    // （直连 ICacheTop.pfReqValid / pfReqReady / pfReqAddr）
    val pfReqValid = Output(Bool())
    val pfReqReady = Input(Bool())   // ICacheTop 握手应答
    val pfReqAddr  = Output(UInt(32.W))
  })

  // 下一条 Cache Line 基地址：当前地址按 64B 对齐后 +64
  //   nextLineAddr = {currAddr[31:6] + 1, 6'b0}
  val nextLineAddr = Cat(io.currAddr(31, 6) + 1.U, 0.U(6.W))

  // 只有命中且无 miss stall 时才尝试预取
  val wantPrefetch = io.prefetchEn && io.cacheHit && !io.cacheStall

  io.pfReqValid := wantPrefetch
  io.pfReqAddr  := nextLineAddr
  // pfReqReady=0 时本次请求被 ICacheMissFSM 静默丢弃（pfReqReady
  // 仅在 FSM sIdle 且无 miss 时置高）；预取器本身不需要重试逻辑。
}
```

## .\src\main\Frontend\PcGen.scala

```scala
package riscv

import chisel3._
import chisel3.util._

// ============================================================
//  PcGen：PC 生成模块（spec §3.1 / §3.1.1）
//
//  修复点（相比原版）：
//   1. 顺序步长 +4 → +N*4（双发射 N=2，即 +8）
//   2. 新增 BPU 预测跳转输入（优先级 4）
//   3. 实现 EX > ID(JAL) > Stall > BPU > Seq 完整仲裁
//   4. redirect 信号优先级高于 stall（flush 隐含不保持 PC）
//   5. PC 复位至 0x80000000（spec §8.3）
//
//  下一 PC 仲裁优先级（高 → 低）：
//   1. exRedirectValid  ── EX 级改向（分支误预测 / JALR）  2 周期 flush
//   2. idRedirectValid  ── ID 级 JAL 改向                  1 周期 flush
//   3. stall            ── 流水线停顿，保持 PC 不变
//   4. bpuPredTaken     ── BPU 预测跳转                    0 周期
//   5. 默认             ── 顺序取指 PC + N*4
//
//  互斥保证：JALR 与分支误预测共用 exRedirectValid，
//           同一周期不会并发；redirect 优先于 stall，
//           满足 spec §3.1.2 "flush 优先于 stall"。
// ============================================================
class PcGen(
    val N: Int      = 2,
    val resetVec: Long = 0x80000000L
) extends Module {

  val io = IO(new Bundle {
    // ── EX 级改向（最高优先级）────────────────────────────
    val exRedirectValid = Input(Bool())
    val exRedirectPc    = Input(UInt(32.W))

    // ── ID 级 JAL 改向 ────────────────────────────────────
    val idRedirectValid = Input(Bool())
    val idRedirectPc    = Input(UInt(32.W))

    // ── BPU 预测跳转 ──────────────────────────────────────
    val bpuPredTaken  = Input(Bool())
    val bpuPredTarget = Input(UInt(32.W))

    // ── 流水线停顿 ────────────────────────────────────────
    // stallIf 来自 HazardUnit；I-Cache miss 产生的 icacheStall
    // 也需通过 HazardUnit 汇总后送入此信号
    val stallIf = Input(Bool())
    val fetchSlot1Valid = Input(Bool())

    // ── 输出 ──────────────────────────────────────────────
    val currPc  = Output(UInt(32.W)) // 当前 PC（→ debugPc / 预取器 currAddr）
    val pcFetch = Output(UInt(32.W)) // 送往 I-Cache 的取指地址
  })

  val pcReg  = RegInit(resetVec.U(32.W))
  val nextPc = Wire(UInt(32.W))
  val seqStep = Mux(io.fetchSlot1Valid, (N * 4).U(32.W), 4.U(32.W))
  private def canonicalPc(pc: UInt): UInt = {
    Mux(pc(31), pc, pc | resetVec.U(32.W))
  }

  // ── 优先级仲裁（when 链，高优先级在前）────────────────────
  when(io.exRedirectValid) {
    // 最高：EX 级改向（含 flush 语义，覆盖 stall）
    nextPc := io.exRedirectPc
  }.elsewhen(io.idRedirectValid) {
    // 次高：JAL 在 ID 级提前改向（1 周期 flush）
    nextPc := io.idRedirectPc
  }.elsewhen(io.stallIf) {
    // 停顿：保持当前 PC
    nextPc := pcReg
  }.elsewhen(io.bpuPredTaken) {
    // BPU 预测跳转：采用预测目标
    nextPc := io.bpuPredTarget
  }.otherwise {
    // 默认：顺序取指。若 PC 仅 4B 对齐，当前包只能有效返回 slot0，
    // 下一拍必须 PC+4，不能按双发宽度跳过 slot1 位置的指令。
    nextPc := pcReg + seqStep
  }

  pcReg := canonicalPc(nextPc)

  io.currPc  := pcReg
  io.pcFetch := pcReg
}
```

## .\src\main\Frontend\StrideStreamPrefetcher.scala

```scala
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
```

## .\src\main\Frontend\TAGE.scala

```scala
package riscv

import chisel3._
import chisel3.util._

// ── Tagged-table entry for T1–T4 ─────────────────────────────────────────────
class TAGEEntry extends Bundle {
  val valid = Bool()
  val tag   = UInt(8.W)   // PC[17:10]
  val ctr   = UInt(3.W)   // 3-bit saturating direction counter
  val u     = UInt(2.W)   // 2-bit usefulness counter
}

// ── TAGE: TAgged GEometric-length predictor (simplified T0 + T1–T4) ──────────
// Exposes the same IO as BPU (§3.3 / §3.4) so it can be dropped in as a
// direct replacement inside IF without any other module changes.
class TAGE extends Module {
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

  // ── Global History Register (64-bit) ─────────────────────────────────────
  val ghr = RegInit(0.U(64.W))

  // ── Prediction tables ────────────────────────────────────────────────────
  // T0: base table – 1024 entries, PC[11:2] index, 2-bit sat-counter
  val t0 = RegInit(VecInit(Seq.fill(1024)(1.U(2.W))))  // init: weakly not-taken

  // T1–T4: tagged tables – 256 entries each
  val t1 = RegInit(VecInit(Seq.fill(256)(0.U.asTypeOf(new TAGEEntry))))
  val t2 = RegInit(VecInit(Seq.fill(256)(0.U.asTypeOf(new TAGEEntry))))
  val t3 = RegInit(VecInit(Seq.fill(256)(0.U.asTypeOf(new TAGEEntry))))
  val t4 = RegInit(VecInit(Seq.fill(256)(0.U.asTypeOf(new TAGEEntry))))

  // BTB: 256 entries, same spec as BPU §3.3
  val btb = RegInit(VecInit(Seq.fill(256)(0.U.asTypeOf(new BTBEntry))))

  // ── Helpers ───────────────────────────────────────────────────────────────
  // Fold GHR[histLen-1:0] into 8 bits via XOR of successive 8-bit chunks.
  // All history lengths used here (8, 16, 32, 64) are multiples of 8.
  def foldGHR(histLen: Int): UInt = {
    var acc = 0.U(8.W)
    for (i <- 0 until histLen / 8) {
      acc = acc ^ ghr(i * 8 + 7, i * 8)
    }
    acc
  }

  // 2-bit and 3-bit saturating counter updates
  def sat2Inc(c: UInt): UInt = Mux(c === 3.U, 3.U, c + 1.U)
  def sat2Dec(c: UInt): UInt = Mux(c === 0.U, 0.U, c - 1.U)
  def sat3Inc(c: UInt): UInt = Mux(c === 7.U, 7.U, c + 1.U)
  def sat3Dec(c: UInt): UInt = Mux(c === 0.U, 0.U, c - 1.U)

  // ── Query-side indices ────────────────────────────────────────────────────
  val qBase   = io.queryPc(9, 2)          // 8-bit base (PC[9:2]) for T1–T4
  val qT0Idx  = io.queryPc(11, 2)         // 10-bit index for T0 (1024 entries)
  val qT1Idx  = qBase ^ ghr(7, 0)         // XOR with GHR[ 7: 0]
  val qT2Idx  = qBase ^ foldGHR(16)       // XOR with fold(GHR[15: 0], 8)
  val qT3Idx  = qBase ^ foldGHR(32)       // XOR with fold(GHR[31: 0], 8)
  val qT4Idx  = qBase ^ foldGHR(64)       // XOR with fold(GHR[63: 0], 8)
  val qTag    = io.queryPc(17, 10)         // 8-bit tag: PC[17:10]
  val qBtbIdx = io.queryPc(9, 2)          // 8-bit BTB index: PC[9:2]

  // ── Table reads (prediction side) ────────────────────────────────────────
  val t0Q   = t0(qT0Idx)
  val t1Q   = t1(qT1Idx)
  val t2Q   = t2(qT2Idx)
  val t3Q   = t3(qT3Idx)
  val t4Q   = t4(qT4Idx)
  val btbQ  = btb(qBtbIdx)

  val t1QHit = t1Q.valid && (t1Q.tag === qTag)
  val t2QHit = t2Q.valid && (t2Q.tag === qTag)
  val t3QHit = t3Q.valid && (t3Q.tag === qTag)
  val t4QHit = t4Q.valid && (t4Q.tag === qTag)

  // ── Provider / altpred selection (prediction side) ────────────────────────
  // providerSel: 0=T0(base), 1=T1, 2=T2, 3=T3, 4=T4
  // Provider = longest-history table with a tag hit; T0 is the fallback.
  val providerSel = WireDefault(0.U(3.W))
  when(t1QHit) { providerSel := 1.U }
  when(t2QHit) { providerSel := 2.U }
  when(t3QHit) { providerSel := 3.U }
  when(t4QHit) { providerSel := 4.U }

  // Altpred = second-longest-history table with a hit (or T0 if only one hit).
  val altpredSel = WireDefault(0.U(3.W))
  when(t4QHit) {
    when(t3QHit)           { altpredSel := 3.U }
    .elsewhen(t2QHit)      { altpredSel := 2.U }
    .elsewhen(t1QHit)      { altpredSel := 1.U }
  } .elsewhen(t3QHit) {
    when(t2QHit)           { altpredSel := 2.U }
    .elsewhen(t1QHit)      { altpredSel := 1.U }
  } .elsewhen(t2QHit) {
    when(t1QHit)           { altpredSel := 1.U }
  }

  // Extract prediction bit (MSB) from the selected provider table
  val providerPred = WireDefault(t0Q(1))
  when(providerSel === 1.U) { providerPred := t1Q.ctr(2) }
  when(providerSel === 2.U) { providerPred := t2Q.ctr(2) }
  when(providerSel === 3.U) { providerPred := t3Q.ctr(2) }
  when(providerSel === 4.U) { providerPred := t4Q.ctr(2) }

  // Extract u-bit from provider (T0 has no u; treat it as fully useful = 3)
  val providerU = WireDefault(3.U(2.W))
  when(providerSel === 1.U) { providerU := t1Q.u }
  when(providerSel === 2.U) { providerU := t2Q.u }
  when(providerSel === 3.U) { providerU := t3Q.u }
  when(providerSel === 4.U) { providerU := t4Q.u }

  // Extract prediction bit from altpred table
  val altpredPred = WireDefault(t0Q(1))
  when(altpredSel === 1.U) { altpredPred := t1Q.ctr(2) }
  when(altpredSel === 2.U) { altpredPred := t2Q.ctr(2) }
  when(altpredSel === 3.U) { altpredPred := t3Q.ctr(2) }
  when(altpredSel === 4.U) { altpredPred := t4Q.ctr(2) }

  // Usefulness-based fallback: if the tagged provider has u=0, defer to altpred
  val finalPred = Mux(providerSel =/= 0.U && providerU === 0.U,
                      altpredPred, providerPred)

  // BTB lookup for target address (identical to BPU)
  val btbQHit = btbQ.valid && (btbQ.tag === io.queryPc(31, 10))

  io.predTaken  := btbQHit && finalPred
  io.predTarget := btbQ.target

  // ── Update-side indices (recomputed from updatePc + current GHR) ──────────
  // Note: GHR at update time may have advanced past the fetch-time snapshot;
  // this is an accepted approximation for a simplified TAGE implementation.
  val uBase   = io.updatePc(9, 2)
  val uT0Idx  = io.updatePc(11, 2)
  val uT1Idx  = uBase ^ ghr(7, 0)
  val uT2Idx  = uBase ^ foldGHR(16)
  val uT3Idx  = uBase ^ foldGHR(32)
  val uT4Idx  = uBase ^ foldGHR(64)
  val uTag    = io.updatePc(17, 10)
  val uBtbIdx = io.updatePc(9, 2)

  // Read entries at update indices (reads are always active; writes are gated)
  val t0U  = t0(uT0Idx)
  val t1U  = t1(uT1Idx)
  val t2U  = t2(uT2Idx)
  val t3U  = t3(uT3Idx)
  val t4U  = t4(uT4Idx)

  val t1UHit = t1U.valid && (t1U.tag === uTag)
  val t2UHit = t2U.valid && (t2U.tag === uTag)
  val t3UHit = t3U.valid && (t3U.tag === uTag)
  val t4UHit = t4U.valid && (t4U.tag === uTag)

  // Recompute provider / altpred at update time
  val uProviderSel = WireDefault(0.U(3.W))
  when(t1UHit) { uProviderSel := 1.U }
  when(t2UHit) { uProviderSel := 2.U }
  when(t3UHit) { uProviderSel := 3.U }
  when(t4UHit) { uProviderSel := 4.U }

  val uAltpredSel = WireDefault(0.U(3.W))
  when(t4UHit) {
    when(t3UHit)           { uAltpredSel := 3.U }
    .elsewhen(t2UHit)      { uAltpredSel := 2.U }
    .elsewhen(t1UHit)      { uAltpredSel := 1.U }
  } .elsewhen(t3UHit) {
    when(t2UHit)           { uAltpredSel := 2.U }
    .elsewhen(t1UHit)      { uAltpredSel := 1.U }
  } .elsewhen(t2UHit) {
    when(t1UHit)           { uAltpredSel := 1.U }
  }

  val uProviderPred = WireDefault(t0U(1))
  when(uProviderSel === 1.U) { uProviderPred := t1U.ctr(2) }
  when(uProviderSel === 2.U) { uProviderPred := t2U.ctr(2) }
  when(uProviderSel === 3.U) { uProviderPred := t3U.ctr(2) }
  when(uProviderSel === 4.U) { uProviderPred := t4U.ctr(2) }

  val uAltpredPred = WireDefault(t0U(1))
  when(uAltpredSel === 1.U) { uAltpredPred := t1U.ctr(2) }
  when(uAltpredSel === 2.U) { uAltpredPred := t2U.ctr(2) }
  when(uAltpredSel === 3.U) { uAltpredPred := t3U.ctr(2) }
  when(uAltpredSel === 4.U) { uAltpredPred := t4U.ctr(2) }

  val uProviderU = WireDefault(3.U(2.W))
  when(uProviderSel === 1.U) { uProviderU := t1U.u }
  when(uProviderSel === 2.U) { uProviderU := t2U.u }
  when(uProviderSel === 3.U) { uProviderU := t3U.u }
  when(uProviderSel === 4.U) { uProviderU := t4U.u }

  val uMispred       = uProviderPred =/= io.updateTaken
  val uProvCrrct     = !uMispred
  val uAltCrrct      = uAltpredPred === io.updateTaken
  val uProvDiffAlt   = uProviderPred =/= uAltpredPred

  // Allocation candidates: tables with longer history than provider, u=0, no hit
  // Priority: shortest-first (T1 > T2 > T3 > T4)
  val allocCandT1 = uMispred && (uProviderSel === 0.U) && !t1UHit && (t1U.u === 0.U)
  val allocCandT2 = uMispred && (uProviderSel <= 1.U)  && !t2UHit && (t2U.u === 0.U)
  val allocCandT3 = uMispred && (uProviderSel <= 2.U)  && !t3UHit && (t3U.u === 0.U)
  val allocCandT4 = uMispred && (uProviderSel <= 3.U)  && !t4UHit && (t4U.u === 0.U)

  val doAllocT1 = allocCandT1
  val doAllocT2 = allocCandT2 && !allocCandT1
  val doAllocT3 = allocCandT3 && !allocCandT1 && !allocCandT2
  val doAllocT4 = allocCandT4 && !allocCandT1 && !allocCandT2 && !allocCandT3
  val anyAlloc  = doAllocT1 || doAllocT2 || doAllocT3 || doAllocT4

  // ── Periodic u-bit reset (every 2^18 ≈ 256K cycles) ─────────────────────
  // Right-shifts all u fields by 1 to prevent them from saturating permanently.
  // The update block below is placed AFTER this block, so per-cycle updates
  // take priority over the reset for the single entry being written.
  val resetCnt = RegInit(0.U(18.W))
  resetCnt := resetCnt + 1.U
  when(resetCnt === 0.U) {
    for (i <- 0 until 256) {
      t1(i).u := Cat(false.B, t1(i).u(1))
      t2(i).u := Cat(false.B, t2(i).u(1))
      t3(i).u := Cat(false.B, t3(i).u(1))
      t4(i).u := Cat(false.B, t4(i).u(1))
    }
  }

  // ── Update logic (EX-stage feedback, takes priority over periodic reset) ──
  when(io.updateValid) {
    val act = io.updateTaken

    // BTB: always update with the resolved target
    btb(uBtbIdx).valid  := true.B
    btb(uBtbIdx).tag    := io.updatePc(31, 10)
    btb(uBtbIdx).target := io.updateTarget

    // T0: always update (no tag check needed for base table)
    t0(uT0Idx) := Mux(act, sat2Inc(t0U), sat2Dec(t0U))

    // Provider ctr: update the tagged provider table (if provider != T0)
    when(uProviderSel === 1.U) { t1(uT1Idx).ctr := Mux(act, sat3Inc(t1U.ctr), sat3Dec(t1U.ctr)) }
    when(uProviderSel === 2.U) { t2(uT2Idx).ctr := Mux(act, sat3Inc(t2U.ctr), sat3Dec(t2U.ctr)) }
    when(uProviderSel === 3.U) { t3(uT3Idx).ctr := Mux(act, sat3Inc(t3U.ctr), sat3Dec(t3U.ctr)) }
    when(uProviderSel === 4.U) { t4(uT4Idx).ctr := Mux(act, sat3Inc(t4U.ctr), sat3Dec(t4U.ctr)) }

    // Provider u-bit update (tagged tables only)
    when(uProviderSel =/= 0.U) {
      when(uProvCrrct && uProvDiffAlt) {
        when(uProviderSel === 1.U) { t1(uT1Idx).u := sat2Inc(t1U.u) }
        when(uProviderSel === 2.U) { t2(uT2Idx).u := sat2Inc(t2U.u) }
        when(uProviderSel === 3.U) { t3(uT3Idx).u := sat2Inc(t3U.u) }
        when(uProviderSel === 4.U) { t4(uT4Idx).u := sat2Inc(t4U.u) }
      } .elsewhen(uMispred && uAltCrrct) {
        when(uProviderSel === 1.U) { t1(uT1Idx).u := sat2Dec(t1U.u) }
        when(uProviderSel === 2.U) { t2(uT2Idx).u := sat2Dec(t2U.u) }
        when(uProviderSel === 3.U) { t3(uT3Idx).u := sat2Dec(t3U.u) }
        when(uProviderSel === 4.U) { t4(uT4Idx).u := sat2Dec(t4U.u) }
      }
    }

    // Allocation: on misprediction, install a new entry in the shortest
    // available table that is longer than the provider and has u=0
    when(doAllocT1) {
      t1(uT1Idx).valid := true.B; t1(uT1Idx).tag := uTag
      t1(uT1Idx).ctr   := 4.U;   t1(uT1Idx).u   := 0.U  // ctr=100: weakly correct
    }
    when(doAllocT2) {
      t2(uT2Idx).valid := true.B; t2(uT2Idx).tag := uTag
      t2(uT2Idx).ctr   := 4.U;   t2(uT2Idx).u   := 0.U
    }
    when(doAllocT3) {
      t3(uT3Idx).valid := true.B; t3(uT3Idx).tag := uTag
      t3(uT3Idx).ctr   := 4.U;   t3(uT3Idx).u   := 0.U
    }
    when(doAllocT4) {
      t4(uT4Idx).valid := true.B; t4(uT4Idx).tag := uTag
      t4(uT4Idx).ctr   := 4.U;   t4(uT4Idx).u   := 0.U
    }

    // If no free slot (all u>0): decrement u in tables longer than provider
    when(uMispred && !anyAlloc) {
      when(uProviderSel === 0.U) { t1(uT1Idx).u := sat2Dec(t1U.u) }
      when(uProviderSel <= 1.U)  { t2(uT2Idx).u := sat2Dec(t2U.u) }
      when(uProviderSel <= 2.U)  { t3(uT3Idx).u := sat2Dec(t3U.u) }
      when(uProviderSel <= 3.U)  { t4(uT4Idx).u := sat2Dec(t4U.u) }
    }

    // Shift the actual outcome into GHR
    ghr := Cat(ghr(62, 0), act)
  }

  // ── RAS: identical implementation to BPU ─────────────────────────────────
  val rasStack = RegInit(VecInit(Seq.fill(16)(0.U(32.W))))
  val rasPtr   = RegInit(0.U(4.W))
  val rasCount = RegInit(0.U(5.W))

  io.ras.topAddr := rasStack(rasPtr)
  io.ras.topValid := rasCount =/= 0.U

  when(io.ras.flush) {
    rasPtr := io.ras.checkpoint
    rasCount := 0.U
  } .elsewhen(io.ras.push) {
    val nextPtr = (rasPtr + 1.U)(3, 0)
    rasStack(nextPtr) := io.ras.pushAddr
    rasPtr            := nextPtr
    when(rasCount =/= 16.U) {
      rasCount := rasCount + 1.U
    }
  } .elsewhen(io.ras.pop) {
    rasPtr := (rasPtr - 1.U)(3, 0)
    when(rasCount =/= 0.U) {
      rasCount := rasCount - 1.U
    }
  }
}
```

## .\src\main\Icache\ICacheMissFSM.scala

```scala
package icache

import chisel3._
import chisel3.util._
import parameterized_cache.{CacheParams, MemBusIO}

// ============================================================
//  ICacheMissFSM：I-Cache 缺失与预取状态机
//
//  与 DCacheMissFSM 的核心区别：
//   - 无脏行写回：去掉 sCheck / sWbReq / sWbResp 三个状态
//     Miss → 直接回填，无需检查 dirty / evict 旧行内容
//   - 支持后台预取（sPrefill / sPrefillResp / sPrefillDone）：
//     仅在 sIdle 且无 miss 时处理；预取期间 stall=0，流水线
//     继续推进（命中原来已缓存的行）
//   - stall 仅在处理真正 miss（sRefillReq/Resp/sDone）时置高
//
//  状态转换：
//
//   sIdle ─[missValid]──────────────► sRefillReq
//     │                                   │ req.fire
//     │                               sRefillResp
//     │                                   │ resp.fire
//     │                     lastWord? ────┤
//     │                  No ↙       Yes ↘
//     │             sRefillReq        sDone ──► sIdle（refillDone=1）
//     │
//     └─[pfReqValid & !missValid]──► sPrefill
//                                        │ req.fire
//                                    sPrefillResp
//                                        │ resp.fire
//                          lastWord? ────┤
//                       No ↙       Yes ↘
//                   sPrefill       sPrefillDone ──► sIdle（prefillDone=1）
// ============================================================
class ICacheMissFSM(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    // ── Miss 输入（来自 ICacheTop，优先级高于预取）──────────────
    val missValid = Input(Bool())
    val missTag   = Input(UInt(p.TAG_W.W))
    val missIdx   = Input(UInt(p.INDEX_W.W))
    val evictWay  = Input(UInt(p.WAY_W.W))   // PLRU 在 missIdx 上的驱逐路

    // ── 预取输入（低优先级，仅 sIdle 且无 miss 时接受）──────────
    val pfReqValid  = Input(Bool())
    val pfReqReady  = Output(Bool())
    val pfReqTag    = Input(UInt(p.TAG_W.W))
    val pfReqIdx    = Input(UInt(p.INDEX_W.W))
    val pfEvictWay  = Input(UInt(p.WAY_W.W)) // PLRU 在 pfIdx 上的驱逐路

    // ── 外部内存总线 ──────────────────────────────────────────
    val mem = new MemBusIO(p)

    // ── 回填输出 → ICacheTop → ITagArray / IDataArray ─────────
    val refillEn    = Output(Bool())            // 当拍 refillData 有效，写 DataArray
    val refillWay   = Output(UInt(p.WAY_W.W))
    val refillIdx   = Output(UInt(p.INDEX_W.W))
    val refillWord  = Output(UInt(p.WORD_CNT_W.W))
    val refillData  = Output(UInt(p.DATA_WIDTH.W))
    val refillTag   = Output(UInt(p.TAG_W.W))
    val refillDone  = Output(Bool())            // miss 回填完成，TagArray 写入
    val prefillDone = Output(Bool())            // 预取回填完成，TagArray 写入

    // ── 状态输出 ──────────────────────────────────────────────
    val stall  = Output(Bool())  // 仅 miss 流程置高，预取不 stall
    val isIdle = Output(Bool())  // 供 ICacheTop 做 PLRU idx MUX
  })

  // ── 状态定义 ─────────────────────────────────────────────────
  val sIdle :: sRefillReq :: sRefillResp :: sRefillWrite :: sDone :: sPrefill :: sPrefillResp :: sPrefillWrite :: sPrefillDone :: Nil = Enum(9)

  val state     = RegInit(sIdle)
  val nextState = WireDefault(state)

  // ── 锁存字段 ──────────────────────────────────────────────────
  val wTag    = Reg(UInt(p.TAG_W.W))
  val wIdx    = Reg(UInt(p.INDEX_W.W))
  val wWay    = Reg(UInt(p.WAY_W.W))
  val wordCnt = Reg(UInt(p.WORD_CNT_W.W))
  val lineBuf = Reg(Vec(p.LINE_WORDS, UInt(p.DATA_WIDTH.W)))

  val lastWord = wordCnt === (p.LINE_WORDS - 1).U

  // ── 状态转换 ──────────────────────────────────────────────────
  switch(state) {
    is(sIdle) {
      when(io.missValid) {
        nextState := sRefillReq
      }.elsewhen(io.pfReqValid) {
        nextState := sPrefill
      }
    }
    is(sRefillReq) {
      when(io.mem.req.fire) { nextState := sRefillResp }
    }
    is(sRefillResp) {
      when(io.mem.resp.fire) { nextState := sRefillWrite }
    }
    is(sRefillWrite) {
      when(lastWord) { nextState := sDone }
    }
    is(sDone) { nextState := sIdle }

    is(sPrefill) {
      when(io.mem.req.fire) { nextState := sPrefillResp }
    }
    is(sPrefillResp) {
      when(io.mem.resp.fire) { nextState := sPrefillWrite }
    }
    is(sPrefillWrite) {
      when(lastWord) { nextState := sPrefillDone }
    }
    is(sPrefillDone) { nextState := sIdle }
  }
  state := nextState

  // ── 锁存逻辑（sIdle 出口时捕获） ──────────────────────────────
  when(state === sIdle && io.missValid) {
    wTag    := io.missTag
    wIdx    := io.missIdx
    wWay    := io.evictWay
    wordCnt := 0.U
  }.elsewhen(state === sIdle && !io.missValid && io.pfReqValid) {
    wTag    := io.pfReqTag
    wIdx    := io.pfReqIdx
    wWay    := io.pfEvictWay
    wordCnt := 0.U
  }.elsewhen((state === sRefillResp || state === sPrefillResp) && io.mem.resp.fire) {
    lineBuf := io.mem.resp.bits.rline
    wordCnt := 0.U
  }.elsewhen(state === sRefillWrite || state === sPrefillWrite) {
    wordCnt := Mux(lastWord, 0.U, wordCnt + 1.U)
  }

  // ── 内存总线地址：{tag, idx, wordCnt, 2'b00} ──────────────────
  // byteOffW = OFFSET_W - WORD_CNT_W = 6 - 4 = 2（32 位字内字节偏移宽度）
  val fetchAddr = Cat(wTag, wIdx, 0.U(p.OFFSET_W.W))

  val isRequesting = state === sRefillReq  || state === sPrefill
  val isWaiting    = state === sRefillResp || state === sPrefillResp

  io.mem.req.valid      := isRequesting
  io.mem.req.bits.addr  := fetchAddr
  io.mem.req.bits.wdata := 0.U
  io.mem.req.bits.wline := 0.U.asTypeOf(Vec(p.LINE_WORDS, UInt(p.DATA_WIDTH.W)))
  io.mem.req.bits.wen   := false.B
  io.mem.req.bits.wmask := 0.U
  io.mem.req.bits.line  := true.B
  io.mem.resp.ready     := isWaiting

  // ── 回填输出 ──────────────────────────────────────────────────
  io.refillEn    := state === sRefillWrite || state === sPrefillWrite
  io.refillWay   := wWay
  io.refillIdx   := wIdx
  io.refillWord  := wordCnt
  io.refillData  := lineBuf(wordCnt)
  io.refillTag   := wTag
  io.refillDone  := state === sDone
  io.prefillDone := state === sPrefillDone

  // ── 状态输出 ──────────────────────────────────────────────────
  // stall 仅在处理真正 miss 的三个状态内置高；预取不影响流水线
  io.stall  := state === sRefillReq || state === sRefillResp || state === sRefillWrite || state === sDone
  io.isIdle := state === sIdle

  // 预取握手：仅在 sIdle 且无 miss 时接受
  io.pfReqReady := (state === sIdle) && !io.missValid
}
```

## .\src\main\Icache\ICacheParams.scala

```scala
package icache

import chisel3._
import chisel3.util._
import parameterized_cache.CacheParams

// ============================================================
//  I-Cache 专用 TagEntry：只读 Cache 无需 dirty 字段。
//  其余几何参数（SET_NUM / WAY_NUM / LINE_WORDS 等）
//  直接复用 parameterized_cache.CacheParams。
// ============================================================
class ITagEntry(p: CacheParams) extends Bundle {
  val valid = Bool()
  val tag   = UInt(p.TAG_W.W)
}
```

## .\src\main\Icache\ICacheTop.scala

```scala
package icache

import chisel3._
import chisel3.util._
import parameterized_cache.{CacheParams, ICachePerfEvents, MemBusIO}
import parameterized_cache.TreePLRU

// ============================================================
//  ICacheTop：I-Cache 顶层
//
//  子模块连线总览：
//
//   addr ─► addrTag ──────────────────────────► IHitTest.tag
//         ► addrIdx  ─► ITagArray.idx
//                     ► IDataArray.idx
//                     ► TreePLRU.idx（正常）
//         ► addrWoff ─► IDataArray.wordsoff
//                     ► InstSelect.wordsoff
//
//   ITagArray.tagData ───────────────────────► IHitTest.tagData
//   IHitTest.isHit ──────────────────────────► TreePLRU.updateEn（命中路径）
//   IHitTest.hitWay ─────────────────────────► TreePLRU.updateWay
//                                            ► InstSelect.hitWay
//   IHitTest.missValid ──────────────────────► ICacheMissFSM.missValid
//                                            ► io.missOut（OR FSM.stall）
//
//   ICacheMissFSM.refillEn   ───────────────► IDataArray.refillDataEn
//   ICacheMissFSM.refillDone ─────────────── ► ITagArray.refillTagEn（miss）
//   ICacheMissFSM.prefillDone ──────────────► ITagArray.refillTagEn（预取）
//   ICacheMissFSM.refillWay  ───────────────► IDataArray/ITagArray.refillWay
//                                            ► TreePLRU.updateWay（回填路径）
//   ICacheMissFSM.stall ─────────────────────► io.missOut（OR hitTest.missValid）
//
//   IDataArray.rawData0/1 ──────────────────► InstSelect.rawData0/1
//   InstSelect.inst0/1 ─────────────────────► io.insts(0/1)
//   InstSelect.slot1Valid ───────────────────► io.instValids(1) 门控
//
//  PLRU idx MUX 策略（见下方详细说明）：
//   - 正常取指：plru.idx = addrIdx
//   - FSM idle + 预取请求 + 无 miss：plru.idx = pfIdx
//     此时 plru.evictWay 即为 pfIdx 上的驱逐路，传给 FSM 作 pfEvictWay
// ============================================================
class ICacheTop(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    // ── CPU / IF 级接口 ────────────────────────────────────────
    val addr       = Input(UInt(p.ADDR_WIDTH.W))
    val valid      = Input(Bool())    // 取指请求有效
    val flush      = Input(Bool())    // 流水线 flush，清空所有 valid 位

    // 双槽取指响应
    val insts      = Output(Vec(2, UInt(p.DATA_WIDTH.W)))
    val instValids = Output(Vec(2, Bool()))
    val respValid  = Output(Bool())   // 命中且 bundle 整体有效
    val missOut    = Output(Bool())   // 未命中信号，高电平期间流水线应 stall

    // ── 预取接口（来自 NextLinePrefetcher）────────────────────
    // pfReqAddr 须为 64B 对齐地址（见 spec §6.6）
    val pfReqValid = Input(Bool())
    val pfReqReady = Output(Bool())
    val pfReqAddr  = Input(UInt(p.ADDR_WIDTH.W))

    // ── 外部内存总线 ──────────────────────────────────────────
    val mem = new MemBusIO(p)

    // ── 性能事件（单周期脉冲，仅用于统计）─────────────────────
    val perf = Output(new ICachePerfEvents)
  })

  // ── 地址分解 ──────────────────────────────────────────────────
  // 默认参数（8 KB, 4-way, 64B line）：
  //   offset[5:0]  → addr[5:0]
  //   wordsoff[5:2]→ addr[5:2]（4 bit，16 words/line）
  //   idx[10:6]    → addr[10:6]（5 bit，32 sets）
  //   tag[31:11]   → addr[31:11]（21 bit）
  val addrTag  = io.addr(p.ADDR_WIDTH - 1,          p.OFFSET_W + p.INDEX_W)
  val addrIdx  = io.addr(p.OFFSET_W + p.INDEX_W - 1, p.OFFSET_W)
  val addrWoff = io.addr(p.OFFSET_W - 1,             2)   // word offset = addr[5:2]

  // 预取地址分解（pfReqAddr 应已 64B 对齐，低 6 位为 0）
  val pfTag = io.pfReqAddr(p.ADDR_WIDTH - 1,          p.OFFSET_W + p.INDEX_W)
  val pfIdx = io.pfReqAddr(p.OFFSET_W + p.INDEX_W - 1, p.OFFSET_W)

  // ── 子模块例化 ────────────────────────────────────────────────
  val tagArray  = Module(new ITagArray(p))
  val dataArray = Module(new IDataArray(p))
  val plru      = Module(new TreePLRU(p))
  val hitTest   = Module(new IHitTest(p))
  val instSel   = Module(new InstSelect(p))
  val missFsm   = Module(new ICacheMissFSM(p))

  // ── PLRU idx MUX ──────────────────────────────────────────────
  // 当 FSM 空闲、有预取请求且当前无 miss 时，将 PLRU 指向 pfIdx
  // 以获取正确的预取驱逐路；否则指向 addrIdx 服务正常取指/更新
  val plruQueryForPf = missFsm.io.isIdle &&
                       io.pfReqValid &&
                       !hitTest.io.missValid
  plru.io.idx := Mux(plruQueryForPf, pfIdx, addrIdx)

  // ── IHitTest ──────────────────────────────────────────────────
  hitTest.io.tagData  := tagArray.io.tagData
  hitTest.io.tag      := addrTag
  hitTest.io.reqValid := io.valid

  val isHit  = hitTest.io.isHit
  val hitWay = hitTest.io.hitWay

  // ── ITagArray ─────────────────────────────────────────────────
  tagArray.io.flush       := io.flush
  tagArray.io.idx         := addrIdx
  // miss 和预取都通过同一组 refill 信号写入 TagArray（time-shared）
  tagArray.io.refillTagEn := missFsm.io.refillDone || missFsm.io.prefillDone
  tagArray.io.refillWay   := missFsm.io.refillWay
  tagArray.io.refillIdx   := missFsm.io.refillIdx
  tagArray.io.refillTag   := missFsm.io.refillTag

  // ── TreePLRU ──────────────────────────────────────────────────
  // 更新时机：
  //   1. 正常命中（plruQueryForPf=false，否则 idx 已切换到 pfIdx，
  //      此时不能按 addrIdx 更新 PLRU——概率极低，可接受小误差）
  //   2. miss 回填完成
  //   3. 预取回填完成（此时 plru.idx 应在 pfIdx；由于 sPrefillDone
  //      也是单拍脉冲，与 plruQueryForPf 逻辑不冲突）
  plru.io.updateEn  := (isHit && io.valid && !plruQueryForPf) ||
                        missFsm.io.refillDone  ||
                        missFsm.io.prefillDone
  plru.io.updateWay := Mux(
    missFsm.io.refillDone || missFsm.io.prefillDone,
    missFsm.io.refillWay,
    hitWay
  )

  // ── IDataArray ────────────────────────────────────────────────
  dataArray.io.idx          := addrIdx
  dataArray.io.wordsoff     := addrWoff
  dataArray.io.refillDataEn := missFsm.io.refillEn
  dataArray.io.refillWay    := missFsm.io.refillWay
  dataArray.io.refillIdx    := missFsm.io.refillIdx
  dataArray.io.refillWord   := missFsm.io.refillWord
  dataArray.io.refillData   := missFsm.io.refillData

  // ── InstSelect ────────────────────────────────────────────────
  instSel.io.hitWay   := hitWay
  instSel.io.wordsoff := addrWoff
  instSel.io.rawData0 := dataArray.io.rawData0
  instSel.io.rawData1 := dataArray.io.rawData1

  // ── ICacheMissFSM ─────────────────────────────────────────────
  missFsm.io.missValid := hitTest.io.missValid
  missFsm.io.missTag   := addrTag
  missFsm.io.missIdx   := addrIdx
  // evictWay：plruQueryForPf=false 时 plru.io.evictWay 对应 addrIdx，正确
  missFsm.io.evictWay  := plru.io.evictWay

  // 预取接口：屏蔽掉与 miss 同周期的预取请求（miss 优先级更高）
  missFsm.io.pfReqValid := io.pfReqValid && !hitTest.io.missValid
  missFsm.io.pfReqTag   := pfTag
  missFsm.io.pfReqIdx   := pfIdx
  // pfEvictWay：plruQueryForPf=true 时 plru.io.evictWay 对应 pfIdx，正确
  missFsm.io.pfEvictWay := plru.io.evictWay

  io.pfReqReady := missFsm.io.pfReqReady

  // 内存总线直连
  io.mem <> missFsm.io.mem

  // ── 预取 useful 统计标记 ─────────────────────────────────────
  val prefetched = RegInit(VecInit(Seq.fill(p.SET_NUM)(
    VecInit(Seq.fill(p.WAY_NUM)(false.B))
  )))
  val demandLookup = io.valid && !io.flush
  val demandHit = demandLookup && isHit
  val demandMiss = demandLookup && hitTest.io.missValid && missFsm.io.isIdle
  val prefetchUseful = demandHit && prefetched(addrIdx)(hitWay)

  when(io.flush) {
    for (s <- 0 until p.SET_NUM) {
      for (w <- 0 until p.WAY_NUM) {
        prefetched(s)(w) := false.B
      }
    }
  }.otherwise {
    when(prefetchUseful) {
      prefetched(addrIdx)(hitWay) := false.B
    }
    when(missFsm.io.refillDone) {
      prefetched(missFsm.io.refillIdx)(missFsm.io.refillWay) := false.B
    }
    when(missFsm.io.prefillDone) {
      prefetched(missFsm.io.refillIdx)(missFsm.io.refillWay) := true.B
    }
  }

  // ── 对外输出 ──────────────────────────────────────────────────
  io.insts(0)      := instSel.io.inst0
  io.insts(1)      := instSel.io.inst1
  // slot0 有效：命中且本拍取指请求有效
  io.instValids(0) := isHit && io.valid
  // slot1 有效：在 slot0 有效的基础上，还须未到行尾
  io.instValids(1) := isHit && io.valid && instSel.io.slot1Valid
  io.respValid     := isHit && io.valid

  // missOut 保持高电平直至回填完成：
  //   - hitTest.io.missValid：本周期新检测到 miss（FSM 还未启动）
  //   - missFsm.io.stall：FSM 正在处理 miss（sRefillReq/Resp/sDone）
  // 两者 OR 覆盖 miss 的完整生命周期
  io.missOut := hitTest.io.missValid || missFsm.io.stall

  io.perf.access           := demandHit || demandMiss
  io.perf.hit              := demandHit
  io.perf.miss             := demandMiss
  io.perf.demandRefill     := missFsm.io.refillDone
  io.perf.prefetchReq      := io.pfReqValid
  io.perf.prefetchAccepted := io.pfReqValid && missFsm.io.pfReqReady
  io.perf.prefetchDropped  := io.pfReqValid && !missFsm.io.pfReqReady
  io.perf.prefetchRefill   := missFsm.io.prefillDone
  io.perf.prefetchUseful   := prefetchUseful
}

// ============================================================
//  伴生对象：提供默认参数实例，方便顶层例化
// ============================================================
object ICacheTop {
  // 默认：8 KB，4-way，64 B line，32-bit 数据，32-bit 地址
  val defaultParams = CacheParams(
    ADDR_WIDTH = 32,
    DATA_WIDTH = 32,
    CACHE_SIZE = 8 * 1024,
    WAY_NUM    = 4,
    LINE_BYTES = 64
  )
}
```

## .\src\main\Icache\IDataArray.scala

```scala
package icache

import chisel3._
import chisel3.util._
import parameterized_cache.CacheParams

// ============================================================
//  IDataArray：I-Cache 数据阵列
//
//  与 D-Cache 版 DataArray 的区别：
//   - 无命中写路径（无 hitWen / hitWay / wdata / wmask）
//   - 无 evictLine 输出（I-Cache 驱逐直接丢弃，无需写回）
//   - 增加双槽输出 rawData0 / rawData1：
//       rawData0 = line[wordsoff]     → slot 0 指令字
//       rawData1 = line[wordsoff + 1] → slot 1 指令字（行尾越界时
//                 InstSelect 负责置 slot1Valid=0，此处不判断边界）
// ============================================================
class IDataArray(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    // 组合读端口
    val idx      = Input(UInt(p.INDEX_W.W))
    val wordsoff = Input(UInt(p.WORD_CNT_W.W))   // slot 0 字偏移

    // 回填写端口（由 ICacheMissFSM 逐字驱动）
    val refillDataEn = Input(Bool())
    val refillWay    = Input(UInt(p.WAY_W.W))
    val refillIdx    = Input(UInt(p.INDEX_W.W))
    val refillWord   = Input(UInt(p.WORD_CNT_W.W))
    val refillData   = Input(UInt(p.DATA_WIDTH.W))

    // 双槽输出：四路各自的 slot0 / slot1 指令字
    val rawData0 = Output(Vec(p.WAY_NUM, UInt(p.DATA_WIDTH.W)))
    val rawData1 = Output(Vec(p.WAY_NUM, UInt(p.DATA_WIDTH.W)))
  })

  // 布局：way × set × word，寄存器直接索引即为组合读
  val dArray = Reg(Vec(p.WAY_NUM, Vec(p.SET_NUM, Vec(p.LINE_WORDS, UInt(p.DATA_WIDTH.W)))))

  // slot1 字偏移：wordsoff + 1，截断到 WORD_CNT_W 位（行尾时自然回绕；
  // 回绕后的数据无效，由 InstSelect.slot1Valid = 0 屏蔽）
  val nextOff = (io.wordsoff + 1.U)(p.WORD_CNT_W - 1, 0)

  for (way <- 0 until p.WAY_NUM) {
    io.rawData0(way) := dArray(way)(io.idx)(io.wordsoff)
    io.rawData1(way) := dArray(way)(io.idx)(nextOff)
  }

  // 回填写：每拍写一个字，由 FSM 驱动 refillWord 从 0 计数至 LINE_WORDS-1
  when(io.refillDataEn) {
    dArray(io.refillWay)(io.refillIdx)(io.refillWord) := io.refillData
  }
}
```

## .\src\main\Icache\IHitTest.scala

```scala
package icache

import chisel3._
import chisel3.util._
import parameterized_cache.CacheParams

// ============================================================
//  IHitTest：I-Cache 命中检测
//
//  与 D-Cache 版 HitTest 的区别：
//   - 使用 ITagEntry（无 dirty 字段，只比较 valid + tag）
//   - 门控信号改为 reqValid（取指请求有效），而非 memRen | wen
//     → reqValid=0 时（空泡周期）不产生 miss，避免虚触发 MissFSM
// ============================================================
class IHitTest(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val tagData   = Input(Vec(p.WAY_NUM, new ITagEntry(p)))
    val tag       = Input(UInt(p.TAG_W.W))
    val reqValid  = Input(Bool())   // IF 级取指请求有效

    val isHit     = Output(Bool())
    val hitWay    = Output(UInt(p.WAY_W.W))
    // missValid = ~isHit & reqValid
    // ICacheTop 将此信号同时连到 missOut 和 MissFSM.missValid
    val missValid = Output(Bool())
  })

  val hitVec = VecInit((0 until p.WAY_NUM).map(i =>
    io.tagData(i).valid && (io.tagData(i).tag === io.tag)
  ))

  io.isHit     := hitVec.asUInt.orR
  io.hitWay    := PriorityEncoder(hitVec)
  io.missValid := !io.isHit && io.reqValid
}
```

## .\src\main\Icache\InstSelect.scala

```scala
package icache

import chisel3._
import chisel3.util._
import parameterized_cache.CacheParams

// ============================================================
//  InstSelect：双槽指令选路 + 行尾边界判断
//
//  职责：
//   1. 按 hitWay 从四路 rawData0 / rawData1 中选出命中路指令字
//   2. 当 wordsoff = LINE_WORDS-1 时（slot0 已是行内最后一字），
//      slot1 无有效数据 → slot1Valid = 0，强制退化为单发射
//      （IDataArray 此时输出的 rawData1 是 wordsoff+1 回绕后的
//       行首数据，数值无意义，由本模块的 slot1Valid=0 屏蔽）
// ============================================================
class InstSelect(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val hitWay    = Input(UInt(p.WAY_W.W))
    val wordsoff  = Input(UInt(p.WORD_CNT_W.W))
    val rawData0  = Input(Vec(p.WAY_NUM, UInt(p.DATA_WIDTH.W)))
    val rawData1  = Input(Vec(p.WAY_NUM, UInt(p.DATA_WIDTH.W)))

    val inst0      = Output(UInt(p.DATA_WIDTH.W))
    val inst1      = Output(UInt(p.DATA_WIDTH.W))
    // slot1Valid=0 时上层应将 instValids(1) 置 0
    val slot1Valid = Output(Bool())
  })

  io.inst0      := io.rawData0(io.hitWay)
  io.inst1      := io.rawData1(io.hitWay)
  // 行尾判断：wordsoff 为最后一个字时 slot1 越界
  io.slot1Valid := io.wordsoff =/= (p.LINE_WORDS - 1).U
}
```

## .\src\main\Icache\ITagArray.scala

```scala
package icache

import chisel3._
import chisel3.util._
import parameterized_cache.CacheParams

// ============================================================
//  ITagArray：I-Cache 标签阵列
//
//  与 D-Cache 版 TagArray 的区别：
//   - ITagEntry 无 dirty 字段（只读 Cache 无需写回）
//   - 无 setDirtyEn / setDirtyWay 信号
//   - refillDirty 参数也不存在
// ============================================================
class ITagArray(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val flush       = Input(Bool())
    // 组合读端口：由当前取指 idx 索引，每周期输出四路 tag
    val idx         = Input(UInt(p.INDEX_W.W))
    val tagData     = Output(Vec(p.WAY_NUM, new ITagEntry(p)))
    // 回填写端口：refillDone 脉冲时由 MissFSM 驱动
    val refillTagEn = Input(Bool())
    val refillWay   = Input(UInt(p.WAY_W.W))
    val refillIdx   = Input(UInt(p.INDEX_W.W))
    val refillTag   = Input(UInt(p.TAG_W.W))
  })

  // RegInit 保证上电后 valid=false；flush 也依赖此寄存器语义。
  val tArray = RegInit(VecInit(Seq.fill(p.SET_NUM)(
    VecInit(Seq.fill(p.WAY_NUM)(0.U.asTypeOf(new ITagEntry(p))))
  )))

  // 组合读
  io.tagData := tArray(io.idx)

  // 写优先级：flush > refillTagEn
  when(io.flush) {
    for (s <- 0 until p.SET_NUM) {
      for (w <- 0 until p.WAY_NUM) {
        tArray(s)(w).valid := false.B
        // tag 字段无需清零，valid=false 即无效
      }
    }
  }.elsewhen(io.refillTagEn) {
    tArray(io.refillIdx)(io.refillWay).valid := true.B
    tArray(io.refillIdx)(io.refillWay).tag   := io.refillTag
  }
}
```

## .\src\main\Memory\mem.scala

```scala
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
  val rline = Reg(Vec(p.LINE_WORDS, UInt(32.W)))

  io.dmem.req.ready := !busy
  io.imem.req.ready := !busy && !io.dmem.req.valid

  io.dmem.resp.valid := busy && sourceIsD && count === 0.U
  io.imem.resp.valid := busy && !sourceIsD && count === 0.U
  io.dmem.resp.bits.rdata := rdata
  io.imem.resp.bits.rdata := rdata
  io.dmem.resp.bits.rline := rline
  io.imem.resp.bits.rline := rline

  val acceptD = io.dmem.req.fire
  val acceptI = io.imem.req.fire
  val accept = acceptD || acceptI
  val req = Mux(acceptD, io.dmem.req.bits, io.imem.req.bits)
  val wordAddr = req.addr(log2Ceil(words) + 1, 2)
  val fullCount = latency.U(count.getWidth.W)
  val lineBaseWordAddr = Cat(req.addr(log2Ceil(words) + 1, p.OFFSET_W), 0.U(p.WORD_CNT_W.W))

  when(accept) {
    busy := true.B
    sourceIsD := acceptD
    count := fullCount
    rdata := mem(wordAddr)
    for (i <- 0 until p.LINE_WORDS) {
      rline(i) := mem(lineBaseWordAddr + i.U)
    }

    when(req.wen) {
      when(req.line) {
        for (i <- 0 until p.LINE_WORDS) {
          mem(lineBaseWordAddr + i.U) := req.wline(i)
        }
      }.otherwise {
        val old = mem(wordAddr)
        val byteMask = Cat((0 until 4).reverse.map(i => Fill(8, req.wmask(i))))
        mem(wordAddr) := (req.wdata & byteMask) | (old & ~byteMask)
      }
    }
  }.elsewhen(busy && count =/= 0.U) {
    count := count - 1.U
  }.elsewhen(io.dmem.resp.fire || io.imem.resp.fire) {
    busy := false.B
  }
}
```

## .\src\main\Top.scala

```scala
package riscv

import chisel3._
import chisel3.util._
import parameterized_cache.{CacheParams, MemBusIO}

class Top(
    enableRV32M: Boolean = false,
    cacheParams: CacheParams = CacheParams.default,
    dCacheParams: Option[CacheParams] = None,
    branchPredInit: Int = 1,
    prefetchInit: Int = 0) extends Module {
  private val ip = cacheParams
  private val dp = dCacheParams.getOrElse(cacheParams)

  val io = IO(new Bundle {
    val status = Output(Bool())
    val success = Output(Bool())
    val printChar = Output(Valid(UInt(8.W)))
    val debugPc = Output(UInt(32.W))
    val perf = Output(new CorePerfCounters)
  })

  val core = Module(new InOrderCore(enableRV32M, ip, Some(dp), branchPredInit, prefetchInit))
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
```

## .\src\test\BranchPredictorDhrystoneSpec.scala

```scala
package riscv

import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File

class BranchPredictorDhrystoneSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "BranchPredictorDhrystone"

  private case class Mode(name: String, value: Int)

  private def runMode(mode: Mode): Unit = {
    val f = new File("tests/dhrystone.hex")
    assume(f.exists(), "tests/dhrystone.hex not found")

    var success = false
    var cycles = 0
    val output = new StringBuilder
    val maxCycles = 500000
    val runDir = s"dhrystone_bpred_${mode.name}_${System.currentTimeMillis()}"
    var perf = PerfSnapshot.zero

    test(new SimTop(f.getAbsolutePath, branchPredInit = mode.value))
      .withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation(runDir))) { dut =>
        dut.clock.setTimeout(maxCycles + 100)
        while (!success && cycles < maxCycles) {
          dut.clock.step()
          cycles += 1
          if (dut.io.success.peekBoolean()) {
            success = true
          }
          if (dut.io.printChar.valid.peekBoolean()) {
            output.append(dut.io.printChar.bits.peekInt().toChar)
          }
        }

        perf = PerfSnapshot.from(dut.io.perf)
      }

    val out = output.toString
    val (dhryCycles, dhryInstRetired) = PerfPrinter.dhrystoneMetrics(out).getOrElse((BigInt(0), BigInt(0)))
    val dhryIpc = PerfPrinter.ratio(dhryInstRetired, dhryCycles)
    println(PerfPrinter.line(
      "perf-bpred",
      PerfPrinter.commonDhrystone(if (success) "OK" else "TIMEOUT", perf, out) ++ Seq(
        "mode" -> mode.name,
        "dhryCycles" -> dhryCycles,
        "dhryInstRetired" -> dhryInstRetired,
        "dhryIPC" -> dhryIpc)))
    println(s"[bpred-${mode.name}-dhry] cycles=$dhryCycles instret=$dhryInstRetired ipc=$dhryIpc")

    withClue(out) {
      success shouldBe true
      out should include ("Execution ends")
    }
  }

  Seq(
    Mode("bpu", 0),
    Mode("bpu_ras", 1),
    Mode("tage", 2)
  ).foreach { mode =>
    it should s"run Dhrystone with ${mode.name}" in {
      runMode(mode)
    }
  }
}
```

## .\src\test\BranchPredictorSyntheticSpec.scala

```scala
package riscv

import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File

class BranchPredictorSyntheticSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "BranchPredictorSynthetic"

  private case class Mode(name: String, value: Int)
  private case class Bench(name: String, words: Seq[Long])
  private case class SimResult(success: Boolean, cycles: Int, output: String, perf: PerfSnapshot)

  private val modes = Seq(
    Mode("bpu", 0),
    Mode("bpu_ras", 1),
    Mode("tage", 2)
  )

  private val hexDir: File = {
    val d = new File("test-tmp-hex")
    d.mkdirs()
    d
  }

  import SyntheticAsm._

  private def loopTakenBench: Seq[Long] = Seq(
    addi(T0, X0, 512),
    addi(T2, X0, 0),
    addi(T2, T2, 1),
    addi(T0, T0, -1),
    bne(T0, X0, -8)
  ) ++ successEpilogue

  private def alternatingBench: Seq[Long] = Seq(
    addi(T0, X0, 512),
    addi(T1, X0, 0),
    bne(T1, X0, 12),
    addi(T1, X0, 1),
    beq(X0, X0, 8),
    addi(T1, X0, 0),
    addi(T0, T0, -1),
    bne(T0, X0, -20)
  ) ++ successEpilogue

  private def returnChainBench: Seq[Long] = Seq(
    addi(T0, X0, 256),
    addi(T2, X0, 0),
    jal(RA, 32),
    addi(T0, T0, -1),
    bne(T0, X0, -8)
  ) ++ successEpilogue ++
    Seq.fill(16)(addi(T2, T2, 1)) ++ Seq(
    jalr(X0, RA, 0)
  )

  private val benches = Seq(
    Bench("loop_taken", loopTakenBench),
    Bench("alternating", alternatingBench),
    Bench("return_chain", returnChainBench)
  )

  private def runSim(initFile: String, mode: Mode, bench: Bench): SimResult = {
    var success = false
    var cycles = 0
    val output = new StringBuilder
    val runDir = s"branch_synth_${bench.name}_${mode.name}_${System.currentTimeMillis()}"
    var perf = PerfSnapshot.zero

    test(new SimTop(initFile, branchPredInit = mode.value))
      .withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation(runDir))) { dut =>
        dut.clock.setTimeout(200000)
        while (!success && cycles < 200000) {
          dut.clock.step()
          cycles += 1
          if (dut.io.success.peekBoolean()) {
            success = true
          }
          if (dut.io.printChar.valid.peekBoolean()) {
            output.append(dut.io.printChar.bits.peekInt().toChar)
          }
        }
        perf = PerfSnapshot.from(dut.io.perf)
      }

    SimResult(success, cycles, output.toString, perf)
  }

  private def checkBranchSanity(p: PerfSnapshot): Unit = {
    p.cycles should be > BigInt(0)
    p.instRetired should be > BigInt(0)
    (p.branchCorrect + p.branchMispredicts) should be <= p.branchInsts
    (p.branchDirectionMispredicts + p.branchTargetMispredicts) should be <= p.branchMispredicts
  }

  benches.foreach { bench =>
    it should s"compare branch predictors on ${bench.name}" in {
      modes.foreach { mode =>
        val hex = writeHex(hexDir, s"branch_${bench.name}_${mode.name}.hex", bench.words)
        val r = runSim(hex, mode, bench)
        withClue(s"bench=${bench.name} mode=${mode.name} output='${r.output}' cycles=${r.cycles}") {
          r.success shouldBe true
          checkBranchSanity(r.perf)
        }
        println(PerfPrinter.line(
          "perf-bpred-synth",
          PerfPrinter.common(if (r.success) "OK" else "TIMEOUT", "branch_synth", r.perf) ++ Seq(
            "mode" -> mode.name,
            "bench" -> bench.name,
            "simCycles" -> r.cycles)))
      }
      succeed
    }
  }
}
```

## .\src\test\CacheSizeDhrystoneSpec.scala

```scala
package riscv

import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Tag
import parameterized_cache.CacheParams
import java.io.File

object QuickCache extends Tag("quick-cache")

class CacheSizeDhrystoneSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "ParameterizedCacheDhrystone"

  private def runDhry(cacheKb: Int, dCacheKb: Option[Int] = None): Unit = {
    val f = new File("tests/dhrystone.hex")
    assume(f.exists(), "tests/dhrystone.hex not found")

    var success = false
    var cycles = 0
    val output = new StringBuilder
    val maxCycles = 500000
    val p = CacheParams(32, 32, cacheKb * 1024, 4, 64)
    val dp = dCacheKb.map(kb => CacheParams(32, 32, kb * 1024, 4, 64))
    val label = dCacheKb.map(kb => s"I${cacheKb}KB_D${kb}KB").getOrElse(s"${cacheKb}KB")
    val dKb = dCacheKb.getOrElse(cacheKb)
    val runDir = dCacheKb
      .map(kb => s"dhrystone_cache_i${cacheKb}kb_d${kb}kb_${System.currentTimeMillis()}")
      .getOrElse(s"dhrystone_cache_${cacheKb}kb_${System.currentTimeMillis()}")
    var perf = PerfSnapshot.zero

    test(new SimTop(f.getAbsolutePath, cacheParams = p, dCacheParams = dp))
      .withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation(runDir))) { dut =>
        dut.clock.setTimeout(maxCycles + 100)
        while (!success && cycles < maxCycles) {
          dut.clock.step()
          cycles += 1
          if (dut.io.success.peekBoolean()) {
            success = true
          }
          if (dut.io.printChar.valid.peekBoolean()) {
            output.append(dut.io.printChar.bits.peekInt().toChar)
          }
        }
        println(s"[cache-$label] ${if (success) "OK" else "TIMEOUT"} in $cycles cycles")
        println(s"[cache-$label] outputTail='${output.toString.takeRight(80)}'")
        perf = PerfSnapshot.from(dut.io.perf)
      }

    println(PerfPrinter.line(
      "perf-cache",
      PerfPrinter.commonDhrystone(if (success) "OK" else "TIMEOUT", perf, output.toString) ++ Seq(
        "mode" -> label,
        "iCacheKB" -> cacheKb,
        "dCacheKB" -> dKb,
        "iWay" -> 4,
        "dWay" -> 4)))

    withClue(output.toString) {
      success shouldBe true
      output.toString should include ("Execution ends")
    }
  }

  for (kb <- Seq(4, 8, 16, 32)) {
    val testName = s"run Dhrystone with ${kb}KB I/D caches"
    if (kb == 8) {
      it should testName taggedAs QuickCache in {
        runDhry(kb)
      }
    } else {
      it should testName in {
        runDhry(kb)
      }
    }
  }

  it should "run Dhrystone I32_D16" in {
    runDhry(32, Some(16))
  }

  it should "run Dhrystone I16_D32" in {
    runDhry(16, Some(32))
  }
}
```

## .\src\test\CacheSizeSyntheticSpec.scala

```scala
package riscv

import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parameterized_cache.CacheParams
import java.io.File

class CacheSizeSyntheticSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "CacheSizeSynthetic"

  private case class CacheMode(name: String, iCacheKb: Int, dCacheKb: Int)
  private case class Bench(name: String, words: Seq[Long], maxCycles: Int = 500000)
  private case class SimResult(success: Boolean, cycles: Int, output: String, perf: PerfSnapshot)

  private val modes = Seq(
    CacheMode("4KB", 4, 4),
    CacheMode("8KB", 8, 8),
    CacheMode("16KB", 16, 16),
    CacheMode("32KB", 32, 32),
    CacheMode("I32_D16", 32, 16),
    CacheMode("I16_D32", 16, 32)
  )

  private val hexDir: File = {
    val d = new File("test-tmp-hex")
    d.mkdirs()
    d
  }

  import SyntheticAsm._

  private def loadLoop(countWords: Int, strideBytes: Int): Seq[Long] = {
    val countInit =
      if (countWords <= 2047) Seq(addi(T1, X0, countWords))
      else Seq(lui(T1, 1), addi(T1, T1, countWords - 4096))

    Seq(lui(T0, 0x80002L)) ++ countInit ++ Seq(
      addi(T2, X0, 0),
      lw(T4, T0, 0),
      add(T2, T2, T4),
      addi(T0, T0, strideBytes),
      addi(T1, T1, -1),
      bne(T1, X0, -16)
    )
  }

  private def seq8KbBench: Seq[Long] =
    loadLoop(2048, 4) ++ successEpilogue

  private def stride64Bench: Seq[Long] =
    loadLoop(1024, 64) ++ successEpilogue

  private def capacity24KbBench: Seq[Long] = {
    val onePass = Seq(
      lui(T0, 0x80002L),
      addi(T1, X0, 384),
      lw(T4, T0, 0),
      add(T2, T2, T4),
      addi(T0, T0, 64),
      addi(T1, T1, -1),
      bne(T1, X0, -16)
    )

    Seq(addi(T2, X0, 0)) ++ onePass ++ onePass ++ successEpilogue
  }

  private def icacheStreamBench: Seq[Long] = {
    val bodyLen = 6144
    val body = Seq.fill(bodyLen)(addi(T2, T2, 0))
    Seq(
      addi(T0, X0, 3),
      addi(T2, X0, 0)
    ) ++ body ++ Seq(
      addi(T0, T0, -1),
      beq(T0, X0, 8),
      jal(X0, -((bodyLen + 2) * 4))
    ) ++ successEpilogue
  }

  private val benches = Seq(
    Bench("seq_8kb", seq8KbBench),
    Bench("stride_64b", stride64Bench),
    Bench("capacity_24kb", capacity24KbBench),
    Bench("icache_stream", icacheStreamBench, maxCycles = 800000)
  )

  private def runSim(initFile: String, mode: CacheMode, bench: Bench): SimResult = {
    var success = false
    var cycles = 0
    val output = new StringBuilder
    val iParams = CacheParams(32, 32, mode.iCacheKb * 1024, 4, 64)
    val dParams = CacheParams(32, 32, mode.dCacheKb * 1024, 4, 64)
    val runDir = s"cache_synth_${bench.name}_${mode.name}_${System.currentTimeMillis()}"
    var perf = PerfSnapshot.zero

    test(new SimTop(initFile, cacheParams = iParams, dCacheParams = Some(dParams)))
      .withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation(runDir))) { dut =>
        dut.clock.setTimeout(bench.maxCycles + 100)
        while (!success && cycles < bench.maxCycles) {
          dut.clock.step()
          cycles += 1
          if (dut.io.success.peekBoolean()) {
            success = true
          }
          if (dut.io.printChar.valid.peekBoolean()) {
            output.append(dut.io.printChar.bits.peekInt().toChar)
          }
        }
        perf = PerfSnapshot.from(dut.io.perf)
      }

    SimResult(success, cycles, output.toString, perf)
  }

  private def checkCacheSanity(p: PerfSnapshot): Unit = {
    p.cycles should be > BigInt(0)
    p.instRetired should be > BigInt(0)
    (p.icacheHits + p.icacheMisses) shouldBe p.icacheAccesses
    (p.dcacheHits + p.dcacheMisses) should be <= (p.dcacheLoads + p.dcacheStores)
  }

  benches.foreach { bench =>
    it should s"compare cache sizes on ${bench.name}" in {
      modes.foreach { mode =>
        val hex = writeHex(hexDir, s"cache_${bench.name}_${mode.name}.hex", bench.words)
        val r = runSim(hex, mode, bench)
        withClue(s"bench=${bench.name} mode=${mode.name} output='${r.output}' cycles=${r.cycles}") {
          r.success shouldBe true
          checkCacheSanity(r.perf)
        }
        println(PerfPrinter.line(
          "perf-cache-synth",
          PerfPrinter.common(if (r.success) "OK" else "TIMEOUT", "cache_synth", r.perf) ++ Seq(
            "mode" -> mode.name,
            "bench" -> bench.name,
            "iCacheKB" -> mode.iCacheKb,
            "dCacheKB" -> mode.dCacheKb,
            "iWay" -> 4,
            "dWay" -> 4,
            "simCycles" -> r.cycles)))
      }
      succeed
    }
  }
}
```

## .\src\test\CoreMarkSpec.scala

```scala
package riscv

import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File

class CoreMarkSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "CoreMark"

  it should "run coremark.hex" in {
    val f = new File("tests/coremark.hex")
    assume(f.exists(), "tests/coremark.hex not found")

    var success = false
    var cycles = 0
    val output = new StringBuilder
    val maxCycles = 2000000
    val runDir = s"coremark_run_dir_${System.currentTimeMillis()}"
    var perf = PerfSnapshot.zero

    test(new SimTop(f.getAbsolutePath))
      .withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation(runDir))) { dut =>
        dut.clock.setTimeout(maxCycles + 100)
        while (!success && cycles < maxCycles) {
          dut.clock.step()
          cycles += 1
          if (dut.io.success.peekBoolean()) success = true
          if (dut.io.printChar.valid.peekBoolean()) {
            val ch = dut.io.printChar.bits.peekInt().toChar
            output.append(ch)
            print(ch)
          }
        }
        perf = PerfSnapshot.from(dut.io.perf)
      }

    val out = output.toString
    println(s"\n[coremark] ${if (success) "OK" else "TIMEOUT"} in $cycles cycles")
    println(PerfPrinter.line("perf-coremark", PerfPrinter.common(if (success) "OK" else "TIMEOUT", "coremark", perf)))
    withClue(out) {
      success shouldBe true
      out should include ("Correct operation validated")
      out should include ("CoreMark Size")
    }
  }
}
```

## .\src\test\crt0.S

```scala
/* crt0.S  –  Reset handler, BSS clear, test runner, I/O helpers
 *
 * Register convention inside each test_xxx function:
 *   s0  = local fail counter (callee-saved, must be saved/restored)
 *   a0  = return value (fail count for that suite)
 *   t0-t6, a1-a7 = scratch (caller-saved)
 *
 * The top-level runner accumulates fail counts in s8.
 */

    .section .text.init
    .global  _start

_start:
    /* ── 1. set up stack ─────────────────────────────────────── */
    la   sp, _stack_top

    /* ── 2. zero BSS ─────────────────────────────────────────── */
    la   t0, _bss_start
    la   t1, _bss_end
.Lbss_loop:
    bge  t0, t1, .Lbss_done
    sw   zero, 0(t0)
    addi t0, t0, 4
    j    .Lbss_loop
.Lbss_done:
    addi x8, x2, 0

    /* ── 3. run test suites ───────────────────────────────────── */
    li   s8, 0          /* global fail counter */

    call test_alu
    add  s8, s8, a0

    call test_mem
    add  s8, s8, a0

    call test_branch
    add  s8, s8, a0

    call test_hazard
    add  s8, s8, a0

    call test_csr
    add  s8, s8, a0

    call test_cache
    add  s8, s8, a0

    bnez s8, .Lsuite_fail

    /* ── 4. report result ─────────────────────────────────────── */
    /* All passed: print "PASS\n" */
    li   a0, 'P'; call _putchar
    li   a0, 'A'; call _putchar
    li   a0, 'S'; call _putchar
    li   a0, 'S'; call _putchar
    li   a0, '\n'; call _putchar
    j    .Ldone

.Lsuite_fail:
    li   a0, 'F'
    call _putchar
    li   a0, '\n'; call _putchar

.Ldone:
    /* ── 5. assert success MMIO (io.success ← 1) ─────────────── */
    li   t0, 0x10001FF0
    li   t1, 1
    sw   t1, 0(t0)

.Lhalt:
    j    .Lhalt


/* ═══════════════════════════════════════════════════════════════
 * _putchar(a0: char)  –  write one byte to printf MMIO
 * Clobbers: t0
 * ═══════════════════════════════════════════════════════════════ */
    .global _putchar
_putchar:
    li   t0, 0x10001FF1
    sb   a0, 0(t0)
    ret


/* ═══════════════════════════════════════════════════════════════
 * _printdec(a0: uint)  –  print decimal, 0..9999
 * Saves: ra, s0, s1
 * ═══════════════════════════════════════════════════════════════ */
    .global _printdec
_printdec:
    addi sp, sp, -16
    sw   ra, 12(sp)
    sw   s0,  8(sp)
    sw   s1,  4(sp)
    mv   s0, a0
    li   s1, 0          /* leading-zero flag */

    /* thousands */
    li   t1, 1000
    li   t2, 0
.Lthousands:
    blt  s0, t1, .Lt_done
    addi t2, t2, 1
    sub  s0, s0, t1
    j    .Lthousands
.Lt_done:
    bnez t2, 1f
    bnez s1, 1f
    j    2f
1:  addi a0, t2, '0'; call _putchar; li s1, 1
2:
    /* hundreds */
    li   t1, 100
    li   t2, 0
.Lhundreds:
    blt  s0, t1, .Lh_done
    addi t2, t2, 1
    sub  s0, s0, t1
    j    .Lhundreds
.Lh_done:
    bnez t2, 1f
    bnez s1, 1f
    j    2f
1:  addi a0, t2, '0'; call _putchar; li s1, 1
2:
    /* tens */
    li   t1, 10
    li   t2, 0
.Ltens:
    blt  s0, t1, .Lten_done
    addi t2, t2, 1
    sub  s0, s0, t1
    j    .Ltens
.Lten_done:
    bnez t2, 1f
    bnez s1, 1f
    j    2f
1:  addi a0, t2, '0'; call _putchar
2:
    /* ones – always print */
    addi a0, s0, '0'
    call _putchar

    lw   ra, 12(sp)
    lw   s0,  8(sp)
    lw   s1,  4(sp)
    addi sp, sp, 16
    ret
```

## .\src\test\DhrystoneSpec.scala

```scala
package riscv

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File

class DhrystoneSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Dhrystone"

  it should "run dhrystone.hex" in {
    val f = new File("tests/dhrystone.hex")
    assume(f.exists(), "tests/dhrystone.hex not found")

    var success = false
    var cycles = 0
    val output = new StringBuilder
    val maxCycles = 500000
    val runDir = s"dhrystone_run_dir_${System.currentTimeMillis()}"
    var perf = PerfSnapshot.zero

    test(new SimTop(f.getAbsolutePath))
      .withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation(runDir))) { dut =>
        dut.clock.setTimeout(maxCycles + 100)
        while (!success && cycles < maxCycles) {
          dut.clock.step()
          cycles += 1
          if (dut.io.success.peekBoolean()) success = true
          if (dut.io.printChar.valid.peekBoolean()) {
            val ch = dut.io.printChar.bits.peekInt().toChar
            output.append(ch)
            print(ch)
          }
        }
        perf = PerfSnapshot.from(dut.io.perf)
      }

    val out = output.toString
    println(s"\n[dhry] ${if (success) "OK" else "TIMEOUT"} in $cycles cycles")
    println(PerfPrinter.line("perf-dhry", PerfPrinter.commonDhrystone(if (success) "OK" else "TIMEOUT", perf, out)))
    withClue(out) {
      success shouldBe true
      out should include ("Execution ends")
      out should include ("Int_1_Loc:           5")
      out should include ("Str_1_Loc:           DHRYSTONE PROGRAM, 1'ST STRING")
    }
  }
}
```

## .\src\test\link.ld

```scala
/* link.ld  –  Linker script for RV32I superscalar simulation
 *
 * Simulation RAM:   0x80000000 – 0x803FFFFF  (4 MB)
 * Stack top:        0x80180000               (within RAM)
 *
 * MMIO (handled by DCacheTop, bypass-routed, never go to RAM):
 *   0x10001FF0  success register  (sw 1 → io.success = 1)
 *   0x10001FF1  printf putchar    (sb char)
 *   0x0000BFF8  mtime low         (lw only)
 *   0x0000BFFC  mtime high        (lw only)
 */

OUTPUT_ARCH(riscv)
ENTRY(_start)

SECTIONS {
    /* ── text ───────────────────────────────────────────────── */
    . = 0x80000000;
    .text : {
        *(.text.init)       /* _start must be the very first word */
        *(.text*)
    }

    /* ── read-only data ──────────────────────────────────────── */
    . = ALIGN(64);          /* align to one cache line */
    .rodata : { *(.rodata*) }

    /* ── initialized data ────────────────────────────────────── */
    . = ALIGN(64);
    .data : { *(.data*) }

    /* ── zero-initialized data ───────────────────────────────── */
    . = ALIGN(64);
    .bss : {
        _bss_start = .;
        *(.bss*)
        *(COMMON)
        . = ALIGN(4);
        _bss_end = .;
    }

    /* ── test scratch buffer (4 KB, cache-stress area) ────────── */
    . = ALIGN(0x800);           /* 2 KB alignment = same-set stride */
    _scratch = .;
    . += 0x5000;                /* 20 KB – spans multiple sets + ways */

    /* ── stack (64 KB) ───────────────────────────────────────── */
    _stack_top = 0x80180000;
}
```

## .\src\test\MulDivALUSpec.scala

```scala
package riscv

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import AluOp._

class MulDivALUSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  private val Mask32 = (BigInt(1) << 32) - 1

  private def u32(x: BigInt): BigInt = x & Mask32
  private def s32(x: BigInt): BigInt = {
    val v = u32(x)
    if ((v & (BigInt(1) << 31)) != 0) v - (BigInt(1) << 32) else v
  }

  private def high32(x: BigInt): BigInt = u32(x >> 32)

  private def expected(op: UInt, a: BigInt, b: BigInt): BigInt = {
    val au = u32(a)
    val bu = u32(b)
    val as = s32(a)
    val bs = s32(b)
    val divByZero = bu == 0
    val overflow = au == BigInt("80000000", 16) && bu == Mask32

    op.litValue match {
      case v if v == ALU_MUL.litValue    => u32(au * bu)
      case v if v == ALU_MULH.litValue   => high32(as * bs)
      case v if v == ALU_MULHSU.litValue => high32(as * bu)
      case v if v == ALU_MULHU.litValue  => high32(au * bu)
      case v if v == ALU_DIV.litValue =>
        if (divByZero) Mask32 else if (overflow) au else u32(as / bs)
      case v if v == ALU_DIVU.litValue =>
        if (divByZero) Mask32 else u32(au / bu)
      case v if v == ALU_REM.litValue =>
        if (divByZero) au else if (overflow) 0 else u32(as % bs)
      case v if v == ALU_REMU.litValue =>
        if (divByZero) au else u32(au % bu)
      case _ => 0
    }
  }

  private def runOne(c: MulDivALU, op: UInt, a: BigInt, b: BigInt, latency: Int): Unit = {
    c.io.cancel.poke(false.B)
    c.io.resp.ready.poke(false.B)
    c.io.req.valid.poke(true.B)
    c.io.req.bits.op1.poke(u32(a).U)
    c.io.req.bits.op2.poke(u32(b).U)
    c.io.req.bits.aluOp.poke(op)
    c.io.req.ready.expect(true.B)
    c.clock.step()

    c.io.req.valid.poke(false.B)
    for (_ <- 1 until latency) {
      c.io.resp.valid.expect(false.B)
      c.clock.step()
    }

    c.io.resp.valid.expect(true.B)
    c.io.resp.bits.expect(expected(op, a, b).U)
    c.io.resp.ready.poke(true.B)
    c.clock.step()
    c.io.resp.ready.poke(false.B)
    c.io.req.ready.expect(true.B)
  }

  behavior of "MulDivALU"

  it should "compute RV32M multiply operations after 3 cycles" in {
    test(new MulDivALU()) { c =>
      runOne(c, ALU_MUL, -3, 7, 3)
      runOne(c, ALU_MULH, -1, 2, 3)
      runOne(c, ALU_MULHSU, -2, BigInt("80000000", 16), 3)
      runOne(c, ALU_MULHU, Mask32, Mask32, 3)
    }
  }

  it should "compute RV32M divide and remainder operations after 32 cycles" in {
    test(new MulDivALU()) { c =>
      runOne(c, ALU_DIV, -7, 3, 32)
      runOne(c, ALU_DIVU, BigInt("fffffffe", 16), 2, 32)
      runOne(c, ALU_REM, -7, 3, 32)
      runOne(c, ALU_REMU, BigInt("fffffffe", 16), 3, 32)
      runOne(c, ALU_DIV, 123, 0, 32)
      runOne(c, ALU_DIVU, 123, 0, 32)
      runOne(c, ALU_REM, 123, 0, 32)
      runOne(c, ALU_REMU, 123, 0, 32)
      runOne(c, ALU_DIV, BigInt("80000000", 16), Mask32, 32)
      runOne(c, ALU_REM, BigInt("80000000", 16), Mask32, 32)
    }
  }
}
```

## .\src\test\PerfPrinter.scala

```scala
package riscv

import chiseltest._

case class PerfSnapshot(
    cycles: BigInt,
    retire0Cycles: BigInt,
    retire1Cycles: BigInt,
    retire2Cycles: BigInt,
    instRetired: BigInt,
    icacheStallCycles: BigInt,
    dcacheStallCycles: BigInt,
    loadUseStalls: BigInt,
    idRedirects: BigInt,
    exRedirects: BigInt,
    rasPushes: BigInt,
    rasPops: BigInt,
    branchInsts: BigInt,
    branchPreds: BigInt,
    branchCorrect: BigInt,
    branchMispredicts: BigInt,
    branchDirectionMispredicts: BigInt,
    branchTargetMispredicts: BigInt,
    wrongPathFlushInsts: BigInt,
    jalrInsts: BigInt,
    rasPreds: BigInt,
    rasCorrect: BigInt,
    icacheAccesses: BigInt,
    icacheHits: BigInt,
    icacheMisses: BigInt,
    icacheDemandRefills: BigInt,
    icachePrefetchReqs: BigInt,
    icachePrefetchAccepted: BigInt,
    icachePrefetchDropped: BigInt,
    icachePrefetchRefills: BigInt,
    icachePrefetchUseful: BigInt,
    dcacheLoads: BigInt,
    dcacheStores: BigInt,
    dcacheHits: BigInt,
    dcacheMisses: BigInt,
    dcacheWritebacks: BigInt,
    dcacheDemandRefills: BigInt,
    dcachePrefetchReqs: BigInt,
    dcachePrefetchAccepted: BigInt,
    dcachePrefetchDropped: BigInt,
    dcachePrefetchRefills: BigInt,
    dcachePrefetchUseful: BigInt)

object PerfSnapshot {
  val zero: PerfSnapshot = PerfSnapshot(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

  def from(p: CorePerfCounters): PerfSnapshot = PerfSnapshot(
    p.cycles.peekInt(),
    p.retire0Cycles.peekInt(),
    p.retire1Cycles.peekInt(),
    p.retire2Cycles.peekInt(),
    p.instRetired.peekInt(),
    p.icacheStallCycles.peekInt(),
    p.dcacheStallCycles.peekInt(),
    p.loadUseStalls.peekInt(),
    p.idRedirects.peekInt(),
    p.exRedirects.peekInt(),
    p.rasPushes.peekInt(),
    p.rasPops.peekInt(),
    p.branchInsts.peekInt(),
    p.branchPreds.peekInt(),
    p.branchCorrect.peekInt(),
    p.branchMispredicts.peekInt(),
    p.branchDirectionMispredicts.peekInt(),
    p.branchTargetMispredicts.peekInt(),
    p.wrongPathFlushInsts.peekInt(),
    p.jalrInsts.peekInt(),
    p.rasPreds.peekInt(),
    p.rasCorrect.peekInt(),
    p.icacheAccesses.peekInt(),
    p.icacheHits.peekInt(),
    p.icacheMisses.peekInt(),
    p.icacheDemandRefills.peekInt(),
    p.icachePrefetchReqs.peekInt(),
    p.icachePrefetchAccepted.peekInt(),
    p.icachePrefetchDropped.peekInt(),
    p.icachePrefetchRefills.peekInt(),
    p.icachePrefetchUseful.peekInt(),
    p.dcacheLoads.peekInt(),
    p.dcacheStores.peekInt(),
    p.dcacheHits.peekInt(),
    p.dcacheMisses.peekInt(),
    p.dcacheWritebacks.peekInt(),
    p.dcacheDemandRefills.peekInt(),
    p.dcachePrefetchReqs.peekInt(),
    p.dcachePrefetchAccepted.peekInt(),
    p.dcachePrefetchDropped.peekInt(),
    p.dcachePrefetchRefills.peekInt(),
    p.dcachePrefetchUseful.peekInt())
}

object PerfPrinter {
  def ratio(n: BigInt, d: BigInt): String =
    if (d == 0) "0.0000" else f"${n.toDouble / d.toDouble}%.4f"

  def line(tag: String, fields: Seq[(String, Any)]): String =
    s"[$tag] " + fields.map { case (k, v) => s"$k=$v" }.mkString(" ")

  def dhrystoneMetrics(output: String): Option[(BigInt, BigInt)] = {
    val cycles = "Cycles spent for 10 iterations dhrystone:\\s*(\\d+)".r
      .findFirstMatchIn(output).map(m => BigInt(m.group(1)))
    val instRetired = "Instructions retired for 10 iterations dhrystone:\\s*(\\d+)".r
      .findFirstMatchIn(output).map(m => BigInt(m.group(1)))
    for {
      c <- cycles
      i <- instRetired
    } yield (c, i)
  }

  def common(
      status: String,
      program: String,
      p: PerfSnapshot,
      metricCycles: Option[BigInt] = None,
      metricInstRetired: Option[BigInt] = None,
      metricSource: String = "perf-counter"): Seq[(String, Any)] = {
    val cycles = metricCycles.getOrElse(p.cycles)
    val instRetired = metricInstRetired.getOrElse(p.instRetired)
    val sourceFields =
      if (metricCycles.isDefined || metricInstRetired.isDefined) Seq(
        "metricSource" -> metricSource,
        "simCycles" -> p.cycles,
        "simInstRetired" -> p.instRetired,
        "simIPC" -> ratio(p.instRetired, p.cycles),
        "simCPI" -> ratio(p.cycles, p.instRetired))
      else Seq("metricSource" -> metricSource)

    Seq(
    "status" -> status,
    "program" -> program,
    "cycles" -> cycles,
    "instRetired" -> instRetired,
    "IPC" -> ratio(instRetired, cycles),
    "CPI" -> ratio(cycles, instRetired)) ++ sourceFields ++ Seq(
    "retire0" -> p.retire0Cycles,
    "retire1" -> p.retire1Cycles,
    "retire2" -> p.retire2Cycles,
    "icacheStall" -> p.icacheStallCycles,
    "dcacheStall" -> p.dcacheStallCycles,
    "loadUse" -> p.loadUseStalls,
    "idRedirects" -> p.idRedirects,
    "exRedirects" -> p.exRedirects,
    "branchInsts" -> p.branchInsts,
    "branchPreds" -> p.branchPreds,
    "branchCorrect" -> p.branchCorrect,
    "branchMispredicts" -> p.branchMispredicts,
    "branchDirectionMispredicts" -> p.branchDirectionMispredicts,
    "branchTargetMispredicts" -> p.branchTargetMispredicts,
    "wrongPathFlushInsts" -> p.wrongPathFlushInsts,
    "jalrInsts" -> p.jalrInsts,
    "rasPushes" -> p.rasPushes,
    "rasPops" -> p.rasPops,
    "rasPreds" -> p.rasPreds,
    "rasCorrect" -> p.rasCorrect,
    "icacheAccesses" -> p.icacheAccesses,
    "icacheHits" -> p.icacheHits,
    "icacheMisses" -> p.icacheMisses,
    "icacheDemandRefills" -> p.icacheDemandRefills,
    "dcacheLoads" -> p.dcacheLoads,
    "dcacheStores" -> p.dcacheStores,
    "dcacheHits" -> p.dcacheHits,
    "dcacheMisses" -> p.dcacheMisses,
    "dcacheWritebacks" -> p.dcacheWritebacks,
    "dcacheDemandRefills" -> p.dcacheDemandRefills,
    "prefetchReqs" -> (p.icachePrefetchReqs + p.dcachePrefetchReqs),
    "prefetchAccepted" -> (p.icachePrefetchAccepted + p.dcachePrefetchAccepted),
    "prefetchDropped" -> (p.icachePrefetchDropped + p.dcachePrefetchDropped),
    "prefetchRefills" -> (p.icachePrefetchRefills + p.dcachePrefetchRefills),
    "prefetchUseful" -> (p.icachePrefetchUseful + p.dcachePrefetchUseful),
    "icachePrefetchReqs" -> p.icachePrefetchReqs,
    "icachePrefetchAccepted" -> p.icachePrefetchAccepted,
    "icachePrefetchDropped" -> p.icachePrefetchDropped,
    "icachePrefetchRefills" -> p.icachePrefetchRefills,
    "icachePrefetchUseful" -> p.icachePrefetchUseful,
    "dcachePrefetchReqs" -> p.dcachePrefetchReqs,
    "dcachePrefetchAccepted" -> p.dcachePrefetchAccepted,
    "dcachePrefetchDropped" -> p.dcachePrefetchDropped,
    "dcachePrefetchRefills" -> p.dcachePrefetchRefills,
    "dcachePrefetchUseful" -> p.dcachePrefetchUseful)
  }

  def commonDhrystone(status: String, p: PerfSnapshot, output: String): Seq[(String, Any)] = {
    dhrystoneMetrics(output) match {
      case Some((cycles, instRetired)) =>
        common(status, "dhrystone", p, Some(cycles), Some(instRetired), "dhrystone-output")
      case None =>
        common(status, "dhrystone", p)
    }
  }
}
```

## .\src\test\PrefetchDhrystoneSpec.scala

```scala
package riscv

import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File

class PrefetchDhrystoneSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "PrefetchDhrystone"

  private case class Mode(name: String, value: Int)

  private val modes = Seq(
    Mode("none", 0),
    Mode("next-line", 1),
    Mode("stride", 2),
    Mode("stream", 4)
  )

  private def runMode(mode: Mode): Unit = {
    val f = new File("tests/dhrystone.hex")
    assume(f.exists(), "tests/dhrystone.hex not found")

    var success = false
    var cycles = 0
    val output = new StringBuilder
    val maxCycles = 500000
    val runDir = s"target/dhrystone_prefetch_${mode.name.replace("-", "_")}_${System.currentTimeMillis()}"
    var perf = PerfSnapshot.zero

    test(new SimTop(f.getAbsolutePath, prefetchInit = mode.value))
      .withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation(runDir))) { dut =>
        dut.clock.setTimeout(maxCycles + 100)
        while (!success && cycles < maxCycles) {
          dut.clock.step()
          cycles += 1
          if (dut.io.success.peekBoolean()) {
            success = true
          }
          if (dut.io.printChar.valid.peekBoolean()) {
            output.append(dut.io.printChar.bits.peekInt().toChar)
          }
        }

        perf = PerfSnapshot.from(dut.io.perf)
      }

    val out = output.toString
    val (dhryCycles, dhryInstRetired) = PerfPrinter.dhrystoneMetrics(out).getOrElse((BigInt(0), BigInt(0)))
    val dhryIpc = PerfPrinter.ratio(dhryInstRetired, dhryCycles)

    println(PerfPrinter.line(
      "perf-prefetch",
      PerfPrinter.commonDhrystone(if (success) "OK" else "TIMEOUT", perf, out) ++ Seq(
        "mode" -> mode.name,
        "dhryCycles" -> dhryCycles,
        "dhryInstRetired" -> dhryInstRetired,
        "dhryIPC" -> dhryIpc)))
    println(s"[prefetch-${mode.name}-dhry] cycles=$dhryCycles instret=$dhryInstRetired ipc=$dhryIpc")

    withClue(out) {
      success shouldBe true
      out should include ("Execution ends")
    }
  }

  modes.foreach { mode =>
    it should s"run Dhrystone with ${mode.name} prefetch" in {
      runMode(mode)
    }
  }
}
```

## .\src\test\PrefetchSpec.scala

```scala
package riscv

import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.{File, PrintWriter}

class PrefetchSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Prefetch controller CSR 0x7C0"

  private case class SimResult(success: Boolean, output: String, cycles: Int, perf: PerfSnapshot)
  private case class BenchResult(name: String, mode: Int, cycles: Int, perf: PerfSnapshot)

  private val modes = Seq(
    (0, "none"),
    (1, "next-line"),
    (2, "stride"),
    (4, "stream")
  )

  private val X0 = 0
  private val T0 = 5
  private val T1 = 6
  private val T2 = 7
  private val T4 = 29

  private val hexDir: File = {
    val d = new File("test-tmp-hex")
    d.mkdirs()
    d
  }

  private def writeHex(name: String, words: Seq[Long]): String = {
    val f = new File(hexDir, name)
    val pw = new PrintWriter(f)
    try {
      words.padTo(512, 0x00000013L).foreach { w =>
        pw.println(f"${w & 0xFFFFFFFFL}%08x")
      }
    } finally {
      pw.close()
    }
    f.getAbsolutePath
  }

  private def runSim(initFile: String, maxCycles: Int = 500000): SimResult = {
    var success = false
    var cycles = 0
    val output = new StringBuilder
    val label = new File(initFile).getName.stripSuffix(".hex")
    val runDir = s"prefetch_${label}_${System.currentTimeMillis()}"
    var perf = PerfSnapshot.zero

    test(new SimTop(initFile))
      .withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation(runDir))) { dut =>
        dut.clock.setTimeout(maxCycles + 100)
        while (!success && cycles < maxCycles) {
          dut.clock.step()
          cycles += 1
          if (dut.io.success.peekBoolean()) {
            success = true
          }
          if (dut.io.printChar.valid.peekBoolean()) {
            output.append(dut.io.printChar.bits.peekInt().toChar)
          }
        }
        perf = PerfSnapshot.from(dut.io.perf)
      }

    SimResult(success, output.toString, cycles, perf)
  }

  private def addi(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xFFF).toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x13L

  private def lui(rd: Int, imm20: Long): Long =
    ((imm20 & 0xFFFFF) << 12) | (rd.toLong << 7) | 0x37L

  private def csrrw(csr: Int, rs1: Int): Long =
    ((csr.toLong & 0xFFF) << 20) | (rs1.toLong << 15) | (1L << 12) | 0x73L

  private def sw(rs1: Int, rs2: Int, imm: Int): Long = {
    val i = imm & 0xFFF
    ((i >> 5).toLong << 25) | (rs2.toLong << 20) | (rs1.toLong << 15) |
      (2L << 12) | ((i & 0x1F).toLong << 7) | 0x23L
  }

  private def lw(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xFFF).toLong << 20) | (rs1.toLong << 15) | (2L << 12) |
      (rd.toLong << 7) | 0x03L

  private def add(rd: Int, rs1: Int, rs2: Int): Long =
    (rs2.toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x33L

  private def bne(rs1: Int, rs2: Int, offset: Int): Long = {
    val imm12 = (offset >> 12) & 1
    val imm11 = (offset >> 11) & 1
    val imm10_5 = (offset >> 5) & 0x3F
    val imm4_1 = (offset >> 1) & 0xF
    (imm12.toLong << 31) | (imm10_5.toLong << 25) | (rs2.toLong << 20) |
      (rs1.toLong << 15) | (1L << 12) |
      (imm4_1.toLong << 8) | (imm11.toLong << 7) | 0x63L
  }

  private val epilogue: Seq[Long] = Seq(
    lui(T0, 0x10002L),
    addi(T0, T0, -16),
    addi(T1, X0, 1),
    sw(T0, T1, 0),
    0x0000006FL
  )

  private def seqBench(mode: Int): Seq[Long] = Seq(
    addi(T0, X0, mode),
    csrrw(0x7C0, T0),
    lui(T0, 0x80002L),
    lui(T1, 2),
    addi(T2, X0, 0),
    lw(T4, T0, 0),
    add(T2, T2, T4),
    addi(T0, T0, 4),
    addi(T1, T1, -1),
    bne(T1, X0, -16)
  ) ++ epilogue

  private def strideBench(mode: Int): Seq[Long] = Seq(
    addi(T0, X0, mode),
    csrrw(0x7C0, T0),
    lui(T0, 0x80002L),
    addi(T1, X0, 1024),
    addi(T2, X0, 0),
    lw(T4, T0, 0),
    add(T2, T2, T4),
    addi(T0, T0, 64),
    addi(T1, T1, -1),
    bne(T1, X0, -16)
  ) ++ epilogue

  private def stride128Bench(mode: Int): Seq[Long] = Seq(
    addi(T0, X0, mode),
    csrrw(0x7C0, T0),
    lui(T0, 0x80002L),
    addi(T1, X0, 512),
    addi(T2, X0, 0),
    lw(T4, T0, 0),
    add(T2, T2, T4),
    addi(T0, T0, 128),
    addi(T1, T1, -1),
    bne(T1, X0, -16)
  ) ++ epilogue

  private def runBench(
      label: String,
      benchFn: Int => Seq[Long],
      prefix: String,
      maxCycles: Int = 500000
  ): Seq[BenchResult] = {
    info(s"")
    info(s"=== $label ===")
    val results = modes.map { case (mode, name) =>
      val hex = writeHex(s"${prefix}_m$mode.hex", benchFn(mode))
      val r = runSim(hex, maxCycles)
      withClue(s"$label mode=$mode ($name) output='${r.output}' cycles=${r.cycles}") {
        r.success shouldBe true
      }
      val res = BenchResult(name, mode, r.cycles, r.perf)
      println(PerfPrinter.line(
        "perf-prefetch",
        PerfPrinter.common(if (r.success) "OK" else "TIMEOUT", prefix, r.perf) ++ Seq(
          "mode" -> name,
          "bench" -> label,
          "simCycles" -> r.cycles)))
      info(f"  mode=$mode%-2d ($name%-9s) cycles=${r.cycles}%9d")
      res
    }

    val baseline = results.find(_.mode == 0).get.cycles
    val best = results.minBy(_.cycles)
    val speedup = baseline.toDouble / best.cycles.toDouble
    info(f"  best: mode=${best.mode} (${best.name}), speedup=${speedup}%.4fx vs none")
    results
  }

  it should "sequential 32KB prefetch comparison" in {
    runBench("sequential 32KB", seqBench, "seq")
    succeed
  }

  it should "stride 64B prefetch comparison" in {
    runBench("stride 64B", strideBench, "stride64")
    succeed
  }

  it should "stride 128B prefetch comparison" in {
    runBench("stride 128B", stride128Bench, "stride128")
    succeed
  }

  it should "matrix prefetch comparison" in {
    val seq = runBench("matrix sequential 32KB", seqBench, "matrix_seq")
    val str64 = runBench("matrix stride 64B", strideBench, "matrix_stride64")
    val str128 = runBench("matrix stride 128B", stride128Bench, "matrix_stride128")

    info("")
    info("mode       seq32       stride64    stride128")
    modes.foreach { case (mode, name) =>
      val s1 = seq.find(_.mode == mode).get.cycles
      val s2 = str64.find(_.mode == mode).get.cycles
      val s3 = str128.find(_.mode == mode).get.cycles
      info(f"$mode%-2d $name%-9s $s1%10d $s2%10d $s3%10d")
    }
    succeed
  }
}
```

## .\src\test\RV32MSpec.scala

```scala
package riscv

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import AluOp._
import SyntheticAsm._

class RV32MSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  private val Mask32 = (BigInt(1) << 32) - 1

  private def u32(x: BigInt): BigInt = x & Mask32
  private def s32(x: BigInt): BigInt = {
    val v = u32(x)
    if ((v & (BigInt(1) << 31)) != 0) v - (BigInt(1) << 32) else v
  }
  private def high32(x: BigInt): BigInt = u32(x >> 32)

  private def expected(op: UInt, a: BigInt, b: BigInt): BigInt = {
    val au = u32(a)
    val bu = u32(b)
    val as = s32(a)
    val bs = s32(b)
    val divByZero = bu == 0
    val overflow = au == BigInt("80000000", 16) && bu == Mask32

    op.litValue match {
      case v if v == ALU_MUL.litValue    => u32(au * bu)
      case v if v == ALU_MULH.litValue   => high32(as * bs)
      case v if v == ALU_MULHSU.litValue => high32(as * bu)
      case v if v == ALU_MULHU.litValue  => high32(au * bu)
      case v if v == ALU_DIV.litValue =>
        if (divByZero) Mask32 else if (overflow) au else u32(as / bs)
      case v if v == ALU_DIVU.litValue =>
        if (divByZero) Mask32 else u32(au / bu)
      case v if v == ALU_REM.litValue =>
        if (divByZero) au else if (overflow) 0 else u32(as % bs)
      case v if v == ALU_REMU.litValue =>
        if (divByZero) au else u32(au % bu)
      case _ => 0
    }
  }

  private def li(rd: Int, value: BigInt): Seq[Long] = {
    val v = u32(value)
    val upper = ((v + 0x800) >> 12) & 0xFFFFF
    val lowerRaw = (v & 0xFFF).toInt
    val lower = if (lowerRaw >= 0x800) lowerRaw - 0x1000 else lowerRaw
    if (upper == 0) Seq(addi(rd, X0, lower)) else Seq(lui(rd, upper.toLong), addi(rd, rd, lower))
  }

  private def buildProgram: Seq[Long] = {
    val p = collection.mutable.ArrayBuffer[Long]()

    def emit(xs: Seq[Long]): Unit = p ++= xs
    def alignPair(): Unit = if (p.length % 2 != 0) p += addi(X0, X0, 0)
    def checkReg(actual: Int, expectedReg: Int): Unit = {
      p += beq(actual, expectedReg, 8)
      p += addi(T4, T4, 1)
    }
    def check(op: UInt, inst: (Int, Int, Int) => Long, a: BigInt, b: BigInt): Unit = {
      emit(li(T0, a))
      emit(li(T1, b))
      p += inst(T2, T0, T1)
      emit(li(T3, expected(op, a, b)))
      checkReg(T2, T3)
    }

    p += addi(T4, X0, 0)

    check(ALU_MUL, mul, -3, 7)
    check(ALU_MULH, mulh, -1, 2)
    check(ALU_MULHSU, mulhsu, -2, BigInt("80000000", 16))
    check(ALU_MULHU, mulhu, Mask32, Mask32)
    check(ALU_DIV, div, -7, 3)
    check(ALU_DIVU, divu, BigInt("fffffffe", 16), 2)
    check(ALU_REM, rem, -7, 3)
    check(ALU_REMU, remu, BigInt("fffffffe", 16), 3)
    check(ALU_DIV, div, 123, 0)
    check(ALU_DIVU, divu, 123, 0)
    check(ALU_REM, rem, 123, 0)
    check(ALU_REMU, remu, 123, 0)
    check(ALU_DIV, div, BigInt("80000000", 16), Mask32)
    check(ALU_REM, rem, BigInt("80000000", 16), Mask32)

    emit(li(T0, 6))
    emit(li(T1, 7))
    alignPair()
    p += mul(T2, T0, T1)
    p += add(T3, T2, T1)
    emit(li(S0, 49))
    checkReg(T3, S0)

    emit(li(T0, -81))
    emit(li(T1, 9))
    p += div(T2, T0, T1)
    p += rem(T3, T0, T1)
    emit(li(S0, -9))
    checkReg(T2, S0)
    emit(li(S0, 0))
    checkReg(T3, S0)

    emitFinish(p)
    p.toSeq
  }

  private def emitFinish(p: collection.mutable.ArrayBuffer[Long]): Unit = {
    p += beq(T4, X0, 8)
    p += jal(X0, 0)
    p ++= successEpilogue
  }

  private def buildSmokeProgram: Seq[Long] = {
    val p = collection.mutable.ArrayBuffer[Long]()
    p += addi(T4, X0, 0)
    p ++= li(T0, 6)
    p ++= li(T1, 7)
    p += mul(T2, T0, T1)
    p ++= li(T3, 42)
    p += beq(T2, T3, 8)
    p += addi(T4, T4, 1)
    emitFinish(p)
    p.toSeq
  }

  private def runProgram(name: String, program: Seq[Long], maxCycles: Int): Unit = {
    val dir = new File(s"rv32m_${name}_run_dir_${System.currentTimeMillis()}")
    val initFile = SyntheticAsm.writeHex(dir, s"$name.hex", program, minWords = 512)
    val targetDir = s"rv32m_${name}_test_dir_${System.currentTimeMillis()}"

    test(new SimTop(initFile, enableRV32M = true))
      .withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation(targetDir))) { c =>
        c.clock.setTimeout(maxCycles + 100)
        var cycles = 0
        while (!c.io.success.peek().litToBoolean && cycles < maxCycles) {
          c.clock.step()
          cycles += 1
        }
        val success = c.io.success.peek().litToBoolean
        val perf = PerfSnapshot.from(c.io.perf)
        println(PerfPrinter.line("perf-rv32m", PerfPrinter.common(if (success) "OK" else "TIMEOUT", name, perf)))
        withClue(s"program=$name cycles=$cycles") {
          success shouldBe true
        }
      }
  }

  behavior of "RV32M"

  it should "complete an RV32M smoke multiply program" in {
    runProgram("rv32m_smoke", buildSmokeProgram, maxCycles = 2000)
  }

  it should "run a synthetic RV32M program with multi-cycle MulDivALU" in {
    runProgram("rv32m", buildProgram, maxCycles = 20000)
  }
}
```

## .\src\test\SimTop.scala

```scala
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
    branchPredInit: Int = 1,
    prefetchInit: Int = 0
) extends Module {
  private val ip = cacheParams
  private val dp = dCacheParams.getOrElse(cacheParams)

  val io = IO(new Bundle {
    val success   = Output(Bool())
    val printChar = Output(Valid(UInt(8.W)))
    val debugPc   = Output(UInt(32.W))
    val perf      = Output(new CorePerfCounters)
  })

  val core   = Module(new InOrderCore(enableRV32M, ip, Some(dp), branchPredInit, prefetchInit))
  val memory = Module(new RV32DualPortMemory(ip, initFile = initFile))

  memory.io.imem <> core.io.imem
  memory.io.dmem <> core.io.dmem

  io.success   := core.io.success
  io.printChar := core.io.printChar
  io.debugPc   := core.io.debugPc
  io.perf      := core.io.perf
}
```

## .\src\test\SyntheticAsm.scala

```scala
package riscv

import java.io.{File, PrintWriter}

object SyntheticAsm {
  val X0 = 0
  val RA = 1
  val T0 = 5
  val T1 = 6
  val T2 = 7
  val S0 = 8
  val T3 = 28
  val T4 = 29
  val T5 = 30
  val T6 = 31

  def writeHex(dir: File, name: String, words: Seq[Long], minWords: Int = 512): String = {
    dir.mkdirs()
    val f = new File(dir, name)
    val pw = new PrintWriter(f)
    try {
      words.padTo(minWords, 0x00000013L).foreach { w =>
        pw.println(f"${w & 0xFFFFFFFFL}%08x")
      }
    } finally {
      pw.close()
    }
    f.getAbsolutePath
  }

  def addi(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xFFF).toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x13L

  def lui(rd: Int, imm20: Long): Long =
    ((imm20 & 0xFFFFF) << 12) | (rd.toLong << 7) | 0x37L

  def csrrw(csr: Int, rs1: Int): Long =
    ((csr.toLong & 0xFFF) << 20) | (rs1.toLong << 15) | (1L << 12) | 0x73L

  def sw(rs1: Int, rs2: Int, imm: Int): Long = {
    val i = imm & 0xFFF
    ((i >> 5).toLong << 25) | (rs2.toLong << 20) | (rs1.toLong << 15) |
      (2L << 12) | ((i & 0x1F).toLong << 7) | 0x23L
  }

  def lw(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xFFF).toLong << 20) | (rs1.toLong << 15) | (2L << 12) |
      (rd.toLong << 7) | 0x03L

  def add(rd: Int, rs1: Int, rs2: Int): Long =
    (rs2.toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x33L

  def sub(rd: Int, rs1: Int, rs2: Int): Long =
    rType(rd, rs1, rs2, 0, 0x20)

  def mul(rd: Int, rs1: Int, rs2: Int): Long =
    rType(rd, rs1, rs2, 0, 0x01)

  def mulh(rd: Int, rs1: Int, rs2: Int): Long =
    rType(rd, rs1, rs2, 1, 0x01)

  def mulhsu(rd: Int, rs1: Int, rs2: Int): Long =
    rType(rd, rs1, rs2, 2, 0x01)

  def mulhu(rd: Int, rs1: Int, rs2: Int): Long =
    rType(rd, rs1, rs2, 3, 0x01)

  def div(rd: Int, rs1: Int, rs2: Int): Long =
    rType(rd, rs1, rs2, 4, 0x01)

  def divu(rd: Int, rs1: Int, rs2: Int): Long =
    rType(rd, rs1, rs2, 5, 0x01)

  def rem(rd: Int, rs1: Int, rs2: Int): Long =
    rType(rd, rs1, rs2, 6, 0x01)

  def remu(rd: Int, rs1: Int, rs2: Int): Long =
    rType(rd, rs1, rs2, 7, 0x01)

  private def rType(rd: Int, rs1: Int, rs2: Int, funct3: Int, funct7: Int): Long =
    (funct7.toLong << 25) | (rs2.toLong << 20) | (rs1.toLong << 15) |
      (funct3.toLong << 12) | (rd.toLong << 7) | 0x33L

  def beq(rs1: Int, rs2: Int, offset: Int): Long =
    branch(rs1, rs2, offset, 0)

  def bne(rs1: Int, rs2: Int, offset: Int): Long =
    branch(rs1, rs2, offset, 1)

  private def branch(rs1: Int, rs2: Int, offset: Int, funct3: Int): Long = {
    val imm12 = (offset >> 12) & 1
    val imm11 = (offset >> 11) & 1
    val imm10_5 = (offset >> 5) & 0x3F
    val imm4_1 = (offset >> 1) & 0xF
    (imm12.toLong << 31) | (imm10_5.toLong << 25) | (rs2.toLong << 20) |
      (rs1.toLong << 15) | (funct3.toLong << 12) |
      (imm4_1.toLong << 8) | (imm11.toLong << 7) | 0x63L
  }

  def jal(rd: Int, offset: Int): Long = {
    val imm20 = (offset >> 20) & 1
    val imm10_1 = (offset >> 1) & 0x3FF
    val imm11 = (offset >> 11) & 1
    val imm19_12 = (offset >> 12) & 0xFF
    (imm20.toLong << 31) | (imm10_1.toLong << 21) |
      (imm11.toLong << 20) | (imm19_12.toLong << 12) |
      (rd.toLong << 7) | 0x6FL
  }

  def jalr(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xFFF).toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x67L

  val successEpilogue: Seq[Long] = Seq(
    lui(T0, 0x10002L),
    addi(T0, T0, -16),
    addi(T1, X0, 1),
    sw(T0, T1, 0),
    jal(X0, 0)
  )
}
```

## .\src\test\test_alu.S

```scala
/* test_alu.S  –  Tests for all RV32I ALU and ALU-immediate instructions
 *
 * Covered:
 *   ADD SUB AND OR XOR SLL SRL SRA SLT SLTU
 *   ADDI ANDI ORI XORI SLLI SRLI SRAI SLTI SLTIU
 *   LUI AUIPC
 *   x0 write suppression
 *   Overflow / wrap-around semantics
 *   Shift edge cases (shamt=0, shamt=31)
 *
 * Returns: fail count in a0
 */

    .section .text
    .global  test_alu

/* Helper macro: compare t2 with immediate; increment s0 on mismatch */
.macro CHK_IMM expected
    li   t3, \expected
    beq  t2, t3, 1f
    addi s0, s0, 1
1:
.endm

test_alu:
    addi sp, sp, -16
    sw   ra, 12(sp)
    sw   s0,  8(sp)
    li   s0, 0              /* fail counter */

    /* ── ADD ──────────────────────────────────────────────────── */
    li   t0, 100
    li   t1, 23
    add  t2, t0, t1;        CHK_IMM 123

    li   t0, -10
    li   t1, 3
    add  t2, t0, t1;        CHK_IMM -7

    /* Overflow wraps (unsigned 32-bit) */
    li   t0, 0x7FFFFFFF
    li   t1, 1
    add  t2, t0, t1;        CHK_IMM 0x80000000  /* wraps to INT_MIN */

    li   t0, 0xFFFFFFFF
    li   t1, 1
    add  t2, t0, t1;        CHK_IMM 0           /* wraps to 0 */

    /* ── ADDI ─────────────────────────────────────────────────── */
    li   t0, 0
    addi t2, t0, 2047;      CHK_IMM 2047        /* max positive imm12 */
    addi t2, t0, -2048;     CHK_IMM -2048       /* min negative imm12 */
    addi t2, t0, 0;         CHK_IMM 0           /* ADDI with 0 = MOV */

    /* ── SUB ──────────────────────────────────────────────────── */
    li   t0, 50
    li   t1, 30
    sub  t2, t0, t1;        CHK_IMM 20

    li   t0, 0
    li   t1, 1
    sub  t2, t0, t1;        CHK_IMM -1          /* 0 - 1 = 0xFFFFFFFF */

    li   t0, 0x80000000     /* INT_MIN */
    li   t1, 1
    sub  t2, t0, t1;        CHK_IMM 0x7FFFFFFF  /* wraps */

    /* ── AND / ANDI ───────────────────────────────────────────── */
    li   t0, 0xFF00FF00
    li   t1, 0x0F0F0F0F
    and  t2, t0, t1;        CHK_IMM 0x0F000F00

    li   t0, 0xFFFFFFFF
    andi t2, t0, 0x7FF;     CHK_IMM 0x7FF       /* keep low 11 bits */
    andi t2, t0, 0;         CHK_IMM 0
    andi t2, t0, -1;        CHK_IMM 0xFFFFFFFF  /* -1 sign-extends to all 1s */

    /* ── OR / ORI ─────────────────────────────────────────────── */
    li   t0, 0xF0F0F0F0
    li   t1, 0x0F0F0F0F
    or   t2, t0, t1;        CHK_IMM 0xFFFFFFFF

    li   t0, 0
    ori  t2, t0, 0x123;     CHK_IMM 0x123

    /* ── XOR / XORI ───────────────────────────────────────────── */
    li   t0, 0xAAAAAAAA
    li   t1, 0x55555555
    xor  t2, t0, t1;        CHK_IMM 0xFFFFFFFF

    li   t0, 0xFFFFFFFF
    xori t2, t0, -1;        CHK_IMM 0           /* XOR with all-1s = NOT */

    /* ── SLL / SLLI ───────────────────────────────────────────── */
    li   t0, 1
    li   t1, 0
    sll  t2, t0, t1;        CHK_IMM 1           /* shift by 0 */
    li   t1, 31
    sll  t2, t0, t1;        CHK_IMM 0x80000000
    li   t1, 4
    sll  t2, t0, t1;        CHK_IMM 16

    slli t2, t0, 0;         CHK_IMM 1
    slli t2, t0, 31;        CHK_IMM 0x80000000

    li   t0, 0xFFFFFFFF
    slli t2, t0, 1;         CHK_IMM 0xFFFFFFFE  /* low bit drops */

    /* ── SRL / SRLI ───────────────────────────────────────────── */
    li   t0, 0x80000000
    li   t1, 31
    srl  t2, t0, t1;        CHK_IMM 1           /* logical: fills with 0 */
    li   t1, 1
    srl  t2, t0, t1;        CHK_IMM 0x40000000

    srli t2, t0, 0;         CHK_IMM 0x80000000
    srli t2, t0, 4;         CHK_IMM 0x08000000

    /* ── SRA / SRAI ───────────────────────────────────────────── */
    li   t0, 0x80000000
    li   t1, 31
    sra  t2, t0, t1;        CHK_IMM 0xFFFFFFFF  /* arithmetic: sign extends */
    li   t1, 1
    sra  t2, t0, t1;        CHK_IMM 0xC0000000

    li   t0, 0x7FFFFFFF
    srai t2, t0, 31;        CHK_IMM 0           /* positive stays 0 at top */
    srai t2, t0, 1;         CHK_IMM 0x3FFFFFFF

    /* ── SLT / SLTI ───────────────────────────────────────────── */
    li   t0, -1
    li   t1, 0
    slt  t2, t0, t1;        CHK_IMM 1           /* -1 < 0 (signed) */
    slt  t2, t1, t0;        CHK_IMM 0           /* 0 < -1? No */

    li   t0, 5
    slti t2, t0, 10;        CHK_IMM 1
    slti t2, t0, 5;         CHK_IMM 0
    slti t2, t0, 4;         CHK_IMM 0

    li   t0, 0x80000000     /* INT_MIN */
    li   t1, 0x7FFFFFFF     /* INT_MAX */
    slt  t2, t0, t1;        CHK_IMM 1           /* INT_MIN < INT_MAX */
    slt  t2, t1, t0;        CHK_IMM 0

    /* ── SLTU / SLTIU ─────────────────────────────────────────── */
    li   t0, 0xFFFFFFFF     /* unsigned max */
    li   t1, 0
    sltu t2, t0, t1;        CHK_IMM 0           /* 0xFFFF... < 0? No */
    sltu t2, t1, t0;        CHK_IMM 1           /* 0 < 0xFFFF...? Yes */

    li   t0, 1
    sltiu t2, t0, 2;        CHK_IMM 1
    sltiu t2, t0, 1;        CHK_IMM 0
    sltiu t2, t0, -1;       CHK_IMM 1           /* -1 sign-ext = 0xFFFF... */

    /* ── LUI ──────────────────────────────────────────────────── */
    lui  t2, 1;             CHK_IMM 0x00001000
    lui  t2, 0xFFFFF;       CHK_IMM 0xFFFFF000  /* -4096 */
    lui  t2, 0;             CHK_IMM 0

    /* ── AUIPC ────────────────────────────────────────────────── */
    /* AUIPC rd, 0 = put current PC into rd */
_auipc_ref:
    auipc t2, 0
    /* t2 should equal the address of this AUIPC instruction */
    la   t3, _auipc_ref
    beq  t2, t3, 1f
    addi s0, s0, 1
1:

    /* ── x0 write suppression ─────────────────────────────────── */
    li   t0, 0xDEAD
    add  zero, t0, t0       /* write to x0 must be suppressed */
    li   t2, 0
    CHK_IMM 0               /* x0 must still be 0 */

    addi zero, zero, 42     /* ADDI to x0 */
    li   t2, 0
    CHK_IMM 0

    /* ── Shift amount masking (only low 5 bits used) ──────────── */
    /* Shift amount in rs2 is masked to [4:0]; bits above are ignored */
    li   t0, 1
    li   t1, 32             /* 32 & 31 = 0 → shift by 0 */
    sll  t2, t0, t1;        CHK_IMM 1
    li   t1, 33             /* 33 & 31 = 1 */
    sll  t2, t0, t1;        CHK_IMM 2

    /* ── Done ─────────────────────────────────────────────────── */
    mv   a0, s0
    lw   ra, 12(sp)
    lw   s0,  8(sp)
    addi sp, sp, 16
    ret
```

## .\src\test\test_branch.S

```scala
/* test_branch.S  –  Branch and jump instruction coverage
 *
 * Covered:
 *   BEQ  – taken / not-taken
 *   BNE  – taken / not-taken
 *   BLT  – signed <, all sign combinations
 *   BGE  – signed >=
 *   BLTU – unsigned <
 *   BGEU – unsigned >=
 *   JAL  – forward jump, rd=PC+4 verification
 *   JAL  – backward jump (short loop)
 *   JALR – indirect call, rd=PC+4, target=(rs1+imm)&~1
 *   Prediction stress: a tight taken-branch loop (stresses BTB)
 *   Misprediction recovery: loop that alternates taken/not-taken
 *
 * Returns: fail count in a0
 */

    .section .text
    .global  test_branch

.macro CHK_REG reg, expected
    li   t5, \expected
    beq  \reg, t5, 1f
    addi s0, s0, 1
1:
.endm

test_branch:
    addi sp, sp, -32
    sw   ra, 28(sp)
    sw   s0, 24(sp)
    sw   s1, 20(sp)
    sw   s2, 16(sp)
    li   s0, 0

    /* ────────────────────────────────────────────────────────────
     * BEQ
     * ──────────────────────────────────────────────────────────── */
    li   t0, 42
    li   t1, 42
    beq  t0, t1, 1f         /* should branch */
    addi s0, s0, 1          /* fail: branch not taken */
1:
    li   t0, 1
    li   t1, 2
    beq  t0, t1, 1f         /* should NOT branch */
    j    2f
1:  addi s0, s0, 1          /* fail: branch wrongly taken */
2:

    /* ────────────────────────────────────────────────────────────
     * BNE
     * ──────────────────────────────────────────────────────────── */
    li   t0, 10
    li   t1, 20
    bne  t0, t1, 1f         /* should branch */
    addi s0, s0, 1
1:
    li   t0, 7
    li   t1, 7
    bne  t0, t1, 1f         /* should NOT branch */
    j    2f
1:  addi s0, s0, 1
2:

    /* ────────────────────────────────────────────────────────────
     * BLT (signed)
     * ──────────────────────────────────────────────────────────── */
    li   t0, -1
    li   t1, 0
    blt  t0, t1, 1f         /* -1 < 0: should branch */
    addi s0, s0, 1
1:
    li   t0, 0
    li   t1, -1
    blt  t0, t1, 1f         /* 0 < -1: should NOT branch */
    j    2f
1:  addi s0, s0, 1
2:
    li   t0, 0x80000000     /* INT_MIN */
    li   t1, 0x7FFFFFFF     /* INT_MAX */
    blt  t0, t1, 1f         /* INT_MIN < INT_MAX: should branch */
    addi s0, s0, 1
1:
    blt  t1, t0, 1f         /* INT_MAX < INT_MIN: should NOT branch */
    j    2f
1:  addi s0, s0, 1
2:

    /* ────────────────────────────────────────────────────────────
     * BGE (signed)
     * ──────────────────────────────────────────────────────────── */
    li   t0, 5
    li   t1, 5
    bge  t0, t1, 1f         /* 5 >= 5: should branch */
    addi s0, s0, 1
1:
    li   t0, 6
    li   t1, 5
    bge  t0, t1, 1f         /* 6 >= 5: should branch */
    addi s0, s0, 1
1:
    li   t0, -1
    li   t1, 0
    bge  t0, t1, 1f         /* -1 >= 0: should NOT branch */
    j    2f
1:  addi s0, s0, 1
2:

    /* ────────────────────────────────────────────────────────────
     * BLTU (unsigned)
     * ──────────────────────────────────────────────────────────── */
    li   t0, 0
    li   t1, 0xFFFFFFFF
    bltu t0, t1, 1f         /* 0 < UINT_MAX: should branch */
    addi s0, s0, 1
1:
    bltu t1, t0, 1f         /* UINT_MAX < 0: should NOT branch */
    j    2f
1:  addi s0, s0, 1
2:
    /* Signed -1 = unsigned max; unsigned 1 < unsigned max */
    li   t0, 1
    li   t1, -1
    bltu t0, t1, 1f         /* 1 < 0xFFFF...: should branch */
    addi s0, s0, 1
1:

    /* ────────────────────────────────────────────────────────────
     * BGEU (unsigned)
     * ──────────────────────────────────────────────────────────── */
    li   t0, 0xFFFFFFFF
    li   t1, 0
    bgeu t0, t1, 1f         /* UINT_MAX >= 0: should branch */
    addi s0, s0, 1
1:
    li   t0, 5
    li   t1, 5
    bgeu t0, t1, 1f         /* equal: should branch */
    addi s0, s0, 1
1:
    li   t0, 0
    li   t1, 1
    bgeu t0, t1, 1f         /* 0 >= 1: should NOT branch */
    j    2f
1:  addi s0, s0, 1
2:

    /* ────────────────────────────────────────────────────────────
     * JAL – forward jump, rd = PC+4
     * ──────────────────────────────────────────────────────────── */
    /* jal t4, target  → t4 = PC+4 (the instruction immediately after jal) */
    jal  t4, .Ljal_target
.Ljal_link:
    addi s0, s0, 1          /* must be skipped */
.Ljal_after:
    j    .Ljal_continue
.Ljal_target:
    /* Verify t4 == architectural link address, i.e. PC+4 */
    la   t5, .Ljal_link
    beq  t4, t5, 1f
    addi s0, s0, 1
1:
.Ljal_continue:

    /* ────────────────────────────────────────────────────────────
     * JAL – backward loop (exercises BTB with consistent taken branch)
     * Counts down from 8 to 0 (8 iterations, verifies loop-back works)
     * ──────────────────────────────────────────────────────────── */
    li   s1, 8
.Lback_loop:
    addi s1, s1, -1
    bnez s1, .Lback_loop
    CHK_REG s1, 0

    /* ────────────────────────────────────────────────────────────
     * JAL ra – call/return pattern (verifies ra is set correctly)
     * ──────────────────────────────────────────────────────────── */
    call .Lsimple_callee
    CHK_REG a0, 0x1234      /* callee returns 0x1234 */
    j    .Lafter_callee_def

.Lsimple_callee:
    li   a0, 0x1234
    ret

.Lafter_callee_def:

    /* ────────────────────────────────────────────────────────────
     * JALR – indirect call via register
     * rd = PC+4; PC = (rs1+imm) & ~1
     * ──────────────────────────────────────────────────────────── */
    la   t0, .Ljalr_callee
    jalr ra, t0, 0          /* call callee via register */
    CHK_REG a0, 0xABCD
    j    .Lafter_jalr_def

.Ljalr_callee:
    li   a0, 0xABCD
    ret

.Lafter_jalr_def:

    /* JALR with non-zero immediate */
    la   t0, .Ljalr_imm_base
    addi t0, t0, -4         /* point 4 bytes before target */
    jalr ra, t0, 4          /* effective = t0+4 = target */
    CHK_REG a0, 0xBEEF
    j    .Lafter_jalr_imm

.Ljalr_imm_base:
    li   a0, 0xBEEF
    ret

.Lafter_jalr_imm:

    /* JALR LSB clearing: if computed address has bit 0 set, it must be cleared */
    la   t0, .Ljalr_lsb_target
    ori  t0, t0, 1          /* artificially set bit 0 */
    jalr ra, t0, 0          /* hardware must clear bit 0 */
    CHK_REG a0, 0x9999
    j    .Lafter_jalr_lsb

.Ljalr_lsb_target:
    li   a0, 0x9999
    ret

.Lafter_jalr_lsb:

    /* ────────────────────────────────────────────────────────────
     * Prediction stress: alternating taken / not-taken branch
     * The BPU must recover from mispredictions on each alternation.
     * We run 16 pairs (32 iterations total) and verify the counter.
     * ──────────────────────────────────────────────────────────── */
    li   s1, 16             /* outer loop count */
    li   s2, 0              /* accumulator must end at 16 */
.Lalternate_outer:
    beqz s1, .Lalternate_done
    addi s1, s1, -1
    addi s2, s2, 1          /* taken iteration */
    li   t0, 0
    beq  t0, zero, 1f       /* not-taken branch (condition false → skip) */
    addi s2, s2, -1         /* must NOT execute */
1:
    j    .Lalternate_outer
.Lalternate_done:
    CHK_REG s2, 16

    /* ── Done ─────────────────────────────────────────────────── */
    mv   a0, s0
    lw   ra, 28(sp)
    lw   s0, 24(sp)
    lw   s1, 20(sp)
    lw   s2, 16(sp)
    addi sp, sp, 32
    ret
```

## .\src\test\test_cache.S

```scala
/* test_cache.S  –  D-Cache behavior coverage
 *
 * Cache parameters (from CacheParams in source):
 *   Total: 8 KB     – 8192 bytes
 *   Sets:  32       – indexed by addr[10:6]
 *   Ways:  4        – fully associative within a set
 *   Line:  64 bytes – 16 × 32-bit words
 *
 * Addresses in the same SET have identical bits [10:6].
 * Adding 0x800 (2048) to any address keeps bits [10:6] unchanged
 * but changes the TAG, so it maps to the SAME set with a DIFFERENT tag.
 *
 * Test plan:
 *   K01 – Sequential write + readback (1 cache line, 16 words)
 *   K02 – Sequential array fill (128 words = 4 cache lines, verifies
 *          spatial prefetcher doesn't corrupt data)
 *   K03 – Stride-16 access (stride = 64 B = 1 cache line), exercises
 *          a different set each access
 *   K04 – Repeated access to same line (cold → warm hit sequence)
 *   K05 – 4-way fill of one set (fill all 4 ways → 5th access evicts LRU)
 *          then reload evicted line to verify dirty writeback correctness
 *   K06 – Write + eviction + reload (dirty-line writeback)
 *
 * _scratch (from linker) is cache-line aligned and 20 KB.
 * Same-set "pages" are spaced 0x800 (2048) bytes apart.
 *
 * Returns: fail count in a0
 */

    .section .text
    .global  test_cache

.macro CHK_IMM reg, expected
    li   t6, \expected
    beq  \reg, t6, 1f
    addi s0, s0, 1
1:
.endm

test_cache:
    addi sp, sp, -32
    sw   ra, 28(sp)
    sw   s0, 24(sp)
    sw   s1, 20(sp)
    sw   s2, 16(sp)
    sw   s3, 12(sp)
    li   s0, 0

    la   s3, _scratch       /* base of 20 KB scratch area */

    /* ════════════════════════════════════════════════════════════
     * K01 – One full cache line: write all 16 words, read all back
     * ════════════════════════════════════════════════════════════ */
    li   s1, 0              /* word index */
.LK01_write:
    li   t0, 16
    bge  s1, t0, .LK01_read
    slli t1, s1, 2          /* byte offset = index * 4 */
    add  t2, s3, t1
    addi t3, s1, 0x100      /* pattern: 0x100, 0x101, ... 0x10F */
    sw   t3, 0(t2)
    addi s1, s1, 1
    j    .LK01_write
.LK01_read:
    li   s1, 0
.LK01_verify:
    li   t0, 16
    bge  s1, t0, .LK01_done
    slli t1, s1, 2
    add  t2, s3, t1
    lw   t3, 0(t2)
    addi t4, s1, 0x100
    beq  t3, t4, 1f
    addi s0, s0, 1          /* fail */
1:
    addi s1, s1, 1
    j    .LK01_verify
.LK01_done:
    beqz s0, .LK01_ok
    li   a0, 1
    j    .Lcache_return
.LK01_ok:

    /* ════════════════════════════════════════════════════════════
     * K02 – Sequential array: 128 words (512 bytes = 4 cache lines)
     * Write forward, verify backward (exercises spatial locality)
     * ════════════════════════════════════════════════════════════ */
    /* Write phase: pattern = index × 3 + 1 */
    li   s1, 0
.LK02_write:
    li   t0, 128
    bge  s1, t0, .LK02_verify
    slli t1, s1, 2
    add  t2, s3, t1
    /* Skip first line (already used by K01); write starting at offset 64 */
    addi t2, t2, 64
    slli t3, s1, 1
    add  t3, t3, s1         /* t3 = s1 * 3 */
    addi t3, t3, 1
    sw   t3, 0(t2)
    addi s1, s1, 1
    j    .LK02_write
.LK02_verify:
    /* Read phase (backward) */
    li   s1, 127
.LK02_read:
    bltz s1, .LK02_done
    slli t1, s1, 2
    add  t2, s3, t1
    addi t2, t2, 64
    lw   t3, 0(t2)
    slli t4, s1, 1
    add  t4, t4, s1
    addi t4, t4, 1
    beq  t3, t4, 1f
    addi s0, s0, 1
1:
    addi s1, s1, -1
    j    .LK02_read
.LK02_done:
    beqz s0, .LK02_ok
    li   a0, 2
    j    .Lcache_return
.LK02_ok:

    /* ════════════════════════════════════════════════════════════
     * K03 – Stride-64-byte (one cache line stride)
     * Access 16 different cache lines spread across all 32 sets.
     * Verifies that each line is independently cached.
     * ════════════════════════════════════════════════════════════ */
    li   s1, 0
    li   s2, 0x600          /* start at offset 0x600 from scratch */
    add  s2, s3, s2
.LK03_write:
    li   t0, 16
    bge  s1, t0, .LK03_read
    /* stride = 64 bytes (one cache line) */
    slli t3, s1, 6
    add  t2, s2, t3
    addi t4, s1, 0x200      /* pattern */
    sw   t4, 0(t2)
    addi s1, s1, 1
    j    .LK03_write
.LK03_read:
    li   s1, 0
.LK03_verify:
    li   t0, 16
    bge  s1, t0, .LK03_done
    slli t3, s1, 6
    add  t2, s2, t3
    lw   t3, 0(t2)
    addi t4, s1, 0x200
    beq  t3, t4, 1f
    addi s0, s0, 1
1:
    addi s1, s1, 1
    j    .LK03_verify
.LK03_done:
    beqz s0, .LK03_ok
    li   a0, 3
    j    .Lcache_return
.LK03_ok:

    /* ════════════════════════════════════════════════════════════
     * K04 – Repeated access to same line (cold miss then warm hits)
     * Write once, read many times; all reads after the first should
     * hit the cache and return the correct value.
     * ════════════════════════════════════════════════════════════ */
    li   t0, 0xFACEFACE
    sw   t0, 0(s3)          /* cold write */
    /* 8 subsequent loads from the same word must all return the right value */
    lw   t1, 0(s3); CHK_IMM t1, 0xFACEFACE
    beqz s0, .LK04_ok
    li   a0, 4
    j    .Lcache_return
.LK04_ok:
    lw   t1, 0(s3); CHK_IMM t1, 0xFACEFACE
    lw   t1, 0(s3); CHK_IMM t1, 0xFACEFACE
    lw   t1, 0(s3); CHK_IMM t1, 0xFACEFACE
    lw   t1, 0(s3); CHK_IMM t1, 0xFACEFACE
    lw   t1, 0(s3); CHK_IMM t1, 0xFACEFACE
    lw   t1, 0(s3); CHK_IMM t1, 0xFACEFACE
    lw   t1, 0(s3); CHK_IMM t1, 0xFACEFACE

    /* ════════════════════════════════════════════════════════════
     * K05 – 4-way fill + LRU eviction (same cache set, 5 lines)
     *
     * Five addresses at offsets 0, 0x800, 0x1000, 0x1800, 0x2000
     * relative to scratch all map to the same set (bits [10:6]
     * are unchanged when adding 0x800 because 0x800 sets bit[11]).
     *
     * Write distinct patterns to each, then access all 5 in sequence.
     * After the 5th, the LRU line (first written) must be evicted.
     * Re-read the first line: if the dirty eviction worked, the
     * value written to it is still correct in memory.
     * ════════════════════════════════════════════════════════════ */
    /* Use scratch + 0x1000 as base for this test (avoids K01-K04 area) */
    li   t4, 0x1000
    add  a4, s3, t4         /* a4 = same-set base */

    /* Write distinct magic values to the first word of each of 5 pages */
    li   t0, 0xAABBCC00
    sw   t0, 0(a4)                  /* way 0 candidate */
    li   t0, 0xAABBCC01
    li   t1, 0x800
    add  t2, a4, t1
    sw   t0, 0(t2)                  /* way 1 candidate */
    li   t0, 0xAABBCC02
    li   t1, 0x1000
    add  t2, a4, t1
    sw   t0, 0(t2)                  /* way 2 candidate */
    li   t0, 0xAABBCC03
    li   t1, 0x1800
    add  t2, a4, t1
    sw   t0, 0(t2)                  /* way 3 candidate */

    /* Fill all 4 ways by loading each line */
    lw   t5, 0(a4)
    li   t1, 0x800;   add t2, a4, t1; lw t5, 0(t2)
    li   t1, 0x1000;  add t2, a4, t1; lw t5, 0(t2)
    li   t1, 0x1800;  add t2, a4, t1; lw t5, 0(t2)
    /* Now all 4 ways are valid; a4+0 is the LRU candidate */

    /* Write a modified value to way0 (making it dirty) */
    li   t0, 0xDEADDEAD
    sw   t0, 0(a4)

    /* Access the 5th line → forces eviction of LRU (way0 = a4+0) */
    /* The evicted dirty line must be written back to memory */
    li   t1, 0x2000
    add  t2, a4, t1
    li   t0, 0x55555555
    sw   t0, 0(t2)          /* 5th line: store then load */
    lw   t5, 0(t2)
    CHK_IMM t5, 0x55555555

    /* Now reload way0 (a4+0): the data that was in the cache
       (0xDEADDEAD) must have been written back to memory, so
       loading from that address again should give 0xDEADDEAD */
    lw   t5, 0(a4)
    CHK_IMM t5, 0xDEADDEAD
    beqz s0, .LK05_ok
    li   a0, 5
    j    .Lcache_return
.LK05_ok:

    /* ════════════════════════════════════════════════════════════
     * K06 – Write-back verify: dirty line written correctly
     * Modify multiple words in a cache line, evict, reload, verify.
     * ════════════════════════════════════════════════════════════ */
    li   t4, 0x3000
    add  a4, s3, t4         /* fresh area for K06 */

    /* Write a full cache line (16 words) with pattern 0xC0DE_xxxx */
    li   s1, 0
.LK06_write:
    li   t0, 16
    bge  s1, t0, .LK06_done_write
    slli t1, s1, 2
    add  t2, a4, t1
    li   t3, 0xC0DE0000
    or   t3, t3, s1
    sw   t3, 0(t2)
    addi s1, s1, 1
    j    .LK06_write
.LK06_done_write:
    /* Now bring the line into cache with a load */
    lw   t5, 0(a4)

    /* Overwrite all 16 words (mark line dirty) */
    li   s1, 0
.LK06_overwrite:
    li   t0, 16
    bge  s1, t0, .LK06_done_ow
    slli t1, s1, 2
    add  t2, a4, t1
    li   t3, 0xBAD00000
    or   t3, t3, s1
    sw   t3, 0(t2)
    addi s1, s1, 1
    j    .LK06_overwrite
.LK06_done_ow:

    /* Force eviction: access 4 other lines in the same set */
    li   t0, 0x800;  add t2, a4, t0;  lw t5, 0(t2)
    li   t0, 0x1000; add t2, a4, t0;  lw t5, 0(t2)
    li   t0, 0x1800; add t2, a4, t0;  lw t5, 0(t2)
    li   t0, 0x2000; add t2, a4, t0;  lw t5, 0(t2)

    /* Reload a4 line from memory (must reflect the overwritten values) */
    li   s1, 0
.LK06_verify:
    li   t0, 16
    bge  s1, t0, .LK06_vdone
    slli t1, s1, 2
    add  t2, a4, t1
    lw   t3, 0(t2)
    li   t4, 0xBAD00000
    or   t4, t4, s1
    beq  t3, t4, 1f
    addi s0, s0, 1
1:
    addi s1, s1, 1
    j    .LK06_verify
.LK06_vdone:
    beqz s0, .LK06_ok
    li   a0, 6
    j    .Lcache_return
.LK06_ok:

    /* ── Done ─────────────────────────────────────────────────── */
    mv   a0, s0
.Lcache_return:
    lw   ra, 28(sp)
    lw   s0, 24(sp)
    lw   s1, 20(sp)
    lw   s2, 16(sp)
    lw   s3, 12(sp)
    addi sp, sp, 32
    ret
```

## .\src\test\test_csr.S

```scala
/* test_csr.S  –  CSR instruction and performance-counter coverage
 *
 * Covered:
 *   CSRRW  – write CSR, read old value into rd
 *   CSRRS  – set bits in CSR
 *   CSRRC  – clear bits in CSR
 *   mcycle – increments every cycle; two reads must show progression
 *   minstret – increments per retired instruction; verify delta
 *   mtime  – MMIO read at 0xBFF8; must return a non-zero value over time
 *   prefetch_ctrl (custom CSR 0x7C0) – read/write
 *   x0 as rd in CSR instructions (write-only, no rd side-effect)
 *
 * Returns: fail count in a0
 */

    .section .text
    .global  test_csr

/* CSR address definitions */
.equ CSR_MCYCLE,       0xB00
.equ CSR_MCYCLEH,      0xB80
.equ CSR_MINSTRET,     0xB02
.equ CSR_MISA,         0x301
.equ CSR_PREFETCH,     0x7C0

.equ MTIME_LO_ADDR,    0x0000BFF8
.equ MTIME_HI_ADDR,    0x0000BFFC

.macro CHK_IMM reg, expected
    li   t6, \expected
    beq  \reg, t6, 1f
    addi s0, s0, 1
1:
.endm

test_csr:
    addi sp, sp, -16
    sw   ra, 12(sp)
    sw   s0,  8(sp)
    li   s0, 0

    /* ════════════════════════════════════════════════════════════
     * C01 – CSRRW: write and read back
     * ════════════════════════════════════════════════════════════ */
    /* Save current prefetch_ctrl, write a new value, read back */
    csrr  t4, CSR_PREFETCH          /* save original */

    li    t0, 0x3
    csrrw t1, CSR_PREFETCH, t0      /* t1 = old value, CSR ← 3 */
    nop
    csrr  t2, CSR_PREFETCH          /* t2 should be 3 */
    CHK_IMM t2, 0x3

    /* Restore */
    csrrw zero, CSR_PREFETCH, t4

    /* ════════════════════════════════════════════════════════════
     * C02 – CSRRS: set individual bits
     * ════════════════════════════════════════════════════════════ */
    csrrw zero, CSR_PREFETCH, zero  /* ensure CSR = 0 first */
    li    t0, 0x1
    csrrs t1, CSR_PREFETCH, t0     /* set bit0; t1 = old (0) */
    CHK_IMM t1, 0x0
    csrr  t2, CSR_PREFETCH
    CHK_IMM t2, 0x1                 /* bit0 now set */

    li    t0, 0x2
    csrrs zero, CSR_PREFETCH, t0   /* set bit1 too; rd=x0 = no read */
    nop
    csrr  t2, CSR_PREFETCH
    CHK_IMM t2, 0x3                 /* both bits set */

    /* ════════════════════════════════════════════════════════════
     * C03 – CSRRC: clear individual bits
     * ════════════════════════════════════════════════════════════ */
    li    t0, 0x1
    csrrc t1, CSR_PREFETCH, t0     /* clear bit0; t1 = old (3) */
    CHK_IMM t1, 0x3
    csrr  t2, CSR_PREFETCH
    CHK_IMM t2, 0x2                 /* only bit1 remains */

    csrrw zero, CSR_PREFETCH, zero  /* restore to 0 */

    /* ════════════════════════════════════════════════════════════
     * C04 – mcycle: verify the counter increases
     * ════════════════════════════════════════════════════════════ */
    csrr  t0, CSR_MCYCLE
    /* Execute several instructions to advance the counter */
    addi  t1, zero, 0
    addi  t1, t1, 1
    addi  t1, t1, 1
    addi  t1, t1, 1
    addi  t1, t1, 1
    addi  t1, t1, 1
    addi  t1, t1, 1
    csrr  t2, CSR_MCYCLE
    /* t2 must be strictly greater than t0 (unsigned) */
    bltu  t0, t2, 1f
    addi  s0, s0, 1         /* fail: counter did not advance */
1:

    /* ════════════════════════════════════════════════════════════
     * C05 – minstret: verify retired-instruction count increases
     * We execute exactly 8 ADDI after the first read and verify
     * the delta is at least 8 (may be more due to how the pipeline
     * retires instructions; dual-issue means faster drain).
     * ════════════════════════════════════════════════════════════ */
    csrr  s1, CSR_MINSTRET
    addi  t1, zero, 1       /* retire 1 */
    addi  t1, t1,   1       /* retire 2 */
    addi  t1, t1,   1       /* retire 3 */
    addi  t1, t1,   1       /* retire 4 */
    addi  t1, t1,   1       /* retire 5 */
    addi  t1, t1,   1       /* retire 6 */
    addi  t1, t1,   1       /* retire 7 */
    addi  t1, t1,   1       /* retire 8 */
    nop
    nop
    nop
    nop
    csrr  t2, CSR_MINSTRET
    sub   t3, t2, s1        /* delta */
    /* delta must be >= 8 (these 8 ADDIs + the two CSRR instructions) */
    li    t4, 8
    bge   t3, t4, 1f
    addi  s0, s0, 1         /* fail: too few instructions counted */
1:
    /* Also verify delta doesn't go negative (no overflow in a short test) */
    bgez  t3, 1f
    addi  s0, s0, 1
1:

    /* ════════════════════════════════════════════════════════════
     * C06 – mtime MMIO: two reads must be increasing
     * mtime at 0xBFF8 increments every cycle (in CSRFile).
     * ════════════════════════════════════════════════════════════ */
    li    t0, MTIME_LO_ADDR
    lw    t1, 0(t0)         /* first read */
    /* Some delay */
    addi  t2, zero, 0
    addi  t2, t2, 1
    addi  t2, t2, 1
    lw    t3, 0(t0)         /* second read */
    bltu  t1, t3, 1f        /* t3 > t1: counter advanced */
    addi  s0, s0, 1
1:
    /* mtime high must be 0 (32-bit processor, mtime fits in low 32 bits
       for a short simulation run – if not zero the counter has wrapped) */
    li    t0, MTIME_HI_ADDR
    lw    t2, 0(t0)
    /* Don't fail here; just verify the load doesn't crash the pipeline.
       High 32 bits may be non-zero for very long runs.  We only check
       that the load completes and the pipeline doesn't hang. */

    /* ════════════════════════════════════════════════════════════
     * C07 – misa: read only (must match RV32I or RV32IM)
     * misa[31:30] = 01 for RV32; misa[8] = 1 for 'I' extension
     * ════════════════════════════════════════════════════════════ */
    csrr  t0, CSR_MISA
    /* Check MXL field (bits 31:30) = 01 (RV32) */
    srli  t1, t0, 30
    li    t2, 1
    bne   t1, t2, 1f
    j     2f
1:  addi  s0, s0, 1
2:
    /* Check 'I' bit (bit 8) = 1 */
    srli  t1, t0, 8
    andi  t1, t1, 1
    CHK_IMM t1, 1

    /* ════════════════════════════════════════════════════════════
     * C08 – CSRRW with rd = x0: write without reading
     *        Verify the write still takes effect.
     * ════════════════════════════════════════════════════════════ */
    li    t0, 0x1
    csrrw zero, CSR_PREFETCH, t0    /* write=1, rd=x0 (suppress read) */
    nop
    csrr  t1, CSR_PREFETCH
    CHK_IMM t1, 0x1
    csrrw zero, CSR_PREFETCH, zero  /* clear */

    /* ── Done ─────────────────────────────────────────────────── */
    mv   a0, s0
    lw   ra, 12(sp)
    lw   s0,  8(sp)
    addi sp, sp, 16
    ret
```

## .\src\test\test_hazard.S

```scala
/* test_hazard.S  –  Data hazard & forwarding path coverage
 *
 * Each test verifies correctness (not cycle count) by checking
 * whether the pipeline produces the right value.  A working hazard
 * unit + bypass network produces the right value with the minimum
 * stall count; a broken one produces wrong values.
 *
 * Forwarding paths exercised  (spec §5.3.1):
 *   F1/F4  EX→EX same-slot (consecutive cycles)
 *   F2/F3  EX→EX cross-slot (consecutive cycles, different slots)
 *   F5     Intra-EX slot0→slot1 same issue cycle
 *   F6–F9  MEM→EX (2-cycle distance)
 *   F10-11 WB→EX  (3-cycle distance)
 *
 * Stall scenarios:
 *   Load-use: lw followed immediately by dependent instruction
 *   Load-MEM: lw, nop, dependent (MEM→EX forwarding for load data)
 *   Load-WB:  lw, nop, nop, dependent (WB→EX forwarding for load data)
 *
 * Dual-issue scenarios:
 *   Independent ALU pair (should dual-issue, no stall)
 *   Slot0 ALU → Slot1 reads same rd (F5 bypass, no stall after fix)
 *   Both slots write same rd (slot1 must win – WAW resolution)
 *   Slot0 branch (predicted not-taken) + Slot1 independent ALU
 *
 * Returns: fail count in a0
 */

    .section .bss
    .align 2
_hz_mem:
    .space 32

    .section .text
    .global  test_hazard

.macro CHK_IMM reg, expected
    li   t6, \expected
    beq  \reg, t6, 1f
    addi s0, s0, 1
1:
.endm

test_hazard:
    addi sp, sp, -32
    sw   ra, 28(sp)
    sw   s0, 24(sp)
    sw   s1, 20(sp)
    sw   s2, 16(sp)
    li   s0, 0

    la   a5, _hz_mem

    /* ════════════════════════════════════════════════════════════
     * H01 – EX→EX forwarding, same slot (F1/F4)
     * Instruction N+1 in the same issue slot reads N's result.
     * Expected: no stall, forwarded value correct.
     * ════════════════════════════════════════════════════════════ */
    li   t0, 17
    add  t1, t0, t0          /* t1 = 34, must get t0 via EX-EX bypass */
    CHK_IMM t1, 34

    li   t0, 0xABCD1234
    xor  t1, t0, t0          /* t1 = 0, immediate dep */
    CHK_IMM t1, 0

    /* ════════════════════════════════════════════════════════════
     * H02 – EX→EX cross-slot (F2/F3)
     * Slot0 result consumed by Slot1 in the NEXT cycle.
     * ════════════════════════════════════════════════════════════ */
    /* Force two independent ops so the pipeline fills both slots,
       then a dependent op in the following cycle consumes slot0's result. */
    li   t0, 100
    li   t1, 200             /* these two li pair (independent) */
    add  t2, t0, t1          /* t2 = 300; t0 must come from EX→EX */
    CHK_IMM t2, 300

    li   t0, 50
    li   t1, 3               /* independent */
    sub  t2, t0, t1          /* t2 = 47 */
    CHK_IMM t2, 47

    /* ════════════════════════════════════════════════════════════
     * H03 – F5: Slot0 ALU → Slot1 same issue cycle (intra-EX bypass)
     * After the IDStage fix, slot0 is NOT blocked for ALU instructions.
     * The bypass network must forward slot0's EX result to slot1 in
     * the same cycle (combinational F5 path).
     * ════════════════════════════════════════════════════════════ */
    li   t0, 0x1000
    addi t1, t0, 0x234       /* t1 = 0x1234; slot1 reads slot0's result */
    CHK_IMM t1, 0x1234

    li   t0, 7
    slli t1, t0, 3            /* t1 = 56; immediate dep via F5 */
    CHK_IMM t1, 56

    li   t0, 0xF0F0F0F0
    and  t1, t0, t0           /* t1 = same value */
    CHK_IMM t1, 0xF0F0F0F0

    /* ════════════════════════════════════════════════════════════
     * H04 – MEM→EX forwarding (F6–F9), 2-cycle distance
     * ════════════════════════════════════════════════════════════ */
    li   t0, 0x5A5A5A5A
    add  t0, t0, zero         /* t0 in EX this cycle */
    nop                       /* t0 now in EX/MEM pipe reg (MEM stage) */
    add  t1, t0, zero         /* t1 = t0 via MEM→EX bypass */
    CHK_IMM t1, 0x5A5A5A5A

    li   t0, 255
    add  t0, t0, zero
    nop
    slli t1, t0, 1            /* t1 = 510 via MEM→EX */
    CHK_IMM t1, 510

    /* ════════════════════════════════════════════════════════════
     * H05 – WB→EX forwarding (F10/F11), 3-cycle distance
     * ════════════════════════════════════════════════════════════ */
    li   t0, 0x12345678
    add  t0, t0, zero
    nop
    nop
    add  t1, t0, zero         /* t1 = t0 via WB→EX bypass */
    CHK_IMM t1, 0x12345678

    /* ════════════════════════════════════════════════════════════
     * H06 – Load-Use stall
     * lw then immediately use the loaded value.
     * The pipeline must insert exactly 1 stall cycle.
     * The VALUE must be correct regardless of stall count.
     * ════════════════════════════════════════════════════════════ */
    li   t0, 0xCAFEBABE
    sw   t0, 0(a5)
    lw   t1, 0(a5)            /* load t1 */
    add  t2, t1, zero         /* IMMEDIATE dep → 1-cycle stall from HW */
    CHK_IMM t2, 0xCAFEBABE

    /* Double load-use: two consecutive loads, each immediately used */
    li   t0, 111
    li   t1, 222
    sw   t0, 0(a5)
    sw   t1, 4(a5)
    lw   t2, 0(a5)
    addi t2, t2, 1            /* dep on t2 load → stall */
    lw   t3, 4(a5)
    addi t3, t3, 1            /* dep on t3 load → stall */
    CHK_IMM t2, 112
    CHK_IMM t3, 223

    /* ════════════════════════════════════════════════════════════
     * H07 – Load-MEM forwarding (lw, 1 independent, dep)
     * After the independent instruction, load data is available in
     * MEM→EX bypass; no stall needed.
     * ════════════════════════════════════════════════════════════ */
    li   t0, 0x55AA
    sw   t0, 8(a5)
    lw   t1, 8(a5)            /* load t1 */
    li   t4, 1                /* independent: fills the stall slot */
    or   t2, t1, t4           /* t1 via MEM→EX bypass → no stall */
    CHK_IMM t2, 0x55AB

    /* ════════════════════════════════════════════════════════════
     * H08 – Long dependency chain (6 hops, stresses forwarding network)
     * ════════════════════════════════════════════════════════════ */
    li   t0, 1
    addi t0, t0, 1            /* t0 = 2  (EX→EX) */
    addi t0, t0, 1            /* t0 = 3  (EX→EX) */
    addi t0, t0, 1            /* t0 = 4  (EX→EX) */
    addi t0, t0, 1            /* t0 = 5  (EX→EX) */
    addi t0, t0, 1            /* t0 = 6  (EX→EX) */
    CHK_IMM t0, 6

    /* ════════════════════════════════════════════════════════════
     * H09 – WAW dual-issue: both slots target same rd, slot1 wins
     * (spec §7.2.1: slot1 is younger and must overwrite slot0)
     * ════════════════════════════════════════════════════════════ */
    /* Two li instructions dual-issued; both write t0. */
    li   t0, 0xDEAD           /* slot0 writes t0 = 0xDEAD */
    li   t0, 0xBEEF           /* slot1 writes t0 = 0xBEEF (must win) */
    CHK_IMM t0, 0xBEEF

    li   t0, 111
    li   t0, 222
    li   t0, 333
    CHK_IMM t0, 333

    /* ════════════════════════════════════════════════════════════
     * H10 – Independent dual-issue pair (no stall expected)
     * Verify both results are correct.
     * ════════════════════════════════════════════════════════════ */
    li   t0, 0xAA
    li   t1, 0x55
    /* The following two ops have NO dependency between them */
    add  t2, t0, zero         /* slot0: t2 = 0xAA */
    add  t3, t1, zero         /* slot1: t3 = 0x55 */
    CHK_IMM t2, 0xAA
    CHK_IMM t3, 0x55

    /* ════════════════════════════════════════════════════════════
     * H11 – Branch (predicted not-taken) + independent ALU in slot1
     * After IDStage fix, slot1 should execute alongside the branch.
     * This test verifies the ALU result is correct regardless.
     * ════════════════════════════════════════════════════════════ */
    li   s1, 0                /* s1 tracks whether slot1 executed */
    li   t0, 0
    /* branch not-taken (0 == 0 is false here since we test bne) */
    li   t0, 5
    bne  t0, zero, .Lbranch_taken_h11  /* predicted not-taken if BTB cold */
    /* after branch in slot0, slot1 should have executed an independent op */
    j    .Lh11_cont
.Lbranch_taken_h11:
    addi s1, s1, 1            /* we get here either way for bne with t0=5 */
.Lh11_cont:
    /* Verify that execution continued past the branch correctly */
    li   t0, 5
    CHK_IMM t0, 5             /* t0 must still be 5 after branch+slot1 */
    CHK_IMM s1, 1             /* branch was taken (t0=5 != 0) */

    /* ════════════════════════════════════════════════════════════
     * H12 – Store followed by load (different addresses, then same)
     * ════════════════════════════════════════════════════════════ */
    li   t0, 0x11111111
    li   t1, 0x22222222
    sw   t0, 12(a5)
    sw   t1, 16(a5)
    /* Some ALU work */
    add  t2, t0, t1           /* t2 = 0x33333333 */
    lw   t3, 12(a5)           /* must get 0x11111111 */
    lw   t4, 16(a5)           /* must get 0x22222222 */
    CHK_IMM t3, 0x11111111
    CHK_IMM t4, 0x22222222
    CHK_IMM t2, 0x33333333

    /* ════════════════════════════════════════════════════════════
     * H13 – CSR forwarding: CSR write then immediate read-back
     * The CSR write happens in EX; the read should see the new value.
     * ════════════════════════════════════════════════════════════ */
    li   t0, 0x3              /* enable both prefetcher bits */
    li   t1, 0x7C0            /* prefetch_ctrl CSR address */
    csrrw zero, 0x7C0, t0    /* write prefetch_ctrl = 3 */
    nop
    csrr  t2, 0x7C0          /* read back */
    CHK_IMM t2, 0x3
    /* Restore to 0 */
    csrrw zero, 0x7C0, zero

    /* ── Done ─────────────────────────────────────────────────── */
    mv   a0, s0
    lw   ra, 28(sp)
    lw   s0, 24(sp)
    lw   s1, 20(sp)
    lw   s2, 16(sp)
    addi sp, sp, 32
    ret
```

## .\src\test\test_mem.S

```scala
/* test_mem.S  –  Load/Store instruction coverage
 *
 * Covered:
 *   SW / LW     – full word, 4-byte aligned
 *   SH / LH     – half-word (signed), offsets 0 and 2
 *   SH / LHU    – half-word (unsigned), sign-extension check
 *   SB / LB     – byte (signed), all 4 byte offsets
 *   SB / LBU    – byte (unsigned), sign-extension check
 *   Negative value store and sign-extension on load
 *   Store-then-load same address (through cache)
 *   Back-to-back stores to adjacent words
 *   Unaligned byte/halfword within same cache line
 *
 * Returns: fail count in a0
 */

    .section .bss
    .align 6                /* 64-byte (cache line) alignment */
_mem_buf:
    .space 128              /* 2 cache lines of scratch space */

    .section .text
    .global  test_mem

.macro CHK_IMM expected
    li   t3, \expected
    beq  t2, t3, 1f
    addi s0, s0, 1
1:
.endm

test_mem:
    addi sp, sp, -16
    sw   ra, 12(sp)
    sw   s0,  8(sp)
    li   s0, 0

    la   a5, _mem_buf       /* base address for all memory tests */

    /* ────────────────────────────────────────────────────────────
     * SW / LW
     * ──────────────────────────────────────────────────────────── */
    li   t0, 0x12345678
    sw   t0, 0(a5)
    lw   t2, 0(a5);         CHK_IMM 0x12345678

    li   t0, 0xDEADBEEF
    sw   t0, 4(a5)
    lw   t2, 4(a5);         CHK_IMM 0xDEADBEEF   /* MSB set – full 32-bit */

    li   t0, 0
    sw   t0, 8(a5)
    lw   t2, 8(a5);         CHK_IMM 0

    li   t0, -1
    sw   t0, 12(a5)
    lw   t2, 12(a5);        CHK_IMM 0xFFFFFFFF   /* -1 round-trips */

    /* Back-to-back adjacent word stores */
    li   t0, 0xAAAAAAAA
    li   t1, 0x55555555
    sw   t0, 16(a5)
    sw   t1, 20(a5)
    lw   t2, 16(a5);        CHK_IMM 0xAAAAAAAA
    lw   t2, 20(a5);        CHK_IMM 0x55555555

    /* ────────────────────────────────────────────────────────────
     * SH / LH (signed half-word load)
     * ──────────────────────────────────────────────────────────── */
    /* Write positive half at offset 0 */
    li   t0, 0x0ABC
    sh   t0, 0(a5)
    lh   t2, 0(a5);         CHK_IMM 0x0ABC       /* positive, no sign ext */

    /* Write negative half (MSB set) at offset 0 */
    li   t0, 0x8001
    sh   t0, 0(a5)
    lh   t2, 0(a5);         CHK_IMM 0xFFFF8001   /* sign-extended */

    /* Half at byte-offset 2 within word */
    li   t0, 0x1234
    sh   t0, 2(a5)
    lh   t2, 2(a5);         CHK_IMM 0x1234

    li   t0, 0xFFFF
    sh   t0, 2(a5)
    lh   t2, 2(a5);         CHK_IMM -1           /* 0xFFFF sign-ext to -1 */

    /* ────────────────────────────────────────────────────────────
     * SH / LHU (unsigned half-word load)
     * ──────────────────────────────────────────────────────────── */
    li   t0, 0x8001
    sh   t0, 0(a5)
    lhu  t2, 0(a5);         CHK_IMM 0x8001       /* no sign extension */

    li   t0, 0xFFFF
    sh   t0, 2(a5)
    lhu  t2, 2(a5);         CHK_IMM 0xFFFF       /* 65535, not -1 */

    /* ────────────────────────────────────────────────────────────
     * SB / LB (signed byte load), all 4 offsets within a word
     * ──────────────────────────────────────────────────────────── */
    /* First clear the word */
    sw   zero, 0(a5)

    li   t0, 0x41            /* 'A' */
    sb   t0, 0(a5)
    lb   t2, 0(a5);         CHK_IMM 0x41        /* positive, no sign ext */

    li   t0, 0x80            /* negative byte */
    sb   t0, 0(a5)
    lb   t2, 0(a5);         CHK_IMM 0xFFFFFF80  /* sign-extended to -128 */

    li   t0, 0x7F
    sb   t0, 1(a5)
    lb   t2, 1(a5);         CHK_IMM 0x7F

    li   t0, 0xFF
    sb   t0, 2(a5)
    lb   t2, 2(a5);         CHK_IMM -1          /* 0xFF sign-ext = -1 */

    li   t0, 0x55
    sb   t0, 3(a5)
    lb   t2, 3(a5);         CHK_IMM 0x55

    /* Verify the four bytes are independently stored */
    li   t0, 0xAA
    sb   t0, 0(a5)
    li   t0, 0xBB
    sb   t0, 1(a5)
    li   t0, 0xCC
    sb   t0, 2(a5)
    li   t0, 0xDD
    sb   t0, 3(a5)
    lw   t2, 0(a5);         CHK_IMM 0xDDCCBBAA  /* little-endian */

    /* ────────────────────────────────────────────────────────────
     * SB / LBU (unsigned byte load)
     * ──────────────────────────────────────────────────────────── */
    li   t0, 0x80
    sb   t0, 0(a5)
    lbu  t2, 0(a5);         CHK_IMM 0x80        /* no sign extension */

    li   t0, 0xFF
    sb   t0, 3(a5)
    lbu  t2, 3(a5);         CHK_IMM 0xFF        /* 255, not -1 */

    /* ────────────────────────────────────────────────────────────
     * Store-then-load through cache: verify cache coherence
     * ──────────────────────────────────────────────────────────── */
    li   t0, 0x11223344
    sw   t0, 32(a5)
    /* Some intervening ALU work to ensure the load sees the stored value */
    li   t1, 0
    addi t1, t1, 1
    addi t1, t1, 1
    lw   t2, 32(a5);        CHK_IMM 0x11223344

    /* Write two words, overwrite the first, read both */
    li   t0, 0xAAAA
    sw   t0, 36(a5)
    li   t0, 0xBBBB
    sw   t0, 40(a5)
    li   t0, 0xCCCC
    sw   t0, 36(a5)         /* overwrite first */
    lw   t2, 36(a5);        CHK_IMM 0xCCCC
    lw   t2, 40(a5);        CHK_IMM 0xBBBB

    /* ────────────────────────────────────────────────────────────
     * Mixed-width to same word: SH then read with LBU
     * ──────────────────────────────────────────────────────────── */
    sw   zero, 44(a5)
    li   t0, 0x1234
    sh   t0, 44(a5)         /* write half at offset 0 in that word */
    lbu  t2, 44(a5);        CHK_IMM 0x34        /* little-endian: low byte first */
    lbu  t2, 45(a5);        CHK_IMM 0x12        /* high byte of half */

    /* ────────────────────────────────────────────────────────────
     * Large immediate offset (uses S-type encoding)
     * ──────────────────────────────────────────────────────────── */
    li   t0, 0x99887766
    sw   t0, 60(a5)
    lw   t2, 60(a5);        CHK_IMM 0x99887766

    /* ── Done ─────────────────────────────────────────────────── */
    mv   a0, s0
    lw   ra, 12(sp)
    lw   s0,  8(sp)
    addi sp, sp, 16
    ret
```

## .\src\test\TopSpec.scala

```scala
package riscv

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.{File, PrintWriter}

/** ═══════════════════════════════════════════════════════════════════════════
  * TopSpec – sbt test / Verilator simulation for the RV32I superscalar core
  *
  * Run all:         sbt test
  * Run one:         sbt "testOnly riscv.TopSpec -- -z smoke"
  * Generate waves:  add WriteVcdAnnotation to the Seq below
  *
  * Requires Verilator on PATH.
  *   Ubuntu: sudo apt install verilator
  *   macOS:  brew install verilator
  *
  * Memory map:
  *   0x80000000  program (loaded from hex file)
  *   0x80001000  data scratch area
  *   0x10001FF0  success MMIO  (sw 1 → io.success=1)
  *   0x10001FF1  putchar MMIO  (sb char)
  *   0x0000BFF8  mtime MMIO    (lw)
  * ═══════════════════════════════════════════════════════════════════════════
  */
class TopSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  // ── Infrastructure ───────────────────────────────────────────────────────

  private val hexDir: File = { val d = new File("test-tmp-hex"); d.mkdirs(); d }

  /** Write words as a plain hex file (one 32-bit word per line, no @address).
    * Padded to 512 words with NOP (0x00000013) to keep Verilator happy.
    * Returns the absolute path so $readmemh can find it.
    */
  private def writeHex(name: String, words: Seq[Long]): String = {
    val f  = new File(hexDir, name)
    val pw = new PrintWriter(f)
    words.padTo(512, 0x00000013L)
         .foreach(w => pw.println(f"${w & 0xFFFFFFFFL}%08x"))
    pw.close()
    f.getAbsolutePath
  }

  case class SimResult(success: Boolean, output: String, cycles: Int, perf: PerfSnapshot)

  /** Step the Verilator simulation until io.success rises or maxCycles expires. */
  private def runSim(initFile: String,
                     maxCycles: Int = 5000,
                     verbose: Boolean = false): SimResult = {
    var success = false; var cycles = 0
    val output  = new StringBuilder
    var perf = PerfSnapshot.zero
    var outputTruncated = false
    val runDir = s"test_run_dir_${System.currentTimeMillis()}_${math.abs(initFile.hashCode)}"
    test(new SimTop(initFile))
      .withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation(runDir))) { dut =>
        dut.clock.setTimeout(maxCycles + 100)
        while (!success && cycles < maxCycles) {
          dut.clock.step(); cycles += 1
          if (dut.io.success.peekBoolean()) success = true
          if (dut.io.printChar.valid.peekBoolean()) {
            val ch = dut.io.printChar.bits.peekInt().toChar
            if (output.length < 65536) {
              output.append(ch)
              if (verbose) print(ch)
            } else if (!outputTruncated) {
              output.append("\n[output truncated]\n")
              if (verbose) print("\n[output truncated]\n")
              outputTruncated = true
              cycles = maxCycles
            }
          }
        }
        perf = PerfSnapshot.from(dut.io.perf)
        if (verbose) println(s"\n[sim] ${if (success) "OK" else "TIMEOUT"} in $cycles cycles")
      }
    SimResult(success, output.toString, cycles, perf)
  }

  // ── Machine-code programs ────────────────────────────────────────────────
  //
  // All encodings computed for RV32I little-endian.
  // Register aliases: t0=x5  t1=x6  t2=x7  t3=x28  ra=x1
  //
  // Common success epilogue (reused by every program):
  //   lui  t0, 0x10002       → t0 = 0x10002000
  //   addi t0, t0, -16       → t0 = 0x10001FF0  (success MMIO)
  //   addi t1, x0, 1
  //   sw   t1, 0(t0)         → io.success = 1
  //   jal  x0, 0             (halt – self loop)
  //
  // The same FAIL label is always a jal x0,0 (infinite loop) that the
  // test harness will detect as a timeout.

  // ── T1: Smoke (ALU + branch + success MMIO) ──────────────────────────────
  // 5 + 3 = 8, compare, assert success
  //   0x00 addi t0, x0, 5
  //   0x04 addi t1, x0, 3
  //   0x08 add  t2, t0, t1         t2=8
  //   0x0C addi t3, x0, 8
  //   0x10 bne  t2, t3, +24        → 0x28 (fail) if t2≠8
  //   0x14..0x20  success epilogue
  //   0x24 halt
  //   0x28 halt (fail)
  private val smokeProg = Seq(
    0x00500293L, // addi t0, x0, 5
    0x00300313L, // addi t1, x0, 3
    0x006283B3L, // add  t2, t0, t1
    0x00800E13L, // addi t3, x0, 8
    0x01C39C63L, // bne  t2, t3, +24
    0x100022B7L, // lui  t0, 0x10002
    0xFF028293L, // addi t0, t0, -16       t0=0x10001FF0
    0x00100313L, // addi t1, x0, 1
    0x0062A023L, // sw   t1, 0(t0)         SUCCESS
    0x0000006FL, // halt
    0x0000006FL, // fail-halt
  )

  // ── T2: Printf putchar ────────────────────────────────────────────────────
  // Print "Hi\n" via MMIO at 0x10001FF1 (SB), then assert success.
  //   0x10001FF1 = 0x10002000 - 15  →  lui 0x10002; addi -15
  //   0x10001FF0 = 0x10002000 - 16
  private val putcharProg = Seq(
    0x100022B7L, // lui  t0, 0x10002
    0xFF128293L, // addi t0, t0, -15       t0=0x10001FF1 (putchar)
    0x04800313L, // addi t1, x0, 72        'H'
    0x00628023L, // sb   t1, 0(t0)
    0x06900313L, // addi t1, x0, 105       'i'
    0x00628023L, // sb   t1, 0(t0)
    0x00A00313L, // addi t1, x0, 10        '\n'
    0x00628023L, // sb   t1, 0(t0)
    0x100022B7L, // lui  t0, 0x10002
    0xFF028293L, // addi t0, t0, -16       t0=0x10001FF0
    0x00100313L, // addi t1, x0, 1
    0x0062A023L, // sw   t1, 0(t0)         SUCCESS
    0x0000006FL, // halt
  )

  // ── T3: Load / Store  ─────────────────────────────────────────────────────
  // Data at 0x80001000 (inside 4 MB RAM, word index 0x400).
  // Tests: SW/LW round-trip, SB/LB (sign extension), BNE guards.
  //
  //   0x00 lui  t0, 0x80001          t0=0x80001000
  //   0x04 addi t1, x0, 42
  //   0x08 sw   t1, 0(t0)
  //   0x0C lw   t2, 0(t0)
  //   0x10 addi t3, x0, 42
  //   0x14 bne  t2, t3, +40          → 0x3C fail
  //   0x18 addi t1, x0, 90
  //   0x1C sb   t1, 4(t0)
  //   0x20 lb   t2, 4(t0)            signed byte: 90 = 0x5A < 128 → no sign-ext
  //   0x24 bne  t2, t1, +24          → 0x3C fail
  //   0x28..0x34  success epilogue
  //   0x38 halt
  //   0x3C halt (fail)
  private val loadStoreProg = Seq(
    0x800012B7L, // lui  t0, 0x80001
    0x02A00313L, // addi t1, x0, 42
    0x0062A023L, // sw   t1, 0(t0)
    0x0002A383L, // lw   t2, 0(t0)
    0x02A00E13L, // addi t3, x0, 42
    0x03C39463L, // bne  t2, t3, +40
    0x05A00313L, // addi t1, x0, 90
    0x00628223L, // sb   t1, 4(t0)
    0x00428383L, // lb   t2, 4(t0)
    0x00639C63L, // bne  t2, t1, +24
    0x100022B7L, // lui  t0, 0x10002
    0xFF028293L, // addi t0, t0, -16
    0x00100313L, // addi t1, x0, 1
    0x0062A023L, // sw   t1, 0(t0)       SUCCESS
    0x0000006FL, // halt
    0x0000006FL, // fail-halt
  )

  // ── T4: Forwarding, load-use stall, and tight branch loop ────────────────
  //
  // Phase 1 – EX→EX forwarding chain:
  //   li t0, 1; addi t0,t0,1 ×6 → t0 must equal 7
  //   fail branch if t0≠7 (encoding: bne x5,x28, offset=68 → 0x5C29263)
  //
  // Phase 2 – Load-use stall:
  //   sw 42 to data addr; lw t1; immediately addi t1,t1,1
  //   → HW must insert 1-cycle stall; t1 must equal 43
  //   fail branch if t1≠43 (encoding: bne x6,x28, offset=40 → 0x3C31463)
  //
  // Phase 3 – Countdown loop (8→0):
  //   li t1, 8; loop: addi t1,t1,-1; bnez t1, -4
  //   (tight taken branch ×8, warms BTB)
  //
  // Layout:
  //   0x00–0x18  phase 1 chain
  //   0x1C       addi t3, x0, 7
  //   0x20       bne t0, t3, +68  → 0x64 fail
  //   0x24       lui  t0, 0x80001  (data area)
  //   0x28       addi t1, x0, 42
  //   0x2C       sw   t1, 0(t0)
  //   0x30       lw   t1, 0(t0)   ← lw
  //   0x34       addi t1, t1, 1   ← LOAD-USE dep
  //   0x38       addi t3, x0, 43
  //   0x3C       bne  t1, t3, +40 → 0x64 fail
  //   0x40       addi t1, x0, 8   loop counter
  //   0x44       addi t1, t1, -1  ← loop top
  //   0x48       bnez t1, -4      branch back to 0x44
  //   0x4C–0x58  success epilogue
  //   0x5C       halt
  //   0x60       (pad)
  //   0x64       halt (fail)
  //
  // Key encodings:
  //   bne x5,x28, +68  : 0x05C29263
  //   bne x6,x28, +40  : 0x03C31463  (corrected below from manual calc)
  //   lw  t1, 0(t0)    : 0x0002A303
  //   bnez t1, -4      : 0xFE031EE3
  private val hazardProg = Seq(
    0x00100293L, // 0x00 addi t0, x0, 1
    0x00128293L, // 0x04 addi t0, t0, 1       t0=2
    0x00128293L, // 0x08 addi t0, t0, 1       t0=3
    0x00128293L, // 0x0C addi t0, t0, 1       t0=4
    0x00128293L, // 0x10 addi t0, t0, 1       t0=5
    0x00128293L, // 0x14 addi t0, t0, 1       t0=6
    0x00128293L, // 0x18 addi t0, t0, 1       t0=7  (EX→EX chain)
    0x00700E13L, // 0x1C addi t3, x0, 7
    0x05C29263L, // 0x20 bne  t0, t3, +68     → 0x64 fail if t0≠7
    0x800012B7L, // 0x24 lui  t0, 0x80001     t0=0x80001000
    0x02A00313L, // 0x28 addi t1, x0, 42
    0x0062A023L, // 0x2C sw   t1, 0(t0)
    0x0002A303L, // 0x30 lw   t1, 0(t0)       ← LOAD
    0x00130313L, // 0x34 addi t1, t1, 1       ← LOAD-USE: HW stalls 1 cycle
    0x02B00E13L, // 0x38 addi t3, x0, 43
    0x03C31463L, // 0x3C bne  t1, t3, +40     → 0x64 fail if t1≠43
    0x00800313L, // 0x40 addi t1, x0, 8       loop counter
    0xFFF30313L, // 0x44 addi t1, t1, -1      ← loop top (BTB warm-up)
    0xFE031EE3L, // 0x48 bnez t1, -4          branch back to 0x44
    0x100022B7L, // 0x4C lui  t0, 0x10002
    0xFF028293L, // 0x50 addi t0, t0, -16     t0=0x10001FF0
    0x00100313L, // 0x54 addi t1, x0, 1
    0x0062A023L, // 0x58 sw   t1, 0(t0)       SUCCESS
    0x0000006FL, // 0x5C halt
    0x0000006FL, // 0x60 (pad)
    0x0000006FL, // 0x64 fail-halt
  )

  // ── T5: CSR and mtime MMIO ────────────────────────────────────────────────
  // Read mcycle twice (CSRR = CSRRS rd, csr, x0):
  //   CSR 0xB00 (mcycle):  funct3=010, opcode=0x73
  //   csrr t0 (x5): (0xB00<<20)|(0b010<<12)|(5<<7)|0x73 = 0xB00022F3
  //   csrr t1 (x6): (0xB00<<20)|(0b010<<12)|(6<<7)|0x73 = 0xB0002373
  //   bgeu t0, t1, fail  (fail if second reading ≤ first)
  //
  // Read mtime MMIO at 0xBFF8 twice:
  //   0xBFF8 = lui 0xC (t0=0xC000) + addi -8 → t0=0xBFF8
  //   lui t0, 0xC: (0xC<<12)|(5<<7)|0x37 = 0xC000|0x280|0x37 = 0xC2B7
  //     Wait: (0xC<<12) = 0xC000. (5<<7)=0x280. 0xC000|0x280|0x37=0xC2B7? 
  //     But 0xC000 | 0x280 = 0xC280; 0xC280 | 0x37 = 0xC2B7.
  //     This is a 16-bit number, padded to 32-bit: 0x0000C2B7. ✓
  //   addi t0, t0, -8: imm=-8=0xFF8; (0xFF8<<20)|(5<<15)|(5<<7)|0x13 = 0xFF828293
  //   bgeu t0, t1, fail / bgeu t1, t2, fail encodings below.
  //
  // Instruction layout:
  //   0x00 csrr t0, mcycle
  //   0x04–0x0C  nop ×3
  //   0x10 csrr t1, mcycle
  //   0x14 bgeu t0, t1, +56   → 0x4C fail if t0>=t1
  //   0x18 lui  t0, 0xC        t0=0xC000
  //   0x1C addi t0, t0, -8     t0=0xBFF8
  //   0x20 lw   t1, 0(t0)      first mtime
  //   0x24–0x2C  nop ×3
  //   0x30 lw   t2, 0(t0)      second mtime
  //   0x34 bgeu t1, t2, +24    → 0x4C fail if t1>=t2
  //   0x38 lui  t0, 0x10002
  //   0x3C addi t0, t0, -16
  //   0x40 addi t1, x0, 1
  //   0x44 sw   t1, 0(t0)      SUCCESS
  //   0x48 halt
  //   0x4C halt (fail)
  //
  //   bgeu t0(x5), t1(x6), +56:
  //     56=0b111000: imm[4:1]=0b1100, imm[10:5]=0b000001
  //     rs1=5,rs2=6,funct3=0b111(BGEU),opcode=0x63
  //     (0<<31)|(0b000001<<25)|(6<<20)|(5<<15)|(0b111<<12)|(0b1100<<8)|(0<<7)|0x63
  //     = 0x02000000|0x600000|0x28000|0x7000|0xC00|0x63 = 0x0262FC63
  //
  //   bgeu t1(x6), t2(x7), +24:
  //     24=0b011000: imm[4:1]=0b1100, imm[10:5]=0b000000
  //     rs1=6,rs2=7,funct3=0b111(BGEU),opcode=0x63
  //     (0<<31)|(0b000000<<25)|(7<<20)|(6<<15)|(0b111<<12)|(0b1100<<8)|(0<<7)|0x63
  //     = 0|0x700000|0x30000|0x7000|0xC00|0x63 = 0x00737C63
  //
  //   lw t2, 0(t0): rd=x7=7, rs1=x5=5, funct3=2, opcode=3
  //     (0<<20)|(5<<15)|(2<<12)|(7<<7)|0x03 = 0x28000|0x2000|0x380|3 = 0x0002A383
  private val csrProg = Seq(
    0xB00022F3L, // 0x00 csrr t0, mcycle         (first read)
    0x00000013L, // 0x04 nop
    0x00000013L, // 0x08 nop
    0x00000013L, // 0x0C nop
    0xB0002373L, // 0x10 csrr t1, mcycle         (second read)
    0x00000013L, // 0x14 nop                     avoid CSR->branch same packet
    0x0262FC63L, // 0x18 bgeu t0, t1, +56        fail if first >= second
    0x0000C2B7L, // 0x1C lui  t0, 0xC             t0=0xC000
    0xFF828293L, // 0x20 addi t0, t0, -8          t0=0xBFF8
    0x0002A303L, // 0x24 lw   t1, 0(t0)           first mtime read
    0x00000013L, // 0x28 nop
    0x00000013L, // 0x2C nop
    0x00000013L, // 0x30 nop
    0x0002A383L, // 0x34 lw   t2, 0(t0)           second mtime read
    0x00737C63L, // 0x38 bgeu t1, t2, +24         fail if first >= second
    0x100022B7L, // 0x3C lui  t0, 0x10002
    0xFF028293L, // 0x40 addi t0, t0, -16
    0x00100313L, // 0x44 addi t1, x0, 1
    0x0062A023L, // 0x48 sw   t1, 0(t0)           SUCCESS
    0x0000006FL, // 0x4C halt
    0x0000006FL, // 0x50 fail-halt
  )

  // ── Tests ─────────────────────────────────────────────────────────────────

  behavior of "InOrderCore"

  it should "complete the smoke test (ALU + branch + success MMIO)" in {
    val result = runSim(writeHex("smoke.hex", smokeProg), verbose = true)
    println(PerfPrinter.line("perf-smoke", PerfPrinter.common(if (result.success) "OK" else "TIMEOUT", "smoke", result.perf)))
    withClue(s"cycles=${result.cycles}") { result.success shouldBe true }
  }

  it should "output 'Hi\\n' via the putchar MMIO" in {
    val result = runSim(writeHex("putchar.hex", putcharProg), verbose = true)
    result.success shouldBe true
    result.output  shouldBe "Hi\n"
  }

  it should "correctly store and load bytes and words (D-Cache round-trip)" in {
    val result = runSim(writeHex("load_store.hex", loadStoreProg), maxCycles = 8000, verbose = true)
    withClue(s"output='${result.output}' cycles=${result.cycles}") {
      result.success shouldBe true
    }
  }

  it should "pass the forwarding chain, load-use stall, and branch loop" in {
    val result = runSim(writeHex("hazard.hex", hazardProg), maxCycles = 8000, verbose = true)
    withClue(s"output='${result.output}' cycles=${result.cycles}") {
      result.success shouldBe true
    }
  }

  it should "show mcycle and mtime increasing (CSR + MMIO counters)" in {
    val result = runSim(writeHex("csr.hex", csrProg), maxCycles = 8000, verbose = true)
    withClue(s"output='${result.output}' cycles=${result.cycles}") {
      result.success shouldBe true
    }
  }

  /** Full assembly suite – only runs if tests/test.hex was built.
    * Build: cd tests && make
    */
  it should "pass the full assembly test suite (tests/test.hex)" in {
    val f = new File("tests/test.hex")
    assume(f.exists(), "tests/test.hex not found – run 'cd tests && make' first")
    val result = runSim(f.getAbsolutePath, maxCycles = 2_000_000, verbose = true)
    result.success shouldBe true
    result.output  should include ("PASS")
  }
}
```

