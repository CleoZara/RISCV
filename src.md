## .\src\main\Common\CSR.scala
```
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

// CSROp 鐜扮粺涓€瀹氫箟鍦?Defines_c.scala锛坥bject CSROp锛夛紝姝ゅ涓嶅啀閲嶅瀹氫箟銆?
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

    // mtime锛圡MIO 瀹炴椂璁℃暟鍣紝姣忓懆鏈熻嚜澧烇級锛氫緵椤跺眰閫佸線 D-Cache 鐨?mtimeLo/mtimeHi銆?    val mtimeLo = Output(UInt(xlen.W))
    val mtimeHi = Output(UInt(xlen.W))
    val branchPredCtrl = Output(UInt(xlen.W))

    // 棰勫彇寮€鍏筹細bit0 浠呮帶鍒?ICache next-line锛沚it1/bit2 鎺у埗 stride/stream銆?    val prefetchCtrl = Output(UInt(xlen.W))
  })

  val mcycle        = RegInit(0.U(64.W))
  val minstret      = RegInit(0.U(64.W))
  val mtime         = RegInit(0.U(64.W))
  val mcountinhibit = RegInit(0.U(xlen.W))
  val prefetchCtrl  = RegInit(prefetchInit.U(xlen.W))
  val branchPredCtrl = RegInit(branchPredInit.U(xlen.W))

  // misa锛氬彧璇汇€侻XL=01锛圧V32锛夌疆浜?bit[31:30]锛?I'=bit8锛涘惈 M 鏃跺啀缃?'M'=bit12銆?  val misaVal = {
    val base = ("h40000000".U(32.W) | (1.U << 8)) // RV32 + I
    if (enableRV32M) base | (1.U << 12) else base
  }

  // 璁℃暟鍣ㄨ嚜澧烇紙mcountinhibit 鍙叧闂級銆俶time 濮嬬粓鑷銆?  when(io.cycleTick && !mcountinhibit(0)) { mcycle := mcycle + 1.U }
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
      // misa 鍙锛氬拷鐣ュ啓鍏?    }
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
```
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
```
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
```
package riscv

import chisel3._
import chisel3.util._

// 璇存槑锛氭湰鏂囦欢鏄『搴忔牳鍞竴鐨勫啋闄?鏃佽矾鍗曞厓锛屽凡瀹炵幇 spec 搂5.3.1 鐨?F1鈥揊11 瀹屾暣
// forwarding 鐭╅樀銆伮?.3.2 鐨勩€屽厛鍒?forwarding 鍐嶅喅瀹?stall銆嶃€乴oad-use stall銆?// 妲介棿 RAW 绮剧粏闄嶇骇锛坕dCanBypassToYounger锛夌瓑銆?// 鏃х殑绠€鍖栫増 HazardUnit.scala 涓庢湰鍗曞厓鍔熻兘閲嶅彔涓旇涔夊啿绐侊紙瀵规Ы闂?RAW 涓€寰嬮檷绾э紝
// 涓?F5 鍚屽懆鏈熸梺璺煕鐩撅級锛屽簲浜堝垹闄わ紝闆嗘垚鏃朵粎淇濈暀鏈崟鍏冦€?
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
      io.idex(i).rfWen && !io.idex(i).memRen && (io.idex(i).wbSel =/= WbSel.WB_MEM)

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
    stallOnWaw: Boolean = false,   // spec 搂7.2.1锛氬弻鍐欑鍙ｅ啿绐佺敱妲戒紭鍏堢骇瑙ｅ喅锛屼笉 stall
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
```
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

    val out = Output(Vec(issueWidth, new EXMEMBundle))
  })

  val alus         = Seq.fill(issueWidth)(Module(new ParamALU(32, enableRV32M)))
  val slotValid    = Wire(Vec(issueWidth, Bool()))
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
    val forwardData = MuxLookup(io.in(i).wbSel, alus(i).io.result, Seq(
      WbSel.WB_ALU -> alus(i).io.result,
      WbSel.WB_PC4 -> fallThrough,
      WbSel.WB_CSR -> io.csrOldData,
      WbSel.WB_MEM -> alus(i).io.result
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
    io.out(i).aluOut    := alus(i).io.result
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

  // 鈹€鈹€ CSR operation 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  val slot0Live = slotValid(0)
  val slot1Live = slotValid(1) && !redirect(0)

  val csrSlot0 = slot0Live && io.in(0).csrOp =/= CSROp.NONE
  val csrSlot1 = slot1Live && io.in(1).csrOp =/= CSROp.NONE
  val csrIdx   = Mux(csrSlot0, 0.U, 1.U)
  io.csrOpValid := csrSlot0 || csrSlot1
  io.csrOpType  := Mux(csrSlot0, io.in(0).csrOp,  io.in(1).csrOp)
  io.csrWaddr   := Mux(csrSlot0, io.in(0).csrAddr, io.in(1).csrAddr)
  io.csrWdata   := io.rs1Data(csrIdx)

  // 鈹€鈹€ Redirect 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  io.exRedirectValid := redirect(0) || redirect(1)
  io.exRedirectPc    := Mux(redirect(0), actualNextPc(0), actualNextPc(1))

  // 鈹€鈹€ BPU update 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
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
```
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

  // 鈹€鈹€ Pending-slot register for slot-1 deferral 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
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

  // 鈹€鈹€ P2 fix: slot 0 can forward its ALU result to slot 1 in the same EX
  // cycle (F5 bypass path) when it is a non-Load register-writing instruction.
  // In that case we must NOT block slot 1 on an intra-slot RAW.
  val slot0Mem = slotValid(0) && (dec(0).io.out.memRen || dec(0).io.out.memWen)
  val slot1Mem = slotValid(1) && (dec(1).io.out.memRen || dec(1).io.out.memWen)

  val slot0CanBypass =
    slotValid(0) &&
    dec(0).io.out.rfWen &&
    !dec(0).io.out.memRen &&                           // Load result not available in EX
    (dec(0).io.out.wbSel === WbSel.WB_ALU) &&         // only ALU results are available to F5
    (dec(0).io.out.rdAddr =/= 0.U)

  // 鈹€鈹€ Slot-1 stall conditions 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
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
  //   a) slot 0 is JAL      鈫?redirect happens in ID, slot 1 is wrong-path
  //   b) slot 0 is JALR     鈫?target unknown until EX, slot 1 is wrong-path
  //   c) slot 0 is a branch predicted-TAKEN 鈫?slot 1 (PC+4) is wrong-path
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

  // 鈹€鈹€ CSR read address MUX 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  val csrSel1 = !slot0Csr && slot1Csr
  io.csrRaddr := Mux(csrSel1, dec(1).io.out.csrAddr, dec(0).io.out.csrAddr)

  // 鈹€鈹€ JAL redirect (ID-level redirect, 1-cycle flush) 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
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

  // 鈹€鈹€ Pipeline register outputs 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
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
```
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
```
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
  hazard.io.backendStall := false.B

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
```
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
```
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
```
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
```
package parameterized_cache

import chisel3._
import chisel3.util._

// ============================================================
//  鍏ㄥ眬鍞竴鐨勫弬鏁板畾涔夈€傚寘鍐呮墍鏈夋ā鍧楀叡鐢ㄦ case class锛?//  鍏朵綑鏂囦欢涓嶅啀鍚勮嚜閲嶅 `case class CacheParams`銆?// ============================================================
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
//  鍏ㄥ眬鍞竴鐨?TagEntry 瀹氫箟锛堝甫 dirty 瀛楁锛夈€?//  HitTest 涓嶄娇鐢?dirty锛屽拷鐣ュ嵆鍙紱DCacheTop 閫氳繃
//  tagArray.io.tagData(...).dirty 璇诲彇鑴忎綅锛岃繛绾跨被鍨嬩竴鑷淬€?// ============================================================
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
```
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
```
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
```
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
  // 0x10001FF1 鈥?printf putchar  (store byte)
  // 0x10001FF0 鈥?halt / success  (store word, wdata[0]=1 鈫?success)
  // 0x0000BFF8 鈥?mtime low       (load word, uncacheable)
  // 0x0000BFFC 鈥?mtime high      (load word, uncacheable)
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

  // 鈹€鈹€ 棰勫彇 useful 缁熻鏍囪 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
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
    (addrIsHalt  && io.memRen) -> 0.U   // load from halt addr 鈫?undefined, return 0
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
```
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

  // HitTest 涓嶄娇鐢?tagData 涓殑 dirty 瀛楁锛屼粎姣斿 valid 涓?tag銆?  val hitVec = VecInit((0 until p.WAY_NUM).map(i =>
    io.tagData(i).valid && (io.tagData(i).tag === io.tag)))

  io.isHit     := hitVec.asUInt.orR
  io.hitWay    := PriorityEncoder(hitVec)
  io.missValid := !io.isHit && (io.memRen || io.wen)
}
```

## .\src\main\Dcache\LoadExtend.scala
```
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
```
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
```
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

  // 鐢?RegInit 淇濊瘉涓婄數澶嶄綅鍚?valid/dirty=0锛屽浣嶈涔変笌 reset 淇″彿缁熶竴锛?  // 涓嶅啀渚濊禆鐙珛鐨?when(reset.asBool) 鍧椼€?  val tagArray = RegInit(VecInit(Seq.fill(p.SET_NUM)(
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
```
package parameterized_cache

import chisel3._
import chisel3.util._

// ================================================================
//  閫氱敤鏍戠姸 PLRU锛屾敮鎸佷换鎰?2 鐨勫箓璺暟锛堝綋鍓嶉渶姹傦細4 / 8 璺級銆?//
//  瀛樺偍锛氭瘡缁?(WAY_NUM-1) 涓?bit锛屾寜 0 鍩哄爢缂栧彿鎽嗘斁鍐呴儴鑺傜偣锛?//        node 0 = 鏍癸紱node n 鐨勫乏/鍙冲瓙鑺傜偣 = 2n+1 / 2n+2銆?//        鍙跺瓙 node 绱㈠紩 (WAY_NUM-1 .. 2*WAY_NUM-2) 瀵瑰簲 way 0 .. WAY_NUM-1銆?//
//  绾﹀畾锛氬唴閮ㄨ妭鐐?bit = 鈥滃彈瀹宠€呮柟鍚戔€濓紙0 = 宸﹀瓙鏍戯紝1 = 鍙冲瓙鏍戯級銆?//
//  椹遍€愶細鑷牴涓嬭 depth 灞傦紝鎸夊悇鑺傜偣 bit 閫夋嫨瀛愭爲锛岃惤鍒扮殑鍙跺瓙鍗宠椹遍€愯矾銆?//  鏇存柊锛氭部 root鈫抣eaf(updateWay) 璺緞锛屾妸姣忎釜鑺傜偣 bit 缃负璇ヨ矾鏂瑰悜鐨勫彇鍙?//        锛堟寚鍚戝厔寮熷瓙鏍戯級锛屼娇琚闂矾鎴愪负 MRU銆?//
//  4 璺┍閫愬簭锛?鈫?鈫?鈫?鈫?
//  8 璺┍閫愬簭锛?鈫?鈫?鈫?鈫?鈫?鈫?鈫?鈫?
// ================================================================
class TreePLRU(p: CacheParams) extends Module {
  require(isPow2(p.WAY_NUM), "TreePLRU 瑕佹眰 WAY_NUM 涓?2 鐨勫箓锛堝綋鍓嶆敮鎸?4 / 8 璺級")

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

  // ---- 椹遍€愯矾璁＄畻锛堢粍鍚堬級----
  var evNode = 0.U(nodeW.W)
  for (_ <- 0 until depth) {
    evNode = (evNode << 1).asUInt + 1.U + tree(evNode).asUInt
  }
  io.evictWay := (evNode - numNodes.U)(p.WAY_W - 1, 0)

  // ---- 鍛戒腑 / 鍥炲～鍚庢洿鏂?----
  when(io.updateEn) {
    val newBits = Wire(Vec(numNodes, Bool()))
    for (i <- 0 until numNodes) newBits(i) := tree(i)
    var upNode = 0.U(nodeW.W)
    for (level <- 0 until depth) {
      val dir = io.updateWay(depth - 1 - level)   // 0 = 宸? 1 = 鍙?      newBits(upNode) := ~dir
      upNode = (upNode << 1).asUInt + 1.U + dir.asUInt
    }
    treeArray(io.idx) := newBits.asUInt
  }
}
```

## .\src\main\Decode\Decoder_c.scala
```
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
```
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

    // Compare flags are convenient for branch units.
    val cmpEq  = Output(Bool())
    val cmpLt  = Output(Bool())
    val cmpLtu = Output(Bool())
  })

  val shamt = io.op2(shamtWidth - 1, 0)

  io.cmpEq  := io.op1 === io.op2
  io.cmpLt  := io.op1.asSInt < io.op2.asSInt
  io.cmpLtu := io.op1 < io.op2

  // 鍩虹 RV32I ALU 鏄犲皠
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

  // 鍔ㄦ€佺敓鎴?RV32M ALU 鎸囦护鏄犲皠
  val rv32mMapping: Seq[(UInt, UInt)] = if (enableRV32M) {
    // --- 涔樻硶 (Multiplication) ---
    val mul    = io.op1 * io.op2                                  // 浣?32 浣嶇浉鍚?    val mulh   = (io.op1.asSInt * io.op2.asSInt).asUInt           // 鏈夌鍙?脳 鏈夌鍙?    val mulhsu = (io.op1.asSInt * Cat(0.U(1.W), io.op2).asSInt).asUInt // 鏈夌鍙?脳 鏃犵鍙?    val mulhu  = io.op1 * io.op2                                  // 鏃犵鍙?脳 鏃犵鍙?
    // --- 闄ゆ硶杈圭晫鏉′欢 ---
    val divByZero   = io.op2 === 0.U
    val isIntMin    = io.op1 === Cat(1.U(1.W), 0.U((xlen - 1).W)) // -2^31
    val isMinusOne  = io.op2.andR                                 // -1
    val divOverflow = isIntMin && isMinusOne

    val divResult = Mux(divByZero, (-1.S(xlen.W)).asUInt,
                    Mux(divOverflow, io.op1,
                    (io.op1.asSInt / io.op2.asSInt).asUInt))(xlen - 1, 0)

    val divuResult = Mux(divByZero, (-1.S(xlen.W)).asUInt,
                     (io.op1 / io.op2))(xlen - 1, 0)

    val remResult = Mux(divByZero, io.op1,
                    Mux(divOverflow, 0.U(xlen.W),
                    (io.op1.asSInt % io.op2.asSInt).asUInt))(xlen - 1, 0)

    val remuResult = Mux(divByZero, io.op1,
                     (io.op1 % io.op2))(xlen - 1, 0)

    Seq(
      ALU_MUL    -> mul(xlen - 1, 0),
      ALU_MULH   -> mulh(xlen * 2 - 1, xlen),
      ALU_MULHSU -> mulhsu(xlen * 2 - 1, xlen),
      ALU_MULHU  -> mulhu(xlen * 2 - 1, xlen),
      ALU_DIV    -> divResult,
      ALU_DIVU   -> divuResult,
      ALU_REM    -> remResult,
      ALU_REMU   -> remuResult
    )
  } else {
    Seq()
  }

  io.result := MuxLookup(io.aluOp, 0.U(xlen.W), baseAluMapping ++ rv32mMapping)
}
```

## .\src\main\frame.md
```
# 娴佹按绾挎鏋惰鏄?
## IF 妯″潡

IF 妯″潡璐熻矗 PC 鐢熸垚銆丅PU 鏌ヨ銆両-Cache 鍙栨寚銆侀鍙栧櫒鎺у埗鍜?IF/ID 娴佹按瀵勫瓨鍣ㄨ緭鍑恒€傚綋鍓嶄富绾垮浐瀹氬弻鍙戯紝鍥犳 `issueWidth = 2`銆?
### 鏆撮湶鎺ュ彛璇存槑

| 淇″彿鍚?| 浣嶅/绫诲瀷 | 鏂瑰悜 | 璇存槑 |
| --- | --- | --- | --- |
| `stallIf` | `Bool` | Input | IF 绾у仠椤夸俊鍙枫€備负 1 鏃?PC 淇濇寔涓嶅彉锛岄€氬父鐢?I-Cache miss銆丏-Cache miss 鎴栧悗绔樆濉炶Е鍙戙€?|
| `flushIf` | `Bool` | Input | IF 绾у啿鍒蜂俊鍙枫€備负 1 鏃跺綋鍓嶅彇鎸囩粨鏋滃簲浣滃簾锛岃緭鍑?bubble銆?|
| `exRedirectValid` | `Bool` | Input | EX 绾?redirect 鏈夋晥淇″彿锛屼紭鍏堢骇鏈€楂樸€傜敤浜庡垎鏀棰勬祴淇銆丣ALR 鐩爣淇绛夈€?|
| `exRedirectPc` | `UInt(32.W)` | Input | EX 绾?redirect 鐩爣 PC銆?|
| `idRedirectValid` | `Bool` | Input | ID 绾?redirect 鏈夋晥淇″彿锛屼富瑕佺敤浜?JAL 鍦ㄨ瘧鐮佺骇鎻愬墠鏀瑰悜銆備紭鍏堢骇浣庝簬 EX redirect锛岄珮浜?stall 鍜?BPU 棰勬祴銆?|
| `idRedirectPc` | `UInt(32.W)` | Input | ID 绾?redirect 鐩爣 PC銆?|
| `bpuQueryPc` | `UInt(32.W)` | Output | 閫佸線 BPU 鐨勬煡璇?PC锛岄€氬父绛変簬褰撳墠 `pcFetch`銆?|
| `bpuPredTaken` | `Bool` | Input | BPU 杩斿洖鐨勯娴嬫柟鍚戙€備负 1 琛ㄧず棰勬祴璺宠浆銆?|
| `bpuPredTarget` | `UInt(32.W)` | Input | BPU 杩斿洖鐨勯娴嬬洰鏍囧湴鍧€銆備粎褰?`bpuPredTaken` 涓?1 鏃舵湁鏁堛€?|
| `nextLinePrefetchEn` | `Bool` | Input | Next-line 棰勫彇鍣ㄥ紑鍏炽€傚缓璁敱 CSR `prefetchCtrl(0)` 鎺у埗銆備笌 stride 棰勫彇鍣ㄥ紑鍏充簰涓嶅奖鍝嶃€?|
| `stridePrefetchEn` | `Bool` | Input | Stride 棰勫彇鍣ㄥ紑鍏炽€傚缓璁敱 CSR `prefetchCtrl(1)` 鎺у埗銆備笌 next-line 棰勫彇鍣ㄥ紑鍏充簰涓嶅奖鍝嶃€?|
| `out` | `Vec(2, IFIDSlot)` | Output | IF 鍒?ID 鐨勫弻鍙戝彇鎸囩粨鏋滐紝姣忔媿鏈€澶氳緭鍑轰袱涓?slot銆?|
| `icacheStall` | `Bool` | Output | I-Cache stall/miss 淇″彿锛岄€佸線 Hazard/Control 鍗曞厓锛岀敤浜庡喕缁撳墠绔垨鍏ㄦ祦姘淬€?|
| `imem` | `MemBusIO` | IO | I-Cache miss/refill 鍜岄鍙栬姹備娇鐢ㄧ殑澶栭儴鍐呭瓨鎬荤嚎銆?|
| `debugPc` | `UInt(32.W)` | Output | 璋冭瘯 PC锛岄€氬父绛変簬褰撳墠 IF 绾?PC銆?|

### `IFIDSlot` 灞曞紑

| 瀛楁鍚?| 浣嶅/绫诲瀷 | 璇存槑 |
| --- | --- | --- |
| `pc` | `UInt(32.W)` | 褰撳墠 slot 瀵瑰簲鎸囦护鐨?PC銆俿lot0 涓?`fetchPc`锛宻lot1 涓?`fetchPc + 4`銆?|
| `inst` | `UInt(32.W)` | 褰撳墠 slot 鐨?32 浣嶆寚浠ゃ€?|
| `ctrl.valid` | `Bool` | 褰撳墠 slot 鏄惁鍖呭惈鏈夋晥鎸囦护銆侷-Cache miss銆乻lot1 璺?cache line銆乫lush 鎴?bubble 鏃朵负 0銆?|
| `ctrl.kill` | `Bool` | 褰撳墠 slot 鏄惁琚?flush 鏉€鎺夈€備富瑕佺敤浜庤皟璇曞拰鍚庣骇闃插尽鎬у垽鏂€?|
| `ctrl.allowIn` | `Bool` | 褰撳墠 slot 鏄惁鍏佽杩涘叆涓嬩竴绾с€侷F 杈撳嚭渚у彲鍥哄畾涓?1锛岀湡姝ｅ啓鍏?IF/ID 瀵勫瓨鍣ㄧ敱椤跺眰 stall 鎺у埗銆?|
| `slotIdx` | `UInt(1.W)` | 鍙屽彂妲界紪鍙枫€俿lot0 涓?0锛宻lot1 涓?1銆?|
| `fetchPc` | `UInt(32.W)` | 鏈媿鍙栨寚 bundle 鐨勫熀鍦板潃锛屽嵆 slot0 鐨?PC銆?|
| `predTaken` | `Bool` | BPU 瀵规湰鎷嶅彇鎸?PC 鐨勯娴嬫柟鍚戙€傚熀纭€鐗堜袱涓?slot 鍙叡浜悓涓€娆?BPU 鏌ヨ缁撴灉銆?|
| `predTarget` | `UInt(32.W)` | BPU 棰勬祴鐩爣鍦板潃銆?|
| `predNextPc` | `UInt(32.W)` | IF 绾ф牴鎹娴嬪疄闄呴€夋嫨鐨勪笅涓€鍙栨寚 PC銆傝嫢棰勬祴璺宠浆鍒欎负 `predTarget`锛屽惁鍒欎负椤哄簭涓嬩竴 bundle PC銆?|
| `seqNextPc` | `UInt(32.W)` | 涓嶈烦杞椂鐨勯『搴忎笅涓€ bundle PC銆傚弻鍙戝浐瀹氫负 `fetchPc + 8`銆?|
| `icacheHit` | `Bool` | 褰撳墠鍙栨寚缁撴灉鏄惁鏉ヨ嚜 I-Cache 鍛戒腑鍝嶅簲銆備富瑕佺敤浜庤皟璇曞拰鎬ц兘缁熻銆?|

### 棰勫彇寮€鍏崇害瀹?
`nextLinePrefetchEn` 鍜?`stridePrefetchEn` 鏄袱涓嫭绔嬪紑鍏筹紝浠绘剰涓€涓叧闂兘涓嶅簲褰卞搷鍙︿竴涓鍙栧櫒鐨勫唴閮ㄧ姸鎬佹洿鏂板拰璇锋眰鐢熸垚绛栫暐銆?
寤鸿 CSR 鏄犲皠濡備笅锛?
| CSR 瀛楁 | 鎺у埗瀵硅薄 | 璇存槑 |
| --- | --- | --- |
| `prefetchCtrl(0)` | `nextLinePrefetchEn` | 涓?1 鏃跺厑璁?next-line 棰勫彇鍣ㄥ彂璧烽鍙栬姹傘€?|
| `prefetchCtrl(1)` | `stridePrefetchEn` | 涓?1 鏃跺厑璁?stride 棰勫彇鍣ㄥ彂璧烽鍙栬姹傘€?|

褰撲袱涓鍙栧櫒鍚屾椂鍙戣捣璇锋眰鏃讹紝IF 鍐呴儴搴斿厛淇濊瘉 demand miss 浼樺厛绾ф渶楂橈紱棰勫彇璇锋眰涔嬮棿鍙厛閲囩敤鍥哄畾浼樺厛绾э紝渚嬪 next-line 浼樺厛浜?stride锛屽悗缁啀鏀逛负杞浠茶銆?
## ID 妯″潡

ID 妯″潡璐熻矗鎺ユ敹 IF/ID 娴佹按瀵勫瓨鍣ㄤ腑鐨勫弻鍙戝彇鎸囩粨鏋滐紝瀹屾垚璇戠爜銆佸瘎瀛樺櫒鍫嗚鍦板潃鐢熸垚銆丆SR 璇诲湴鍧€鐢熸垚銆佹Ы闂村彂灏勭害鏉熷垽鏂紝骞惰緭鍑?ID/EX 娴佹按瀵勫瓨鍣ㄥ唴瀹广€?
褰撳墠椤哄簭鏍搁噰鐢ㄥ墠缂€杩炵画鍙戝皠绛栫暐锛氭Ы 0 涓嶈兘鍙戝皠鏃讹紝妲?1 蹇呴』鍚屾椂鍙樹负 bubble锛涙Ы 1 涓嶈兘鍙戝皠鏃讹紝鍙奖鍝嶆Ы 1锛屼笉鍏佽璺宠繃妲?0 鍘诲彂灏勬洿鍚庨潰鐨勬寚浠ゃ€?
### 鏆撮湶鎺ュ彛璇存槑

| 淇″彿鍚?| 浣嶅/绫诲瀷 | 鏂瑰悜 | 璇存槑 |
| --- | --- | --- | --- |
| `in` | `Vec(2, IFIDSlot)` | Input | 鏉ヨ嚜 IF/ID 娴佹按瀵勫瓨鍣ㄧ殑鍙屽彂鍙栨寚缁撴灉銆?|
| `stallId` | `Bool` | Input | ID 绾у仠椤夸俊鍙枫€備负 1 鏃?ID/EX 娴佹按瀵勫瓨鍣ㄤ繚鎸佷笉鍙橈紝涓や釜妲藉潎鍐荤粨銆?|
| `flushId` | `Bool` | Input | ID 绾у啿鍒蜂俊鍙枫€備负 1 鏃跺綋鍓?ID 杈撳嚭鍏ㄩ儴鍙樹负 bubble銆?|
| `regRs1Addr` | `Vec(2, UInt(5.W))` | Output | 閫佸線 RegFile 鐨?rs1 璇诲湴鍧€銆?|
| `regRs2Addr` | `Vec(2, UInt(5.W))` | Output | 閫佸線 RegFile 鐨?rs2 璇诲湴鍧€銆?|
| `regRs1Data` | `Vec(2, UInt(32.W))` | Input | RegFile 杩斿洖鐨?rs1 鏁版嵁銆?|
| `regRs2Data` | `Vec(2, UInt(32.W))` | Input | RegFile 杩斿洖鐨?rs2 鏁版嵁銆?|
| `csrRaddr` | `UInt(12.W)` | Output | 閫佸線 CSRFile 鐨勮鍦板潃銆傚熀纭€瀹炵幇寤鸿鍚屽懆鏈熸渶澶氬厑璁镐竴鏉?CSR 鎸囦护杩涘叆 EX銆?|
| `csrRdata` | `UInt(32.W)` | Input | CSRFile 杩斿洖鐨?CSR 鏃у€硷紝鐢ㄤ簬鍚庣画 CSR 鍐欏洖璇箟銆?|
| `idRedirectValid` | `Bool` | Output | ID 绾?redirect 鏈夋晥淇″彿锛屼富瑕佺敤浜?JAL 鎻愬墠鏀瑰悜銆?|
| `idRedirectPc` | `UInt(32.W)` | Output | ID 绾?redirect 鐩爣 PC锛岄€氬父涓?JAL 鐨?`pc + immJ`銆?|
| `hazardIdValid` | `Vec(2, Bool)` | Output | 閫佸線 HazardUnit 鐨?ID 妲芥湁鏁堜俊鍙枫€?|
| `hazardRs1Addr` | `Vec(2, UInt(5.W))` | Output | 閫佸線 HazardUnit 鐨?rs1 鍦板潃銆?|
| `hazardRs2Addr` | `Vec(2, UInt(5.W))` | Output | 閫佸線 HazardUnit 鐨?rs2 鍦板潃銆?|
| `hazardRs1Use` | `Vec(2, Bool)` | Output | 褰撳墠鎸囦护鏄惁瀹為檯浣跨敤 rs1銆?|
| `hazardRs2Use` | `Vec(2, Bool)` | Output | 褰撳墠鎸囦护鏄惁瀹為檯浣跨敤 rs2銆?|
| `hazardRdAddr` | `Vec(2, UInt(5.W))` | Output | 閫佸線 HazardUnit 鐨?rd 鍦板潃銆?|
| `hazardRfWen` | `Vec(2, Bool)` | Output | 褰撳墠鎸囦护鏄惁鍐欓€氱敤瀵勫瓨鍣ㄣ€?|
| `loadUseStall` | `Bool` | Input | HazardUnit 缁欏嚭鐨?load-use stall銆備负 1 鏃舵Ы 0 鍜屾Ы 1 鍧囧喕缁擄紝涓嶄骇鐢熸柊鍙戝皠銆?|
| `structuralStall` | `Bool` | Input | I-Cache miss銆丏-Cache miss 鎴栧悗绔粨鏋勯樆濉炵殑鍚堝苟鍋滈】銆備负 1 鏃舵墍鏈夋Ы鍐荤粨銆?|
| `out` | `Vec(2, IDEXBundle)` | Output | ID 鍒?EX 鐨勫弻鍙戣瘧鐮佺粨鏋溿€?|

### 鍐呴儴妲介棿 RAW 妫€娴?
ID 绾у繀椤绘娴嬪悓涓€鍙栨寚鍖呭唴鐨勬Ы闂?RAW 鍐茬獊銆傝鍒欏涓嬶細

| 鏉′欢 | 澶勭悊鏂瑰紡 |
| --- | --- |
| `slot0Valid && slot1Valid && slot0.rfWen && slot0.rdAddr =/= 0.U && slot0.rdAddr === slot1.rs1Addr && slot1.rs1Use` | 鏈懆鏈熼檷绾у崟鍙戝皠锛氫粎鍙戝皠妲?0锛屾Ы 1 鍙樹负 bubble锛屽苟鍦ㄤ笅涓€鍛ㄦ湡閲嶆柊灏濊瘯鍙戝皠銆?|
| `slot0Valid && slot1Valid && slot0.rfWen && slot0.rdAddr =/= 0.U && slot0.rdAddr === slot1.rs2Addr && slot1.rs2Use` | 鏈懆鏈熼檷绾у崟鍙戝皠锛氫粎鍙戝皠妲?0锛屾Ы 1 鍙樹负 bubble锛屽苟鍦ㄤ笅涓€鍛ㄦ湡閲嶆柊灏濊瘯鍙戝皠銆?|

缁勫悎琛ㄨ揪寮忓彲鍐欎负锛?
```scala
val slotRaw01 =
  slot0Valid &&
  slot1Valid &&
  slot0.rfWen &&
  (slot0.rdAddr =/= 0.U) &&
  ((slot1.rs1Use && slot0.rdAddr === slot1.rs1Addr) ||
   (slot1.rs2Use && slot0.rdAddr === slot1.rs2Addr))
```

鏈璁′笉鍦?ID 绾ч€氳繃鍚屽懆鏈熸梺璺В鍐虫Ы 0 鍒版Ы 1 鐨?RAW銆傚彧瑕佹娴嬪埌妲介棿 RAW锛屾Ы 1 蹇呴』绛夊緟妲?0 瀹屾垚鍐欏洖鍚庡啀閲嶆柊灏濊瘯鍙戝皠銆?
### 鍓嶇紑杩炵画鍙戝皠璇箟

ID 绾у彂灏勫繀椤绘弧瓒冲墠缂€杩炵画鎬с€備换鎰忔Ы鏃犳硶鍙戝皠鏃讹紝鍏跺悗鐨勬Ы閮藉繀椤诲彉涓?bubble锛屼笉鑳借烦杩囧綋鍓嶆Ы鍙戝皠鍚庣画鎸囦护銆?
| 鏉′欢 | 褰卞搷妲戒綅 |
| --- | --- |
| 妲?0 `valid=0`锛屼緥濡傚彇鎸囧寘涓嶈冻鎴栧榻愰棶棰?| 妲?0 鍜屾Ы 1 鍧囦负 bubble |
| 妲?1 `valid=0`锛屼緥濡傚彇鎸囧寘浠呰繑鍥?1 鏉?| 浠呮Ы 1 涓?bubble |
| 妲?1 涓庢Ы 0 瀛樺湪鍐呴儴 RAW 鍐茬獊 | 浠呮Ы 1 涓?bubble |
| `loadUseStall=1` | 妲?0 鍜屾Ы 1 鍧囧喕缁擄紝ID/EX 淇濇寔涓嶅彉 |
| `structuralStall=1`锛屼緥濡?D-Cache miss 鎴?I-Cache miss | 鎵€鏈夋Ы鍐荤粨锛屾暣鏉℃祦姘寸嚎淇濇寔涓嶅彉 |
| `flushId=1` | 妲?0 鍜屾Ы 1 鍧囧彉涓?bubble |

鎺ㄨ崘鍙戝皠鏈夋晥淇″彿锛?
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

杈撳嚭鍒?ID/EX 鏃讹細

```scala
out(0).ctrl.valid := slot0CanIssue
out(1).ctrl.valid := slot1CanIssue
```

褰?`slot1CanIssue=false` 涓?`slot0CanIssue=true` 鏃讹紝鏈懆鏈熶负鍗曞彂灏勶紱妲?1 瀵瑰簲 ID/EX 鍐呭蹇呴』鍐欐垚瀹夊叏 bubble锛岃嚦灏戜繚璇侊細

```scala
rfWen  := false.B
memRen := false.B
memWen := false.B
csrOp  := CSROp.NONE
brType := BrType.BR_NONE
```

### 妲?1 閲嶆柊灏濊瘯鍙戝皠鐨勮姹?
褰撴Ы 1 鍥犲唴閮?RAW 鍐茬獊琚檷绾т负 bubble 鏃讹紝绯荤粺蹇呴』淇濊瘉妲?1 鎸囦护涓嶄細涓㈠け銆傚疄鐜版柟寮忎簩閫変竴锛?
| 鏂瑰紡 | 璇存槑 |
| --- | --- |
| 淇濇寔 IF/ID 瀵勫瓨鍣?| 褰撴Ы 1 鍥犲唴閮?RAW 鍋滃彂鏃讹紝鍐荤粨 IF/ID 涓殑妲?1锛屼笅涓€鍛ㄦ湡缁х画灏濊瘯鍙戝皠銆傚疄鐜扮畝鍗曚絾闇€瑕佸鐞嗘Ы 0 宸插彂灏勫悗鐨勭姸鎬併€?|
| 寮曞叆灏忓瀷 pending slot | 灏嗘湭鍙戝皠鐨勬Ы 1 淇濆瓨鍒?ID 鍐呴儴 pending 瀵勫瓨鍣紝涓嬩竴鍛ㄦ湡浼樺厛浣滀负妲?0 灏濊瘯鍙戝皠銆傛帴鍙ｆ洿娓呮櫚锛屾帹鑽愮敤浜庡悗缁噸鏋勩€?|

涓轰簡淇濇寔鍓嶇紑杩炵画鍙戝皠璇箟锛屾帹鑽愪娇鐢?pending slot 鏂规锛氳闄嶇骇鐨勬Ы 1 涓嬩竴鍛ㄦ湡搴斾綔涓烘渶鑰佹寚浠や紭鍏堣繘鍏ヨ瘧鐮?鍙戝皠锛岃€屼笉鏄鏂板彇鎸囧寘瑕嗙洊銆?
## EX 妯″潡

EX 妯″潡璐熻矗鎵ц ID/EX 娴佹按瀵勫瓨鍣ㄤ紶鍏ョ殑鍙屽彂鎸囦护锛屽畬鎴?ALU 杩愮畻銆佽瀛樺湴鍧€璁＄畻銆佸垎鏀垽鏂€丣ALR 鐩爣璁＄畻銆丆SR 鎿嶄綔璇锋眰鐢熸垚銆丅PU 鏇存柊淇℃伅鐢熸垚锛屽苟杈撳嚭 EX/MEM 娴佹按瀵勫瓨鍣ㄥ唴瀹广€?
ALU 涓嶆槸鐙珛娴佹按绾э紝鑰屾槸 EX 绾у唴閮ㄧ殑鎵ц鍗曞厓銆傚弻鍙戦『搴忔牳涓?EX 鑷冲皯瀹炰緥鍖?2 涓?ALU锛屽垎鍒湇鍔℃Ы 0 鍜屾Ы 1銆?
### 鍐呴儴寤鸿妯″潡

| 瀛愭ā鍧?閫昏緫 | 璇存槑 |
| --- | --- |
| `ALU0` / `ALU1` | 涓や釜骞惰 ALU锛屽垎鍒鐞嗘Ы 0 鍜屾Ы 1銆?|
| 鎿嶄綔鏁伴€夋嫨閫昏緫 | 鏍规嵁 `op1Sel/op2Sel` 閫夋嫨 `rs1/rs2/pc/imm/pc+4` 绛夋搷浣滄暟銆?|
| 鍒嗘敮姣旇緝閫昏緫 | 鏍规嵁 `brType` 鍒ゆ柇鏉′欢鍒嗘敮鏄惁璺宠浆銆?|
| 鍒嗘敮鐩爣璁＄畻閫昏緫 | 鏉′欢鍒嗘敮鐩爣涓?`pc + imm`锛宖all-through 涓?`pc + 4`銆?|
| JALR 鐩爣璁＄畻閫昏緫 | JALR 鐩爣涓?`(rs1 + imm) & ~1`锛屽湪 EX 绾т骇鐢?redirect銆?|
| CSR 鎵ц閫昏緫 | 鐢熸垚 CSR 鍐欒姹傦紝骞舵妸 CSR 鏃у€间紶鍏ュ悗缁啓鍥炶矾寰勩€?|
| BPU 鏇存柊閫昏緫 | 浣跨敤 EX 寰楀埌鐨勭湡瀹炲垎鏀柟鍚戝拰鐩爣鏇存柊 BPU銆?|
| Redirect 鍒ゆ柇閫昏緫 | 姣旇緝鐪熷疄 next PC 涓庨娴?next PC锛屽彂鐜拌棰勬祴鏃堕€氱煡 IF flush/redirect銆?|
| EX/MEM 鎵撳寘閫昏緫 | 灏?ALU 缁撴灉銆佽瀛樻帶鍒躲€佸啓鍥炴帶鍒躲€丆SR 缁撴灉绛夋墦鍖呯粰 MEM 绾с€?|

### 鏆撮湶鎺ュ彛璇存槑

| 淇″彿鍚?| 浣嶅/绫诲瀷 | 鏂瑰悜 | 璇存槑 |
| --- | --- | --- | --- |
| `in` | `Vec(2, IDEXBundle)` | Input | 鏉ヨ嚜 ID/EX 娴佹按瀵勫瓨鍣ㄧ殑鍙屽彂璇戠爜缁撴灉銆?|
| `stallEx` | `Bool` | Input | EX 绾у仠椤夸俊鍙枫€備负 1 鏃?EX/MEM 娴佹按瀵勫瓨鍣ㄤ繚鎸佷笉鍙樸€?|
| `flushEx` | `Bool` | Input | EX 绾у啿鍒蜂俊鍙枫€備负 1 鏃跺綋鍓?EX 杈撳嚭鍙樹负 bubble锛屼笉鍏佽鍐欏瘎瀛樺櫒銆佽瀛樸€佸啓 CSR 鎴栨洿鏂?BPU銆?|
| `rs1Data` | `Vec(2, UInt(32.W))` | Input | 缁忚繃鏃佽矾缃戠粶淇鍚庣殑 rs1 鏁版嵁銆?|
| `rs2Data` | `Vec(2, UInt(32.W))` | Input | 缁忚繃鏃佽矾缃戠粶淇鍚庣殑 rs2 鏁版嵁銆?|
| `op1Data` | `Vec(2, UInt(32.W))` | Input | ALU 鎿嶄綔鏁?1锛岄€氬父鐢辨梺璺悗鐨?rs1銆丳C 鎴?0 閫夋嫨寰楀埌銆?|
| `op2Data` | `Vec(2, UInt(32.W))` | Input | ALU 鎿嶄綔鏁?2锛岄€氬父鐢辨梺璺悗鐨?rs2銆佺珛鍗虫暟鎴?`PC+4` 閫夋嫨寰楀埌銆?|
| `storeData` | `Vec(2, UInt(32.W))` | Input | Store 鎸囦护鍐欏叆鍐呭瓨鐨勬暟鎹紝搴斾娇鐢ㄦ梺璺悗鐨?rs2 鏁版嵁銆?|
| `csrOpValid` | `Bool` | Output | 褰撳墠鍛ㄦ湡鏄惁鏈?CSR 鎿嶄綔璇锋眰銆傚熀纭€瀹炵幇寤鸿鍚屽懆鏈熸渶澶氫竴鏉?CSR 鎸囦护杩涘叆 EX銆?|
| `csrOpType` | `UInt(2.W)` | Output | CSR 鎿嶄綔绫诲瀷锛屽搴?`CSROp.WRITE/SET/CLEAR`銆備富绾胯嚦灏戦渶瑕佹敮鎸?`CSROp.WRITE`銆?|
| `csrWaddr` | `UInt(12.W)` | Output | CSR 鍐欏湴鍧€銆?|
| `csrWdata` | `UInt(32.W)` | Output | CSR 鍐欏叆鐨勬柊鍊笺€傚 `csrrw` 鏉ヨ閫氬父涓烘梺璺悗鐨?rs1 鏁版嵁銆?|
| `csrOldData` | `UInt(32.W)` | Input | CSR 鍐欏叆鍓嶇殑鏃у€硷紝鐢ㄤ簬鍚庣画鍐欏洖 rd銆?|
| `exRedirectValid` | `Bool` | Output | EX 绾?redirect 鏈夋晥淇″彿銆傜敤浜庡垎鏀棰勬祴淇銆丣ALR 鏀瑰悜绛夈€?|
| `exRedirectPc` | `UInt(32.W)` | Output | EX 绾?redirect 鐩爣 PC銆?|
| `bpuUpdateValid` | `Bool` | Output | BPU 鏇存柊鏈夋晥淇″彿銆傛潯浠跺垎鏀湪 EX 寰楀埌鐪熷疄缁撴灉鍚庢洿鏂?BPU銆?|
| `bpuUpdatePc` | `UInt(32.W)` | Output | 闇€瑕佹洿鏂扮殑鍒嗘敮鎸囦护 PC銆?|
| `bpuUpdateTaken` | `Bool` | Output | 鍒嗘敮鐪熷疄鏂瑰悜銆備负 1 琛ㄧず瀹為檯璺宠浆銆?|
| `bpuUpdateTarget` | `UInt(32.W)` | Output | 鍒嗘敮鐪熷疄鐩爣鍦板潃銆?|
| `exValid` | `Vec(2, Bool)` | Output | 褰撳墠 EX 鍚勬Ы鏄惁鏈夋晥锛屼緵 HazardUnit/BypassUnit 浣跨敤銆?|
| `exMemRen` | `Vec(2, Bool)` | Output | 褰撳墠 EX 鍚勬Ы鏄惁涓?load 鎸囦护锛岀敤浜?load-use stall 妫€娴嬨€?|
| `exRdAddr` | `Vec(2, UInt(5.W))` | Output | 褰撳墠 EX 鍚勬Ы鐨勭洰鐨勫瘎瀛樺櫒鍦板潃銆?|
| `exRfWen` | `Vec(2, Bool)` | Output | 褰撳墠 EX 鍚勬Ы鏄惁浼氬啓閫氱敤瀵勫瓨鍣ㄣ€?|
| `exResult` | `Vec(2, UInt(32.W))` | Output | EX 绾?ALU 缁撴灉锛岀敤浜庢梺璺€傛敞鎰?load 鎸囦护鐨勮鍊兼槸璁垮瓨鍦板潃锛屼笉鏄?load 鏁版嵁銆?|
| `out` | `Vec(2, EXMEMBundle)` | Output | EX 鍒?MEM 鐨勫弻鍙戞祦姘磋緭鍑恒€?|

### ALU 鎿嶄綔鏁拌涔?
EX 绾?ALU 杈撳叆寤鸿鐢辨梺璺崟鍏冨畬鎴愰€夋嫨鍚庨€佸叆 EX锛?
| 淇″彿 | 璇存槑 |
| --- | --- |
| `rs1Data` | 鏃佽矾淇鍚庣殑 rs1 鍊硷紝鐢ㄤ簬鍒嗘敮姣旇緝銆丣ALR銆丆SR 鍐欐暟鎹瓑銆?|
| `rs2Data` | 鏃佽矾淇鍚庣殑 rs2 鍊硷紝鐢ㄤ簬鍒嗘敮姣旇緝銆?|
| `op1Data` | 宸叉牴鎹?`op1Sel` 閫夋嫨瀹屾垚鐨?ALU 杈撳叆 1銆?|
| `op2Data` | 宸叉牴鎹?`op2Sel` 閫夋嫨瀹屾垚鐨?ALU 杈撳叆 2銆?|
| `storeData` | Store 鍐欏唴瀛樻暟鎹紝蹇呴』鏄梺璺悗鐨?rs2 鍊笺€?|

ALU 杩炴帴鏂瑰紡锛?
```scala
alu(i).io.op1   := op1Data(i)
alu(i).io.op2   := op2Data(i)
alu(i).io.aluOp := in(i).aluOp
```

### 鍒嗘敮涓?Redirect 璇箟

鏉′欢鍒嗘敮鍦?EX 绾цВ鏋愮湡瀹炴柟鍚戯細

| `brType` | 鍒ゆ柇鏉′欢 |
| --- | --- |
| `BR_EQ` | `rs1Data === rs2Data` |
| `BR_NE` | `rs1Data =/= rs2Data` |
| `BR_LT` | `rs1Data.asSInt < rs2Data.asSInt` |
| `BR_GE` | `rs1Data.asSInt >= rs2Data.asSInt` |
| `BR_LTU` | `rs1Data < rs2Data` |
| `BR_GEU` | `rs1Data >= rs2Data` |

鍒嗘敮鐩爣涓庨『搴忕洰鏍囷細

```scala
val branchTarget = pc + imm
val fallThrough  = pc + 4.U
val actualNextPc = Mux(branchTaken, branchTarget, fallThrough)
```

JALR 鐩爣锛?
```scala
val jalrTarget = (rs1Data + imm) & "hfffffffe".U
```

EX 绾у簲姣旇緝鐪熷疄 next PC 涓?IF 绾ч娴?next PC銆傝嫢涓嶄竴鑷达紝鍒欎骇鐢?redirect锛?
```scala
val mispred = actualNextPc =/= predNextPc
exRedirectValid := mispred
exRedirectPc    := actualNextPc
```

鍥犳寤鸿 `IDEXBundle` 鍚庣画琛ュ厖浠?IF/ID 浼犱笅鏉ョ殑棰勬祴瀛楁锛?
| 瀛楁 | 璇存槑 |
| --- | --- |
| `predTaken` | IF 绾ч娴嬫柟鍚戙€?|
| `predTarget` | IF 绾ч娴嬬洰鏍囥€?|
| `predNextPc` | IF 绾у疄闄呴€夋嫨鐨勪笅涓€ PC銆?|

### CSR 鎵ц璇箟

鍩虹涓荤嚎鑷冲皯鏀寔 `csrrw`锛?
```text
CSR[csr] <- rs1
rd       <- old CSR value
```

EX 绾х敓鎴?CSR 鍐欒姹傦細

```scala
csrOpValid := slotValid && in(i).csrOp =/= CSROp.NONE
csrOpType  := in(i).csrOp
csrWaddr   := in(i).csrAddr
csrWdata   := rs1Data(i)
```

CSR 鏃у€?`csrOldData` 搴旇繘鍏?EX/MEM 鐨?`csrRdata`锛屽悗缁?WB 鏍规嵁 `wbSel=WB_CSR` 鍐欏洖 rd銆?
濡傛灉涓や釜妲藉悓鍛ㄦ湡閮芥槸 CSR 鎸囦护锛屽熀纭€瀹炵幇搴斿湪 ID 绾ч樆姝㈡Ы 1 鍙戝皠锛岄伩鍏?EX 绾?CSR 鍐欑鍙ｅ啿绐併€?
### 鍙屽彂鎺у埗娴佺害鏉?
涓洪檷浣?redirect 浠茶澶嶆潅搴︼紝鍩虹椤哄簭鏍稿缓璁噰鐢ㄤ繚瀹堣鍒欙細

| 鎯呭喌 | 寤鸿澶勭悊 |
| --- | --- |
| 妲?0 鏄?branch/JAL/JALR | 妲?1 鍦?ID 绾у彉涓?bubble锛屾垨鑷冲皯涓嶅厑璁告Ы 1 鍐嶄骇鐢?redirect銆?|
| 妲?1 鏄?branch/JAL/JALR锛屾Ы 0 鏄櫘閫氭寚浠?| 鍙互鍏佽妲?1 杩涘叆 EX銆?|
| 涓や釜妲介兘鍙兘浜х敓 redirect | 妲?0 浼樺厛锛屽洜涓烘Ы 0 绋嬪簭搴忔洿鑰併€?|

褰撳墠 ID 绾у凡缁忚瀹氬唴閮?RAW 鏃舵Ы 1 闄嶇骇锛屽洜姝?EX 涓嶉渶瑕佸疄鐜版Ы 0 鍒版Ы 1 鐨勫悓鍛ㄦ湡鏃佽矾銆?
### EX 杈撳嚭鏈夋晥鎬?
鎺ㄨ崘姣忔Ы EX 鏈夋晥淇″彿锛?
```scala
val exSlotValid =
  in(i).ctrl.valid &&
  !in(i).ctrl.kill &&
  !flushEx
```

褰?`exSlotValid=false` 鏃讹紝EX/MEM 杈撳嚭蹇呴』涓哄畨鍏?bubble锛岃嚦灏戜繚璇侊細

```scala
out(i).ctrl.valid := false.B
out(i).rfWen  := false.B
out(i).memRen := false.B
out(i).memWen := false.B
out(i).brType := BrType.BR_NONE
```

### EX/MEM 鎵撳寘瀛楁璇箟

| 瀛楁 | 璇存槑 |
| --- | --- |
| `pc` | 褰撳墠鎸囦护 PC銆?|
| `inst` | 褰撳墠鎸囦护缂栫爜锛屼富瑕佺敤浜庤皟璇曘€?|
| `aluOut` | ALU 缁撴灉銆傚 load/store 鏄瀛樺湴鍧€锛涘鏅€?ALU 鎸囦护鏄繍绠楃粨鏋溿€?|
| `rs2Data` | Store 鍐欏唴瀛樻暟鎹紝寤鸿濉叆鏃佽矾鍚庣殑 `storeData`銆?|
| `rdAddr` | 鐩殑瀵勫瓨鍣ㄥ湴鍧€銆?|
| `wbSel` | WB 闃舵鍐欏洖鏁版嵁鏉ユ簮閫夋嫨銆?|
| `rfWen` | 鏄惁鍐欓€氱敤瀵勫瓨鍣ㄣ€?|
| `memRen` | 鏄惁涓?load銆?|
| `memWen` | 鏄惁涓?store銆?|
| `memWd` | 璁垮瓨瀹藉害锛寃ord/half/byte銆?|
| `memSigned` | load 鏄惁绗﹀彿鎵╁睍銆?|
| `csrRdata` | CSR 鏃у€硷紝鐢ㄤ簬 `WB_CSR` 鍐欏洖銆?|
| `ctrl` | 娴佹按鎺у埗淇℃伅銆?|

## MEM 妯″潡

MEM 妯″潡璐熻矗鎺ユ敹 EX/MEM 娴佹按瀵勫瓨鍣ㄤ腑鐨勫弻鍙戠粨鏋滐紝瀹屾垚 load/store 璁块棶銆丏-Cache 鎺ュ叆銆乵time MMIO 璇汇€乸rintf 鍦板潃杈撳嚭锛屼互鍙婂悜 MEM/WB 鎵撳寘鍐欏洖鏁版嵁銆?
褰撳墠 `DCacheTop` 宸茬粡鍦?`src/main/Dcache` 涓疄鐜帮紝MEM 妯″潡闇€瑕侀€傞厤瀹冪殑鐪熷疄鎺ュ彛銆俙DCacheTop` 褰撳墠鏄崟绔彛鏁版嵁 cache锛屽洜姝ゅ熀纭€椤哄簭鏍稿缓璁悓鍛ㄦ湡鏈€澶氬厑璁镐竴鏉¤瀛樻寚浠よ繘鍏?MEM锛涘鏋滀袱涓Ы鍚屾椂鏄?load/store锛屽簲鍦?ID 鎴?EX 涔嬪墠璁╄緝骞磋交妲藉彉涓?bubble锛屾垨鑰呭湪 MEM 鍐呴儴鍙湇鍔℃渶鑰佽瀛樺苟鍐荤粨娴佹按銆?
### `DCacheTop` 閫傞厤鎺ュ彛

`DCacheTop` 鐨勭湡瀹炴帴鍙ｅ涓嬶紝MEM 妯″潡搴旂洿鎺ヨ繛鎺ヨ繖浜涗俊鍙凤細

| DCacheTop 淇″彿 | 浣嶅/绫诲瀷 | 鏂瑰悜锛堢浉瀵?DCache锛?| MEM 渚ц繛鎺ヨ涔?|
| --- | --- | --- | --- |
| `addr` | `UInt(p.ADDR_WIDTH.W)` | Input | 璁垮瓨鍦板潃锛屾潵鑷瀛樻Ы鐨?`EXMEMBundle.aluOut`銆?|
| `flush` | `Bool` | Input | cache flush 淇″彿銆傚熀纭€鐗堝彲鎺ュ叏灞€ flush 鎴栧浐瀹氫负 0锛屽悗缁敤浜?cache 娓呯┖銆?|
| `wen` | `Bool` | Input | store 浣胯兘锛屾潵鑷瀛樻Ы `memWen`銆?|
| `wmask` | `UInt(p.WMASK_BITS.W)` | Input | store 瀛楄妭鍐欐帺鐮侊紝鐢卞湴鍧€浣庝綅鍜?`memWd` 鐢熸垚銆?|
| `wdata` | `UInt(p.DATA_WIDTH.W)` | Input | store 鍐欐暟鎹紝鏉ヨ嚜 EX 绾т紶涓嬫潵鐨勬梺璺悗 `rs2Data/storeData`銆?|
| `memRen` | `Bool` | Input | load 浣胯兘锛屾潵鑷瀛樻Ы `memRen`銆?|
| `memWd` | `UInt(2.W)` | Input | 璁垮瓨瀹藉害锛岀洿鎺ユ潵鑷?`EXMEMBundle.memWd`銆?|
| `signed` | `Bool` | Input | load 鏄惁绗﹀彿鎵╁睍锛岀洿鎺ユ潵鑷?`EXMEMBundle.memSigned`銆?|
| `rdata` | `UInt(p.DATA_WIDTH.W)` | Output | load 璇诲嚭骞跺畬鎴愭墿灞曞悗鐨勬暟鎹紝閫佸叆瀵瑰簲妲界殑 `MEMWBBundle.memData`銆?|
| `missOut` | `Bool` | Output | D-Cache miss/stall 鐘舵€侊紝鍙€?Hazard/Control 浣滀负 `dcacheStall`銆?|
| `stall` | `Bool` | Output | 涓?`missOut` 璇箟涓€鑷达紝琛ㄧず D-Cache 姝ｅ湪澶勭悊 miss锛屾祦姘寸嚎搴斿喕缁撱€?|
| `mtimeLo` | `UInt(p.DATA_WIDTH.W)` | Input | 鏉ヨ嚜 CSRFile 鐨?`mtimeLo`锛岀敤浜?DCache 鍐呴儴 MMIO 鍦板潃璇诲彇銆?|
| `mtimeHi` | `UInt(p.DATA_WIDTH.W)` | Input | 鏉ヨ嚜 CSRFile 鐨?`mtimeHi`锛岀敤浜?DCache 鍐呴儴 MMIO 鍦板潃璇诲彇銆?|
| `printChar` | `Valid(UInt(8.W))` | Output | printf 鍦板潃 store 浜х敓鐨勫瓧绗﹁緭鍑恒€?|
| `mem` | `MemBusIO(p)` | IO | D-Cache miss/writeback/refill 浣跨敤鐨勫閮ㄥ唴瀛樻€荤嚎銆?|

### MEM 鏆撮湶鎺ュ彛璇存槑

| 淇″彿鍚?| 浣嶅/绫诲瀷 | 鏂瑰悜 | 璇存槑 |
| --- | --- | --- | --- |
| `in` | `Vec(2, EXMEMBundle)` | Input | 鏉ヨ嚜 EX/MEM 娴佹按瀵勫瓨鍣ㄧ殑鍙屽彂缁撴灉銆?|
| `stallMem` | `Bool` | Input | MEM 绾у仠椤夸俊鍙枫€備负 1 鏃?MEM/WB 娴佹按瀵勫瓨鍣ㄤ繚鎸佷笉鍙樸€?|
| `flushMem` | `Bool` | Input | MEM 绾у啿鍒蜂俊鍙枫€備负 1 鏃跺綋鍓?MEM 杈撳嚭搴斿彉涓?bubble锛屼笉鑳藉啓鍥炪€?|
| `mtimeLo` | `UInt(32.W)` | Input | CSRFile 杈撳嚭鐨?mtime 浣?32 浣嶏紝閫忎紶缁?DCacheTop銆?|
| `mtimeHi` | `UInt(32.W)` | Input | CSRFile 杈撳嚭鐨?mtime 楂?32 浣嶏紝閫忎紶缁?DCacheTop銆?|
| `dcacheFlush` | `Bool` | Input | D-Cache flush 鎺у埗淇″彿銆傚熀纭€鐗堝彲鐢遍《灞傚浐瀹氫负 0銆?|
| `dcacheStall` | `Bool` | Output | D-Cache miss/stall 鐘舵€侊紝閫佸線 Hazard/Control 瑙﹀彂鍏ㄦ祦姘村喕缁撱€?|
| `printChar` | `Valid(UInt(8.W))` | Output | printf 鍦板潃 store 鐨勫瓧绗﹁緭鍑恒€?|
| `dmem` | `MemBusIO(p)` | IO | D-Cache 瀵瑰鍐呭瓨鎬荤嚎銆?|
| `out` | `Vec(2, MEMWBBundle)` | Output | MEM 鍒?WB 鐨勫弻鍙戞祦姘磋緭鍑恒€?|

### MEM 鍐呴儴璁垮瓨妲介€夋嫨

鐢变簬褰撳墠 D-Cache 鏄崟绔彛锛孧EM 姣忓懆鏈熷彧鑳藉悜 D-Cache 鍙戣捣涓€涓?load/store銆傛帹鑽愰€夋嫨绋嬪簭搴忔渶鑰佺殑鏈夋晥璁垮瓨妲斤細

```scala
val slot0Mem = in(0).ctrl.valid && (in(0).memRen || in(0).memWen)
val slot1Mem = in(1).ctrl.valid && (in(1).memRen || in(1).memWen)

val memSel0 = slot0Mem
val memSel1 = !slot0Mem && slot1Mem
```

鍩虹瀹炵幇鏇存帹鑽愬湪 ID 闃舵绂佹鍙岃瀛樺悓鍙戯紝杩欐牱 MEM 涓笉浼氬嚭鐜?`slot0Mem && slot1Mem`銆傚鏋滀粛鐒跺嚭鐜帮紝搴斿綋浠ユЫ 0 涓哄噯锛屽苟瑙﹀彂娴佹按鍐荤粨鎴栨柇瑷€锛岄槻姝㈡Ы 1 璁垮瓨涓㈠け銆?
### Store Mask 鐢熸垚

MEM 绾ф牴鎹湴鍧€浣?2 浣嶅拰 `memWd` 鐢熸垚 DCache `wmask`锛?
| 璁垮瓨瀹藉害 | `addr(1,0)` | `wmask` 璇箟 |
| --- | --- | --- |
| word | 浠绘剰锛岄€氬父瑕佹眰瀵归綈 | `1111` |
| half | `00` | `0011` |
| half | `10` | `1100` |
| byte | `00` | `0001` |
| byte | `01` | `0010` |
| byte | `10` | `0100` |
| byte | `11` | `1000` |

鑻ヤ笉瀹炵幇绮剧‘寮傚父锛岄潪瀵归綈璁块棶鍙厛鎸夌‖浠惰嚜鐒舵帺鐮佸鐞嗘垨鍦ㄦ祴璇曚腑閬垮厤銆?
### MEM/WB 鎵撳寘瑙勫垯

| 杈撳叆绫诲瀷 | MEM/WB 瀛楁濉厖 |
| --- | --- |
| 鏅€?ALU/JAL/CSR 鎸囦护 | `memData := 0.U`锛屽叾浣欏啓鍥炰俊鎭粠 EX/MEM 閫忎紶銆?|
| load 鎸囦护 | `memData := dcache.io.rdata`锛宍wbSel` 淇濇寔 `WB_MEM`銆?|
| store 鎸囦护 | `rfWen := false.B`锛屼笉鍐欏洖閫氱敤瀵勫瓨鍣ㄣ€?|
| bubble/flush | `ctrl.valid := false.B`锛宍rfWen := false.B`锛宍memData := 0.U`銆?|

### MEM 鍋滈】璇箟

褰?`dcache.io.stall` 鎴?`dcache.io.missOut` 涓?1 鏃讹細

| 琛屼负 | 璇存槑 |
| --- | --- |
| `dcacheStall := true.B` | 閫氱煡 Hazard/Control銆?|
| IF/ID/EX/MEM/WB | 鍩虹鐗堝缓璁叏娴佹按鍐荤粨锛岀洿鍒?D-Cache miss 瀹屾垚銆?|
| MEM/WB | 淇濇寔鍘熷€硷紝涓嶆彁浜ゆ柊鐨?load 缁撴灉銆?|

## WB 妯″潡

WB 妯″潡璐熻矗浠?MEM/WB 娴佹按瀵勫瓨鍣ㄤ腑閫夋嫨鏈€缁堝啓鍥炴暟鎹紝椹卞姩 RegFile 鍐欑鍙ｏ紝骞朵骇鐢熸彁浜よ鏁颁俊鍙风粰 CSRFile銆?
### 鏆撮湶鎺ュ彛璇存槑

| 淇″彿鍚?| 浣嶅/绫诲瀷 | 鏂瑰悜 | 璇存槑 |
| --- | --- | --- | --- |
| `in` | `Vec(2, MEMWBBundle)` | Input | 鏉ヨ嚜 MEM/WB 娴佹按瀵勫瓨鍣ㄧ殑鍙屽彂缁撴灉銆?|
| `regWen` | `Vec(2, Bool)` | Output | RegFile 鍐欎娇鑳姐€傛棤鏁堟Ы銆乣rd=x0` 鎴?`rfWen=0` 鏃跺繀椤讳负 0銆?|
| `regWaddr` | `Vec(2, UInt(5.W))` | Output | RegFile 鍐欏湴鍧€銆?|
| `regWdata` | `Vec(2, UInt(32.W))` | Output | RegFile 鍐欐暟鎹€?|
| `instRetire` | `Vec(2, Bool)` | Output | 姣忎釜妲芥槸鍚︽垚鍔熼€€浼戯紝鐢ㄤ簬 CSRFile 缁熻 `minstret`銆?|
| `wbValid` | `Vec(2, Bool)` | Output | 鍐欏洖妲芥湁鏁堜俊鍙凤紝渚?BypassUnit 浣跨敤銆?|
| `wbRfWen` | `Vec(2, Bool)` | Output | 鍐欏洖妲芥槸鍚﹀啓閫氱敤瀵勫瓨鍣紝渚?BypassUnit 浣跨敤銆?|
| `wbRdAddr` | `Vec(2, UInt(5.W))` | Output | 鍐欏洖鐩殑瀵勫瓨鍣ㄥ彿锛屼緵 BypassUnit 浣跨敤銆?|
| `wbData` | `Vec(2, UInt(32.W))` | Output | 鍐欏洖鏁版嵁锛屼緵 BypassUnit 浣跨敤銆?|

### 鍐欏洖鏁版嵁閫夋嫨

WB 鏍规嵁 `wbSel` 閫夋嫨鍐欏洖鏁版嵁锛?
| `wbSel` | 鍐欏洖鏁版嵁 |
| --- | --- |
| `WB_ALU` | `aluOut` |
| `WB_MEM` | `memData` |
| `WB_PC4` | `pc + 4` |
| `WB_CSR` | `csrRdata` |

鎺ㄨ崘缁勫悎閫昏緫锛?
```scala
val wbData = MuxLookup(in(i).wbSel, in(i).aluOut, Seq(
  WbSel.WB_ALU -> in(i).aluOut,
  WbSel.WB_MEM -> in(i).memData,
  WbSel.WB_PC4 -> (in(i).pc + 4.U),
  WbSel.WB_CSR -> in(i).csrRdata
))
```

### 鍙屽啓绔彛璇箟

RegFile 鏀寔鍙屽啓绔彛鏃讹紝WB 涓や釜妲藉彲鍚屾椂鍐欏洖銆傝嫢妲?0 鍜屾Ы 1 鍚屽懆鏈熷啓鍚屼竴涓潪闆?`rd`锛屽繀椤讳繚璇佺▼搴忓簭鏇村勾杞荤殑妲?1 鏈€缁堝彲瑙併€?
| 鎯呭喌 | 澶勭悊 |
| --- | --- |
| `rd=x0` | 鍐欎娇鑳藉己鍒朵负 0銆?|
| 妲?0銆佹Ы 1 鍐欎笉鍚?rd | 涓や釜鍐欑鍙ｅ悓鏃跺啓銆?|
| 妲?0銆佹Ы 1 鍐欏悓涓€闈為浂 rd | 妲?1 浼樺厛锛孯egFile 鍐呴儴鎴?WB 鍐欑鍙ｄ徊瑁佸繀椤讳繚璇佹Ы 1 瑕嗙洊妲?0銆?|

### 閫€浼戣鏁拌涔?
`instRetire(i)` 寤鸿瀹氫箟涓猴細

```scala
instRetire(i) := in(i).ctrl.valid && !in(i).ctrl.kill
```

濡傛灉鍚庣画鍔犲叆寮傚父銆侀樆濉炴彁浜ゆ垨绮剧‘閫€浼戯紝鍐嶆妸璇ュ畾涔夋敹绱с€傚綋鍓嶈绋嬭姹備笉瀹炵幇绮剧‘寮傚父锛屽洜姝ゅ熀纭€鐗堝彲鎸?WB 鏈夋晥妲借鏁般€?
## RegFile 妯″潡

RegFile 鏄?32 涓?32 浣嶉€氱敤瀵勫瓨鍣ㄦ枃浠讹紝鏈嶅姟鍙屽彂 ID 璇诲拰鍙屽彂 WB 鍐欍€?
### 鏆撮湶鎺ュ彛璇存槑

| 淇″彿鍚?| 浣嶅/绫诲瀷 | 鏂瑰悜 | 璇存槑 |
| --- | --- | --- | --- |
| `rs1Addr` | `Vec(2, UInt(5.W))` | Input | 涓や釜妲界殑 rs1 璇诲湴鍧€銆?|
| `rs2Addr` | `Vec(2, UInt(5.W))` | Input | 涓や釜妲界殑 rs2 璇诲湴鍧€銆?|
| `rs1Data` | `Vec(2, UInt(32.W))` | Output | 涓や釜妲界殑 rs1 璇绘暟鎹€?|
| `rs2Data` | `Vec(2, UInt(32.W))` | Output | 涓や釜妲界殑 rs2 璇绘暟鎹€?|
| `wen` | `Vec(2, Bool)` | Input | 涓や釜鍐欑鍙ｇ殑鍐欎娇鑳斤紝鏉ヨ嚜 WB銆?|
| `waddr` | `Vec(2, UInt(5.W))` | Input | 涓や釜鍐欑鍙ｇ殑鍐欏湴鍧€銆?|
| `wdata` | `Vec(2, UInt(32.W))` | Input | 涓や釜鍐欑鍙ｇ殑鍐欐暟鎹€?|

### RegFile 璇箟绾﹀畾

| 瑙勫垯 | 璇存槑 |
| --- | --- |
| `x0` 鎭掍负 0 | 璇?`x0` 蹇呴』杩斿洖 0锛屽啓 `x0` 蹇呴』蹇界暐銆?|
| 鍙岃鍙屽啓 | 鍙屽彂闇€瑕?4 涓绔彛鍜?2 涓啓绔彛銆?|
| 鍚屽懆鏈熻鍐欏悓涓€瀵勫瓨鍣?| 寤鸿瀹炵幇 write-first 鎴栧湪 ID/EX 鏃佽矾涓鐩栵紝淇濊瘉璇诲埌鏈€鏂板彲瑙佸€笺€?|
| 鍙屽啓鍚屼竴瀵勫瓨鍣?| 鑻ヤ袱涓啓绔彛鍐欏悓涓€闈為浂瀵勫瓨鍣紝妲?1 浼樺厛銆?|

鎺ㄨ崘鍙屽啓浼樺厛绾э細

```scala
when(wen(0) && waddr(0) =/= 0.U) {
  regs(waddr(0)) := wdata(0)
}
when(wen(1) && waddr(1) =/= 0.U) {
  regs(waddr(1)) := wdata(1)
}
```

杩欐牱褰撲袱涓鍙ｅ啓鍚屼竴鍦板潃鏃讹紝鍚庡啓鐨勬Ы 1 瑕嗙洊妲?0銆?
## CSRFile 妯″潡

CSRFile 璐熻矗瀹炵幇鍩虹 CSR銆佹€ц兘璁℃暟鍣ㄣ€乵time 璁℃暟鍣ㄥ拰棰勫彇寮€鍏炽€傚綋鍓嶆簮鐮佷腑鐨?`CSRFile` 宸茬粡鍖呭惈 `mcycle`銆乣mcycleh`銆乣minstret`銆乣mcountinhibit`銆乣misa`銆乣prefetchCtrl`銆乣mtimeLo/mtimeHi`銆?
### 鏆撮湶鎺ュ彛璇存槑

| 淇″彿鍚?| 浣嶅/绫诲瀷 | 鏂瑰悜 | 璇存槑 |
| --- | --- | --- | --- |
| `raddr` | `UInt(12.W)` | Input | CSR 璇诲湴鍧€銆傚熀纭€瀹炵幇鍙湁涓€涓绔彛锛屽洜姝?ID 闃舵搴旈檺鍒跺悓鍛ㄦ湡鏈€澶氫竴鏉?CSR 鎸囦护銆?|
| `rdata` | `UInt(32.W)` | Output | CSR 璇绘暟鎹€?|
| `opValid` | `Bool` | Input | EX 绾?CSR 鎿嶄綔鏈夋晥淇″彿銆?|
| `opType` | `UInt(2.W)` | Input | CSR 鎿嶄綔绫诲瀷锛屼娇鐢?`CSROp.WRITE/SET/CLEAR`銆備富绾胯嚦灏戦渶瑕?`CSROp.WRITE`銆?|
| `waddr` | `UInt(12.W)` | Input | CSR 鍐欏湴鍧€銆?|
| `wdata` | `UInt(32.W)` | Input | CSR 鍐欐暟鎹€?|
| `oldData` | `UInt(32.W)` | Output | CSR 鍐欏叆鍓嶆棫鍊硷紝鐢ㄤ簬 `csrrw` 鍐欏洖 rd銆?|
| `cycleTick` | `Bool` | Input | 鍛ㄦ湡璁℃暟浣胯兘銆備负 1 涓?`mcountinhibit(0)=0` 鏃讹紝`mcycle` 鑷銆?|
| `instRetire` | `Vec(2, Bool)` | Input | WB 闃舵姣忔Ы閫€浼戜俊鍙凤紝鐢ㄤ簬鏇存柊 `minstret`銆?|
| `mcycleLo` | `UInt(32.W)` | Output | `mcycle` 浣?32 浣嶃€?|
| `mcycleHi` | `UInt(32.W)` | Output | `mcycle` 楂?32 浣嶃€?|
| `minstretLo` | `UInt(32.W)` | Output | `minstret` 浣?32 浣嶃€?|
| `mtimeLo` | `UInt(32.W)` | Output | `mtime` 浣?32 浣嶏紝閫?DCacheTop 澶勭悊 MMIO 璇汇€?|
| `mtimeHi` | `UInt(32.W)` | Output | `mtime` 楂?32 浣嶏紝閫?DCacheTop 澶勭悊 MMIO 璇汇€?|
| `prefetchCtrl` | `UInt(32.W)` | Output | 棰勫彇鎺у埗 CSR銆俙bit0` 鎺у埗 next-line锛宍bit1` 鎺у埗 stride銆?|

### CSR 鍦板潃绾﹀畾

| CSR | 鍦板潃 | 璇存槑 |
| --- | --- | --- |
| `mcycle` | `0xB00` | 鍛ㄦ湡璁℃暟浣?32 浣嶃€?|
| `mcycleh` | `0xB80` | 鍛ㄦ湡璁℃暟楂?32 浣嶃€?|
| `minstret` | `0xB02` | 閫€浼戞寚浠よ鏁颁綆 32 浣嶃€?|
| `mcountinhibit` | `0x320` | 璁℃暟鍣ㄦ姂鍒舵帶鍒躲€?|
| `misa` | `0x301` | ISA 淇℃伅锛屽彧璇汇€?|
| `prefetchCtrl` | `0x7C0` | 鑷畾涔夐鍙栨帶鍒?CSR銆?|

### 棰勫彇鎺у埗浣?
| 浣?| 鍚嶇О | 璇存槑 |
| --- | --- | --- |
| `prefetchCtrl(0)` | `nextLinePrefetchEn` | 涓?1 鏃跺厑璁?next-line 棰勫彇鍣ㄥ彂璧疯姹傘€?|
| `prefetchCtrl(1)` | `stridePrefetchEn` | 涓?1 鏃跺厑璁?stride 棰勫彇鍣ㄥ彂璧疯姹傘€?|

### `csrrw` 璇箟

涓荤嚎鑷冲皯闇€瑕佹敮鎸侊細

```text
old = CSR[csr]
CSR[csr] = rs1
rd = old
```

EX 绾у簲鎶?`rs1Data` 浣滀负 `wdata`锛孋SRFile 杈撳嚭 `oldData`锛岄殢鍚?WB 閫氳繃 `WB_CSR` 鍐欏洖 `rd`銆?
### CSR 鍙屽彂闄愬埗

鐢变簬褰撳墠 CSRFile 鍙湁涓€涓绔彛鍜屼竴涓啓璇锋眰绔彛锛屽熀纭€椤哄簭鏍稿簲鍦?ID 绾ч檺鍒跺悓鍛ㄦ湡鏈€澶氫竴鏉?CSR 鎸囦护杩涘叆 EX銆傝嫢妲?0 鍜屾Ы 1 閮芥槸 CSR 鎸囦护锛屽簲鍙彂灏勬Ы 0锛屾Ы 1 涓嬩竴鍛ㄦ湡閲嶆柊灏濊瘯銆?
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
```
package riscv

import chisel3._
import chisel3.util._

// BTBEntry 瀹氫箟鍦ㄦ澶勶紝BPU / BPU_RAS / TAGE 鍏辩敤锛堝悓灞?riscv 鍖咃級銆?class BTBEntry extends Bundle {
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

  // spec 搂8.3锛氭墍鏈?2-bit 楗卞拰璁℃暟鍣ㄥ浣嶄负寮变笉璺宠浆 2'b01銆?  val choiceTable = RegInit(VecInit(Seq.fill(512)(1.U(2.W))))
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
```
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

// BTBEntry 瀹氫箟鍦?BPU.scala锛屽悓灞?riscv 鍖咃紝鐩存帴澶嶇敤銆?
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

  // spec 搂8.3锛氭墍鏈?2-bit 楗卞拰璁℃暟鍣ㄥ浣嶄负寮变笉璺宠浆 2'b01銆?  val choiceTable = RegInit(VecInit(Seq.fill(512)(1.U(2.W))))
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
```
package riscv

import chisel3._
import chisel3.util._

// ============================================================
//  NextLinePrefetcher锛歂ext-line 棰勫彇鍣紙spec 搂6.6锛?//
//  淇鐐癸紙鐩告瘮鍘熺増锛夛細
//   1. 棰勫彇鍦板潃 +4 鈫?涓嬩竴鏉?64B Cache Line 鍩哄湴鍧€锛?64锛屽榻愶級
//   2. 鎺ュ彛瀵归綈 ICacheTop 鐨?pfReqValid / pfReqReady / pfReqAddr
//   3. 鏂板 prefetchEn锛圕SR prefetch_ctrl[0]锛夊紑鍏?//   4. 鎬荤嚎蹇欙紙pfReqReady=0锛夋椂鏈璇锋眰涓㈠純锛屼笉閲嶈瘯
//
//  鍙戣捣鏉′欢锛?//    prefetchEn && cacheHit && !cacheStall
//
//  涓嶅彂璧锋潯浠讹紙浠讳竴鎴愮珛鍗充涪寮冿級锛?//    - prefetchEn=0锛圕SR 鍏抽棴锛?//    - cacheStall=1锛圛-Cache 姝ｅ湪澶勭悊 miss锛屾€荤嚎蹇欙級
//    - pfReqReady=0锛圛CacheTop FSM 涓嶅湪 sIdle 鎴栨澶勭悊 miss锛?//                   ICacheMissFSM 浼氳嚜鍔?drop锛屾棤闇€澶栭儴閲嶈瘯锛?//
//  娉ㄦ剰锛氱洰鏍囪鏄惁宸插湪 Cache 鐢?ICacheTop 鍐呴儴鐨?IHitTest 璐熻矗
//  鍒ゆ柇鈥斺€旇嫢宸插懡涓垯 FSM 涓嶄細鐪熸鍙戝嚭鍐呭瓨鎬荤嚎璇锋眰锛涘閮ㄩ鍙栧櫒
//  鏃犻渶锛堜篃鏃犳硶锛夌嫭绔嬫煡璇?TagArray锛屾晠 lineInCache 妫€鏌ョ渷鐣ャ€?// ============================================================
class NextLinePrefetcher extends Module {
  val io = IO(new Bundle {
    // 鈹€鈹€ 鏉ヨ嚜 ICacheTop / IFStage 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
    // 褰撳墠鍛戒腑琛岀殑鍩哄湴鍧€锛堝嵆鍙栨寚 PC锛岀敤浜庤绠椾笅涓€琛屽湴鍧€锛?    val currAddr   = Input(UInt(32.W))
    val cacheHit   = Input(Bool())   // ICacheTop.respValid
    val cacheStall = Input(Bool())   // ICacheTop.missOut

    // 鈹€鈹€ 鏉ヨ嚜 CSRFile 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
    val prefetchEn = Input(Bool())   // CSR prefetch_ctrl[0]

    // 鈹€鈹€ 涓?ICacheTop 鐨勬彙鎵嬫帴鍙?鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
    // 锛堢洿杩?ICacheTop.pfReqValid / pfReqReady / pfReqAddr锛?    val pfReqValid = Output(Bool())
    val pfReqReady = Input(Bool())   // ICacheTop 鎻℃墜搴旂瓟
    val pfReqAddr  = Output(UInt(32.W))
  })

  // 涓嬩竴鏉?Cache Line 鍩哄湴鍧€锛氬綋鍓嶅湴鍧€鎸?64B 瀵归綈鍚?+64
  //   nextLineAddr = {currAddr[31:6] + 1, 6'b0}
  val nextLineAddr = Cat(io.currAddr(31, 6) + 1.U, 0.U(6.W))

  // 鍙湁鍛戒腑涓旀棤 miss stall 鏃舵墠灏濊瘯棰勫彇
  val wantPrefetch = io.prefetchEn && io.cacheHit && !io.cacheStall

  io.pfReqValid := wantPrefetch
  io.pfReqAddr  := nextLineAddr
  // pfReqReady=0 鏃舵湰娆¤姹傝 ICacheMissFSM 闈欓粯涓㈠純锛坧fReqReady
  // 浠呭湪 FSM sIdle 涓旀棤 miss 鏃剁疆楂橈級锛涢鍙栧櫒鏈韩涓嶉渶瑕侀噸璇曢€昏緫銆?}
```

## .\src\main\Frontend\PcGen.scala
```
package riscv

import chisel3._
import chisel3.util._

// ============================================================
//  PcGen锛歅C 鐢熸垚妯″潡锛坰pec 搂3.1 / 搂3.1.1锛?//
//  淇鐐癸紙鐩告瘮鍘熺増锛夛細
//   1. 椤哄簭姝ラ暱 +4 鈫?+N*4锛堝弻鍙戝皠 N=2锛屽嵆 +8锛?//   2. 鏂板 BPU 棰勬祴璺宠浆杈撳叆锛堜紭鍏堢骇 4锛?//   3. 瀹炵幇 EX > ID(JAL) > Stall > BPU > Seq 瀹屾暣浠茶
//   4. redirect 淇″彿浼樺厛绾ч珮浜?stall锛坒lush 闅愬惈涓嶄繚鎸?PC锛?//   5. PC 澶嶄綅鑷?0x80000000锛坰pec 搂8.3锛?//
//  涓嬩竴 PC 浠茶浼樺厛绾э紙楂?鈫?浣庯級锛?//   1. exRedirectValid  鈹€鈹€ EX 绾ф敼鍚戯紙鍒嗘敮璇娴?/ JALR锛? 2 鍛ㄦ湡 flush
//   2. idRedirectValid  鈹€鈹€ ID 绾?JAL 鏀瑰悜                  1 鍛ㄦ湡 flush
//   3. stall            鈹€鈹€ 娴佹按绾垮仠椤匡紝淇濇寔 PC 涓嶅彉
//   4. bpuPredTaken     鈹€鈹€ BPU 棰勬祴璺宠浆                    0 鍛ㄦ湡
//   5. 榛樿             鈹€鈹€ 椤哄簭鍙栨寚 PC + N*4
//
//  浜掓枼淇濊瘉锛欽ALR 涓庡垎鏀棰勬祴鍏辩敤 exRedirectValid锛?//           鍚屼竴鍛ㄦ湡涓嶄細骞跺彂锛況edirect 浼樺厛浜?stall锛?//           婊¤冻 spec 搂3.1.2 "flush 浼樺厛浜?stall"銆?// ============================================================
class PcGen(
    val N: Int      = 2,
    val resetVec: Long = 0x80000000L
) extends Module {

  val io = IO(new Bundle {
    // 鈹€鈹€ EX 绾ф敼鍚戯紙鏈€楂樹紭鍏堢骇锛夆攢鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
    val exRedirectValid = Input(Bool())
    val exRedirectPc    = Input(UInt(32.W))

    // 鈹€鈹€ ID 绾?JAL 鏀瑰悜 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
    val idRedirectValid = Input(Bool())
    val idRedirectPc    = Input(UInt(32.W))

    // 鈹€鈹€ BPU 棰勬祴璺宠浆 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
    val bpuPredTaken  = Input(Bool())
    val bpuPredTarget = Input(UInt(32.W))

    // 鈹€鈹€ 娴佹按绾垮仠椤?鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
    // stallIf 鏉ヨ嚜 HazardUnit锛汭-Cache miss 浜х敓鐨?icacheStall
    // 涔熼渶閫氳繃 HazardUnit 姹囨€诲悗閫佸叆姝や俊鍙?    val stallIf = Input(Bool())
    val fetchSlot1Valid = Input(Bool())

    // 鈹€鈹€ 杈撳嚭 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
    val currPc  = Output(UInt(32.W)) // 褰撳墠 PC锛堚啋 debugPc / 棰勫彇鍣?currAddr锛?    val pcFetch = Output(UInt(32.W)) // 閫佸線 I-Cache 鐨勫彇鎸囧湴鍧€
  })

  val pcReg  = RegInit(resetVec.U(32.W))
  val nextPc = Wire(UInt(32.W))
  val seqStep = Mux(io.fetchSlot1Valid, (N * 4).U(32.W), 4.U(32.W))
  private def canonicalPc(pc: UInt): UInt = {
    Mux(pc(31), pc, pc | resetVec.U(32.W))
  }

  // 鈹€鈹€ 浼樺厛绾т徊瑁侊紙when 閾撅紝楂樹紭鍏堢骇鍦ㄥ墠锛夆攢鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  when(io.exRedirectValid) {
    // 鏈€楂橈細EX 绾ф敼鍚戯紙鍚?flush 璇箟锛岃鐩?stall锛?    nextPc := io.exRedirectPc
  }.elsewhen(io.idRedirectValid) {
    // 娆￠珮锛欽AL 鍦?ID 绾ф彁鍓嶆敼鍚戯紙1 鍛ㄦ湡 flush锛?    nextPc := io.idRedirectPc
  }.elsewhen(io.stallIf) {
    // 鍋滈】锛氫繚鎸佸綋鍓?PC
    nextPc := pcReg
  }.elsewhen(io.bpuPredTaken) {
    // BPU 棰勬祴璺宠浆锛氶噰鐢ㄩ娴嬬洰鏍?    nextPc := io.bpuPredTarget
  }.otherwise {
    // 榛樿锛氶『搴忓彇鎸囥€傝嫢 PC 浠?4B 瀵归綈锛屽綋鍓嶅寘鍙兘鏈夋晥杩斿洖 slot0锛?    // 涓嬩竴鎷嶅繀椤?PC+4锛屼笉鑳芥寜鍙屽彂瀹藉害璺宠繃 slot1 浣嶇疆鐨勬寚浠ゃ€?    nextPc := pcReg + seqStep
  }

  pcReg := canonicalPc(nextPc)

  io.currPc  := pcReg
  io.pcFetch := pcReg
}
```

## .\src\main\Frontend\StrideStreamPrefetcher.scala
```
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
```
package riscv

import chisel3._
import chisel3.util._

// 鈹€鈹€ Tagged-table entry for T1鈥揟4 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
class TAGEEntry extends Bundle {
  val valid = Bool()
  val tag   = UInt(8.W)   // PC[17:10]
  val ctr   = UInt(3.W)   // 3-bit saturating direction counter
  val u     = UInt(2.W)   // 2-bit usefulness counter
}

// 鈹€鈹€ TAGE: TAgged GEometric-length predictor (simplified T0 + T1鈥揟4) 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
// Exposes the same IO as BPU (搂3.3 / 搂3.4) so it can be dropped in as a
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

  // 鈹€鈹€ Global History Register (64-bit) 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  val ghr = RegInit(0.U(64.W))

  // 鈹€鈹€ Prediction tables 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  // T0: base table 鈥?1024 entries, PC[11:2] index, 2-bit sat-counter
  val t0 = RegInit(VecInit(Seq.fill(1024)(1.U(2.W))))  // init: weakly not-taken

  // T1鈥揟4: tagged tables 鈥?256 entries each
  val t1 = RegInit(VecInit(Seq.fill(256)(0.U.asTypeOf(new TAGEEntry))))
  val t2 = RegInit(VecInit(Seq.fill(256)(0.U.asTypeOf(new TAGEEntry))))
  val t3 = RegInit(VecInit(Seq.fill(256)(0.U.asTypeOf(new TAGEEntry))))
  val t4 = RegInit(VecInit(Seq.fill(256)(0.U.asTypeOf(new TAGEEntry))))

  // BTB: 256 entries, same spec as BPU 搂3.3
  val btb = RegInit(VecInit(Seq.fill(256)(0.U.asTypeOf(new BTBEntry))))

  // 鈹€鈹€ Helpers 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
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

  // 鈹€鈹€ Query-side indices 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  val qBase   = io.queryPc(9, 2)          // 8-bit base (PC[9:2]) for T1鈥揟4
  val qT0Idx  = io.queryPc(11, 2)         // 10-bit index for T0 (1024 entries)
  val qT1Idx  = qBase ^ ghr(7, 0)         // XOR with GHR[ 7: 0]
  val qT2Idx  = qBase ^ foldGHR(16)       // XOR with fold(GHR[15: 0], 8)
  val qT3Idx  = qBase ^ foldGHR(32)       // XOR with fold(GHR[31: 0], 8)
  val qT4Idx  = qBase ^ foldGHR(64)       // XOR with fold(GHR[63: 0], 8)
  val qTag    = io.queryPc(17, 10)         // 8-bit tag: PC[17:10]
  val qBtbIdx = io.queryPc(9, 2)          // 8-bit BTB index: PC[9:2]

  // 鈹€鈹€ Table reads (prediction side) 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
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

  // 鈹€鈹€ Provider / altpred selection (prediction side) 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
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

  // 鈹€鈹€ Update-side indices (recomputed from updatePc + current GHR) 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
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

  // 鈹€鈹€ Periodic u-bit reset (every 2^18 鈮?256K cycles) 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
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

  // 鈹€鈹€ Update logic (EX-stage feedback, takes priority over periodic reset) 鈹€鈹€
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

  // 鈹€鈹€ RAS: identical implementation to BPU 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
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
```
package icache

import chisel3._
import chisel3.util._
import parameterized_cache.{CacheParams, MemBusIO}

// ============================================================
//  ICacheMissFSM锛欼-Cache 缂哄け涓庨鍙栫姸鎬佹満
//
//  涓?DCacheMissFSM 鐨勬牳蹇冨尯鍒細
//   - 鏃犺剰琛屽啓鍥烇細鍘绘帀 sCheck / sWbReq / sWbResp 涓変釜鐘舵€?//     Miss 鈫?鐩存帴鍥炲～锛屾棤闇€妫€鏌?dirty / evict 鏃ц鍐呭
//   - 鏀寔鍚庡彴棰勫彇锛坰Prefill / sPrefillResp / sPrefillDone锛夛細
//     浠呭湪 sIdle 涓旀棤 miss 鏃跺鐞嗭紱棰勫彇鏈熼棿 stall=0锛屾祦姘寸嚎
//     缁х画鎺ㄨ繘锛堝懡涓師鏉ュ凡缂撳瓨鐨勮锛?//   - stall 浠呭湪澶勭悊鐪熸 miss锛坰RefillReq/Resp/sDone锛夋椂缃珮
//
//  鐘舵€佽浆鎹細
//
//   sIdle 鈹€[missValid]鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈻?sRefillReq
//     鈹?                                  鈹?req.fire
//     鈹?                              sRefillResp
//     鈹?                                  鈹?resp.fire
//     鈹?                    lastWord? 鈹€鈹€鈹€鈹€鈹?//     鈹?                 No 鈫?      Yes 鈫?//     鈹?            sRefillReq        sDone 鈹€鈹€鈻?sIdle锛坮efillDone=1锛?//     鈹?//     鈹斺攢[pfReqValid & !missValid]鈹€鈹€鈻?sPrefill
//                                        鈹?req.fire
//                                    sPrefillResp
//                                        鈹?resp.fire
//                          lastWord? 鈹€鈹€鈹€鈹€鈹?//                       No 鈫?      Yes 鈫?//                   sPrefill       sPrefillDone 鈹€鈹€鈻?sIdle锛坧refillDone=1锛?// ============================================================
class ICacheMissFSM(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    // 鈹€鈹€ Miss 杈撳叆锛堟潵鑷?ICacheTop锛屼紭鍏堢骇楂樹簬棰勫彇锛夆攢鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
    val missValid = Input(Bool())
    val missTag   = Input(UInt(p.TAG_W.W))
    val missIdx   = Input(UInt(p.INDEX_W.W))
    val evictWay  = Input(UInt(p.WAY_W.W))   // PLRU 鍦?missIdx 涓婄殑椹遍€愯矾

    // 鈹€鈹€ 棰勫彇杈撳叆锛堜綆浼樺厛绾э紝浠?sIdle 涓旀棤 miss 鏃舵帴鍙楋級鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
    val pfReqValid  = Input(Bool())
    val pfReqReady  = Output(Bool())
    val pfReqTag    = Input(UInt(p.TAG_W.W))
    val pfReqIdx    = Input(UInt(p.INDEX_W.W))
    val pfEvictWay  = Input(UInt(p.WAY_W.W)) // PLRU 鍦?pfIdx 涓婄殑椹遍€愯矾

    // 鈹€鈹€ 澶栭儴鍐呭瓨鎬荤嚎 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
    val mem = new MemBusIO(p)

    // 鈹€鈹€ 鍥炲～杈撳嚭 鈫?ICacheTop 鈫?ITagArray / IDataArray 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
    val refillEn    = Output(Bool())            // 褰撴媿 refillData 鏈夋晥锛屽啓 DataArray
    val refillWay   = Output(UInt(p.WAY_W.W))
    val refillIdx   = Output(UInt(p.INDEX_W.W))
    val refillWord  = Output(UInt(p.WORD_CNT_W.W))
    val refillData  = Output(UInt(p.DATA_WIDTH.W))
    val refillTag   = Output(UInt(p.TAG_W.W))
    val refillDone  = Output(Bool())            // miss 鍥炲～瀹屾垚锛孴agArray 鍐欏叆
    val prefillDone = Output(Bool())            // 棰勫彇鍥炲～瀹屾垚锛孴agArray 鍐欏叆

    // 鈹€鈹€ 鐘舵€佽緭鍑?鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
    val stall  = Output(Bool())  // 浠?miss 娴佺▼缃珮锛岄鍙栦笉 stall
    val isIdle = Output(Bool())  // 渚?ICacheTop 鍋?PLRU idx MUX
  })

  // 鈹€鈹€ 鐘舵€佸畾涔?鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  val sIdle :: sRefillReq :: sRefillResp :: sRefillWrite :: sDone :: sPrefill :: sPrefillResp :: sPrefillWrite :: sPrefillDone :: Nil = Enum(9)

  val state     = RegInit(sIdle)
  val nextState = WireDefault(state)

  // 鈹€鈹€ 閿佸瓨瀛楁 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  val wTag    = Reg(UInt(p.TAG_W.W))
  val wIdx    = Reg(UInt(p.INDEX_W.W))
  val wWay    = Reg(UInt(p.WAY_W.W))
  val wordCnt = Reg(UInt(p.WORD_CNT_W.W))
  val lineBuf = Reg(Vec(p.LINE_WORDS, UInt(p.DATA_WIDTH.W)))

  val lastWord = wordCnt === (p.LINE_WORDS - 1).U

  // 鈹€鈹€ 鐘舵€佽浆鎹?鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
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

  // 鈹€鈹€ 閿佸瓨閫昏緫锛坰Idle 鍑哄彛鏃舵崟鑾凤級 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
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

  // 鈹€鈹€ 鍐呭瓨鎬荤嚎鍦板潃锛歿tag, idx, wordCnt, 2'b00} 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  // byteOffW = OFFSET_W - WORD_CNT_W = 6 - 4 = 2锛?2 浣嶅瓧鍐呭瓧鑺傚亸绉诲搴︼級
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

  // 鈹€鈹€ 鍥炲～杈撳嚭 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  io.refillEn    := state === sRefillWrite || state === sPrefillWrite
  io.refillWay   := wWay
  io.refillIdx   := wIdx
  io.refillWord  := wordCnt
  io.refillData  := lineBuf(wordCnt)
  io.refillTag   := wTag
  io.refillDone  := state === sDone
  io.prefillDone := state === sPrefillDone

  // 鈹€鈹€ 鐘舵€佽緭鍑?鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  // stall 浠呭湪澶勭悊鐪熸 miss 鐨勪笁涓姸鎬佸唴缃珮锛涢鍙栦笉褰卞搷娴佹按绾?  io.stall  := state === sRefillReq || state === sRefillResp || state === sRefillWrite || state === sDone
  io.isIdle := state === sIdle

  // 棰勫彇鎻℃墜锛氫粎鍦?sIdle 涓旀棤 miss 鏃舵帴鍙?  io.pfReqReady := (state === sIdle) && !io.missValid
}
```

## .\src\main\Icache\ICacheParams.scala
```
package icache

import chisel3._
import chisel3.util._
import parameterized_cache.CacheParams

// ============================================================
//  I-Cache 涓撶敤 TagEntry锛氬彧璇?Cache 鏃犻渶 dirty 瀛楁銆?//  鍏朵綑鍑犱綍鍙傛暟锛圫ET_NUM / WAY_NUM / LINE_WORDS 绛夛級
//  鐩存帴澶嶇敤 parameterized_cache.CacheParams銆?// ============================================================
class ITagEntry(p: CacheParams) extends Bundle {
  val valid = Bool()
  val tag   = UInt(p.TAG_W.W)
}
```

## .\src\main\Icache\ICacheTop.scala
```
package icache

import chisel3._
import chisel3.util._
import parameterized_cache.{CacheParams, ICachePerfEvents, MemBusIO}
import parameterized_cache.TreePLRU

// ============================================================
//  ICacheTop锛欼-Cache 椤跺眰
//
//  瀛愭ā鍧楄繛绾挎€昏锛?//
//   addr 鈹€鈻?addrTag 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈻?IHitTest.tag
//         鈻?addrIdx  鈹€鈻?ITagArray.idx
//                     鈻?IDataArray.idx
//                     鈻?TreePLRU.idx锛堟甯革級
//         鈻?addrWoff 鈹€鈻?IDataArray.wordsoff
//                     鈻?InstSelect.wordsoff
//
//   ITagArray.tagData 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈻?IHitTest.tagData
//   IHitTest.isHit 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈻?TreePLRU.updateEn锛堝懡涓矾寰勶級
//   IHitTest.hitWay 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈻?TreePLRU.updateWay
//                                            鈻?InstSelect.hitWay
//   IHitTest.missValid 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈻?ICacheMissFSM.missValid
//                                            鈻?io.missOut锛圤R FSM.stall锛?//
//   ICacheMissFSM.refillEn   鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈻?IDataArray.refillDataEn
//   ICacheMissFSM.refillDone 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ 鈻?ITagArray.refillTagEn锛坢iss锛?//   ICacheMissFSM.prefillDone 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈻?ITagArray.refillTagEn锛堥鍙栵級
//   ICacheMissFSM.refillWay  鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈻?IDataArray/ITagArray.refillWay
//                                            鈻?TreePLRU.updateWay锛堝洖濉矾寰勶級
//   ICacheMissFSM.stall 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈻?io.missOut锛圤R hitTest.missValid锛?//
//   IDataArray.rawData0/1 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈻?InstSelect.rawData0/1
//   InstSelect.inst0/1 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈻?io.insts(0/1)
//   InstSelect.slot1Valid 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈻?io.instValids(1) 闂ㄦ帶
//
//  PLRU idx MUX 绛栫暐锛堣涓嬫柟璇︾粏璇存槑锛夛細
//   - 姝ｅ父鍙栨寚锛歱lru.idx = addrIdx
//   - FSM idle + 棰勫彇璇锋眰 + 鏃?miss锛歱lru.idx = pfIdx
//     姝ゆ椂 plru.evictWay 鍗充负 pfIdx 涓婄殑椹遍€愯矾锛屼紶缁?FSM 浣?pfEvictWay
// ============================================================
class ICacheTop(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    // 鈹€鈹€ CPU / IF 绾ф帴鍙?鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
    val addr       = Input(UInt(p.ADDR_WIDTH.W))
    val valid      = Input(Bool())    // 鍙栨寚璇锋眰鏈夋晥
    val flush      = Input(Bool())    // 娴佹按绾?flush锛屾竻绌烘墍鏈?valid 浣?
    // 鍙屾Ы鍙栨寚鍝嶅簲
    val insts      = Output(Vec(2, UInt(p.DATA_WIDTH.W)))
    val instValids = Output(Vec(2, Bool()))
    val respValid  = Output(Bool())   // 鍛戒腑涓?bundle 鏁翠綋鏈夋晥
    val missOut    = Output(Bool())   // 鏈懡涓俊鍙凤紝楂樼數骞虫湡闂存祦姘寸嚎搴?stall

    // 鈹€鈹€ 棰勫彇鎺ュ彛锛堟潵鑷?NextLinePrefetcher锛夆攢鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
    // pfReqAddr 椤讳负 64B 瀵归綈鍦板潃锛堣 spec 搂6.6锛?    val pfReqValid = Input(Bool())
    val pfReqReady = Output(Bool())
    val pfReqAddr  = Input(UInt(p.ADDR_WIDTH.W))

    // 鈹€鈹€ 澶栭儴鍐呭瓨鎬荤嚎 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
    val mem = new MemBusIO(p)

    // 鈹€鈹€ 鎬ц兘浜嬩欢锛堝崟鍛ㄦ湡鑴夊啿锛屼粎鐢ㄤ簬缁熻锛夆攢鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
    val perf = Output(new ICachePerfEvents)
  })

  // 鈹€鈹€ 鍦板潃鍒嗚В 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  // 榛樿鍙傛暟锛? KB, 4-way, 64B line锛夛細
  //   offset[5:0]  鈫?addr[5:0]
  //   wordsoff[5:2]鈫?addr[5:2]锛? bit锛?6 words/line锛?  //   idx[10:6]    鈫?addr[10:6]锛? bit锛?2 sets锛?  //   tag[31:11]   鈫?addr[31:11]锛?1 bit锛?  val addrTag  = io.addr(p.ADDR_WIDTH - 1,          p.OFFSET_W + p.INDEX_W)
  val addrIdx  = io.addr(p.OFFSET_W + p.INDEX_W - 1, p.OFFSET_W)
  val addrWoff = io.addr(p.OFFSET_W - 1,             2)   // word offset = addr[5:2]

  // 棰勫彇鍦板潃鍒嗚В锛坧fReqAddr 搴斿凡 64B 瀵归綈锛屼綆 6 浣嶄负 0锛?  val pfTag = io.pfReqAddr(p.ADDR_WIDTH - 1,          p.OFFSET_W + p.INDEX_W)
  val pfIdx = io.pfReqAddr(p.OFFSET_W + p.INDEX_W - 1, p.OFFSET_W)

  // 鈹€鈹€ 瀛愭ā鍧椾緥鍖?鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  val tagArray  = Module(new ITagArray(p))
  val dataArray = Module(new IDataArray(p))
  val plru      = Module(new TreePLRU(p))
  val hitTest   = Module(new IHitTest(p))
  val instSel   = Module(new InstSelect(p))
  val missFsm   = Module(new ICacheMissFSM(p))

  // 鈹€鈹€ PLRU idx MUX 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  // 褰?FSM 绌洪棽銆佹湁棰勫彇璇锋眰涓斿綋鍓嶆棤 miss 鏃讹紝灏?PLRU 鎸囧悜 pfIdx
  // 浠ヨ幏鍙栨纭殑棰勫彇椹遍€愯矾锛涘惁鍒欐寚鍚?addrIdx 鏈嶅姟姝ｅ父鍙栨寚/鏇存柊
  val plruQueryForPf = missFsm.io.isIdle &&
                       io.pfReqValid &&
                       !hitTest.io.missValid
  plru.io.idx := Mux(plruQueryForPf, pfIdx, addrIdx)

  // 鈹€鈹€ IHitTest 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  hitTest.io.tagData  := tagArray.io.tagData
  hitTest.io.tag      := addrTag
  hitTest.io.reqValid := io.valid

  val isHit  = hitTest.io.isHit
  val hitWay = hitTest.io.hitWay

  // 鈹€鈹€ ITagArray 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  tagArray.io.flush       := io.flush
  tagArray.io.idx         := addrIdx
  // miss 鍜岄鍙栭兘閫氳繃鍚屼竴缁?refill 淇″彿鍐欏叆 TagArray锛坱ime-shared锛?  tagArray.io.refillTagEn := missFsm.io.refillDone || missFsm.io.prefillDone
  tagArray.io.refillWay   := missFsm.io.refillWay
  tagArray.io.refillIdx   := missFsm.io.refillIdx
  tagArray.io.refillTag   := missFsm.io.refillTag

  // 鈹€鈹€ TreePLRU 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  // 鏇存柊鏃舵満锛?  //   1. 姝ｅ父鍛戒腑锛坧lruQueryForPf=false锛屽惁鍒?idx 宸插垏鎹㈠埌 pfIdx锛?  //      姝ゆ椂涓嶈兘鎸?addrIdx 鏇存柊 PLRU鈥斺€旀鐜囨瀬浣庯紝鍙帴鍙楀皬璇樊锛?  //   2. miss 鍥炲～瀹屾垚
  //   3. 棰勫彇鍥炲～瀹屾垚锛堟鏃?plru.idx 搴斿湪 pfIdx锛涚敱浜?sPrefillDone
  //      涔熸槸鍗曟媿鑴夊啿锛屼笌 plruQueryForPf 閫昏緫涓嶅啿绐侊級
  plru.io.updateEn  := (isHit && io.valid && !plruQueryForPf) ||
                        missFsm.io.refillDone  ||
                        missFsm.io.prefillDone
  plru.io.updateWay := Mux(
    missFsm.io.refillDone || missFsm.io.prefillDone,
    missFsm.io.refillWay,
    hitWay
  )

  // 鈹€鈹€ IDataArray 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  dataArray.io.idx          := addrIdx
  dataArray.io.wordsoff     := addrWoff
  dataArray.io.refillDataEn := missFsm.io.refillEn
  dataArray.io.refillWay    := missFsm.io.refillWay
  dataArray.io.refillIdx    := missFsm.io.refillIdx
  dataArray.io.refillWord   := missFsm.io.refillWord
  dataArray.io.refillData   := missFsm.io.refillData

  // 鈹€鈹€ InstSelect 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  instSel.io.hitWay   := hitWay
  instSel.io.wordsoff := addrWoff
  instSel.io.rawData0 := dataArray.io.rawData0
  instSel.io.rawData1 := dataArray.io.rawData1

  // 鈹€鈹€ ICacheMissFSM 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  missFsm.io.missValid := hitTest.io.missValid
  missFsm.io.missTag   := addrTag
  missFsm.io.missIdx   := addrIdx
  // evictWay锛歱lruQueryForPf=false 鏃?plru.io.evictWay 瀵瑰簲 addrIdx锛屾纭?  missFsm.io.evictWay  := plru.io.evictWay

  // 棰勫彇鎺ュ彛锛氬睆钄芥帀涓?miss 鍚屽懆鏈熺殑棰勫彇璇锋眰锛坢iss 浼樺厛绾ф洿楂橈級
  missFsm.io.pfReqValid := io.pfReqValid && !hitTest.io.missValid
  missFsm.io.pfReqTag   := pfTag
  missFsm.io.pfReqIdx   := pfIdx
  // pfEvictWay锛歱lruQueryForPf=true 鏃?plru.io.evictWay 瀵瑰簲 pfIdx锛屾纭?  missFsm.io.pfEvictWay := plru.io.evictWay

  io.pfReqReady := missFsm.io.pfReqReady

  // 鍐呭瓨鎬荤嚎鐩磋繛
  io.mem <> missFsm.io.mem

  // 鈹€鈹€ 棰勫彇 useful 缁熻鏍囪 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
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

  // 鈹€鈹€ 瀵瑰杈撳嚭 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  io.insts(0)      := instSel.io.inst0
  io.insts(1)      := instSel.io.inst1
  // slot0 鏈夋晥锛氬懡涓笖鏈媿鍙栨寚璇锋眰鏈夋晥
  io.instValids(0) := isHit && io.valid
  // slot1 鏈夋晥锛氬湪 slot0 鏈夋晥鐨勫熀纭€涓婏紝杩橀』鏈埌琛屽熬
  io.instValids(1) := isHit && io.valid && instSel.io.slot1Valid
  io.respValid     := isHit && io.valid

  // missOut 淇濇寔楂樼數骞崇洿鑷冲洖濉畬鎴愶細
  //   - hitTest.io.missValid锛氭湰鍛ㄦ湡鏂版娴嬪埌 miss锛團SM 杩樻湭鍚姩锛?  //   - missFsm.io.stall锛欶SM 姝ｅ湪澶勭悊 miss锛坰RefillReq/Resp/sDone锛?  // 涓よ€?OR 瑕嗙洊 miss 鐨勫畬鏁寸敓鍛藉懆鏈?  io.missOut := hitTest.io.missValid || missFsm.io.stall

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
//  浼寸敓瀵硅薄锛氭彁渚涢粯璁ゅ弬鏁板疄渚嬶紝鏂逛究椤跺眰渚嬪寲
// ============================================================
object ICacheTop {
  // 榛樿锛? KB锛?-way锛?4 B line锛?2-bit 鏁版嵁锛?2-bit 鍦板潃
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
```
package icache

import chisel3._
import chisel3.util._
import parameterized_cache.CacheParams

// ============================================================
//  IDataArray锛欼-Cache 鏁版嵁闃靛垪
//
//  涓?D-Cache 鐗?DataArray 鐨勫尯鍒細
//   - 鏃犲懡涓啓璺緞锛堟棤 hitWen / hitWay / wdata / wmask锛?//   - 鏃?evictLine 杈撳嚭锛圛-Cache 椹遍€愮洿鎺ヤ涪寮冿紝鏃犻渶鍐欏洖锛?//   - 澧炲姞鍙屾Ы杈撳嚭 rawData0 / rawData1锛?//       rawData0 = line[wordsoff]     鈫?slot 0 鎸囦护瀛?//       rawData1 = line[wordsoff + 1] 鈫?slot 1 鎸囦护瀛楋紙琛屽熬瓒婄晫鏃?//                 InstSelect 璐熻矗缃?slot1Valid=0锛屾澶勪笉鍒ゆ柇杈圭晫锛?// ============================================================
class IDataArray(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    // 缁勫悎璇荤鍙?    val idx      = Input(UInt(p.INDEX_W.W))
    val wordsoff = Input(UInt(p.WORD_CNT_W.W))   // slot 0 瀛楀亸绉?
    // 鍥炲～鍐欑鍙ｏ紙鐢?ICacheMissFSM 閫愬瓧椹卞姩锛?    val refillDataEn = Input(Bool())
    val refillWay    = Input(UInt(p.WAY_W.W))
    val refillIdx    = Input(UInt(p.INDEX_W.W))
    val refillWord   = Input(UInt(p.WORD_CNT_W.W))
    val refillData   = Input(UInt(p.DATA_WIDTH.W))

    // 鍙屾Ы杈撳嚭锛氬洓璺悇鑷殑 slot0 / slot1 鎸囦护瀛?    val rawData0 = Output(Vec(p.WAY_NUM, UInt(p.DATA_WIDTH.W)))
    val rawData1 = Output(Vec(p.WAY_NUM, UInt(p.DATA_WIDTH.W)))
  })

  // 甯冨眬锛歸ay 脳 set 脳 word锛屽瘎瀛樺櫒鐩存帴绱㈠紩鍗充负缁勫悎璇?  val dArray = Reg(Vec(p.WAY_NUM, Vec(p.SET_NUM, Vec(p.LINE_WORDS, UInt(p.DATA_WIDTH.W)))))

  // slot1 瀛楀亸绉伙細wordsoff + 1锛屾埅鏂埌 WORD_CNT_W 浣嶏紙琛屽熬鏃惰嚜鐒跺洖缁曪紱
  // 鍥炵粫鍚庣殑鏁版嵁鏃犳晥锛岀敱 InstSelect.slot1Valid = 0 灞忚斀锛?  val nextOff = (io.wordsoff + 1.U)(p.WORD_CNT_W - 1, 0)

  for (way <- 0 until p.WAY_NUM) {
    io.rawData0(way) := dArray(way)(io.idx)(io.wordsoff)
    io.rawData1(way) := dArray(way)(io.idx)(nextOff)
  }

  // 鍥炲～鍐欙細姣忔媿鍐欎竴涓瓧锛岀敱 FSM 椹卞姩 refillWord 浠?0 璁℃暟鑷?LINE_WORDS-1
  when(io.refillDataEn) {
    dArray(io.refillWay)(io.refillIdx)(io.refillWord) := io.refillData
  }
}
```

## .\src\main\Icache\IHitTest.scala
```
package icache

import chisel3._
import chisel3.util._
import parameterized_cache.CacheParams

// ============================================================
//  IHitTest锛欼-Cache 鍛戒腑妫€娴?//
//  涓?D-Cache 鐗?HitTest 鐨勫尯鍒細
//   - 浣跨敤 ITagEntry锛堟棤 dirty 瀛楁锛屽彧姣旇緝 valid + tag锛?//   - 闂ㄦ帶淇″彿鏀逛负 reqValid锛堝彇鎸囪姹傛湁鏁堬級锛岃€岄潪 memRen | wen
//     鈫?reqValid=0 鏃讹紙绌烘场鍛ㄦ湡锛変笉浜х敓 miss锛岄伩鍏嶈櫄瑙﹀彂 MissFSM
// ============================================================
class IHitTest(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val tagData   = Input(Vec(p.WAY_NUM, new ITagEntry(p)))
    val tag       = Input(UInt(p.TAG_W.W))
    val reqValid  = Input(Bool())   // IF 绾у彇鎸囪姹傛湁鏁?
    val isHit     = Output(Bool())
    val hitWay    = Output(UInt(p.WAY_W.W))
    // missValid = ~isHit & reqValid
    // ICacheTop 灏嗘淇″彿鍚屾椂杩炲埌 missOut 鍜?MissFSM.missValid
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
```
package icache

import chisel3._
import chisel3.util._
import parameterized_cache.CacheParams

// ============================================================
//  InstSelect锛氬弻妲芥寚浠ら€夎矾 + 琛屽熬杈圭晫鍒ゆ柇
//
//  鑱岃矗锛?//   1. 鎸?hitWay 浠庡洓璺?rawData0 / rawData1 涓€夊嚭鍛戒腑璺寚浠ゅ瓧
//   2. 褰?wordsoff = LINE_WORDS-1 鏃讹紙slot0 宸叉槸琛屽唴鏈€鍚庝竴瀛楋級锛?//      slot1 鏃犳湁鏁堟暟鎹?鈫?slot1Valid = 0锛屽己鍒堕€€鍖栦负鍗曞彂灏?//      锛圛DataArray 姝ゆ椂杈撳嚭鐨?rawData1 鏄?wordsoff+1 鍥炵粫鍚庣殑
//       琛岄鏁版嵁锛屾暟鍊兼棤鎰忎箟锛岀敱鏈ā鍧楃殑 slot1Valid=0 灞忚斀锛?// ============================================================
class InstSelect(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val hitWay    = Input(UInt(p.WAY_W.W))
    val wordsoff  = Input(UInt(p.WORD_CNT_W.W))
    val rawData0  = Input(Vec(p.WAY_NUM, UInt(p.DATA_WIDTH.W)))
    val rawData1  = Input(Vec(p.WAY_NUM, UInt(p.DATA_WIDTH.W)))

    val inst0      = Output(UInt(p.DATA_WIDTH.W))
    val inst1      = Output(UInt(p.DATA_WIDTH.W))
    // slot1Valid=0 鏃朵笂灞傚簲灏?instValids(1) 缃?0
    val slot1Valid = Output(Bool())
  })

  io.inst0      := io.rawData0(io.hitWay)
  io.inst1      := io.rawData1(io.hitWay)
  // 琛屽熬鍒ゆ柇锛歸ordsoff 涓烘渶鍚庝竴涓瓧鏃?slot1 瓒婄晫
  io.slot1Valid := io.wordsoff =/= (p.LINE_WORDS - 1).U
}
```

## .\src\main\Icache\ITagArray.scala
```
package icache

import chisel3._
import chisel3.util._
import parameterized_cache.CacheParams

// ============================================================
//  ITagArray锛欼-Cache 鏍囩闃靛垪
//
//  涓?D-Cache 鐗?TagArray 鐨勫尯鍒細
//   - ITagEntry 鏃?dirty 瀛楁锛堝彧璇?Cache 鏃犻渶鍐欏洖锛?//   - 鏃?setDirtyEn / setDirtyWay 淇″彿
//   - refillDirty 鍙傛暟涔熶笉瀛樺湪
// ============================================================
class ITagArray(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val flush       = Input(Bool())
    // 缁勫悎璇荤鍙ｏ細鐢卞綋鍓嶅彇鎸?idx 绱㈠紩锛屾瘡鍛ㄦ湡杈撳嚭鍥涜矾 tag
    val idx         = Input(UInt(p.INDEX_W.W))
    val tagData     = Output(Vec(p.WAY_NUM, new ITagEntry(p)))
    // 鍥炲～鍐欑鍙ｏ細refillDone 鑴夊啿鏃剁敱 MissFSM 椹卞姩
    val refillTagEn = Input(Bool())
    val refillWay   = Input(UInt(p.WAY_W.W))
    val refillIdx   = Input(UInt(p.INDEX_W.W))
    val refillTag   = Input(UInt(p.TAG_W.W))
  })

  // RegInit 淇濊瘉涓婄數鍚?valid=false锛沠lush 涔熶緷璧栨瀵勫瓨鍣ㄨ涔夈€?  val tArray = RegInit(VecInit(Seq.fill(p.SET_NUM)(
    VecInit(Seq.fill(p.WAY_NUM)(0.U.asTypeOf(new ITagEntry(p))))
  )))

  // 缁勫悎璇?  io.tagData := tArray(io.idx)

  // 鍐欎紭鍏堢骇锛歠lush > refillTagEn
  when(io.flush) {
    for (s <- 0 until p.SET_NUM) {
      for (w <- 0 until p.WAY_NUM) {
        tArray(s)(w).valid := false.B
        // tag 瀛楁鏃犻渶娓呴浂锛寁alid=false 鍗虫棤鏁?      }
    }
  }.elsewhen(io.refillTagEn) {
    tArray(io.refillIdx)(io.refillWay).valid := true.B
    tArray(io.refillIdx)(io.refillWay).tag   := io.refillTag
  }
}
```

## .\src\main\Memory\mem.scala
```
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
```
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
```
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

## .\src\test\CacheSizeDhrystoneSpec.scala
```
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

## .\src\test\CoreMarkSpec.scala
```
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
```
/* crt0.S  鈥? Reset handler, BSS clear, test runner, I/O helpers
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
    /* 鈹€鈹€ 1. set up stack 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    la   sp, _stack_top

    /* 鈹€鈹€ 2. zero BSS 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    la   t0, _bss_start
    la   t1, _bss_end
.Lbss_loop:
    bge  t0, t1, .Lbss_done
    sw   zero, 0(t0)
    addi t0, t0, 4
    j    .Lbss_loop
.Lbss_done:
    addi x8, x2, 0

    /* 鈹€鈹€ 3. run test suites 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
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

    /* 鈹€鈹€ 4. report result 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
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
    /* 鈹€鈹€ 5. assert success MMIO (io.success 鈫?1) 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    li   t0, 0x10001FF0
    li   t1, 1
    sw   t1, 0(t0)

.Lhalt:
    j    .Lhalt


/* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺? * _putchar(a0: char)  鈥? write one byte to printf MMIO
 * Clobbers: t0
 * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺?*/
    .global _putchar
_putchar:
    li   t0, 0x10001FF1
    sb   a0, 0(t0)
    ret


/* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺? * _printdec(a0: uint)  鈥? print decimal, 0..9999
 * Saves: ra, s0, s1
 * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺?*/
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
    /* ones 鈥?always print */
    addi a0, s0, '0'
    call _putchar

    lw   ra, 12(sp)
    lw   s0,  8(sp)
    lw   s1,  4(sp)
    addi sp, sp, 16
    ret
```

## .\src\test\DhrystoneSpec.scala
```
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
```
/* link.ld  鈥? Linker script for RV32I superscalar simulation
 *
 * Simulation RAM:   0x80000000 鈥?0x803FFFFF  (4 MB)
 * Stack top:        0x80180000               (within RAM)
 *
 * MMIO (handled by DCacheTop, bypass-routed, never go to RAM):
 *   0x10001FF0  success register  (sw 1 鈫?io.success = 1)
 *   0x10001FF1  printf putchar    (sb char)
 *   0x0000BFF8  mtime low         (lw only)
 *   0x0000BFFC  mtime high        (lw only)
 */

OUTPUT_ARCH(riscv)
ENTRY(_start)

SECTIONS {
    /* 鈹€鈹€ text 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    . = 0x80000000;
    .text : {
        *(.text.init)       /* _start must be the very first word */
        *(.text*)
    }

    /* 鈹€鈹€ read-only data 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    . = ALIGN(64);          /* align to one cache line */
    .rodata : { *(.rodata*) }

    /* 鈹€鈹€ initialized data 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    . = ALIGN(64);
    .data : { *(.data*) }

    /* 鈹€鈹€ zero-initialized data 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    . = ALIGN(64);
    .bss : {
        _bss_start = .;
        *(.bss*)
        *(COMMON)
        . = ALIGN(4);
        _bss_end = .;
    }

    /* 鈹€鈹€ test scratch buffer (4 KB, cache-stress area) 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    . = ALIGN(0x800);           /* 2 KB alignment = same-set stride */
    _scratch = .;
    . += 0x5000;                /* 20 KB 鈥?spans multiple sets + ways */

    /* 鈹€鈹€ stack (64 KB) 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    _stack_top = 0x80180000;
}
```

## .\src\test\PerfPrinter.scala
```
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
```
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
```
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

## .\src\test\SimTop.scala
```
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

## .\src\test\test_alu.S
```
/* test_alu.S  鈥? Tests for all RV32I ALU and ALU-immediate instructions
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

    /* 鈹€鈹€ ADD 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
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

    /* 鈹€鈹€ ADDI 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    li   t0, 0
    addi t2, t0, 2047;      CHK_IMM 2047        /* max positive imm12 */
    addi t2, t0, -2048;     CHK_IMM -2048       /* min negative imm12 */
    addi t2, t0, 0;         CHK_IMM 0           /* ADDI with 0 = MOV */

    /* 鈹€鈹€ SUB 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    li   t0, 50
    li   t1, 30
    sub  t2, t0, t1;        CHK_IMM 20

    li   t0, 0
    li   t1, 1
    sub  t2, t0, t1;        CHK_IMM -1          /* 0 - 1 = 0xFFFFFFFF */

    li   t0, 0x80000000     /* INT_MIN */
    li   t1, 1
    sub  t2, t0, t1;        CHK_IMM 0x7FFFFFFF  /* wraps */

    /* 鈹€鈹€ AND / ANDI 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    li   t0, 0xFF00FF00
    li   t1, 0x0F0F0F0F
    and  t2, t0, t1;        CHK_IMM 0x0F000F00

    li   t0, 0xFFFFFFFF
    andi t2, t0, 0x7FF;     CHK_IMM 0x7FF       /* keep low 11 bits */
    andi t2, t0, 0;         CHK_IMM 0
    andi t2, t0, -1;        CHK_IMM 0xFFFFFFFF  /* -1 sign-extends to all 1s */

    /* 鈹€鈹€ OR / ORI 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    li   t0, 0xF0F0F0F0
    li   t1, 0x0F0F0F0F
    or   t2, t0, t1;        CHK_IMM 0xFFFFFFFF

    li   t0, 0
    ori  t2, t0, 0x123;     CHK_IMM 0x123

    /* 鈹€鈹€ XOR / XORI 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    li   t0, 0xAAAAAAAA
    li   t1, 0x55555555
    xor  t2, t0, t1;        CHK_IMM 0xFFFFFFFF

    li   t0, 0xFFFFFFFF
    xori t2, t0, -1;        CHK_IMM 0           /* XOR with all-1s = NOT */

    /* 鈹€鈹€ SLL / SLLI 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
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

    /* 鈹€鈹€ SRL / SRLI 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    li   t0, 0x80000000
    li   t1, 31
    srl  t2, t0, t1;        CHK_IMM 1           /* logical: fills with 0 */
    li   t1, 1
    srl  t2, t0, t1;        CHK_IMM 0x40000000

    srli t2, t0, 0;         CHK_IMM 0x80000000
    srli t2, t0, 4;         CHK_IMM 0x08000000

    /* 鈹€鈹€ SRA / SRAI 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    li   t0, 0x80000000
    li   t1, 31
    sra  t2, t0, t1;        CHK_IMM 0xFFFFFFFF  /* arithmetic: sign extends */
    li   t1, 1
    sra  t2, t0, t1;        CHK_IMM 0xC0000000

    li   t0, 0x7FFFFFFF
    srai t2, t0, 31;        CHK_IMM 0           /* positive stays 0 at top */
    srai t2, t0, 1;         CHK_IMM 0x3FFFFFFF

    /* 鈹€鈹€ SLT / SLTI 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
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

    /* 鈹€鈹€ SLTU / SLTIU 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    li   t0, 0xFFFFFFFF     /* unsigned max */
    li   t1, 0
    sltu t2, t0, t1;        CHK_IMM 0           /* 0xFFFF... < 0? No */
    sltu t2, t1, t0;        CHK_IMM 1           /* 0 < 0xFFFF...? Yes */

    li   t0, 1
    sltiu t2, t0, 2;        CHK_IMM 1
    sltiu t2, t0, 1;        CHK_IMM 0
    sltiu t2, t0, -1;       CHK_IMM 1           /* -1 sign-ext = 0xFFFF... */

    /* 鈹€鈹€ LUI 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    lui  t2, 1;             CHK_IMM 0x00001000
    lui  t2, 0xFFFFF;       CHK_IMM 0xFFFFF000  /* -4096 */
    lui  t2, 0;             CHK_IMM 0

    /* 鈹€鈹€ AUIPC 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    /* AUIPC rd, 0 = put current PC into rd */
_auipc_ref:
    auipc t2, 0
    /* t2 should equal the address of this AUIPC instruction */
    la   t3, _auipc_ref
    beq  t2, t3, 1f
    addi s0, s0, 1
1:

    /* 鈹€鈹€ x0 write suppression 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    li   t0, 0xDEAD
    add  zero, t0, t0       /* write to x0 must be suppressed */
    li   t2, 0
    CHK_IMM 0               /* x0 must still be 0 */

    addi zero, zero, 42     /* ADDI to x0 */
    li   t2, 0
    CHK_IMM 0

    /* 鈹€鈹€ Shift amount masking (only low 5 bits used) 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    /* Shift amount in rs2 is masked to [4:0]; bits above are ignored */
    li   t0, 1
    li   t1, 32             /* 32 & 31 = 0 鈫?shift by 0 */
    sll  t2, t0, t1;        CHK_IMM 1
    li   t1, 33             /* 33 & 31 = 1 */
    sll  t2, t0, t1;        CHK_IMM 2

    /* 鈹€鈹€ Done 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    mv   a0, s0
    lw   ra, 12(sp)
    lw   s0,  8(sp)
    addi sp, sp, 16
    ret
```

## .\src\test\test_branch.S
```
/* test_branch.S  鈥? Branch and jump instruction coverage
 *
 * Covered:
 *   BEQ  鈥?taken / not-taken
 *   BNE  鈥?taken / not-taken
 *   BLT  鈥?signed <, all sign combinations
 *   BGE  鈥?signed >=
 *   BLTU 鈥?unsigned <
 *   BGEU 鈥?unsigned >=
 *   JAL  鈥?forward jump, rd=PC+4 verification
 *   JAL  鈥?backward jump (short loop)
 *   JALR 鈥?indirect call, rd=PC+4, target=(rs1+imm)&~1
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

    /* 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
     * BEQ
     * 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
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

    /* 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
     * BNE
     * 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
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

    /* 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
     * BLT (signed)
     * 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
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

    /* 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
     * BGE (signed)
     * 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
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

    /* 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
     * BLTU (unsigned)
     * 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
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

    /* 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
     * BGEU (unsigned)
     * 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
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

    /* 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
     * JAL 鈥?forward jump, rd = PC+4
     * 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    /* jal t4, target  鈫?t4 = PC+4 (the instruction immediately after jal) */
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

    /* 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
     * JAL 鈥?backward loop (exercises BTB with consistent taken branch)
     * Counts down from 8 to 0 (8 iterations, verifies loop-back works)
     * 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    li   s1, 8
.Lback_loop:
    addi s1, s1, -1
    bnez s1, .Lback_loop
    CHK_REG s1, 0

    /* 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
     * JAL ra 鈥?call/return pattern (verifies ra is set correctly)
     * 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    call .Lsimple_callee
    CHK_REG a0, 0x1234      /* callee returns 0x1234 */
    j    .Lafter_callee_def

.Lsimple_callee:
    li   a0, 0x1234
    ret

.Lafter_callee_def:

    /* 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
     * JALR 鈥?indirect call via register
     * rd = PC+4; PC = (rs1+imm) & ~1
     * 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
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

    /* 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
     * Prediction stress: alternating taken / not-taken branch
     * The BPU must recover from mispredictions on each alternation.
     * We run 16 pairs (32 iterations total) and verify the counter.
     * 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    li   s1, 16             /* outer loop count */
    li   s2, 0              /* accumulator must end at 16 */
.Lalternate_outer:
    beqz s1, .Lalternate_done
    addi s1, s1, -1
    addi s2, s2, 1          /* taken iteration */
    li   t0, 0
    beq  t0, zero, 1f       /* not-taken branch (condition false 鈫?skip) */
    addi s2, s2, -1         /* must NOT execute */
1:
    j    .Lalternate_outer
.Lalternate_done:
    CHK_REG s2, 16

    /* 鈹€鈹€ Done 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    mv   a0, s0
    lw   ra, 28(sp)
    lw   s0, 24(sp)
    lw   s1, 20(sp)
    lw   s2, 16(sp)
    addi sp, sp, 32
    ret
```

## .\src\test\test_cache.S
```
/* test_cache.S  鈥? D-Cache behavior coverage
 *
 * Cache parameters (from CacheParams in source):
 *   Total: 8 KB     鈥?8192 bytes
 *   Sets:  32       鈥?indexed by addr[10:6]
 *   Ways:  4        鈥?fully associative within a set
 *   Line:  64 bytes 鈥?16 脳 32-bit words
 *
 * Addresses in the same SET have identical bits [10:6].
 * Adding 0x800 (2048) to any address keeps bits [10:6] unchanged
 * but changes the TAG, so it maps to the SAME set with a DIFFERENT tag.
 *
 * Test plan:
 *   K01 鈥?Sequential write + readback (1 cache line, 16 words)
 *   K02 鈥?Sequential array fill (128 words = 4 cache lines, verifies
 *          spatial prefetcher doesn't corrupt data)
 *   K03 鈥?Stride-16 access (stride = 64 B = 1 cache line), exercises
 *          a different set each access
 *   K04 鈥?Repeated access to same line (cold 鈫?warm hit sequence)
 *   K05 鈥?4-way fill of one set (fill all 4 ways 鈫?5th access evicts LRU)
 *          then reload evicted line to verify dirty writeback correctness
 *   K06 鈥?Write + eviction + reload (dirty-line writeback)
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

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * K01 鈥?One full cache line: write all 16 words, read all back
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
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

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * K02 鈥?Sequential array: 128 words (512 bytes = 4 cache lines)
     * Write forward, verify backward (exercises spatial locality)
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
    /* Write phase: pattern = index 脳 3 + 1 */
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

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * K03 鈥?Stride-64-byte (one cache line stride)
     * Access 16 different cache lines spread across all 32 sets.
     * Verifies that each line is independently cached.
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
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

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * K04 鈥?Repeated access to same line (cold miss then warm hits)
     * Write once, read many times; all reads after the first should
     * hit the cache and return the correct value.
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
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

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * K05 鈥?4-way fill + LRU eviction (same cache set, 5 lines)
     *
     * Five addresses at offsets 0, 0x800, 0x1000, 0x1800, 0x2000
     * relative to scratch all map to the same set (bits [10:6]
     * are unchanged when adding 0x800 because 0x800 sets bit[11]).
     *
     * Write distinct patterns to each, then access all 5 in sequence.
     * After the 5th, the LRU line (first written) must be evicted.
     * Re-read the first line: if the dirty eviction worked, the
     * value written to it is still correct in memory.
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
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

    /* Access the 5th line 鈫?forces eviction of LRU (way0 = a4+0) */
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

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * K06 鈥?Write-back verify: dirty line written correctly
     * Modify multiple words in a cache line, evict, reload, verify.
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
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

    /* 鈹€鈹€ Done 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
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
```
/* test_csr.S  鈥? CSR instruction and performance-counter coverage
 *
 * Covered:
 *   CSRRW  鈥?write CSR, read old value into rd
 *   CSRRS  鈥?set bits in CSR
 *   CSRRC  鈥?clear bits in CSR
 *   mcycle 鈥?increments every cycle; two reads must show progression
 *   minstret 鈥?increments per retired instruction; verify delta
 *   mtime  鈥?MMIO read at 0xBFF8; must return a non-zero value over time
 *   prefetch_ctrl (custom CSR 0x7C0) 鈥?read/write
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

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * C01 鈥?CSRRW: write and read back
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
    /* Save current prefetch_ctrl, write a new value, read back */
    csrr  t4, CSR_PREFETCH          /* save original */

    li    t0, 0x3
    csrrw t1, CSR_PREFETCH, t0      /* t1 = old value, CSR 鈫?3 */
    nop
    csrr  t2, CSR_PREFETCH          /* t2 should be 3 */
    CHK_IMM t2, 0x3

    /* Restore */
    csrrw zero, CSR_PREFETCH, t4

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * C02 鈥?CSRRS: set individual bits
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
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

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * C03 鈥?CSRRC: clear individual bits
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
    li    t0, 0x1
    csrrc t1, CSR_PREFETCH, t0     /* clear bit0; t1 = old (3) */
    CHK_IMM t1, 0x3
    csrr  t2, CSR_PREFETCH
    CHK_IMM t2, 0x2                 /* only bit1 remains */

    csrrw zero, CSR_PREFETCH, zero  /* restore to 0 */

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * C04 鈥?mcycle: verify the counter increases
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
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

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * C05 鈥?minstret: verify retired-instruction count increases
     * We execute exactly 8 ADDI after the first read and verify
     * the delta is at least 8 (may be more due to how the pipeline
     * retires instructions; dual-issue means faster drain).
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
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

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * C06 鈥?mtime MMIO: two reads must be increasing
     * mtime at 0xBFF8 increments every cycle (in CSRFile).
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
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
       for a short simulation run 鈥?if not zero the counter has wrapped) */
    li    t0, MTIME_HI_ADDR
    lw    t2, 0(t0)
    /* Don't fail here; just verify the load doesn't crash the pipeline.
       High 32 bits may be non-zero for very long runs.  We only check
       that the load completes and the pipeline doesn't hang. */

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * C07 鈥?misa: read only (must match RV32I or RV32IM)
     * misa[31:30] = 01 for RV32; misa[8] = 1 for 'I' extension
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
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

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * C08 鈥?CSRRW with rd = x0: write without reading
     *        Verify the write still takes effect.
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
    li    t0, 0x1
    csrrw zero, CSR_PREFETCH, t0    /* write=1, rd=x0 (suppress read) */
    nop
    csrr  t1, CSR_PREFETCH
    CHK_IMM t1, 0x1
    csrrw zero, CSR_PREFETCH, zero  /* clear */

    /* 鈹€鈹€ Done 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    mv   a0, s0
    lw   ra, 12(sp)
    lw   s0,  8(sp)
    addi sp, sp, 16
    ret
```

## .\src\test\test_hazard.S
```
/* test_hazard.S  鈥? Data hazard & forwarding path coverage
 *
 * Each test verifies correctness (not cycle count) by checking
 * whether the pipeline produces the right value.  A working hazard
 * unit + bypass network produces the right value with the minimum
 * stall count; a broken one produces wrong values.
 *
 * Forwarding paths exercised  (spec 搂5.3.1):
 *   F1/F4  EX鈫扙X same-slot (consecutive cycles)
 *   F2/F3  EX鈫扙X cross-slot (consecutive cycles, different slots)
 *   F5     Intra-EX slot0鈫抯lot1 same issue cycle
 *   F6鈥揊9  MEM鈫扙X (2-cycle distance)
 *   F10-11 WB鈫扙X  (3-cycle distance)
 *
 * Stall scenarios:
 *   Load-use: lw followed immediately by dependent instruction
 *   Load-MEM: lw, nop, dependent (MEM鈫扙X forwarding for load data)
 *   Load-WB:  lw, nop, nop, dependent (WB鈫扙X forwarding for load data)
 *
 * Dual-issue scenarios:
 *   Independent ALU pair (should dual-issue, no stall)
 *   Slot0 ALU 鈫?Slot1 reads same rd (F5 bypass, no stall after fix)
 *   Both slots write same rd (slot1 must win 鈥?WAW resolution)
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

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * H01 鈥?EX鈫扙X forwarding, same slot (F1/F4)
     * Instruction N+1 in the same issue slot reads N's result.
     * Expected: no stall, forwarded value correct.
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
    li   t0, 17
    add  t1, t0, t0          /* t1 = 34, must get t0 via EX-EX bypass */
    CHK_IMM t1, 34

    li   t0, 0xABCD1234
    xor  t1, t0, t0          /* t1 = 0, immediate dep */
    CHK_IMM t1, 0

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * H02 鈥?EX鈫扙X cross-slot (F2/F3)
     * Slot0 result consumed by Slot1 in the NEXT cycle.
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
    /* Force two independent ops so the pipeline fills both slots,
       then a dependent op in the following cycle consumes slot0's result. */
    li   t0, 100
    li   t1, 200             /* these two li pair (independent) */
    add  t2, t0, t1          /* t2 = 300; t0 must come from EX鈫扙X */
    CHK_IMM t2, 300

    li   t0, 50
    li   t1, 3               /* independent */
    sub  t2, t0, t1          /* t2 = 47 */
    CHK_IMM t2, 47

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * H03 鈥?F5: Slot0 ALU 鈫?Slot1 same issue cycle (intra-EX bypass)
     * After the IDStage fix, slot0 is NOT blocked for ALU instructions.
     * The bypass network must forward slot0's EX result to slot1 in
     * the same cycle (combinational F5 path).
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
    li   t0, 0x1000
    addi t1, t0, 0x234       /* t1 = 0x1234; slot1 reads slot0's result */
    CHK_IMM t1, 0x1234

    li   t0, 7
    slli t1, t0, 3            /* t1 = 56; immediate dep via F5 */
    CHK_IMM t1, 56

    li   t0, 0xF0F0F0F0
    and  t1, t0, t0           /* t1 = same value */
    CHK_IMM t1, 0xF0F0F0F0

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * H04 鈥?MEM鈫扙X forwarding (F6鈥揊9), 2-cycle distance
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
    li   t0, 0x5A5A5A5A
    add  t0, t0, zero         /* t0 in EX this cycle */
    nop                       /* t0 now in EX/MEM pipe reg (MEM stage) */
    add  t1, t0, zero         /* t1 = t0 via MEM鈫扙X bypass */
    CHK_IMM t1, 0x5A5A5A5A

    li   t0, 255
    add  t0, t0, zero
    nop
    slli t1, t0, 1            /* t1 = 510 via MEM鈫扙X */
    CHK_IMM t1, 510

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * H05 鈥?WB鈫扙X forwarding (F10/F11), 3-cycle distance
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
    li   t0, 0x12345678
    add  t0, t0, zero
    nop
    nop
    add  t1, t0, zero         /* t1 = t0 via WB鈫扙X bypass */
    CHK_IMM t1, 0x12345678

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * H06 鈥?Load-Use stall
     * lw then immediately use the loaded value.
     * The pipeline must insert exactly 1 stall cycle.
     * The VALUE must be correct regardless of stall count.
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
    li   t0, 0xCAFEBABE
    sw   t0, 0(a5)
    lw   t1, 0(a5)            /* load t1 */
    add  t2, t1, zero         /* IMMEDIATE dep 鈫?1-cycle stall from HW */
    CHK_IMM t2, 0xCAFEBABE

    /* Double load-use: two consecutive loads, each immediately used */
    li   t0, 111
    li   t1, 222
    sw   t0, 0(a5)
    sw   t1, 4(a5)
    lw   t2, 0(a5)
    addi t2, t2, 1            /* dep on t2 load 鈫?stall */
    lw   t3, 4(a5)
    addi t3, t3, 1            /* dep on t3 load 鈫?stall */
    CHK_IMM t2, 112
    CHK_IMM t3, 223

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * H07 鈥?Load-MEM forwarding (lw, 1 independent, dep)
     * After the independent instruction, load data is available in
     * MEM鈫扙X bypass; no stall needed.
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
    li   t0, 0x55AA
    sw   t0, 8(a5)
    lw   t1, 8(a5)            /* load t1 */
    li   t4, 1                /* independent: fills the stall slot */
    or   t2, t1, t4           /* t1 via MEM鈫扙X bypass 鈫?no stall */
    CHK_IMM t2, 0x55AB

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * H08 鈥?Long dependency chain (6 hops, stresses forwarding network)
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
    li   t0, 1
    addi t0, t0, 1            /* t0 = 2  (EX鈫扙X) */
    addi t0, t0, 1            /* t0 = 3  (EX鈫扙X) */
    addi t0, t0, 1            /* t0 = 4  (EX鈫扙X) */
    addi t0, t0, 1            /* t0 = 5  (EX鈫扙X) */
    addi t0, t0, 1            /* t0 = 6  (EX鈫扙X) */
    CHK_IMM t0, 6

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * H09 鈥?WAW dual-issue: both slots target same rd, slot1 wins
     * (spec 搂7.2.1: slot1 is younger and must overwrite slot0)
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
    /* Two li instructions dual-issued; both write t0. */
    li   t0, 0xDEAD           /* slot0 writes t0 = 0xDEAD */
    li   t0, 0xBEEF           /* slot1 writes t0 = 0xBEEF (must win) */
    CHK_IMM t0, 0xBEEF

    li   t0, 111
    li   t0, 222
    li   t0, 333
    CHK_IMM t0, 333

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * H10 鈥?Independent dual-issue pair (no stall expected)
     * Verify both results are correct.
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
    li   t0, 0xAA
    li   t1, 0x55
    /* The following two ops have NO dependency between them */
    add  t2, t0, zero         /* slot0: t2 = 0xAA */
    add  t3, t1, zero         /* slot1: t3 = 0x55 */
    CHK_IMM t2, 0xAA
    CHK_IMM t3, 0x55

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * H11 鈥?Branch (predicted not-taken) + independent ALU in slot1
     * After IDStage fix, slot1 should execute alongside the branch.
     * This test verifies the ALU result is correct regardless.
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
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

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * H12 鈥?Store followed by load (different addresses, then same)
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
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

    /* 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲
     * H13 鈥?CSR forwarding: CSR write then immediate read-back
     * The CSR write happens in EX; the read should see the new value.
     * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲 */
    li   t0, 0x3              /* enable both prefetcher bits */
    li   t1, 0x7C0            /* prefetch_ctrl CSR address */
    csrrw zero, 0x7C0, t0    /* write prefetch_ctrl = 3 */
    nop
    csrr  t2, 0x7C0          /* read back */
    CHK_IMM t2, 0x3
    /* Restore to 0 */
    csrrw zero, 0x7C0, zero

    /* 鈹€鈹€ Done 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    mv   a0, s0
    lw   ra, 28(sp)
    lw   s0, 24(sp)
    lw   s1, 20(sp)
    lw   s2, 16(sp)
    addi sp, sp, 32
    ret
```

## .\src\test\test_mem.S
```
/* test_mem.S  鈥? Load/Store instruction coverage
 *
 * Covered:
 *   SW / LW     鈥?full word, 4-byte aligned
 *   SH / LH     鈥?half-word (signed), offsets 0 and 2
 *   SH / LHU    鈥?half-word (unsigned), sign-extension check
 *   SB / LB     鈥?byte (signed), all 4 byte offsets
 *   SB / LBU    鈥?byte (unsigned), sign-extension check
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

    /* 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
     * SW / LW
     * 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    li   t0, 0x12345678
    sw   t0, 0(a5)
    lw   t2, 0(a5);         CHK_IMM 0x12345678

    li   t0, 0xDEADBEEF
    sw   t0, 4(a5)
    lw   t2, 4(a5);         CHK_IMM 0xDEADBEEF   /* MSB set 鈥?full 32-bit */

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

    /* 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
     * SH / LH (signed half-word load)
     * 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
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

    /* 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
     * SH / LHU (unsigned half-word load)
     * 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    li   t0, 0x8001
    sh   t0, 0(a5)
    lhu  t2, 0(a5);         CHK_IMM 0x8001       /* no sign extension */

    li   t0, 0xFFFF
    sh   t0, 2(a5)
    lhu  t2, 2(a5);         CHK_IMM 0xFFFF       /* 65535, not -1 */

    /* 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
     * SB / LB (signed byte load), all 4 offsets within a word
     * 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
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

    /* 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
     * SB / LBU (unsigned byte load)
     * 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    li   t0, 0x80
    sb   t0, 0(a5)
    lbu  t2, 0(a5);         CHK_IMM 0x80        /* no sign extension */

    li   t0, 0xFF
    sb   t0, 3(a5)
    lbu  t2, 3(a5);         CHK_IMM 0xFF        /* 255, not -1 */

    /* 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
     * Store-then-load through cache: verify cache coherence
     * 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
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

    /* 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
     * Mixed-width to same word: SH then read with LBU
     * 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    sw   zero, 44(a5)
    li   t0, 0x1234
    sh   t0, 44(a5)         /* write half at offset 0 in that word */
    lbu  t2, 44(a5);        CHK_IMM 0x34        /* little-endian: low byte first */
    lbu  t2, 45(a5);        CHK_IMM 0x12        /* high byte of half */

    /* 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
     * Large immediate offset (uses S-type encoding)
     * 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    li   t0, 0x99887766
    sw   t0, 60(a5)
    lw   t2, 60(a5);        CHK_IMM 0x99887766

    /* 鈹€鈹€ Done 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€ */
    mv   a0, s0
    lw   ra, 12(sp)
    lw   s0,  8(sp)
    addi sp, sp, 16
    ret
```

## .\src\test\TopSpec.scala
```
package riscv

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.{File, PrintWriter}

/** 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺?  * TopSpec 鈥?sbt test / Verilator simulation for the RV32I superscalar core
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
  *   0x10001FF0  success MMIO  (sw 1 鈫?io.success=1)
  *   0x10001FF1  putchar MMIO  (sb char)
  *   0x0000BFF8  mtime MMIO    (lw)
  * 鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺愨晲鈺?  */
class TopSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  // 鈹€鈹€ Infrastructure 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€

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

  // 鈹€鈹€ Machine-code programs 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  //
  // All encodings computed for RV32I little-endian.
  // Register aliases: t0=x5  t1=x6  t2=x7  t3=x28  ra=x1
  //
  // Common success epilogue (reused by every program):
  //   lui  t0, 0x10002       鈫?t0 = 0x10002000
  //   addi t0, t0, -16       鈫?t0 = 0x10001FF0  (success MMIO)
  //   addi t1, x0, 1
  //   sw   t1, 0(t0)         鈫?io.success = 1
  //   jal  x0, 0             (halt 鈥?self loop)
  //
  // The same FAIL label is always a jal x0,0 (infinite loop) that the
  // test harness will detect as a timeout.

  // 鈹€鈹€ T1: Smoke (ALU + branch + success MMIO) 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  // 5 + 3 = 8, compare, assert success
  //   0x00 addi t0, x0, 5
  //   0x04 addi t1, x0, 3
  //   0x08 add  t2, t0, t1         t2=8
  //   0x0C addi t3, x0, 8
  //   0x10 bne  t2, t3, +24        鈫?0x28 (fail) if t2鈮?
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

  // 鈹€鈹€ T2: Printf putchar 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  // Print "Hi\n" via MMIO at 0x10001FF1 (SB), then assert success.
  //   0x10001FF1 = 0x10002000 - 15  鈫? lui 0x10002; addi -15
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

  // 鈹€鈹€ T3: Load / Store  鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  // Data at 0x80001000 (inside 4 MB RAM, word index 0x400).
  // Tests: SW/LW round-trip, SB/LB (sign extension), BNE guards.
  //
  //   0x00 lui  t0, 0x80001          t0=0x80001000
  //   0x04 addi t1, x0, 42
  //   0x08 sw   t1, 0(t0)
  //   0x0C lw   t2, 0(t0)
  //   0x10 addi t3, x0, 42
  //   0x14 bne  t2, t3, +40          鈫?0x3C fail
  //   0x18 addi t1, x0, 90
  //   0x1C sb   t1, 4(t0)
  //   0x20 lb   t2, 4(t0)            signed byte: 90 = 0x5A < 128 鈫?no sign-ext
  //   0x24 bne  t2, t1, +24          鈫?0x3C fail
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

  // 鈹€鈹€ T4: Forwarding, load-use stall, and tight branch loop 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  //
  // Phase 1 鈥?EX鈫扙X forwarding chain:
  //   li t0, 1; addi t0,t0,1 脳6 鈫?t0 must equal 7
  //   fail branch if t0鈮? (encoding: bne x5,x28, offset=68 鈫?0x5C29263)
  //
  // Phase 2 鈥?Load-use stall:
  //   sw 42 to data addr; lw t1; immediately addi t1,t1,1
  //   鈫?HW must insert 1-cycle stall; t1 must equal 43
  //   fail branch if t1鈮?3 (encoding: bne x6,x28, offset=40 鈫?0x3C31463)
  //
  // Phase 3 鈥?Countdown loop (8鈫?):
  //   li t1, 8; loop: addi t1,t1,-1; bnez t1, -4
  //   (tight taken branch 脳8, warms BTB)
  //
  // Layout:
  //   0x00鈥?x18  phase 1 chain
  //   0x1C       addi t3, x0, 7
  //   0x20       bne t0, t3, +68  鈫?0x64 fail
  //   0x24       lui  t0, 0x80001  (data area)
  //   0x28       addi t1, x0, 42
  //   0x2C       sw   t1, 0(t0)
  //   0x30       lw   t1, 0(t0)   鈫?lw
  //   0x34       addi t1, t1, 1   鈫?LOAD-USE dep
  //   0x38       addi t3, x0, 43
  //   0x3C       bne  t1, t3, +40 鈫?0x64 fail
  //   0x40       addi t1, x0, 8   loop counter
  //   0x44       addi t1, t1, -1  鈫?loop top
  //   0x48       bnez t1, -4      branch back to 0x44
  //   0x4C鈥?x58  success epilogue
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
    0x00128293L, // 0x18 addi t0, t0, 1       t0=7  (EX鈫扙X chain)
    0x00700E13L, // 0x1C addi t3, x0, 7
    0x05C29263L, // 0x20 bne  t0, t3, +68     鈫?0x64 fail if t0鈮?
    0x800012B7L, // 0x24 lui  t0, 0x80001     t0=0x80001000
    0x02A00313L, // 0x28 addi t1, x0, 42
    0x0062A023L, // 0x2C sw   t1, 0(t0)
    0x0002A303L, // 0x30 lw   t1, 0(t0)       鈫?LOAD
    0x00130313L, // 0x34 addi t1, t1, 1       鈫?LOAD-USE: HW stalls 1 cycle
    0x02B00E13L, // 0x38 addi t3, x0, 43
    0x03C31463L, // 0x3C bne  t1, t3, +40     鈫?0x64 fail if t1鈮?3
    0x00800313L, // 0x40 addi t1, x0, 8       loop counter
    0xFFF30313L, // 0x44 addi t1, t1, -1      鈫?loop top (BTB warm-up)
    0xFE031EE3L, // 0x48 bnez t1, -4          branch back to 0x44
    0x100022B7L, // 0x4C lui  t0, 0x10002
    0xFF028293L, // 0x50 addi t0, t0, -16     t0=0x10001FF0
    0x00100313L, // 0x54 addi t1, x0, 1
    0x0062A023L, // 0x58 sw   t1, 0(t0)       SUCCESS
    0x0000006FL, // 0x5C halt
    0x0000006FL, // 0x60 (pad)
    0x0000006FL, // 0x64 fail-halt
  )

  // 鈹€鈹€ T5: CSR and mtime MMIO 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  // Read mcycle twice (CSRR = CSRRS rd, csr, x0):
  //   CSR 0xB00 (mcycle):  funct3=010, opcode=0x73
  //   csrr t0 (x5): (0xB00<<20)|(0b010<<12)|(5<<7)|0x73 = 0xB00022F3
  //   csrr t1 (x6): (0xB00<<20)|(0b010<<12)|(6<<7)|0x73 = 0xB0002373
  //   bgeu t0, t1, fail  (fail if second reading 鈮?first)
  //
  // Read mtime MMIO at 0xBFF8 twice:
  //   0xBFF8 = lui 0xC (t0=0xC000) + addi -8 鈫?t0=0xBFF8
  //   lui t0, 0xC: (0xC<<12)|(5<<7)|0x37 = 0xC000|0x280|0x37 = 0xC2B7
  //     Wait: (0xC<<12) = 0xC000. (5<<7)=0x280. 0xC000|0x280|0x37=0xC2B7? 
  //     But 0xC000 | 0x280 = 0xC280; 0xC280 | 0x37 = 0xC2B7.
  //     This is a 16-bit number, padded to 32-bit: 0x0000C2B7. 鉁?  //   addi t0, t0, -8: imm=-8=0xFF8; (0xFF8<<20)|(5<<15)|(5<<7)|0x13 = 0xFF828293
  //   bgeu t0, t1, fail / bgeu t1, t2, fail encodings below.
  //
  // Instruction layout:
  //   0x00 csrr t0, mcycle
  //   0x04鈥?x0C  nop 脳3
  //   0x10 csrr t1, mcycle
  //   0x14 bgeu t0, t1, +56   鈫?0x4C fail if t0>=t1
  //   0x18 lui  t0, 0xC        t0=0xC000
  //   0x1C addi t0, t0, -8     t0=0xBFF8
  //   0x20 lw   t1, 0(t0)      first mtime
  //   0x24鈥?x2C  nop 脳3
  //   0x30 lw   t2, 0(t0)      second mtime
  //   0x34 bgeu t1, t2, +24    鈫?0x4C fail if t1>=t2
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

  // 鈹€鈹€ Tests 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€

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

  /** Full assembly suite 鈥?only runs if tests/test.hex was built.
    * Build: cd tests && make
    */
  it should "pass the full assembly test suite (tests/test.hex)" in {
    val f = new File("tests/test.hex")
    assume(f.exists(), "tests/test.hex not found 鈥?run 'cd tests && make' first")
    val result = runSim(f.getAbsolutePath, maxCycles = 2_000_000, verbose = true)
    result.success shouldBe true
    result.output  should include ("PASS")
  }
}
```

