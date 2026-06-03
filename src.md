# src/main source files

## src\main\Common\CSR.scala

```scala
package riscv

import chisel3._
import chisel3.util._

object CSRAddr {
  val mcycle        = "hB00".U(12.W)
  val mcycleh       = "hB80".U(12.W)
  val minstret      = "hB02".U(12.W)
  val mcountinhibit = "h320".U(12.W)
  val misa          = "h301".U(12.W)
  val prefetchCtrl  = "h7C0".U(12.W) // 鑷畾涔夛細bit0=Next-line, bit1=Stride
}

// CSROp 鐜扮粺涓€瀹氫箟鍦?Defines_c.scala锛坥bject CSROp锛夛紝姝ゅ涓嶅啀閲嶅瀹氫箟銆?
class CSRFile(val xlen: Int = 32, val issueWidth: Int = 2, val enableRV32M: Boolean = false) extends Module {
  require(xlen == 32, "Current CSRFile implementation targets RV32")
  require(issueWidth >= 1, "issueWidth must be >= 1")

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

    // 棰勫彇寮€鍏筹細bit0=Next-line, bit1=Stride锛岄€佸線棰勫彇鍣ㄣ€?    val prefetchCtrl = Output(UInt(xlen.W))
  })

  val mcycle        = RegInit(0.U(64.W))
  val minstret      = RegInit(0.U(64.W))
  val mtime         = RegInit(0.U(64.W))
  val mcountinhibit = RegInit(0.U(xlen.W))
  val prefetchCtrl  = RegInit(0.U(xlen.W))

  // misa锛氬彧璇汇€侻XL=01锛圧V32锛夌疆浜?bit[31:30]锛?I'=bit8锛涘惈 M 鏃跺啀缃?'M'=bit12銆?  val misaVal = {
    val base = ("h40000000".U(32.W) | (1.U << 8)) // RV32 + I
    if (enableRV32M) base | (1.U << 12) else base
  }

  // 璁℃暟鍣ㄨ嚜澧烇紙mcountinhibit 鍙叧闂級銆俶time 濮嬬粓鑷銆?  when(io.cycleTick && !mcountinhibit(0)) { mcycle := mcycle + 1.U }
  when(!mcountinhibit(2)) { minstret := minstret + PopCount(io.instRetire) }
  mtime := mtime + 1.U

  private def csrRead(addr: UInt): UInt = {
    MuxLookup(addr, 0.U(xlen.W), Seq(
      CSRAddr.mcycle        -> mcycle(31, 0),
      CSRAddr.mcycleh       -> mcycle(63, 32),
      CSRAddr.minstret      -> minstret(31, 0),
      CSRAddr.mcountinhibit -> mcountinhibit,
      CSRAddr.misa          -> misaVal,
      CSRAddr.prefetchCtrl  -> prefetchCtrl
    ))
  }

  io.rdata := csrRead(io.raddr)

  val oldVal = csrRead(io.waddr)
  io.oldData := oldVal

  val writeVal = WireDefault(oldVal)
  switch(io.opType) {
    is(CSROp.WRITE) { writeVal := io.wdata }
    is(CSROp.SET)   { writeVal := oldVal | io.wdata }
    is(CSROp.CLEAR) { writeVal := oldVal & (~io.wdata).asUInt }
  }

  when(io.opValid) {
    switch(io.waddr) {
      is(CSRAddr.mcycle)        { mcycle := Cat(mcycle(63, 32), writeVal) }
      is(CSRAddr.mcycleh)       { mcycle := Cat(writeVal, mcycle(31, 0)) }
      is(CSRAddr.minstret)      { minstret := Cat(minstret(63, 32), writeVal) }
      is(CSRAddr.mcountinhibit) { mcountinhibit := writeVal }
      is(CSRAddr.prefetchCtrl)  { prefetchCtrl := writeVal }
      // misa 鍙锛氬拷鐣ュ啓鍏?    }
  }

  io.mcycleLo   := mcycle(31, 0)
  io.mcycleHi   := mcycle(63, 32)
  io.minstretLo := minstret(31, 0)
  io.mtimeLo    := mtime(31, 0)
  io.mtimeHi    := mtime(63, 32)
  io.prefetchCtrl := prefetchCtrl
}
```

## src\main\Common\Defines_c.scala

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
```

## src\main\Compat\ICacheMissFSMCompat.scala

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

  val sIdle :: sRefillReq :: sRefillResp :: sDone :: sPrefill :: sPrefillResp :: sPrefillDone :: Nil = Enum(7)

  val state = RegInit(sIdle)
  val nextState = WireDefault(state)
  val wTag = Reg(UInt(p.TAG_W.W))
  val wIdx = Reg(UInt(p.INDEX_W.W))
  val wWay = Reg(UInt(p.WAY_W.W))
  val wordCnt = RegInit(0.U(p.WORD_CNT_W.W))
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
    is(sRefillResp) { when(io.mem.resp.fire) { nextState := Mux(lastWord, sDone, sRefillReq) } }
    is(sDone)       { nextState := sIdle }
    is(sPrefill)    { when(io.mem.req.fire)  { nextState := sPrefillResp } }
    is(sPrefillResp) {
      when(io.mem.resp.fire) { nextState := Mux(lastWord, sPrefillDone, sPrefill) }
    }
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
    wordCnt := Mux(lastWord, 0.U, wordCnt + 1.U)
  }

  val byteOffW = p.OFFSET_W - p.WORD_CNT_W
  val fetchAddr = Cat(wTag, wIdx, wordCnt, 0.U(byteOffW.W))
  val isRequesting = state === sRefillReq || state === sPrefill
  val isWaiting = state === sRefillResp || state === sPrefillResp

  io.mem.req.valid := isRequesting
  io.mem.req.bits.addr := fetchAddr
  io.mem.req.bits.wdata := 0.U
  io.mem.req.bits.wen := false.B
  io.mem.req.bits.wmask := 0.U
  io.mem.resp.ready := isWaiting

  io.refillEn := isWaiting && io.mem.resp.fire
  io.refillWay := wWay
  io.refillIdx := wIdx
  io.refillWord := wordCnt
  io.refillData := io.mem.resp.bits.rdata
  io.refillTag := wTag
  io.refillDone := state === sDone
  io.prefillDone := state === sPrefillDone
  io.stall := state === sRefillReq || state === sRefillResp || state === sDone
  io.isIdle := state === sIdle
  io.pfReqReady := state === sIdle && !io.missValid
}
```

## src\main\Core\BypassHazardUnit.scala

```scala
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

## src\main\Core\EXStage.scala

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

    actualNextPc(i) := MuxCase(fallThrough, Seq(
      io.in(i).isJalr          -> jalrTarget(i),
      (isBranch && branchTaken(i)) -> branchTarget(i)
    ))

    redirect(i) := slotValid(i) &&
                   (isBranch || io.in(i).isJalr) &&
                   (actualNextPc(i) =/= io.in(i).predNextPc)

    io.exValid(i)  := slotValid(i)
    io.exMemRen(i) := slotValid(i) && io.in(i).memRen
    io.exRdAddr(i) := io.in(i).rdAddr
    io.exRfWen(i)  := slotValid(i) && io.in(i).rfWen
    io.exResult(i) := alus(i).io.result

    io.out(i) := 0.U.asTypeOf(new EXMEMBundle)
    io.out(i).pc        := io.in(i).pc
    io.out(i).inst      := io.in(i).inst
    io.out(i).slotIdx   := io.in(i).slotIdx
    io.out(i).aluOut    := alus(i).io.result
    io.out(i).rs2Data   := io.storeData(i)
    io.out(i).rdAddr    := io.in(i).rdAddr
    io.out(i).wbSel     := io.in(i).wbSel
    io.out(i).rfWen     := slotValid(i) && io.in(i).rfWen
    io.out(i).memRen    := slotValid(i) && io.in(i).memRen
    io.out(i).memWen    := slotValid(i) && io.in(i).memWen
    io.out(i).memWd     := io.in(i).memWd
    io.out(i).memSigned := io.in(i).memSigned
    io.out(i).csrRdata  := io.csrOldData
    io.out(i).csrOp     := Mux(slotValid(i), io.in(i).csrOp, CSROp.NONE)
    io.out(i).brType    := Mux(slotValid(i), io.in(i).brType, BrType.BR_NONE)
    io.out(i).isJump    := slotValid(i) && io.in(i).isJump
    io.out(i).ctrl.valid   := slotValid(i)
    io.out(i).ctrl.kill    := io.flushEx || io.in(i).ctrl.kill
    io.out(i).ctrl.allowIn := true.B
  }

  // 鈹€鈹€ CSR operation 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  val csrSlot0 = slotValid(0) && io.in(0).csrOp =/= CSROp.NONE
  val csrSlot1 = slotValid(1) && io.in(1).csrOp =/= CSROp.NONE
  val csrIdx   = Mux(csrSlot0, 0.U, 1.U)
  io.csrOpValid := csrSlot0 || csrSlot1
  io.csrOpType  := Mux(csrSlot0, io.in(0).csrOp,  io.in(1).csrOp)
  io.csrWaddr   := Mux(csrSlot0, io.in(0).csrAddr, io.in(1).csrAddr)
  io.csrWdata   := io.rs1Data(csrIdx)

  // 鈹€鈹€ Redirect 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  io.exRedirectValid := redirect(0) || redirect(1)
  io.exRedirectPc    := Mux(redirect(0), actualNextPc(0), actualNextPc(1))

  // 鈹€鈹€ BPU update 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  // P1 fix: JALR must also update the BPU/BTB so the predictor learns the
  // indirect-call target.  Without this the BTB never converges and every
  // JALR (function call/return) pays the full 2-cycle flush penalty forever.
  //
  // Update source priority: slot 0 > slot 1 (slot 0 is the older instruction).
  // If both slots trigger an update in the same cycle (extremely rare after
  // the IDStage slot0Ctrl fix), slot 0 is recorded and slot 1 is silently
  // dropped.  The dropped update will be re-trained on the next occurrence.
  val br0   = slotValid(0) && io.in(0).brType =/= BrType.BR_NONE
  val br1   = slotValid(1) && io.in(1).brType =/= BrType.BR_NONE
  val jalr0 = slotValid(0) && io.in(0).isJalr
  val jalr1 = slotValid(1) && io.in(1).isJalr

  val isUpdate0 = br0 || jalr0
  val isUpdate1 = br1 || jalr1

  io.bpuUpdateValid := isUpdate0 || isUpdate1

  val bpuFromSlot0 = isUpdate0   // slot 0 wins when both want to update

  io.bpuUpdatePc := Mux(bpuFromSlot0, io.in(0).pc, io.in(1).pc)

  // For JALR: always "taken" (unconditional jump).
  // For branches: use the resolved branch outcome.
  io.bpuUpdateTaken := Mux(bpuFromSlot0,
    Mux(jalr0, true.B, branchTaken(0)),
    Mux(jalr1, true.B, branchTaken(1)))

  // BTB target: for branches, always store the branch-target address (pc+imm)
  // regardless of taken/not-taken, so the BTB is correct when the branch IS
  // later taken.  For JALR, store the resolved indirect target.
  val updateTarget0 = Mux(jalr0, jalrTarget(0), branchTarget(0))
  val updateTarget1 = Mux(jalr1, jalrTarget(1), branchTarget(1))
  io.bpuUpdateTarget := Mux(bpuFromSlot0, updateTarget0, updateTarget1)

}
```

## src\main\Core\IDStage.scala

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
  val slot0CanBypass =
    slotValid(0) &&
    dec(0).io.out.rfWen &&
    !dec(0).io.out.memRen &&                           // Load result not available in EX
    (dec(0).io.out.wbSel =/= WbSel.WB_MEM) &&         // belt-and-suspenders
    (dec(0).io.out.rdAddr =/= 0.U)

  // 鈹€鈹€ Slot-1 stall conditions 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  // P2 fix: intra-slot RAW is resolved by the intra-EX bypass (F5) when
  // slot 0 can forward.  Only truly block slot 1 when slot 0 is a Load.
  val slotRaw01 =
    !slot0CanBypass &&
    slotValid(0) && slotValid(1) &&
    dec(0).io.out.rfWen &&
    (dec(0).io.out.rdAddr =/= 0.U) &&
    ((dec(1).io.out.rs1Use && (dec(0).io.out.rdAddr === dec(1).io.out.rs1Addr)) ||
     (dec(1).io.out.rs2Use && (dec(0).io.out.rdAddr === dec(1).io.out.rs2Addr)))

  val slot0Mem = slotValid(0) && (dec(0).io.out.memRen || dec(0).io.out.memWen)
  val slot1Mem = slotValid(1) && (dec(1).io.out.memRen || dec(1).io.out.memWen)
  val slot0Csr = slotValid(0) && dec(0).io.out.csrOp =/= CSROp.NONE
  val slot1Csr = slotValid(1) && dec(1).io.out.csrOp =/= CSROp.NONE

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

  val slot1Blocked = slotRaw01 || (slot0Mem && slot1Mem) || (slot0Csr && slot1Csr) || slot0Ctrl

  val canIssue0 = slotValid(0) && !io.stallId && !io.flushId
  val canIssue1 = canIssue0 && slotValid(1) && !slot1Blocked

  // Capture slot 1 for the next cycle when it is blocked
  val capturePending = canIssue0 && slotValid(1) && !canIssue1 && !pendingValid
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
  val jal0 = slotValid(0) && !io.flushId && dec(0).io.out.isJump && !dec(0).io.out.isJalr
  val jal1 = slotValid(1) && !io.flushId && !slot1Blocked &&
             dec(1).io.out.isJump && !dec(1).io.out.isJalr
  io.idRedirectValid := jal0 || jal1
  io.idRedirectPc    := Mux(jal0,
    dec(0).io.out.pc + dec(0).io.out.imm,
    dec(1).io.out.pc + dec(1).io.out.imm)

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

## src\main\Core\IFStage.scala

```scala
package riscv

import chisel3._
import chisel3.util._
import icache.ICacheTop
import parameterized_cache.{CacheParams, MemBusIO}

class IFStage extends Module {
  val issueWidth = 2
  private val p = ICacheTop.defaultParams

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

    val nextLinePrefetchEn = Input(Bool())

    val out = Output(Vec(issueWidth, new IFIDSlot))
    val icacheStall = Output(Bool())
    val imem = new MemBusIO(p)
    val debugPc = Output(UInt(32.W))
  })

  val pcGen = Module(new PcGen(N = issueWidth))
  val icache = Module(new ICacheTop(p))
  val prefetcher = Module(new NextLinePrefetcher)

  pcGen.io.exRedirectValid := io.exRedirectValid
  pcGen.io.exRedirectPc    := io.exRedirectPc
  pcGen.io.idRedirectValid := io.idRedirectValid
  pcGen.io.idRedirectPc    := io.idRedirectPc
  pcGen.io.bpuPredTaken    := io.bpuPredTaken
  pcGen.io.bpuPredTarget   := io.bpuPredTarget
  pcGen.io.stallIf         := io.stallIf

  io.bpuQueryPc := pcGen.io.pcFetch
  io.debugPc := pcGen.io.currPc

  icache.io.addr  := pcGen.io.pcFetch
  icache.io.valid := true.B
  icache.io.flush := false.B
  io.imem <> icache.io.mem

  prefetcher.io.currAddr   := pcGen.io.currPc
  prefetcher.io.cacheHit   := icache.io.respValid
  prefetcher.io.cacheStall := icache.io.missOut
  prefetcher.io.prefetchEn := io.nextLinePrefetchEn

  icache.io.pfReqValid := prefetcher.io.pfReqValid
  icache.io.pfReqAddr  := prefetcher.io.pfReqAddr
  prefetcher.io.pfReqReady := icache.io.pfReqReady

  val seqNextPc = pcGen.io.pcFetch + (issueWidth * 4).U
  for (i <- 0 until issueWidth) {
    val slotPc = pcGen.io.pcFetch + (i * 4).U
    val slotPredNextPc = Mux(io.bpuPredTaken, io.bpuPredTarget, slotPc + 4.U)

    io.out(i) := 0.U.asTypeOf(new IFIDSlot)
    io.out(i).pc := slotPc
    io.out(i).inst := icache.io.insts(i)
    io.out(i).slotIdx := i.U
    io.out(i).fetchPc := pcGen.io.pcFetch
    io.out(i).predTaken := io.bpuPredTaken
    io.out(i).predTarget := io.bpuPredTarget
    io.out(i).predNextPc := slotPredNextPc
    io.out(i).seqNextPc := seqNextPc
    io.out(i).icacheHit := icache.io.respValid
    io.out(i).ctrl.valid := icache.io.instValids(i) && !io.flushIf
    io.out(i).ctrl.kill := io.flushIf
    io.out(i).ctrl.allowIn := true.B
  }

  io.icacheStall := icache.io.missOut
}
```

## src\main\Core\InOrderCore.scala

```scala
package riscv

import chisel3._
import chisel3.util._
import parameterized_cache.{CacheParams, MemBusIO}

class InOrderCore(enableRV32M: Boolean = false) extends Module {
  val issueWidth = 2
  private val p = CacheParams(32, 32, 8 * 1024, 4, 64)

  val io = IO(new Bundle {
    val imem      = new MemBusIO(p)
    val dmem      = new MemBusIO(p)
    val printChar = Output(Valid(UInt(8.W)))
    val success   = Output(Bool())
    val debugPc   = Output(UInt(32.W))
  })

  val ifStage  = Module(new IFStage)
  val idStage  = Module(new IDStage(enableRV32M))
  val exStage  = Module(new EXStage(enableRV32M))
  val memStage = Module(new MEMStage)
  val wbStage  = Module(new WBStage)
  val regFile  = Module(new RegFile(issueWidth))
  val csrFile  = Module(new CSRFile(32, issueWidth, enableRV32M))
  val bpu      = Module(new BPU)
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
  // 鈹€鈹€ BPU 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  bpu.io.queryPc      := ifStage.io.bpuQueryPc
  ifStage.io.bpuPredTaken  := bpu.io.predTaken
  ifStage.io.bpuPredTarget := bpu.io.predTarget
  bpu.io.updateValid  := exStage.io.bpuUpdateValid
  bpu.io.updatePc     := exStage.io.bpuUpdatePc
  bpu.io.updateTaken  := exStage.io.bpuUpdateTaken
  bpu.io.updateTarget := exStage.io.bpuUpdateTarget

  // 鈹€鈹€ IF Stage 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  ifStage.io.exRedirectValid     := exStage.io.exRedirectValid
  ifStage.io.exRedirectPc        := exStage.io.exRedirectPc
  ifStage.io.idRedirectValid     := idStage.io.idRedirectValid
  ifStage.io.idRedirectPc        := idStage.io.idRedirectPc
  ifStage.io.flushIf             := hazard.io.flushIF
  ifStage.io.stallIf             := hazard.io.stallIF || idStage.io.holdIfId
  ifStage.io.nextLinePrefetchEn  := csrFile.io.prefetchCtrl(0)

  // 鈹€鈹€ ID Stage 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  idStage.io.in         := ifidReg
  idStage.io.stallId    := hazard.io.stallID
  idStage.io.flushId    := hazard.io.flushID
  idStage.io.regRs1Data := regFile.io.rs1Data
  idStage.io.regRs2Data := regFile.io.rs2Data
  idStage.io.csrRdata   := csrFile.io.rdata

  regFile.io.rs1Addr := idStage.io.regRs1Addr
  regFile.io.rs2Addr := idStage.io.regRs2Addr
  regFile.io.wen     := wbStage.io.regWen
  regFile.io.waddr   := wbStage.io.regWaddr
  regFile.io.wdata   := wbStage.io.regWdata

  // 鈹€鈹€ CSR File 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  csrFile.io.raddr   := idStage.io.csrRaddr
  csrFile.io.opValid := exStage.io.csrOpValid
  csrFile.io.opType  := exStage.io.csrOpType
  csrFile.io.waddr   := exStage.io.csrWaddr
  csrFile.io.wdata   := exStage.io.csrWdata
  csrFile.io.cycleTick := true.B

  // P1 fix: minstret must count each instruction exactly once.
  // During any structural stall (D-/I-Cache miss), memwbReg is frozen, so
  // wbStage.io.instRetire stays high every stall cycle 鈫?over-counts.
  // Solution: only count in the *first* cycle of a stall (when the WB
  // instruction genuinely completes) and suppress all subsequent stall cycles.
  val prevStallWB    = RegNext(hazard.io.stallWB, false.B)
  val firstStallCycle = hazard.io.stallWB && !prevStallWB
  // retireEnable = true when pipeline is advancing OR it is the very first
  // cycle of a stall (the instruction that just "stopped" still retires once).
  val retireEnable = !hazard.io.stallWB || firstStallCycle
  csrFile.io.instRetire := VecInit(wbStage.io.instRetire.map(_ && retireEnable))

  // 鈹€鈹€ Bypass Network 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  bypass.io.idex      := idexReg
  bypass.io.idexValid := VecInit(idexReg.map(x => x.ctrl.valid && !x.ctrl.kill))
  bypass.io.exResult  := exStage.io.exResult
  bypass.io.exmem     := exmemReg
  bypass.io.exmemValid := VecInit(exmemReg.map(x => x.ctrl.valid && !x.ctrl.kill))
  bypass.io.memwb     := memwbReg
  bypass.io.memwbValid := VecInit(memwbReg.map(x => x.ctrl.valid && !x.ctrl.kill))
  bypass.io.wbValid   := wbStage.io.wbValid
  bypass.io.wbRfWen   := wbStage.io.wbRfWen
  bypass.io.wbRdAddr  := wbStage.io.wbRdAddr
  bypass.io.wbData    := wbStage.io.wbData
  bypass.io.rs1Use    := VecInit(idexReg.map(_.rs1Use))
  bypass.io.rs2Use    := VecInit(idexReg.map(_.rs2Use))

  // 鈹€鈹€ EX Stage 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  exStage.io.in        := idexReg
  exStage.io.stallEx   := hazard.io.stallEX
  exStage.io.flushEx   := false.B
  exStage.io.rs1Data   := bypass.io.rs1Data
  exStage.io.rs2Data   := bypass.io.rs2Data
  exStage.io.op1Data   := bypass.io.op1Data
  exStage.io.op2Data   := bypass.io.op2Data
  exStage.io.storeData := bypass.io.storeData
  exStage.io.csrOldData := csrFile.io.oldData

  // 鈹€鈹€ MEM Stage 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  memStage.io.in         := exmemReg
  memStage.io.flushMem   := false.B
  memStage.io.mtimeLo    := csrFile.io.mtimeLo
  memStage.io.mtimeHi    := csrFile.io.mtimeHi
  memStage.io.dcacheFlush := false.B

  // 鈹€鈹€ WB Stage 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  wbStage.io.in := memwbReg

  // 鈹€鈹€ Hazard Unit 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
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
  // brTaken is merged into exRedirect (branch mispred 鈫?exRedirectValid).
  hazard.io.brTaken     := false.B
  hazard.io.exRedirect  := exStage.io.exRedirectValid
  hazard.io.idRedirect  := idStage.io.idRedirectValid
  hazard.io.icacheStall := ifStage.io.icacheStall
  hazard.io.dcacheStall := memStage.io.dcacheStall
  hazard.io.backendStall := false.B

  // 鈹€鈹€ Pipeline register update logic 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
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

## src\main\Core\MEMStage.scala

```scala
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
```

## src\main\Core\RegFile.scala

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

## src\main\Core\WBStage.scala

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

## src\main\Dcache\CacheParams.scala

```scala
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
```

## src\main\Dcache\DataArray.scala

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

## src\main\Dcache\DCacheMissFSM.scala

```scala
package parameterized_cache

import chisel3._
import chisel3.util._

class DCacheMissFSMIO(p: CacheParams) extends Bundle {
  val missValid   = Input(Bool())
  val missTag     = Input(UInt(p.TAG_W.W))
  val missIsStore = Input(Bool())

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

  val stall       = Output(Bool())
}

class DCacheMissFSM(p: CacheParams) extends Module {
  val io = IO(new DCacheMissFSMIO(p))

  val sIdle :: sCheck :: sWbReq :: sWbResp :: sRefillReq :: sRefillResp :: sDone :: Nil = Enum(7)
  val state = RegInit(sIdle)
  val nextState = WireDefault(state)

  val missTag   = Reg(UInt(p.TAG_W.W))
  val evictTag  = Reg(UInt(p.TAG_W.W))
  val idx       = Reg(UInt(p.INDEX_W.W))
  val way       = Reg(UInt(p.WAY_W.W))
  val dirty     = Reg(Bool())
  val isStore   = Reg(Bool())
  val wordCnt   = Reg(UInt(p.WORD_CNT_W.W))
  val lineBuf   = Reg(Vec(p.LINE_WORDS, UInt(p.DATA_WIDTH.W)))

  val lastWord = wordCnt === (p.LINE_WORDS - 1).U

  switch(state) {
    is(sIdle)       { when(io.missValid) { nextState := sCheck } }
    is(sCheck)      { nextState := Mux(dirty, sWbReq, sRefillReq) }
    is(sWbReq)      { when(io.mem.req.fire) { nextState := sWbResp } }
    is(sWbResp)     { when(io.mem.resp.fire) { nextState := Mux(lastWord, sRefillReq, sWbReq) } }
    is(sRefillReq)  { when(io.mem.req.fire) { nextState := sRefillResp } }
    is(sRefillResp) { when(io.mem.resp.fire) { nextState := Mux(lastWord, sDone, sRefillReq) } }
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
    wordCnt  := 0.U
    lineBuf  := io.evictLine
  }.elsewhen((state === sWbResp || state === sRefillResp) && io.mem.resp.fire) {
    wordCnt := Mux(lastWord, 0.U, wordCnt + 1.U)
  }

  val wbAddr     = Cat(evictTag, idx, wordCnt, 0.U((p.OFFSET_W - p.WORD_CNT_W).W))
  val refillAddr = Cat(missTag, idx, wordCnt, 0.U((p.OFFSET_W - p.WORD_CNT_W).W))

  val isWb       = state === sWbReq
  val isRefill   = state === sRefillReq
  io.mem.req.valid      := isWb || isRefill
  io.mem.req.bits.addr  := Mux(isWb, wbAddr, refillAddr)
  io.mem.req.bits.wdata := Mux(isWb, lineBuf(wordCnt), 0.U)
  io.mem.req.bits.wen   := isWb
  io.mem.req.bits.wmask := Mux(isWb, ~0.U(p.WMASK_BITS.W), 0.U)
  io.mem.resp.ready     := state === sWbResp || state === sRefillResp

  io.refillEn     := state === sRefillResp && io.mem.resp.fire
  io.refillWay    := way
  io.refillIdx    := idx
  io.refillWord   := wordCnt
  io.refillData   := io.mem.resp.bits.rdata
  io.refillTag    := missTag
  io.refillDone   := state === sDone
  io.refillIsStore := isStore
  io.stall        := state =/= sIdle
}
```

## src\main\Dcache\DCacheTop.scala

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

    val mem = new MemBusIO(p)
  })

  val addrTag  = io.addr(p.ADDR_WIDTH - 1, p.OFFSET_W + p.INDEX_W)
  val addrIdx  = io.addr(p.OFFSET_W + p.INDEX_W - 1, p.OFFSET_W)
  val addrWoff = io.addr(p.OFFSET_W - 1, 2)
  val addrBoff = io.addr(1, 0)

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

  val isMtimeLo = addrIsMtimeLo && io.memRen
  val isMtimeHi = addrIsMtimeHi && io.memRen

  // P0 fix: latch success when a store word/byte to ADDR_HALT arrives with wdata[0]=1
  val isHaltWrite = addrIsHalt && io.wen
  val successReg  = RegInit(false.B)
  when(isHaltWrite && io.wdata(0)) { successReg := true.B }
  io.success := successReg

  val tagArray  = Module(new TagArray(p))
  val dataArray = Module(new DataArray(p))
  val plru      = Module(new TreePLRU(p))
  val hitTest   = Module(new HitTest(p))
  val loadExt   = Module(new LoadExtend(p))
  val missFsm   = Module(new DCacheMissFSM(p))

  hitTest.io.tagData := tagArray.io.tagData
  hitTest.io.tag     := addrTag
  hitTest.io.memRen  := io.memRen
  hitTest.io.wen     := io.wen

  val isHit  = hitTest.io.isHit && !isBypass
  val hitWay = hitTest.io.hitWay
  val cacheMiss = hitTest.io.missValid && !isBypass

  // TagArray
  tagArray.io.flush       := io.flush
  tagArray.io.idx         := addrIdx
  tagArray.io.refillTagEn := missFsm.io.refillDone
  tagArray.io.refillWay   := missFsm.io.refillWay
  tagArray.io.refillIdx   := missFsm.io.refillIdx
  tagArray.io.refillTag   := missFsm.io.refillTag
  tagArray.io.refillDirty := missFsm.io.refillIsStore
  tagArray.io.setDirtyEn  := isHit && io.wen && !isBypass
  tagArray.io.setDirtyWay := hitWay

  // PLRU
  plru.io.idx       := addrIdx
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
  dataArray.io.evictIdx     := addrIdx
  dataArray.io.evictWay     := plru.io.evictWay

  // LoadExt
  loadExt.io.hitWay  := hitWay
  loadExt.io.byteoff := addrBoff
  loadExt.io.memWd   := io.memWd
  loadExt.io.signed  := io.signed
  loadExt.io.rawData := dataArray.io.rawData

  // Miss FSM
  missFsm.io.missValid   := cacheMiss
  missFsm.io.missTag     := addrTag
  missFsm.io.missIsStore := io.wen
  missFsm.io.evictIdx    := addrIdx
  missFsm.io.evictWay    := plru.io.evictWay
  missFsm.io.evictTag    := tagArray.io.tagData(plru.io.evictWay).tag
  missFsm.io.evictDirty  := tagArray.io.tagData(plru.io.evictWay).dirty
  missFsm.io.evictLine   := dataArray.io.evictLine

  io.mem <> missFsm.io.mem

  io.printChar.valid := isPrintf
  io.printChar.bits  := io.wdata(7, 0)

  io.rdata := MuxCase(loadExt.io.rdata, Seq(
    isMtimeLo -> io.mtimeLo,
    isMtimeHi -> io.mtimeHi,
    (addrIsPrintf && io.memRen) -> 0.U,
    (addrIsHalt  && io.memRen) -> 0.U   // load from halt addr 鈫?undefined, return 0
  ))

  val topStall = cacheMiss || missFsm.io.stall
  io.missOut := topStall
  io.stall   := topStall
}
```

## src\main\Dcache\HitTest.scala

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

  // HitTest 涓嶄娇鐢?tagData 涓殑 dirty 瀛楁锛屼粎姣斿 valid 涓?tag銆?  val hitVec = VecInit((0 until p.WAY_NUM).map(i =>
    io.tagData(i).valid && (io.tagData(i).tag === io.tag)))

  io.isHit     := hitVec.asUInt.orR
  io.hitWay    := PriorityEncoder(hitVec)
  io.missValid := !io.isHit && (io.memRen || io.wen)
}
```

## src\main\Dcache\LoadExtend.scala

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

## src\main\Dcache\MemBusIO.scala

```scala
package parameterized_cache

import chisel3._
import chisel3.util._

class MemBusReq(p: CacheParams) extends Bundle {
  val addr  = UInt(p.ADDR_WIDTH.W)
  val wdata = UInt(p.DATA_WIDTH.W)
  val wen   = Bool()
  val wmask = UInt(p.WMASK_BITS.W)
}

class MemBusResp(p: CacheParams) extends Bundle {
  val rdata = UInt(p.DATA_WIDTH.W)
}

class MemBusIO(p: CacheParams) extends Bundle {
  val req  = Decoupled(new MemBusReq(p))
  val resp = Flipped(Decoupled(new MemBusResp(p)))
}
```

## src\main\Dcache\TagArray.scala

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

## src\main\Dcache\TreePLRU.scala

```scala
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

## src\main\Decode\Decoder_c.scala

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

## src\main\Execute\ALU.scala

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

## src\main\Frontend\BPU.scala

```scala
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

## src\main\Frontend\BPU_RAS.scala

```scala
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
```

## src\main\Frontend\NextLinePrefetcher.scala

```scala
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

## src\main\Frontend\PcGen.scala

```scala
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

    // 鈹€鈹€ 杈撳嚭 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
    val currPc  = Output(UInt(32.W)) // 褰撳墠 PC锛堚啋 debugPc / 棰勫彇鍣?currAddr锛?    val pcFetch = Output(UInt(32.W)) // 閫佸線 I-Cache 鐨勫彇鎸囧湴鍧€
  })

  val pcReg  = RegInit(resetVec.U(32.W))
  val nextPc = Wire(UInt(32.W))

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
    // 榛樿锛氶『搴忓彇鎸囷紝鍙屽彂灏勬闀?N*4
    nextPc := pcReg + (N * 4).U
  }

  pcReg := nextPc

  io.currPc  := pcReg
  io.pcFetch := pcReg
}
```

## src\main\Frontend\TAGE.scala

```scala
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
```

## src\main\Icache\ICacheMissFSM.scala

```scala
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
  val sIdle :: sRefillReq :: sRefillResp :: sDone ::
      sPrefill :: sPrefillResp :: sPrefillDone :: Nil = Enum(7)

  val state     = RegInit(sIdle)
  val nextState = WireDefault(state)

  // 鈹€鈹€ 閿佸瓨瀛楁 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  val wTag    = Reg(UInt(p.TAG_W.W))
  val wIdx    = Reg(UInt(p.INDEX_W.W))
  val wWay    = Reg(UInt(p.WAY_W.W))
  val wordCnt = Reg(UInt(p.WORD_CNT_W.W))

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
      when(io.mem.resp.fire) {
        nextState := Mux(lastWord, sDone, sRefillReq)
      }
    }
    is(sDone) { nextState := sIdle }

    is(sPrefill) {
      when(io.mem.req.fire) { nextState := sPrefillResp }
    }
    is(sPrefillResp) {
      when(io.mem.resp.fire) {
        nextState := Mux(lastWord, sPrefillDone, sPrefill)
      }
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
    wordCnt := Mux(lastWord, 0.U, wordCnt + 1.U)
  }

  // 鈹€鈹€ 鍐呭瓨鎬荤嚎鍦板潃锛歿tag, idx, wordCnt, 2'b00} 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  // byteOffW = OFFSET_W - WORD_CNT_W = 6 - 4 = 2锛?2 浣嶅瓧鍐呭瓧鑺傚亸绉诲搴︼級
  private val byteOffW  = p.OFFSET_W - p.WORD_CNT_W
  val fetchAddr = Cat(wTag, wIdx, wordCnt, 0.U(byteOffW.W))

  val isRequesting = state === sRefillReq  || state === sPrefill
  val isWaiting    = state === sRefillResp || state === sPrefillResp

  io.mem.req.valid      := isRequesting
  io.mem.req.bits.addr  := fetchAddr
  io.mem.req.bits.wdata := 0.U
  io.mem.req.bits.wen   := false.B
  io.mem.req.bits.wmask := 0.U
  io.mem.resp.ready     := isWaiting

  // 鈹€鈹€ 鍥炲～杈撳嚭 鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  io.refillEn    := isWaiting && io.mem.resp.fire
  io.refillWay   := wWay
  io.refillIdx   := wIdx
  io.refillWord  := wordCnt
  io.refillData  := io.mem.resp.bits.rdata
  io.refillTag   := wTag
  io.refillDone  := state === sDone
  io.prefillDone := state === sPrefillDone

  // 鈹€鈹€ 鐘舵€佽緭鍑?鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€鈹€
  // stall 浠呭湪澶勭悊鐪熸 miss 鐨勪笁涓姸鎬佸唴缃珮锛涢鍙栦笉褰卞搷娴佹按绾?  io.stall  := state === sRefillReq || state === sRefillResp || state === sDone
  io.isIdle := state === sIdle

  // 棰勫彇鎻℃墜锛氫粎鍦?sIdle 涓旀棤 miss 鏃舵帴鍙?  io.pfReqReady := (state === sIdle) && !io.missValid
}
```

## src\main\Icache\ICacheParams.scala

```scala
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

## src\main\Icache\ICacheTop.scala

```scala
package icache

import chisel3._
import chisel3.util._
import parameterized_cache.{CacheParams, MemBusIO}
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

## src\main\Icache\IDataArray.scala

```scala
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

## src\main\Icache\IHitTest.scala

```scala
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

## src\main\Icache\InstSelect.scala

```scala
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

## src\main\Icache\ITagArray.scala

```scala
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

## src\main\Memory\mem.scala

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
```

## src\main\Top.scala

```scala
package riscv

import chisel3._
import chisel3.util._
import parameterized_cache.{CacheParams, MemBusIO}

class Top(enableRV32M: Boolean = false) extends Module {
  private val p = CacheParams(32, 32, 8 * 1024, 4, 64)

  val io = IO(new Bundle {
    val status = Output(Bool())
    val success = Output(Bool())
    val printChar = Output(Valid(UInt(8.W)))
    val debugPc = Output(UInt(32.W))
  })

  val core = Module(new InOrderCore(enableRV32M))
  val memory = Module(new RV32DualPortMemory(p))

  memory.io.imem <> core.io.imem
  memory.io.dmem <> core.io.dmem

  io.status := false.B
  io.success := core.io.success
  io.printChar := core.io.printChar
  io.debugPc := core.io.debugPc
}

object Elaborate extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new Top)
}
```

