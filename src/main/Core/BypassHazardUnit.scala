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
