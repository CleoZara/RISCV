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
