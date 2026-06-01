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
