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

    // ── 输出 ──────────────────────────────────────────────
    val currPc  = Output(UInt(32.W)) // 当前 PC（→ debugPc / 预取器 currAddr）
    val pcFetch = Output(UInt(32.W)) // 送往 I-Cache 的取指地址
  })

  val pcReg  = RegInit(resetVec.U(32.W))
  val nextPc = Wire(UInt(32.W))

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
    // 默认：顺序取指，双发射步长 N*4
    nextPc := pcReg + (N * 4).U
  }

  pcReg := nextPc

  io.currPc  := pcReg
  io.pcFetch := pcReg
}
