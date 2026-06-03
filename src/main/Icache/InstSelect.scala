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
