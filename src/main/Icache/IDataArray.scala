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
