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
