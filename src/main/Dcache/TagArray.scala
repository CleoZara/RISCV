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
