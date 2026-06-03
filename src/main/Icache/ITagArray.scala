package icache

import chisel3._
import chisel3.util._
import parameterized_cache.CacheParams

// ============================================================
//  ITagArray：I-Cache 标签阵列
//
//  与 D-Cache 版 TagArray 的区别：
//   - ITagEntry 无 dirty 字段（只读 Cache 无需写回）
//   - 无 setDirtyEn / setDirtyWay 信号
//   - refillDirty 参数也不存在
// ============================================================
class ITagArray(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val flush       = Input(Bool())
    // 组合读端口：由当前取指 idx 索引，每周期输出四路 tag
    val idx         = Input(UInt(p.INDEX_W.W))
    val tagData     = Output(Vec(p.WAY_NUM, new ITagEntry(p)))
    // 回填写端口：refillDone 脉冲时由 MissFSM 驱动
    val refillTagEn = Input(Bool())
    val refillWay   = Input(UInt(p.WAY_W.W))
    val refillIdx   = Input(UInt(p.INDEX_W.W))
    val refillTag   = Input(UInt(p.TAG_W.W))
  })

  // RegInit 保证上电后 valid=false；flush 也依赖此寄存器语义。
  val tArray = RegInit(VecInit(Seq.fill(p.SET_NUM)(
    VecInit(Seq.fill(p.WAY_NUM)(0.U.asTypeOf(new ITagEntry(p))))
  )))

  // 组合读
  io.tagData := tArray(io.idx)

  // 写优先级：flush > refillTagEn
  when(io.flush) {
    for (s <- 0 until p.SET_NUM) {
      for (w <- 0 until p.WAY_NUM) {
        tArray(s)(w).valid := false.B
        // tag 字段无需清零，valid=false 即无效
      }
    }
  }.elsewhen(io.refillTagEn) {
    tArray(io.refillIdx)(io.refillWay).valid := true.B
    tArray(io.refillIdx)(io.refillWay).tag   := io.refillTag
  }
}
