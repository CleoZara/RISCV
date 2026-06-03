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

  // HitTest 不使用 tagData 中的 dirty 字段，仅比对 valid 与 tag。
  val hitVec = VecInit((0 until p.WAY_NUM).map(i =>
    io.tagData(i).valid && (io.tagData(i).tag === io.tag)))

  io.isHit     := hitVec.asUInt.orR
  io.hitWay    := PriorityEncoder(hitVec)
  io.missValid := !io.isHit && (io.memRen || io.wen)
}
