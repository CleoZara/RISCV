package icache

import chisel3._
import chisel3.util._
import parameterized_cache.CacheParams

// ============================================================
//  IHitTest：I-Cache 命中检测
//
//  与 D-Cache 版 HitTest 的区别：
//   - 使用 ITagEntry（无 dirty 字段，只比较 valid + tag）
//   - 门控信号改为 reqValid（取指请求有效），而非 memRen | wen
//     → reqValid=0 时（空泡周期）不产生 miss，避免虚触发 MissFSM
// ============================================================
class IHitTest(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val tagData   = Input(Vec(p.WAY_NUM, new ITagEntry(p)))
    val tag       = Input(UInt(p.TAG_W.W))
    val reqValid  = Input(Bool())   // IF 级取指请求有效

    val isHit     = Output(Bool())
    val hitWay    = Output(UInt(p.WAY_W.W))
    // missValid = ~isHit & reqValid
    // ICacheTop 将此信号同时连到 missOut 和 MissFSM.missValid
    val missValid = Output(Bool())
  })

  val hitVec = VecInit((0 until p.WAY_NUM).map(i =>
    io.tagData(i).valid && (io.tagData(i).tag === io.tag)
  ))

  io.isHit     := hitVec.asUInt.orR
  io.hitWay    := PriorityEncoder(hitVec)
  io.missValid := !io.isHit && io.reqValid
}
