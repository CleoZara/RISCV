package icache

import chisel3._
import chisel3.util._
import parameterized_cache.CacheParams

// ============================================================
//  I-Cache 专用 TagEntry：只读 Cache 无需 dirty 字段。
//  其余几何参数（SET_NUM / WAY_NUM / LINE_WORDS 等）
//  直接复用 parameterized_cache.CacheParams。
// ============================================================
class ITagEntry(p: CacheParams) extends Bundle {
  val valid = Bool()
  val tag   = UInt(p.TAG_W.W)
}
