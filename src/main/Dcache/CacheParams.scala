package parameterized_cache

import chisel3._
import chisel3.util._

// ============================================================
//  全局唯一的参数定义。包内所有模块共用此 case class，
//  其余文件不再各自重复 `case class CacheParams`。
// ============================================================
case class CacheParams(
  ADDR_WIDTH: Int,
  DATA_WIDTH: Int,
  CACHE_SIZE: Int,
  WAY_NUM: Int,
  LINE_BYTES: Int
) {
  val WORD_BYTES  = DATA_WIDTH / 8
  val LINE_WORDS  = LINE_BYTES / WORD_BYTES
  val SET_NUM     = CACHE_SIZE / (WAY_NUM * LINE_BYTES)

  val OFFSET_W    = log2Ceil(LINE_BYTES)
  val INDEX_W     = log2Ceil(SET_NUM)
  val TAG_W       = ADDR_WIDTH - INDEX_W - OFFSET_W
  val WAY_W       = log2Ceil(WAY_NUM)
  val WORD_CNT_W  = log2Ceil(LINE_WORDS)
  val WMASK_BITS  = WORD_BYTES
}

// ============================================================
//  全局唯一的 TagEntry 定义（带 dirty 字段）。
//  HitTest 不使用 dirty，忽略即可；DCacheTop 通过
//  tagArray.io.tagData(...).dirty 读取脏位，连线类型一致。
// ============================================================
class TagEntry(p: CacheParams) extends Bundle {
  val valid = Bool()
  val dirty = Bool()
  val tag   = UInt(p.TAG_W.W)
}

object CacheParams {
  val default: CacheParams = CacheParams(
    ADDR_WIDTH = 32,
    DATA_WIDTH = 32,
    CACHE_SIZE = 8 * 1024,
    WAY_NUM    = 4,
    LINE_BYTES = 64
  )
}

class ICachePerfEvents extends Bundle {
  val access           = Bool()
  val hit              = Bool()
  val miss             = Bool()
  val demandRefill     = Bool()
  val prefetchReq      = Bool()
  val prefetchAccepted = Bool()
  val prefetchDropped  = Bool()
  val prefetchRefill   = Bool()
  val prefetchUseful   = Bool()
}

class DCachePerfEvents extends Bundle {
  val load             = Bool()
  val store            = Bool()
  val hit              = Bool()
  val miss             = Bool()
  val writeback        = Bool()
  val demandRefill     = Bool()
  val prefetchReq      = Bool()
  val prefetchAccepted = Bool()
  val prefetchDropped  = Bool()
  val prefetchRefill   = Bool()
  val prefetchUseful   = Bool()
}
