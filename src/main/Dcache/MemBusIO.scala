package parameterized_cache

import chisel3._
import chisel3.util._

class MemBusReq(p: CacheParams) extends Bundle {
  val addr  = UInt(p.ADDR_WIDTH.W)
  val wdata = UInt(p.DATA_WIDTH.W)
  val wen   = Bool()
  val wmask = UInt(p.WMASK_BITS.W)
}

class MemBusResp(p: CacheParams) extends Bundle {
  val rdata = UInt(p.DATA_WIDTH.W)
}

class MemBusIO(p: CacheParams) extends Bundle {
  val req  = Decoupled(new MemBusReq(p))
  val resp = Flipped(Decoupled(new MemBusResp(p)))
}
