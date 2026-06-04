package parameterized_cache

import chisel3._
import chisel3.util._

class DCacheTop(p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val addr    = Input(UInt(p.ADDR_WIDTH.W))
    val flush   = Input(Bool())
    val wen     = Input(Bool())
    val wmask   = Input(UInt(p.WMASK_BITS.W))
    val wdata   = Input(UInt(p.DATA_WIDTH.W))
    val memRen  = Input(Bool())
    val memWd   = Input(UInt(2.W))
    val signed  = Input(Bool())

    val rdata   = Output(UInt(p.DATA_WIDTH.W))
    val missOut = Output(Bool())
    val stall   = Output(Bool())

    val mtimeLo = Input(UInt(p.DATA_WIDTH.W))
    val mtimeHi = Input(UInt(p.DATA_WIDTH.W))

    val printChar = Output(Valid(UInt(8.W)))
    // P0 fix: success signal propagated from halt MMIO write
    val success   = Output(Bool())

    val pfReqValid = Input(Bool())
    val pfReqReady = Output(Bool())
    val pfReqAddr  = Input(UInt(p.ADDR_WIDTH.W))

    val mem = new MemBusIO(p)
  })

  require(p.ADDR_WIDTH >= 22, "DCacheTop expects at least 22-bit physical addresses")
  private def toPhysAddr(addr: UInt): UInt = {
    Cat(0.U((p.ADDR_WIDTH - 22).W), addr(21, 0))
  }

  // The simulation memory is addressed by low 22 bits.  Dhrystone uses both
  // high reset-vector aliases (0x8002_xxxx via gp) and low absolute data
  // addresses (0x0002_xxxx via lui), so cache lookup must use the same
  // canonical physical address as the memory model.  MMIO checks below still
  // use the original full address.
  val cacheAddr = toPhysAddr(io.addr)
  val pfCacheAddr = toPhysAddr(io.pfReqAddr)

  val addrTag  = cacheAddr(p.ADDR_WIDTH - 1, p.OFFSET_W + p.INDEX_W)
  val addrIdx  = cacheAddr(p.OFFSET_W + p.INDEX_W - 1, p.OFFSET_W)
  val addrWoff = cacheAddr(p.OFFSET_W - 1, 2)
  val addrBoff = cacheAddr(1, 0)
  val pfTag    = pfCacheAddr(p.ADDR_WIDTH - 1, p.OFFSET_W + p.INDEX_W)
  val pfIdx    = pfCacheAddr(p.OFFSET_W + p.INDEX_W - 1, p.OFFSET_W)

  // -------------------------------------------------------------------
  // Special / MMIO addresses
  // 0x10001FF1 – printf putchar  (store byte)
  // 0x10001FF0 – halt / success  (store word, wdata[0]=1 → success)
  // 0x0000BFF8 – mtime low       (load word, uncacheable)
  // 0x0000BFFC – mtime high      (load word, uncacheable)
  // -------------------------------------------------------------------
  val ADDR_PRINTF   = "h10001FF1".U(p.ADDR_WIDTH.W)
  val ADDR_HALT     = "h10001FF0".U(p.ADDR_WIDTH.W)   // P0 fix: halt/success MMIO
  val ADDR_MTIME_LO = "h0000BFF8".U(p.ADDR_WIDTH.W)
  val ADDR_MTIME_HI = "h0000BFFC".U(p.ADDR_WIDTH.W)

  val isPrintf      = (io.addr === ADDR_PRINTF) && io.wen && (io.memWd === 2.U)
  val addrIsPrintf  = (io.addr === ADDR_PRINTF) && (isPrintf || io.memRen)
  val addrIsMtimeLo = io.addr === ADDR_MTIME_LO
  val addrIsMtimeHi = io.addr === ADDR_MTIME_HI
  // P0 fix: halt address is uncacheable
  val addrIsHalt    = io.addr === ADDR_HALT
  val isBypass      = addrIsPrintf || addrIsMtimeLo || addrIsMtimeHi || addrIsHalt
  val pfIsBypass    = (io.pfReqAddr === ADDR_PRINTF) ||
                      (io.pfReqAddr === ADDR_HALT) ||
                      (io.pfReqAddr === ADDR_MTIME_LO) ||
                      (io.pfReqAddr === ADDR_MTIME_HI)

  val isMtimeLo = addrIsMtimeLo && io.memRen
  val isMtimeHi = addrIsMtimeHi && io.memRen

  // Latch success on any non-zero store to ADDR_HALT. Dhrystone writes 2,
  // while the assembly smoke tests write 1.
  val isHaltWrite = addrIsHalt && io.wen
  val successReg  = RegInit(false.B)
  when(isHaltWrite && (io.wdata =/= 0.U)) { successReg := true.B }
  io.success := successReg

  val tagArray  = Module(new TagArray(p))
  val dataArray = Module(new DataArray(p))
  val plru      = Module(new TreePLRU(p))
  val hitTest   = Module(new HitTest(p))
  val loadExt   = Module(new LoadExtend(p))
  val missFsm   = Module(new DCacheMissFSM(p))

  val demandReq = (io.memRen || io.wen) && !isBypass
  val queryPrefetch = missFsm.io.isIdle && io.pfReqValid && !demandReq && !pfIsBypass

  hitTest.io.tagData := tagArray.io.tagData
  hitTest.io.tag     := Mux(queryPrefetch, pfTag, addrTag)
  hitTest.io.memRen  := Mux(queryPrefetch, true.B, io.memRen)
  hitTest.io.wen     := Mux(queryPrefetch, false.B, io.wen)

  val pfMiss = queryPrefetch && hitTest.io.missValid
  val isHit  = hitTest.io.isHit && !isBypass && !queryPrefetch
  val hitWay = hitTest.io.hitWay
  val cacheMiss = hitTest.io.missValid && !isBypass && !queryPrefetch

  // TagArray
  tagArray.io.flush       := io.flush
  tagArray.io.idx         := Mux(queryPrefetch, pfIdx, addrIdx)
  tagArray.io.refillTagEn := missFsm.io.refillDone
  tagArray.io.refillWay   := missFsm.io.refillWay
  tagArray.io.refillIdx   := missFsm.io.refillIdx
  tagArray.io.refillTag   := missFsm.io.refillTag
  tagArray.io.refillDirty := missFsm.io.refillIsStore
  tagArray.io.setDirtyEn  := isHit && io.wen && !isBypass
  tagArray.io.setDirtyWay := hitWay

  // PLRU
  plru.io.idx       := Mux(queryPrefetch, pfIdx, addrIdx)
  plru.io.updateEn  := ((isHit && (io.memRen || io.wen)) || missFsm.io.refillDone) && !isBypass
  plru.io.updateWay := Mux(missFsm.io.refillDone, missFsm.io.refillWay, hitWay)

  // DataArray
  dataArray.io.idx          := addrIdx
  dataArray.io.wordsoff     := addrWoff
  dataArray.io.hitWen       := isHit && io.wen && !isBypass
  dataArray.io.hitWay       := hitWay
  dataArray.io.wdata        := io.wdata
  dataArray.io.wmask        := io.wmask
  dataArray.io.refillDataEn := missFsm.io.refillEn
  dataArray.io.refillWay    := missFsm.io.refillWay
  dataArray.io.refillIdx    := missFsm.io.refillIdx
  dataArray.io.refillWord   := missFsm.io.refillWord
  dataArray.io.refillData   := missFsm.io.refillData
  dataArray.io.evictIdx     := Mux(queryPrefetch, pfIdx, addrIdx)
  dataArray.io.evictWay     := plru.io.evictWay

  // LoadExt
  loadExt.io.hitWay  := hitWay
  loadExt.io.byteoff := addrBoff
  loadExt.io.memWd   := io.memWd
  loadExt.io.signed  := io.signed
  loadExt.io.rawData := dataArray.io.rawData

  // Miss FSM
  missFsm.io.missValid   := cacheMiss || pfMiss
  missFsm.io.missTag     := Mux(pfMiss, pfTag, addrTag)
  missFsm.io.missIsStore := cacheMiss && io.wen
  missFsm.io.missIsPrefetch := pfMiss
  missFsm.io.missWordOff := addrWoff
  missFsm.io.missWdata   := io.wdata
  missFsm.io.missWmask   := io.wmask
  missFsm.io.evictIdx    := Mux(pfMiss, pfIdx, addrIdx)
  missFsm.io.evictWay    := plru.io.evictWay
  missFsm.io.evictTag    := tagArray.io.tagData(plru.io.evictWay).tag
  missFsm.io.evictDirty  := tagArray.io.tagData(plru.io.evictWay).dirty
  missFsm.io.evictLine   := dataArray.io.evictLine
  io.pfReqReady := missFsm.io.isIdle && !demandReq

  io.mem <> missFsm.io.mem

  io.printChar.valid := isPrintf
  io.printChar.bits  := io.wdata(7, 0)

  io.rdata := MuxCase(loadExt.io.rdata, Seq(
    isMtimeLo -> io.mtimeLo,
    isMtimeHi -> io.mtimeHi,
    (addrIsPrintf && io.memRen) -> 0.U,
    (addrIsHalt  && io.memRen) -> 0.U   // load from halt addr → undefined, return 0
  ))

  val topStall = cacheMiss || missFsm.io.stall
  io.missOut := topStall
  io.stall   := topStall
}
