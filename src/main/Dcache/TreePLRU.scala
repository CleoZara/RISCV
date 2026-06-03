package parameterized_cache

import chisel3._
import chisel3.util._

// ================================================================
//  通用树状 PLRU，支持任意 2 的幂路数（当前需求：4 / 8 路）。
//
//  存储：每组 (WAY_NUM-1) 个 bit，按 0 基堆编号摆放内部节点：
//        node 0 = 根；node n 的左/右子节点 = 2n+1 / 2n+2。
//        叶子 node 索引 (WAY_NUM-1 .. 2*WAY_NUM-2) 对应 way 0 .. WAY_NUM-1。
//
//  约定：内部节点 bit = “受害者方向”（0 = 左子树，1 = 右子树）。
//
//  驱逐：自根下行 depth 层，按各节点 bit 选择子树，落到的叶子即被驱逐路。
//  更新：沿 root→leaf(updateWay) 路径，把每个节点 bit 置为该路方向的取反
//        （指向兄弟子树），使被访问路成为 MRU。
//
//  4 路驱逐序：0→2→1→3→0
//  8 路驱逐序：0→4→2→6→1→5→3→7→0
// ================================================================
class TreePLRU(p: CacheParams) extends Module {
  require(isPow2(p.WAY_NUM), "TreePLRU 要求 WAY_NUM 为 2 的幂（当前支持 4 / 8 路）")

  val numNodes = p.WAY_NUM - 1
  val depth    = log2Ceil(p.WAY_NUM)
  val nodeW    = log2Ceil(2 * p.WAY_NUM)

  val io = IO(new Bundle {
    val idx       = Input(UInt(p.INDEX_W.W))
    val updateEn  = Input(Bool())
    val updateWay = Input(UInt(p.WAY_W.W))
    val evictWay  = Output(UInt(p.WAY_W.W))
  })

  val treeArray = RegInit(VecInit(Seq.fill(p.SET_NUM)(0.U(numNodes.W))))
  val tree = treeArray(io.idx)

  // ---- 驱逐路计算（组合）----
  var evNode = 0.U(nodeW.W)
  for (_ <- 0 until depth) {
    evNode = (evNode << 1).asUInt + 1.U + tree(evNode).asUInt
  }
  io.evictWay := (evNode - numNodes.U)(p.WAY_W - 1, 0)

  // ---- 命中 / 回填后更新 ----
  when(io.updateEn) {
    val newBits = Wire(Vec(numNodes, Bool()))
    for (i <- 0 until numNodes) newBits(i) := tree(i)
    var upNode = 0.U(nodeW.W)
    for (level <- 0 until depth) {
      val dir = io.updateWay(depth - 1 - level)   // 0 = 左, 1 = 右
      newBits(upNode) := ~dir
      upNode = (upNode << 1).asUInt + 1.U + dir.asUInt
    }
    treeArray(io.idx) := newBits.asUInt
  }
}
