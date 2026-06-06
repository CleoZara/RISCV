package riscv

import chiseltest._

case class PerfSnapshot(
    cycles: BigInt,
    retire0Cycles: BigInt,
    retire1Cycles: BigInt,
    retire2Cycles: BigInt,
    instRetired: BigInt,
    icacheStallCycles: BigInt,
    dcacheStallCycles: BigInt,
    loadUseStalls: BigInt,
    idRedirects: BigInt,
    exRedirects: BigInt,
    rasPushes: BigInt,
    rasPops: BigInt,
    branchInsts: BigInt,
    branchPreds: BigInt,
    branchCorrect: BigInt,
    branchMispredicts: BigInt,
    branchDirectionMispredicts: BigInt,
    branchTargetMispredicts: BigInt,
    wrongPathFlushInsts: BigInt,
    jalrInsts: BigInt,
    rasPreds: BigInt,
    rasCorrect: BigInt,
    icacheAccesses: BigInt,
    icacheHits: BigInt,
    icacheMisses: BigInt,
    icacheDemandRefills: BigInt,
    icachePrefetchReqs: BigInt,
    icachePrefetchAccepted: BigInt,
    icachePrefetchDropped: BigInt,
    icachePrefetchRefills: BigInt,
    icachePrefetchUseful: BigInt,
    dcacheLoads: BigInt,
    dcacheStores: BigInt,
    dcacheHits: BigInt,
    dcacheMisses: BigInt,
    dcacheWritebacks: BigInt,
    dcacheDemandRefills: BigInt,
    dcachePrefetchReqs: BigInt,
    dcachePrefetchAccepted: BigInt,
    dcachePrefetchDropped: BigInt,
    dcachePrefetchRefills: BigInt,
    dcachePrefetchUseful: BigInt)

object PerfSnapshot {
  val zero: PerfSnapshot = PerfSnapshot(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

  def from(p: CorePerfCounters): PerfSnapshot = PerfSnapshot(
    p.cycles.peekInt(),
    p.retire0Cycles.peekInt(),
    p.retire1Cycles.peekInt(),
    p.retire2Cycles.peekInt(),
    p.instRetired.peekInt(),
    p.icacheStallCycles.peekInt(),
    p.dcacheStallCycles.peekInt(),
    p.loadUseStalls.peekInt(),
    p.idRedirects.peekInt(),
    p.exRedirects.peekInt(),
    p.rasPushes.peekInt(),
    p.rasPops.peekInt(),
    p.branchInsts.peekInt(),
    p.branchPreds.peekInt(),
    p.branchCorrect.peekInt(),
    p.branchMispredicts.peekInt(),
    p.branchDirectionMispredicts.peekInt(),
    p.branchTargetMispredicts.peekInt(),
    p.wrongPathFlushInsts.peekInt(),
    p.jalrInsts.peekInt(),
    p.rasPreds.peekInt(),
    p.rasCorrect.peekInt(),
    p.icacheAccesses.peekInt(),
    p.icacheHits.peekInt(),
    p.icacheMisses.peekInt(),
    p.icacheDemandRefills.peekInt(),
    p.icachePrefetchReqs.peekInt(),
    p.icachePrefetchAccepted.peekInt(),
    p.icachePrefetchDropped.peekInt(),
    p.icachePrefetchRefills.peekInt(),
    p.icachePrefetchUseful.peekInt(),
    p.dcacheLoads.peekInt(),
    p.dcacheStores.peekInt(),
    p.dcacheHits.peekInt(),
    p.dcacheMisses.peekInt(),
    p.dcacheWritebacks.peekInt(),
    p.dcacheDemandRefills.peekInt(),
    p.dcachePrefetchReqs.peekInt(),
    p.dcachePrefetchAccepted.peekInt(),
    p.dcachePrefetchDropped.peekInt(),
    p.dcachePrefetchRefills.peekInt(),
    p.dcachePrefetchUseful.peekInt())
}

object PerfPrinter {
  def ratio(n: BigInt, d: BigInt): String =
    if (d == 0) "0.0000" else f"${n.toDouble / d.toDouble}%.4f"

  def line(tag: String, fields: Seq[(String, Any)]): String =
    s"[$tag] " + fields.map { case (k, v) => s"$k=$v" }.mkString(" ")

  def dhrystoneMetrics(output: String): Option[(BigInt, BigInt)] = {
    val cycles = "Cycles spent for 10 iterations dhrystone:\\s*(\\d+)".r
      .findFirstMatchIn(output).map(m => BigInt(m.group(1)))
    val instRetired = "Instructions retired for 10 iterations dhrystone:\\s*(\\d+)".r
      .findFirstMatchIn(output).map(m => BigInt(m.group(1)))
    for {
      c <- cycles
      i <- instRetired
    } yield (c, i)
  }

  def common(
      status: String,
      program: String,
      p: PerfSnapshot,
      metricCycles: Option[BigInt] = None,
      metricInstRetired: Option[BigInt] = None,
      metricSource: String = "perf-counter"): Seq[(String, Any)] = {
    val cycles = metricCycles.getOrElse(p.cycles)
    val instRetired = metricInstRetired.getOrElse(p.instRetired)
    val sourceFields =
      if (metricCycles.isDefined || metricInstRetired.isDefined) Seq(
        "metricSource" -> metricSource,
        "simCycles" -> p.cycles,
        "simInstRetired" -> p.instRetired,
        "simIPC" -> ratio(p.instRetired, p.cycles),
        "simCPI" -> ratio(p.cycles, p.instRetired))
      else Seq("metricSource" -> metricSource)

    Seq(
    "status" -> status,
    "program" -> program,
    "cycles" -> cycles,
    "instRetired" -> instRetired,
    "IPC" -> ratio(instRetired, cycles),
    "CPI" -> ratio(cycles, instRetired)) ++ sourceFields ++ Seq(
    "retire0" -> p.retire0Cycles,
    "retire1" -> p.retire1Cycles,
    "retire2" -> p.retire2Cycles,
    "icacheStall" -> p.icacheStallCycles,
    "dcacheStall" -> p.dcacheStallCycles,
    "loadUse" -> p.loadUseStalls,
    "idRedirects" -> p.idRedirects,
    "exRedirects" -> p.exRedirects,
    "branchInsts" -> p.branchInsts,
    "branchPreds" -> p.branchPreds,
    "branchCorrect" -> p.branchCorrect,
    "branchMispredicts" -> p.branchMispredicts,
    "branchDirectionMispredicts" -> p.branchDirectionMispredicts,
    "branchTargetMispredicts" -> p.branchTargetMispredicts,
    "wrongPathFlushInsts" -> p.wrongPathFlushInsts,
    "jalrInsts" -> p.jalrInsts,
    "rasPushes" -> p.rasPushes,
    "rasPops" -> p.rasPops,
    "rasPreds" -> p.rasPreds,
    "rasCorrect" -> p.rasCorrect,
    "icacheAccesses" -> p.icacheAccesses,
    "icacheHits" -> p.icacheHits,
    "icacheMisses" -> p.icacheMisses,
    "icacheDemandRefills" -> p.icacheDemandRefills,
    "dcacheLoads" -> p.dcacheLoads,
    "dcacheStores" -> p.dcacheStores,
    "dcacheHits" -> p.dcacheHits,
    "dcacheMisses" -> p.dcacheMisses,
    "dcacheWritebacks" -> p.dcacheWritebacks,
    "dcacheDemandRefills" -> p.dcacheDemandRefills,
    "prefetchReqs" -> (p.icachePrefetchReqs + p.dcachePrefetchReqs),
    "prefetchAccepted" -> (p.icachePrefetchAccepted + p.dcachePrefetchAccepted),
    "prefetchDropped" -> (p.icachePrefetchDropped + p.dcachePrefetchDropped),
    "prefetchRefills" -> (p.icachePrefetchRefills + p.dcachePrefetchRefills),
    "prefetchUseful" -> (p.icachePrefetchUseful + p.dcachePrefetchUseful),
    "icachePrefetchReqs" -> p.icachePrefetchReqs,
    "icachePrefetchAccepted" -> p.icachePrefetchAccepted,
    "icachePrefetchDropped" -> p.icachePrefetchDropped,
    "icachePrefetchRefills" -> p.icachePrefetchRefills,
    "icachePrefetchUseful" -> p.icachePrefetchUseful,
    "dcachePrefetchReqs" -> p.dcachePrefetchReqs,
    "dcachePrefetchAccepted" -> p.dcachePrefetchAccepted,
    "dcachePrefetchDropped" -> p.dcachePrefetchDropped,
    "dcachePrefetchRefills" -> p.dcachePrefetchRefills,
    "dcachePrefetchUseful" -> p.dcachePrefetchUseful)
  }

  def commonDhrystone(status: String, p: PerfSnapshot, output: String): Seq[(String, Any)] = {
    dhrystoneMetrics(output) match {
      case Some((cycles, instRetired)) =>
        common(status, "dhrystone", p, Some(cycles), Some(instRetired), "dhrystone-output")
      case None =>
        common(status, "dhrystone", p)
    }
  }
}
