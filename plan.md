# 综合性能测试计数器与日志方案

## Summary

在现有 `CorePerfCounters` 基础上扩展硬件内部计数器，让综合测试不仅给出 `cycles`、`instret`、`IPC`、`CPI`，还能够解释不同预取器、分支预测器和 cache 参数为什么带来性能变化。测试日志统一保存到根目录 `reports/run-YYYYmmdd-HHMMSS/`，每次运行保留原始输出和汇总结果。

本方案只定义测试体系、计数器和日志归档方式，不立即修改核心流水线行为。

## Performance Counters

保留已有计数器：

- `cycles`
- `instRetired`
- `retire0Cycles`
- `retire1Cycles`
- `retire2Cycles`
- `icacheStallCycles`
- `dcacheStallCycles`
- `loadUseStalls`
- `idRedirects`
- `exRedirects`
- `rasPushes`
- `rasPops`

建议新增分支预测指标：

- `branchInsts`：实际执行到 EX 阶段的 branch/JAL/JALR/control-flow 指令数。
- `branchPreds`：IF 阶段做过方向或目标预测的次数。
- `branchCorrect`：预测方向和目标均正确的次数。
- `branchMispredicts`：发生分支预测错误并触发 redirect 的次数。
- `branchDirectionMispredicts`：预测 taken 与实际 taken 不一致。
- `branchTargetMispredicts`：方向正确但目标地址不一致。
- `wrongPathFlushInsts`：flush 时被清掉的 valid 指令数，用于衡量错误预测浪费。
- `jalrInsts`：实际执行的 JALR 数量。
- `rasPreds`：RAS 参与 return 预测次数。
- `rasCorrect`：RAS return 预测正确次数。

建议新增 ICache 指标：

- `icacheAccesses`
- `icacheHits`
- `icacheMisses`
- `icacheDemandRefills`
- `icachePrefetchReqs`
- `icachePrefetchAccepted`
- `icachePrefetchDropped`
- `icachePrefetchRefills`
- `icachePrefetchUseful`

建议新增 DCache 指标：

- `dcacheLoads`
- `dcacheStores`
- `dcacheHits`
- `dcacheMisses`
- `dcacheWritebacks`
- `dcacheDemandRefills`
- `dcachePrefetchReqs`
- `dcachePrefetchAccepted`
- `dcachePrefetchDropped`
- `dcachePrefetchRefills`
- `dcachePrefetchUseful`

## Prefetch Metrics

预取器有用性建议用 cache line 级 `prefetched` 标记统计：

- 预取 refill 写入 cache line 时置 `prefetched = 1`。
- 后续 demand fetch/load 命中该 line 时，计 `prefetchUseful += 1`，并清除该 line 的 `prefetched`。
- 若预取请求目标行已经在 cache 中，计入 `prefetchDropped` 或 `prefetchRedundant`；第一版建议统一归入 `prefetchDropped`。
- `prefetched` 标记只用于统计，不参与数据选择、替换策略或 stall 控制。

派生指标：

- `prefetchAcceptRate = prefetchAccepted / prefetchReqs`
- `prefetchUseRate = prefetchUseful / prefetchRefills`
- `prefetchDropRate = prefetchDropped / prefetchReqs`

预取测试模式：

- `none`：关闭全部预取。
- `next-line`：只打开 ICache next-line。
- `stride`：打开 DCache stride。
- `stream`：打开 DCache stream。

Dhrystone 中的 `next-line` 只代表 ICache next-line，不再代表 DCache next-line。

## Branch Prediction Metrics

分支预测对比应输出：

- 总控制流指令数：`branchInsts`
- 总预测次数：`branchPreds`
- 正确预测次数：`branchCorrect`
- 错误预测次数：`branchMispredicts`
- 方向错误次数：`branchDirectionMispredicts`
- 目标错误次数：`branchTargetMispredicts`
- 错误路径浪费指令数：`wrongPathFlushInsts`
- RAS push/pop 次数：`rasPushes`、`rasPops`
- RAS return 预测次数和正确次数：`rasPreds`、`rasCorrect`

派生指标：

- `branchAccuracy = branchCorrect / branchInsts`
- `mispredictRate = branchMispredicts / branchInsts`
- `targetMispredictRate = branchTargetMispredicts / branchInsts`
- `wrongPathInstPerMispredict = wrongPathFlushInsts / branchMispredicts`
- `rasAccuracy = rasCorrect / rasPreds`

分支预测测试模式：

- `BPU`
- `BPU + RAS`
- `TAGE + RAS`

## Cache Parameter Metrics

cache 参数对比应记录：

- ICache 大小和路数：`iCacheKB`、`iWay`
- DCache 大小和路数：`dCacheKB`、`dWay`
- ICache hit/miss/access/refill 统计。
- DCache load/store/hit/miss/writeback/refill 统计。
- stall cycles：`icacheStallCycles`、`dcacheStallCycles`
- 总体性能：`cycles`、`instRetired`、`IPC`、`CPI`

派生指标：

- `icacheHitRate = icacheHits / icacheAccesses`
- `dcacheHitRate = dcacheHits / (dcacheLoads + dcacheStores)`
- `icacheMissRate = icacheMisses / icacheAccesses`
- `dcacheMissRate = dcacheMisses / (dcacheLoads + dcacheStores)`
- `avgIcacheMissPenalty = icacheStallCycles / icacheMisses`
- `avgDcacheMissPenalty = dcacheStallCycles / dcacheMisses`

建议测试矩阵：

- cache size：4KB、8KB、16KB、32KB。
- 默认 way：4-way。
- 固定 8KB 时额外比较 4-way 和 8-way。
- 如时间允许，再扩展到 ICache/DCache 不同大小组合。

## Test And Logging

建议新增 Ubuntu 虚拟机端主入口：

```bash
tests/run_perf_suite.sh
```

每次运行创建：

```text
reports/
  run-YYYYmmdd-HHMMSS/
    logs/
      smoke.txt
      dhrystone.txt
      coremark.txt
      prefetch_dhrystone.txt
      prefetch_synthetic.txt
      branch_predictor.txt
      cache_params.txt
    summary.txt
    summary.csv
  latest_summary.txt
```

`reports/` 默认加入 `.gitignore`，日志只在本地留底，不进入 Git。

标准测试项目：

- `TopSpec -- -z smoke`
- `DhrystoneSpec`
- `CoreMarkSpec`
- `PrefetchDhrystoneSpec`
- `PrefetchSpec`
- `BranchPredictorDhrystoneSpec`
- `CacheSizeDhrystoneSpec`

建议保留单项入口：

```bash
bash tests/run_perf_suite.sh --quick
bash tests/run_perf_suite.sh --dhrystone
bash tests/run_perf_suite.sh --coremark
bash tests/run_perf_suite.sh --prefetch
bash tests/run_perf_suite.sh --branch
bash tests/run_perf_suite.sh --cache
```

## Output Format

每个 Scala spec 输出机器可解析的一行或多行标签，例如：

```text
[perf-dhry] status=OK cycles=... instret=... ipc=... cpi=... istall=... dstall=...
[perf-coremark] status=OK cycles=... instret=... ipc=... cpi=...
[perf-prefetch] program=dhrystone mode=stride cycles=... instret=... useful=... refills=...
[perf-bpred] mode=tage branchInsts=... branchCorrect=... mispredicts=...
[perf-cache] program=dhrystone iKB=8 dKB=8 iWay=4 dWay=4 cycles=...
```

runner 只解析这些标签，不解析大段程序原始输出。

`summary.csv` 建议字段：

```text
suite,program,mode,iCacheKB,dCacheKB,iWay,dWay,status,cycles,instRetired,IPC,CPI,
icacheStall,dcacheStall,loadUse,branchInsts,branchCorrect,branchMispredicts,
wrongPathFlushInsts,icacheHits,icacheMisses,dcacheHits,dcacheMisses,
prefetchReqs,prefetchAccepted,prefetchRefills,prefetchUseful
```

## Implementation Notes

- `InOrderCore` 负责全局 retire、flush、branch predictor 结果类计数。
- `IFStage` / `ICacheTop` 暴露 ICache hit/miss/prefetch 事件。
- `MEMStage` / `DCacheTop` 暴露 DCache load/store/hit/miss/writeback/prefetch 事件。
- `SimTop` 只透传扩展后的 `CorePerfCounters`。
- 计数器只在真实握手或流水线真实推进时更新，避免 stall 周期重复计数。
- 新增统计信号不得改变功能路径，不参与数据选择、替换策略或 stall 控制。
- CoreMark 默认使用当前短测版本，不追求官方 10 秒规则。

## Assumptions

- 本方案当前只作为实现设计文档。
- 主运行环境是 Ubuntu 虚拟机。
- `reports/` 只本地保留，不提交 Git。
- Dhrystone 的 next-line 模式只表示 ICache next-line。
- DCache 只比较 stride 和 stream 预取。
- 预取 useful 以“预取行后续被 demand 命中一次”为准，不统计更复杂的多次复用收益。
