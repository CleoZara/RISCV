# 流水线框架说明

## IF 模块

IF 模块负责 PC 生成、BPU 查询、I-Cache 取指、预取器控制和 IF/ID 流水寄存器输出。当前主线固定双发，因此 `issueWidth = 2`。

### 暴露接口说明

| 信号名 | 位宽/类型 | 方向 | 说明 |
| --- | --- | --- | --- |
| `stallIf` | `Bool` | Input | IF 级停顿信号。为 1 时 PC 保持不变，通常由 I-Cache miss、D-Cache miss 或后端阻塞触发。 |
| `flushIf` | `Bool` | Input | IF 级冲刷信号。为 1 时当前取指结果应作废，输出 bubble。 |
| `exRedirectValid` | `Bool` | Input | EX 级 redirect 有效信号，优先级最高。用于分支误预测修正、JALR 目标修正等。 |
| `exRedirectPc` | `UInt(32.W)` | Input | EX 级 redirect 目标 PC。 |
| `idRedirectValid` | `Bool` | Input | ID 级 redirect 有效信号，主要用于 JAL 在译码级提前改向。优先级低于 EX redirect，高于 stall 和 BPU 预测。 |
| `idRedirectPc` | `UInt(32.W)` | Input | ID 级 redirect 目标 PC。 |
| `bpuQueryPc` | `UInt(32.W)` | Output | 送往 BPU 的查询 PC，通常等于当前 `pcFetch`。 |
| `bpuPredTaken` | `Bool` | Input | BPU 返回的预测方向。为 1 表示预测跳转。 |
| `bpuPredTarget` | `UInt(32.W)` | Input | BPU 返回的预测目标地址。仅当 `bpuPredTaken` 为 1 时有效。 |
| `nextLinePrefetchEn` | `Bool` | Input | Next-line 预取器开关。建议由 CSR `prefetchCtrl(0)` 控制。与 stride 预取器开关互不影响。 |
| `stridePrefetchEn` | `Bool` | Input | Stride 预取器开关。建议由 CSR `prefetchCtrl(1)` 控制。与 next-line 预取器开关互不影响。 |
| `out` | `Vec(2, IFIDSlot)` | Output | IF 到 ID 的双发取指结果，每拍最多输出两个 slot。 |
| `icacheStall` | `Bool` | Output | I-Cache stall/miss 信号，送往 Hazard/Control 单元，用于冻结前端或全流水。 |
| `imem` | `MemBusIO` | IO | I-Cache miss/refill 和预取请求使用的外部内存总线。 |
| `debugPc` | `UInt(32.W)` | Output | 调试 PC，通常等于当前 IF 级 PC。 |

### `IFIDSlot` 展开

| 字段名 | 位宽/类型 | 说明 |
| --- | --- | --- |
| `pc` | `UInt(32.W)` | 当前 slot 对应指令的 PC。slot0 为 `fetchPc`，slot1 为 `fetchPc + 4`。 |
| `inst` | `UInt(32.W)` | 当前 slot 的 32 位指令。 |
| `ctrl.valid` | `Bool` | 当前 slot 是否包含有效指令。I-Cache miss、slot1 跨 cache line、flush 或 bubble 时为 0。 |
| `ctrl.kill` | `Bool` | 当前 slot 是否被 flush 杀掉。主要用于调试和后级防御性判断。 |
| `ctrl.allowIn` | `Bool` | 当前 slot 是否允许进入下一级。IF 输出侧可固定为 1，真正写入 IF/ID 寄存器由顶层 stall 控制。 |
| `slotIdx` | `UInt(1.W)` | 双发槽编号。slot0 为 0，slot1 为 1。 |
| `fetchPc` | `UInt(32.W)` | 本拍取指 bundle 的基地址，即 slot0 的 PC。 |
| `predTaken` | `Bool` | BPU 对本拍取指 PC 的预测方向。基础版两个 slot 可共享同一次 BPU 查询结果。 |
| `predTarget` | `UInt(32.W)` | BPU 预测目标地址。 |
| `predNextPc` | `UInt(32.W)` | IF 级根据预测实际选择的下一取指 PC。若预测跳转则为 `predTarget`，否则为顺序下一 bundle PC。 |
| `seqNextPc` | `UInt(32.W)` | 不跳转时的顺序下一 bundle PC。双发固定为 `fetchPc + 8`。 |
| `icacheHit` | `Bool` | 当前取指结果是否来自 I-Cache 命中响应。主要用于调试和性能统计。 |

### 预取开关约定

`nextLinePrefetchEn` 和 `stridePrefetchEn` 是两个独立开关，任意一个关闭都不应影响另一个预取器的内部状态更新和请求生成策略。

建议 CSR 映射如下：

| CSR 字段 | 控制对象 | 说明 |
| --- | --- | --- |
| `prefetchCtrl(0)` | `nextLinePrefetchEn` | 为 1 时允许 next-line 预取器发起预取请求。 |
| `prefetchCtrl(1)` | `stridePrefetchEn` | 为 1 时允许 stride 预取器发起预取请求。 |

当两个预取器同时发起请求时，IF 内部应先保证 demand miss 优先级最高；预取请求之间可先采用固定优先级，例如 next-line 优先于 stride，后续再改为轮询仲裁。

## ID 模块

ID 模块负责接收 IF/ID 流水寄存器中的双发取指结果，完成译码、寄存器堆读地址生成、CSR 读地址生成、槽间发射约束判断，并输出 ID/EX 流水寄存器内容。

当前顺序核采用前缀连续发射策略：槽 0 不能发射时，槽 1 必须同时变为 bubble；槽 1 不能发射时，只影响槽 1，不允许跳过槽 0 去发射更后面的指令。

### 暴露接口说明

| 信号名 | 位宽/类型 | 方向 | 说明 |
| --- | --- | --- | --- |
| `in` | `Vec(2, IFIDSlot)` | Input | 来自 IF/ID 流水寄存器的双发取指结果。 |
| `stallId` | `Bool` | Input | ID 级停顿信号。为 1 时 ID/EX 流水寄存器保持不变，两个槽均冻结。 |
| `flushId` | `Bool` | Input | ID 级冲刷信号。为 1 时当前 ID 输出全部变为 bubble。 |
| `regRs1Addr` | `Vec(2, UInt(5.W))` | Output | 送往 RegFile 的 rs1 读地址。 |
| `regRs2Addr` | `Vec(2, UInt(5.W))` | Output | 送往 RegFile 的 rs2 读地址。 |
| `regRs1Data` | `Vec(2, UInt(32.W))` | Input | RegFile 返回的 rs1 数据。 |
| `regRs2Data` | `Vec(2, UInt(32.W))` | Input | RegFile 返回的 rs2 数据。 |
| `csrRaddr` | `UInt(12.W)` | Output | 送往 CSRFile 的读地址。基础实现建议同周期最多允许一条 CSR 指令进入 EX。 |
| `csrRdata` | `UInt(32.W)` | Input | CSRFile 返回的 CSR 旧值，用于后续 CSR 写回语义。 |
| `idRedirectValid` | `Bool` | Output | ID 级 redirect 有效信号，主要用于 JAL 提前改向。 |
| `idRedirectPc` | `UInt(32.W)` | Output | ID 级 redirect 目标 PC，通常为 JAL 的 `pc + immJ`。 |
| `hazardIdValid` | `Vec(2, Bool)` | Output | 送往 HazardUnit 的 ID 槽有效信号。 |
| `hazardRs1Addr` | `Vec(2, UInt(5.W))` | Output | 送往 HazardUnit 的 rs1 地址。 |
| `hazardRs2Addr` | `Vec(2, UInt(5.W))` | Output | 送往 HazardUnit 的 rs2 地址。 |
| `hazardRs1Use` | `Vec(2, Bool)` | Output | 当前指令是否实际使用 rs1。 |
| `hazardRs2Use` | `Vec(2, Bool)` | Output | 当前指令是否实际使用 rs2。 |
| `hazardRdAddr` | `Vec(2, UInt(5.W))` | Output | 送往 HazardUnit 的 rd 地址。 |
| `hazardRfWen` | `Vec(2, Bool)` | Output | 当前指令是否写通用寄存器。 |
| `loadUseStall` | `Bool` | Input | HazardUnit 给出的 load-use stall。为 1 时槽 0 和槽 1 均冻结，不产生新发射。 |
| `structuralStall` | `Bool` | Input | I-Cache miss、D-Cache miss 或后端结构阻塞的合并停顿。为 1 时所有槽冻结。 |
| `out` | `Vec(2, IDEXBundle)` | Output | ID 到 EX 的双发译码结果。 |

### 内部槽间 RAW 检测

ID 级必须检测同一取指包内的槽间 RAW 冲突。规则如下：

| 条件 | 处理方式 |
| --- | --- |
| `slot0Valid && slot1Valid && slot0.rfWen && slot0.rdAddr =/= 0.U && slot0.rdAddr === slot1.rs1Addr && slot1.rs1Use` | 本周期降级单发射：仅发射槽 0，槽 1 变为 bubble，并在下一周期重新尝试发射。 |
| `slot0Valid && slot1Valid && slot0.rfWen && slot0.rdAddr =/= 0.U && slot0.rdAddr === slot1.rs2Addr && slot1.rs2Use` | 本周期降级单发射：仅发射槽 0，槽 1 变为 bubble，并在下一周期重新尝试发射。 |

组合表达式可写为：

```scala
val slotRaw01 =
  slot0Valid &&
  slot1Valid &&
  slot0.rfWen &&
  (slot0.rdAddr =/= 0.U) &&
  ((slot1.rs1Use && slot0.rdAddr === slot1.rs1Addr) ||
   (slot1.rs2Use && slot0.rdAddr === slot1.rs2Addr))
```

本设计不在 ID 级通过同周期旁路解决槽 0 到槽 1 的 RAW。只要检测到槽间 RAW，槽 1 必须等待槽 0 完成写回后再重新尝试发射。

### 前缀连续发射语义

ID 级发射必须满足前缀连续性。任意槽无法发射时，其后的槽都必须变为 bubble，不能跳过当前槽发射后续指令。

| 条件 | 影响槽位 |
| --- | --- |
| 槽 0 `valid=0`，例如取指包不足或对齐问题 | 槽 0 和槽 1 均为 bubble |
| 槽 1 `valid=0`，例如取指包仅返回 1 条 | 仅槽 1 为 bubble |
| 槽 1 与槽 0 存在内部 RAW 冲突 | 仅槽 1 为 bubble |
| `loadUseStall=1` | 槽 0 和槽 1 均冻结，ID/EX 保持不变 |
| `structuralStall=1`，例如 D-Cache miss 或 I-Cache miss | 所有槽冻结，整条流水线保持不变 |
| `flushId=1` | 槽 0 和槽 1 均变为 bubble |

推荐发射有效信号：

```scala
val slot0CanIssue =
  in(0).ctrl.valid &&
  !in(0).ctrl.kill &&
  !flushId &&
  !stallId &&
  !loadUseStall &&
  !structuralStall

val slot1CanIssue =
  slot0CanIssue &&
  in(1).ctrl.valid &&
  !in(1).ctrl.kill &&
  !slotRaw01
```

输出到 ID/EX 时：

```scala
out(0).ctrl.valid := slot0CanIssue
out(1).ctrl.valid := slot1CanIssue
```

当 `slot1CanIssue=false` 且 `slot0CanIssue=true` 时，本周期为单发射；槽 1 对应 ID/EX 内容必须写成安全 bubble，至少保证：

```scala
rfWen  := false.B
memRen := false.B
memWen := false.B
csrOp  := CSROp.NONE
brType := BrType.BR_NONE
```

### 槽 1 重新尝试发射的要求

当槽 1 因内部 RAW 冲突被降级为 bubble 时，系统必须保证槽 1 指令不会丢失。实现方式二选一：

| 方式 | 说明 |
| --- | --- |
| 保持 IF/ID 寄存器 | 当槽 1 因内部 RAW 停发时，冻结 IF/ID 中的槽 1，下一周期继续尝试发射。实现简单但需要处理槽 0 已发射后的状态。 |
| 引入小型 pending slot | 将未发射的槽 1 保存到 ID 内部 pending 寄存器，下一周期优先作为槽 0 尝试发射。接口更清晰，推荐用于后续重构。 |

为了保持前缀连续发射语义，推荐使用 pending slot 方案：被降级的槽 1 下一周期应作为最老指令优先进入译码/发射，而不是被新取指包覆盖。

## EX 模块

EX 模块负责执行 ID/EX 流水寄存器传入的双发指令，完成 ALU 运算、访存地址计算、分支判断、JALR 目标计算、CSR 操作请求生成、BPU 更新信息生成，并输出 EX/MEM 流水寄存器内容。

ALU 不是独立流水级，而是 EX 级内部的执行单元。双发顺序核中 EX 至少实例化 2 个 ALU，分别服务槽 0 和槽 1。

### 内部建议模块

| 子模块/逻辑 | 说明 |
| --- | --- |
| `ALU0` / `ALU1` | 两个并行 ALU，分别处理槽 0 和槽 1。 |
| 操作数选择逻辑 | 根据 `op1Sel/op2Sel` 选择 `rs1/rs2/pc/imm/pc+4` 等操作数。 |
| 分支比较逻辑 | 根据 `brType` 判断条件分支是否跳转。 |
| 分支目标计算逻辑 | 条件分支目标为 `pc + imm`，fall-through 为 `pc + 4`。 |
| JALR 目标计算逻辑 | JALR 目标为 `(rs1 + imm) & ~1`，在 EX 级产生 redirect。 |
| CSR 执行逻辑 | 生成 CSR 写请求，并把 CSR 旧值传入后续写回路径。 |
| BPU 更新逻辑 | 使用 EX 得到的真实分支方向和目标更新 BPU。 |
| Redirect 判断逻辑 | 比较真实 next PC 与预测 next PC，发现误预测时通知 IF flush/redirect。 |
| EX/MEM 打包逻辑 | 将 ALU 结果、访存控制、写回控制、CSR 结果等打包给 MEM 级。 |

### 暴露接口说明

| 信号名 | 位宽/类型 | 方向 | 说明 |
| --- | --- | --- | --- |
| `in` | `Vec(2, IDEXBundle)` | Input | 来自 ID/EX 流水寄存器的双发译码结果。 |
| `stallEx` | `Bool` | Input | EX 级停顿信号。为 1 时 EX/MEM 流水寄存器保持不变。 |
| `flushEx` | `Bool` | Input | EX 级冲刷信号。为 1 时当前 EX 输出变为 bubble，不允许写寄存器、访存、写 CSR 或更新 BPU。 |
| `rs1Data` | `Vec(2, UInt(32.W))` | Input | 经过旁路网络修正后的 rs1 数据。 |
| `rs2Data` | `Vec(2, UInt(32.W))` | Input | 经过旁路网络修正后的 rs2 数据。 |
| `op1Data` | `Vec(2, UInt(32.W))` | Input | ALU 操作数 1，通常由旁路后的 rs1、PC 或 0 选择得到。 |
| `op2Data` | `Vec(2, UInt(32.W))` | Input | ALU 操作数 2，通常由旁路后的 rs2、立即数或 `PC+4` 选择得到。 |
| `storeData` | `Vec(2, UInt(32.W))` | Input | Store 指令写入内存的数据，应使用旁路后的 rs2 数据。 |
| `csrOpValid` | `Bool` | Output | 当前周期是否有 CSR 操作请求。基础实现建议同周期最多一条 CSR 指令进入 EX。 |
| `csrOpType` | `UInt(2.W)` | Output | CSR 操作类型，对应 `CSROp.WRITE/SET/CLEAR`。主线至少需要支持 `CSROp.WRITE`。 |
| `csrWaddr` | `UInt(12.W)` | Output | CSR 写地址。 |
| `csrWdata` | `UInt(32.W)` | Output | CSR 写入的新值。对 `csrrw` 来说通常为旁路后的 rs1 数据。 |
| `csrOldData` | `UInt(32.W)` | Input | CSR 写入前的旧值，用于后续写回 rd。 |
| `exRedirectValid` | `Bool` | Output | EX 级 redirect 有效信号。用于分支误预测修正、JALR 改向等。 |
| `exRedirectPc` | `UInt(32.W)` | Output | EX 级 redirect 目标 PC。 |
| `bpuUpdateValid` | `Bool` | Output | BPU 更新有效信号。条件分支在 EX 得到真实结果后更新 BPU。 |
| `bpuUpdatePc` | `UInt(32.W)` | Output | 需要更新的分支指令 PC。 |
| `bpuUpdateTaken` | `Bool` | Output | 分支真实方向。为 1 表示实际跳转。 |
| `bpuUpdateTarget` | `UInt(32.W)` | Output | 分支真实目标地址。 |
| `exValid` | `Vec(2, Bool)` | Output | 当前 EX 各槽是否有效，供 HazardUnit/BypassUnit 使用。 |
| `exMemRen` | `Vec(2, Bool)` | Output | 当前 EX 各槽是否为 load 指令，用于 load-use stall 检测。 |
| `exRdAddr` | `Vec(2, UInt(5.W))` | Output | 当前 EX 各槽的目的寄存器地址。 |
| `exRfWen` | `Vec(2, Bool)` | Output | 当前 EX 各槽是否会写通用寄存器。 |
| `exResult` | `Vec(2, UInt(32.W))` | Output | EX 级 ALU 结果，用于旁路。注意 load 指令的该值是访存地址，不是 load 数据。 |
| `out` | `Vec(2, EXMEMBundle)` | Output | EX 到 MEM 的双发流水输出。 |

### ALU 操作数语义

EX 级 ALU 输入建议由旁路单元完成选择后送入 EX：

| 信号 | 说明 |
| --- | --- |
| `rs1Data` | 旁路修正后的 rs1 值，用于分支比较、JALR、CSR 写数据等。 |
| `rs2Data` | 旁路修正后的 rs2 值，用于分支比较。 |
| `op1Data` | 已根据 `op1Sel` 选择完成的 ALU 输入 1。 |
| `op2Data` | 已根据 `op2Sel` 选择完成的 ALU 输入 2。 |
| `storeData` | Store 写内存数据，必须是旁路后的 rs2 值。 |

ALU 连接方式：

```scala
alu(i).io.op1   := op1Data(i)
alu(i).io.op2   := op2Data(i)
alu(i).io.aluOp := in(i).aluOp
```

### 分支与 Redirect 语义

条件分支在 EX 级解析真实方向：

| `brType` | 判断条件 |
| --- | --- |
| `BR_EQ` | `rs1Data === rs2Data` |
| `BR_NE` | `rs1Data =/= rs2Data` |
| `BR_LT` | `rs1Data.asSInt < rs2Data.asSInt` |
| `BR_GE` | `rs1Data.asSInt >= rs2Data.asSInt` |
| `BR_LTU` | `rs1Data < rs2Data` |
| `BR_GEU` | `rs1Data >= rs2Data` |

分支目标与顺序目标：

```scala
val branchTarget = pc + imm
val fallThrough  = pc + 4.U
val actualNextPc = Mux(branchTaken, branchTarget, fallThrough)
```

JALR 目标：

```scala
val jalrTarget = (rs1Data + imm) & "hfffffffe".U
```

EX 级应比较真实 next PC 与 IF 级预测 next PC。若不一致，则产生 redirect：

```scala
val mispred = actualNextPc =/= predNextPc
exRedirectValid := mispred
exRedirectPc    := actualNextPc
```

因此建议 `IDEXBundle` 后续补充从 IF/ID 传下来的预测字段：

| 字段 | 说明 |
| --- | --- |
| `predTaken` | IF 级预测方向。 |
| `predTarget` | IF 级预测目标。 |
| `predNextPc` | IF 级实际选择的下一 PC。 |

### CSR 执行语义

基础主线至少支持 `csrrw`：

```text
CSR[csr] <- rs1
rd       <- old CSR value
```

EX 级生成 CSR 写请求：

```scala
csrOpValid := slotValid && in(i).csrOp =/= CSROp.NONE
csrOpType  := in(i).csrOp
csrWaddr   := in(i).csrAddr
csrWdata   := rs1Data(i)
```

CSR 旧值 `csrOldData` 应进入 EX/MEM 的 `csrRdata`，后续 WB 根据 `wbSel=WB_CSR` 写回 rd。

如果两个槽同周期都是 CSR 指令，基础实现应在 ID 级阻止槽 1 发射，避免 EX 级 CSR 写端口冲突。

### 双发控制流约束

为降低 redirect 仲裁复杂度，基础顺序核建议采用保守规则：

| 情况 | 建议处理 |
| --- | --- |
| 槽 0 是 branch/JAL/JALR | 槽 1 在 ID 级变为 bubble，或至少不允许槽 1 再产生 redirect。 |
| 槽 1 是 branch/JAL/JALR，槽 0 是普通指令 | 可以允许槽 1 进入 EX。 |
| 两个槽都可能产生 redirect | 槽 0 优先，因为槽 0 程序序更老。 |

当前 ID 级已经规定内部 RAW 时槽 1 降级，因此 EX 不需要实现槽 0 到槽 1 的同周期旁路。

### EX 输出有效性

推荐每槽 EX 有效信号：

```scala
val exSlotValid =
  in(i).ctrl.valid &&
  !in(i).ctrl.kill &&
  !flushEx
```

当 `exSlotValid=false` 时，EX/MEM 输出必须为安全 bubble，至少保证：

```scala
out(i).ctrl.valid := false.B
out(i).rfWen  := false.B
out(i).memRen := false.B
out(i).memWen := false.B
out(i).brType := BrType.BR_NONE
```

### EX/MEM 打包字段语义

| 字段 | 说明 |
| --- | --- |
| `pc` | 当前指令 PC。 |
| `inst` | 当前指令编码，主要用于调试。 |
| `aluOut` | ALU 结果。对 load/store 是访存地址；对普通 ALU 指令是运算结果。 |
| `rs2Data` | Store 写内存数据，建议填入旁路后的 `storeData`。 |
| `rdAddr` | 目的寄存器地址。 |
| `wbSel` | WB 阶段写回数据来源选择。 |
| `rfWen` | 是否写通用寄存器。 |
| `memRen` | 是否为 load。 |
| `memWen` | 是否为 store。 |
| `memWd` | 访存宽度，word/half/byte。 |
| `memSigned` | load 是否符号扩展。 |
| `csrRdata` | CSR 旧值，用于 `WB_CSR` 写回。 |
| `ctrl` | 流水控制信息。 |

## MEM 模块

MEM 模块负责接收 EX/MEM 流水寄存器中的双发结果，完成 load/store 访问、D-Cache 接入、mtime MMIO 读、printf 地址输出，以及向 MEM/WB 打包写回数据。

当前 `DCacheTop` 已经在 `src/main/Dcache` 中实现，MEM 模块需要适配它的真实接口。`DCacheTop` 当前是单端口数据 cache，因此基础顺序核建议同周期最多允许一条访存指令进入 MEM；如果两个槽同时是 load/store，应在 ID 或 EX 之前让较年轻槽变为 bubble，或者在 MEM 内部只服务最老访存并冻结流水。

### `DCacheTop` 适配接口

`DCacheTop` 的真实接口如下，MEM 模块应直接连接这些信号：

| DCacheTop 信号 | 位宽/类型 | 方向（相对 DCache） | MEM 侧连接语义 |
| --- | --- | --- | --- |
| `addr` | `UInt(p.ADDR_WIDTH.W)` | Input | 访存地址，来自访存槽的 `EXMEMBundle.aluOut`。 |
| `flush` | `Bool` | Input | cache flush 信号。基础版可接全局 flush 或固定为 0，后续用于 cache 清空。 |
| `wen` | `Bool` | Input | store 使能，来自访存槽 `memWen`。 |
| `wmask` | `UInt(p.WMASK_BITS.W)` | Input | store 字节写掩码，由地址低位和 `memWd` 生成。 |
| `wdata` | `UInt(p.DATA_WIDTH.W)` | Input | store 写数据，来自 EX 级传下来的旁路后 `rs2Data/storeData`。 |
| `memRen` | `Bool` | Input | load 使能，来自访存槽 `memRen`。 |
| `memWd` | `UInt(2.W)` | Input | 访存宽度，直接来自 `EXMEMBundle.memWd`。 |
| `signed` | `Bool` | Input | load 是否符号扩展，直接来自 `EXMEMBundle.memSigned`。 |
| `rdata` | `UInt(p.DATA_WIDTH.W)` | Output | load 读出并完成扩展后的数据，送入对应槽的 `MEMWBBundle.memData`。 |
| `missOut` | `Bool` | Output | D-Cache miss/stall 状态，可送 Hazard/Control 作为 `dcacheStall`。 |
| `stall` | `Bool` | Output | 与 `missOut` 语义一致，表示 D-Cache 正在处理 miss，流水线应冻结。 |
| `mtimeLo` | `UInt(p.DATA_WIDTH.W)` | Input | 来自 CSRFile 的 `mtimeLo`，用于 DCache 内部 MMIO 地址读取。 |
| `mtimeHi` | `UInt(p.DATA_WIDTH.W)` | Input | 来自 CSRFile 的 `mtimeHi`，用于 DCache 内部 MMIO 地址读取。 |
| `printChar` | `Valid(UInt(8.W))` | Output | printf 地址 store 产生的字符输出。 |
| `mem` | `MemBusIO(p)` | IO | D-Cache miss/writeback/refill 使用的外部内存总线。 |

### MEM 暴露接口说明

| 信号名 | 位宽/类型 | 方向 | 说明 |
| --- | --- | --- | --- |
| `in` | `Vec(2, EXMEMBundle)` | Input | 来自 EX/MEM 流水寄存器的双发结果。 |
| `stallMem` | `Bool` | Input | MEM 级停顿信号。为 1 时 MEM/WB 流水寄存器保持不变。 |
| `flushMem` | `Bool` | Input | MEM 级冲刷信号。为 1 时当前 MEM 输出应变为 bubble，不能写回。 |
| `mtimeLo` | `UInt(32.W)` | Input | CSRFile 输出的 mtime 低 32 位，透传给 DCacheTop。 |
| `mtimeHi` | `UInt(32.W)` | Input | CSRFile 输出的 mtime 高 32 位，透传给 DCacheTop。 |
| `dcacheFlush` | `Bool` | Input | D-Cache flush 控制信号。基础版可由顶层固定为 0。 |
| `dcacheStall` | `Bool` | Output | D-Cache miss/stall 状态，送往 Hazard/Control 触发全流水冻结。 |
| `printChar` | `Valid(UInt(8.W))` | Output | printf 地址 store 的字符输出。 |
| `dmem` | `MemBusIO(p)` | IO | D-Cache 对外内存总线。 |
| `out` | `Vec(2, MEMWBBundle)` | Output | MEM 到 WB 的双发流水输出。 |

### MEM 内部访存槽选择

由于当前 D-Cache 是单端口，MEM 每周期只能向 D-Cache 发起一个 load/store。推荐选择程序序最老的有效访存槽：

```scala
val slot0Mem = in(0).ctrl.valid && (in(0).memRen || in(0).memWen)
val slot1Mem = in(1).ctrl.valid && (in(1).memRen || in(1).memWen)

val memSel0 = slot0Mem
val memSel1 = !slot0Mem && slot1Mem
```

基础实现更推荐在 ID 阶段禁止双访存同发，这样 MEM 中不会出现 `slot0Mem && slot1Mem`。如果仍然出现，应当以槽 0 为准，并触发流水冻结或断言，防止槽 1 访存丢失。

### Store Mask 生成

MEM 级根据地址低 2 位和 `memWd` 生成 DCache `wmask`：

| 访存宽度 | `addr(1,0)` | `wmask` 语义 |
| --- | --- | --- |
| word | 任意，通常要求对齐 | `1111` |
| half | `00` | `0011` |
| half | `10` | `1100` |
| byte | `00` | `0001` |
| byte | `01` | `0010` |
| byte | `10` | `0100` |
| byte | `11` | `1000` |

若不实现精确异常，非对齐访问可先按硬件自然掩码处理或在测试中避免。

### MEM/WB 打包规则

| 输入类型 | MEM/WB 字段填充 |
| --- | --- |
| 普通 ALU/JAL/CSR 指令 | `memData := 0.U`，其余写回信息从 EX/MEM 透传。 |
| load 指令 | `memData := dcache.io.rdata`，`wbSel` 保持 `WB_MEM`。 |
| store 指令 | `rfWen := false.B`，不写回通用寄存器。 |
| bubble/flush | `ctrl.valid := false.B`，`rfWen := false.B`，`memData := 0.U`。 |

### MEM 停顿语义

当 `dcache.io.stall` 或 `dcache.io.missOut` 为 1 时：

| 行为 | 说明 |
| --- | --- |
| `dcacheStall := true.B` | 通知 Hazard/Control。 |
| IF/ID/EX/MEM/WB | 基础版建议全流水冻结，直到 D-Cache miss 完成。 |
| MEM/WB | 保持原值，不提交新的 load 结果。 |

## WB 模块

WB 模块负责从 MEM/WB 流水寄存器中选择最终写回数据，驱动 RegFile 写端口，并产生提交计数信号给 CSRFile。

### 暴露接口说明

| 信号名 | 位宽/类型 | 方向 | 说明 |
| --- | --- | --- | --- |
| `in` | `Vec(2, MEMWBBundle)` | Input | 来自 MEM/WB 流水寄存器的双发结果。 |
| `regWen` | `Vec(2, Bool)` | Output | RegFile 写使能。无效槽、`rd=x0` 或 `rfWen=0` 时必须为 0。 |
| `regWaddr` | `Vec(2, UInt(5.W))` | Output | RegFile 写地址。 |
| `regWdata` | `Vec(2, UInt(32.W))` | Output | RegFile 写数据。 |
| `instRetire` | `Vec(2, Bool)` | Output | 每个槽是否成功退休，用于 CSRFile 统计 `minstret`。 |
| `wbValid` | `Vec(2, Bool)` | Output | 写回槽有效信号，供 BypassUnit 使用。 |
| `wbRfWen` | `Vec(2, Bool)` | Output | 写回槽是否写通用寄存器，供 BypassUnit 使用。 |
| `wbRdAddr` | `Vec(2, UInt(5.W))` | Output | 写回目的寄存器号，供 BypassUnit 使用。 |
| `wbData` | `Vec(2, UInt(32.W))` | Output | 写回数据，供 BypassUnit 使用。 |

### 写回数据选择

WB 根据 `wbSel` 选择写回数据：

| `wbSel` | 写回数据 |
| --- | --- |
| `WB_ALU` | `aluOut` |
| `WB_MEM` | `memData` |
| `WB_PC4` | `pc + 4` |
| `WB_CSR` | `csrRdata` |

推荐组合逻辑：

```scala
val wbData = MuxLookup(in(i).wbSel, in(i).aluOut, Seq(
  WbSel.WB_ALU -> in(i).aluOut,
  WbSel.WB_MEM -> in(i).memData,
  WbSel.WB_PC4 -> (in(i).pc + 4.U),
  WbSel.WB_CSR -> in(i).csrRdata
))
```

### 双写端口语义

RegFile 支持双写端口时，WB 两个槽可同时写回。若槽 0 和槽 1 同周期写同一个非零 `rd`，必须保证程序序更年轻的槽 1 最终可见。

| 情况 | 处理 |
| --- | --- |
| `rd=x0` | 写使能强制为 0。 |
| 槽 0、槽 1 写不同 rd | 两个写端口同时写。 |
| 槽 0、槽 1 写同一非零 rd | 槽 1 优先，RegFile 内部或 WB 写端口仲裁必须保证槽 1 覆盖槽 0。 |

### 退休计数语义

`instRetire(i)` 建议定义为：

```scala
instRetire(i) := in(i).ctrl.valid && !in(i).ctrl.kill
```

如果后续加入异常、阻塞提交或精确退休，再把该定义收紧。当前课程要求不实现精确异常，因此基础版可按 WB 有效槽计数。

## RegFile 模块

RegFile 是 32 个 32 位通用寄存器文件，服务双发 ID 读和双发 WB 写。

### 暴露接口说明

| 信号名 | 位宽/类型 | 方向 | 说明 |
| --- | --- | --- | --- |
| `rs1Addr` | `Vec(2, UInt(5.W))` | Input | 两个槽的 rs1 读地址。 |
| `rs2Addr` | `Vec(2, UInt(5.W))` | Input | 两个槽的 rs2 读地址。 |
| `rs1Data` | `Vec(2, UInt(32.W))` | Output | 两个槽的 rs1 读数据。 |
| `rs2Data` | `Vec(2, UInt(32.W))` | Output | 两个槽的 rs2 读数据。 |
| `wen` | `Vec(2, Bool)` | Input | 两个写端口的写使能，来自 WB。 |
| `waddr` | `Vec(2, UInt(5.W))` | Input | 两个写端口的写地址。 |
| `wdata` | `Vec(2, UInt(32.W))` | Input | 两个写端口的写数据。 |

### RegFile 语义约定

| 规则 | 说明 |
| --- | --- |
| `x0` 恒为 0 | 读 `x0` 必须返回 0，写 `x0` 必须忽略。 |
| 双读双写 | 双发需要 4 个读端口和 2 个写端口。 |
| 同周期读写同一寄存器 | 建议实现 write-first 或在 ID/EX 旁路中覆盖，保证读到最新可见值。 |
| 双写同一寄存器 | 若两个写端口写同一非零寄存器，槽 1 优先。 |

推荐双写优先级：

```scala
when(wen(0) && waddr(0) =/= 0.U) {
  regs(waddr(0)) := wdata(0)
}
when(wen(1) && waddr(1) =/= 0.U) {
  regs(waddr(1)) := wdata(1)
}
```

这样当两个端口写同一地址时，后写的槽 1 覆盖槽 0。

## CSRFile 模块

CSRFile 负责实现基础 CSR、性能计数器、mtime 计数器和预取开关。当前源码中的 `CSRFile` 已经包含 `mcycle`、`mcycleh`、`minstret`、`mcountinhibit`、`misa`、`prefetchCtrl`、`mtimeLo/mtimeHi`。

### 暴露接口说明

| 信号名 | 位宽/类型 | 方向 | 说明 |
| --- | --- | --- | --- |
| `raddr` | `UInt(12.W)` | Input | CSR 读地址。基础实现只有一个读端口，因此 ID 阶段应限制同周期最多一条 CSR 指令。 |
| `rdata` | `UInt(32.W)` | Output | CSR 读数据。 |
| `opValid` | `Bool` | Input | EX 级 CSR 操作有效信号。 |
| `opType` | `UInt(2.W)` | Input | CSR 操作类型，使用 `CSROp.WRITE/SET/CLEAR`。主线至少需要 `CSROp.WRITE`。 |
| `waddr` | `UInt(12.W)` | Input | CSR 写地址。 |
| `wdata` | `UInt(32.W)` | Input | CSR 写数据。 |
| `oldData` | `UInt(32.W)` | Output | CSR 写入前旧值，用于 `csrrw` 写回 rd。 |
| `cycleTick` | `Bool` | Input | 周期计数使能。为 1 且 `mcountinhibit(0)=0` 时，`mcycle` 自增。 |
| `instRetire` | `Vec(2, Bool)` | Input | WB 阶段每槽退休信号，用于更新 `minstret`。 |
| `mcycleLo` | `UInt(32.W)` | Output | `mcycle` 低 32 位。 |
| `mcycleHi` | `UInt(32.W)` | Output | `mcycle` 高 32 位。 |
| `minstretLo` | `UInt(32.W)` | Output | `minstret` 低 32 位。 |
| `mtimeLo` | `UInt(32.W)` | Output | `mtime` 低 32 位，送 DCacheTop 处理 MMIO 读。 |
| `mtimeHi` | `UInt(32.W)` | Output | `mtime` 高 32 位，送 DCacheTop 处理 MMIO 读。 |
| `prefetchCtrl` | `UInt(32.W)` | Output | 预取控制 CSR。`bit0` 控制 next-line，`bit1` 控制 stride。 |

### CSR 地址约定

| CSR | 地址 | 说明 |
| --- | --- | --- |
| `mcycle` | `0xB00` | 周期计数低 32 位。 |
| `mcycleh` | `0xB80` | 周期计数高 32 位。 |
| `minstret` | `0xB02` | 退休指令计数低 32 位。 |
| `mcountinhibit` | `0x320` | 计数器抑制控制。 |
| `misa` | `0x301` | ISA 信息，只读。 |
| `prefetchCtrl` | `0x7C0` | 自定义预取控制 CSR。 |

### 预取控制位

| 位 | 名称 | 说明 |
| --- | --- | --- |
| `prefetchCtrl(0)` | `nextLinePrefetchEn` | 为 1 时允许 next-line 预取器发起请求。 |
| `prefetchCtrl(1)` | `stridePrefetchEn` | 为 1 时允许 stride 预取器发起请求。 |

### `csrrw` 语义

主线至少需要支持：

```text
old = CSR[csr]
CSR[csr] = rs1
rd = old
```

EX 级应把 `rs1Data` 作为 `wdata`，CSRFile 输出 `oldData`，随后 WB 通过 `WB_CSR` 写回 `rd`。

### CSR 双发限制

由于当前 CSRFile 只有一个读端口和一个写请求端口，基础顺序核应在 ID 级限制同周期最多一条 CSR 指令进入 EX。若槽 0 和槽 1 都是 CSR 指令，应只发射槽 0，槽 1 下一周期重新尝试。
