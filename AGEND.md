# 顺序双发核心统筹 AGEND

## 1. 项目目标

主线目标是完成一个具有 cache 和 next-line 预取功能的 RV32I 顺序双发五级流水处理器，并至少跑通 Dhrystone。

当前主线聚焦顺序核，不展开乱序实现。加分项如 RV32M、TAGE/RAS、Coremark、参数化 cache、复杂预取器等，先作为后续扩展路线保留。

必须覆盖的基础能力：

- RV32I 指令支持，除 `ecall`、`fence`、`ebreak` 外应覆盖基本整数指令。
- 双发五级流水：IF、ID、EX、MEM、WB。
- EX 级至少 2 个 ALU。
- `csrrw`、`mtime`、`mcycle` CSR。
- bi-mode 分支预测器。
- 8KB、64B line、4 路或 8 路组相联 cache。
- LRU 替换、写回、写分配。
- printf 地址 store 绕过 cache，直接写内存。
- next-line prefetch，并支持开关控制。
- Dhrystone 验收。

## 2. 当前现状

### 2.1 已有模块雏形

当前 `src/main` 下已经有以下顺序核相关模块基础：

- `ALU.scala`：参数化 ALU，包含 RV32I 基本运算，预留 RV32M 逻辑。
- `Decoder_c.scala`：RV32I 译码，输出 `DecodedSlot`。
- `Defines_c.scala`：指令编码、控制信号枚举、流水寄存器 bundle、内存总线 bundle。
- `CSR.scala`：CSR 文件，包含 `mcycle`、`mtime`、`minstret`、`prefetchCtrl` 等。
- `BPU.scala`：bi-mode BPU。
- `BPU_RAS.scala`、`TAGE.scala`：RAS/TAGE 加分项雏形。
- `PcGen.scala`：PC 选择和 redirect 优先级。
- `iCache.scala`：FetchStage，尝试连接 ICacheTop 和 next-line prefetcher。
- `NextLinePrefetcher.scala`：next-line 预取器。
- `BypassHazardUnit.scala`：旁路网络、load-use stall、槽间 RAW/WAW 检测等。

### 2.2 主要缺口

当前还缺少把顺序核跑起来所需的关键闭环：

- `Top` / `Core` 顶层互联。
- 寄存器堆。
- IF/ID、ID/EX、EX/MEM、MEM/WB 流水寄存器的统一实例化和更新策略。
- ID 级双发译码、寄存器读、槽间发射裁剪。
- EX 级双 ALU、分支解析、JALR 目标计算、CSR 写请求。
- MEM 级访存、load 扩展、store mask、D-Cache 接入。
- WB 级双写回仲裁。
- D-Cache 实体。
- I-Cache 真实依赖模块：当前 `FetchStage` 引用了 `icache.ICacheTop` 与 `parameterized_cache.MemBusIO`，但对应源码尚未出现在仓库中。
- 统一内存模型，包含 10 周期 miss 延迟。
- 测试框架和 benchmark 运行路径。

### 2.3 工程风险

- 当前 Scala 源码直接放在 `src/main`，而 sbt 默认源码目录是 `src/main/scala`。后续构建前必须选择一种处理方式：
  - 移动源码到 `src/main/scala`；或
  - 在 `build.sbt` 中显式加入 `src/main` 作为 unmanaged source directory。
- `doc/spec.md` 在当前终端显示存在中文编码乱码，维护文档前需要统一编码或确认编辑器编码设置。
- `src/main/Dcache`、`src/main/Icache`、`src/test` 当前为空目录，需要补齐实现和测试。

## 3. 阶段任务

### P0：工程骨架对齐

- 明确源码目录策略，保证 sbt 能实际编译 `src/main` 下的 Scala 文件。
- 统一 package 结构，避免 `riscv`、`icache`、`parameterized_cache` 混用导致顶层无法连接。
- 只保留一套内存总线语义，避免多个 `MemBusIO` 定义互相冲突。
- 建立最小 ChiselTest 目录，先能跑模块级 smoke test。

完成标准：

- 工程能编译到当前已有模块。
- 顶层接口、cache 接口、CSR 接口命名稳定。

### P1：顺序核最小流水闭环

- 新增顺序核 `Core`，固定 `issueWidth = 2`。
- 实现寄存器堆，支持双发读取和双写回。
- 实例化 IF/ID、ID/EX、EX/MEM、MEM/WB 流水寄存器。
- ID 级并行实例化 2 个 Decoder。
- EX 级实例化 2 个 ALU。
- MEM 级先接入简单单周期 memory stub，后续替换为 D-Cache。
- WB 级完成 ALU、MEM、PC+4、CSR 数据选择并写回。

完成标准：

- 不接 cache 和 BPU 时，能跑简单 RV32I 直线程序。
- 双发情况下能正确处理 slot0、slot1 的 valid 和 bubble。

### P2：hazard、forwarding、stall/flush 集成

- 接入 `PipelineBypassUnit`，覆盖 EX/MEM/WB 到 EX 的旁路。
- 接入 `BypassHazardUnit`，统一生成 `stallIF`、`stallID`、`stallEX`、`stallMEM`、`stallWB`、`flushIF`、`flushID`、`flushEX`。
- 明确槽间 RAW 策略：
  - 若 slot0 结果可在同周期旁路给 slot1，则允许双发。
  - 若 slot0 是 load 或无法同周期旁路，则 slot1 转 bubble。
- 明确 WAW 策略：
  - 同周期双写同一 `rd` 时，程序序更年轻的 slot1 结果最终可见。
  - 写回端口实现必须保证该优先级。

完成标准：

- 通过 ALU-ALU RAW、load-use、slot0-slot1 RAW、WAW、CSR 写回相关小程序。

### P3：分支预测和 PC redirect

- 接入 `BPU.scala` 作为主线分支预测器。
- IF 查询 BPU，PCGen 根据预测结果选择下一 PC。
- EX 级解析条件分支和 JALR，生成 `exRedirectValid/exRedirectPc`。
- ID 级提前解析 JAL，生成 `idRedirectValid/idRedirectPc`。
- BPU 在 EX 级根据真实结果更新。
- 明确 redirect 优先级：
  1. EX redirect。
  2. ID JAL redirect。
  3. stall 保持 PC。
  4. BPU predicted target。
  5. 顺序 `PC + issueWidth * 4`。

完成标准：

- branch taken / not-taken、JAL、JALR、小循环程序结果正确。
- mispredict 后 IF/ID/EX 中错误路径指令被清空。

### P4：CSR 与预取开关

- 接入 `CSRFile`。
- 支持 `csrrw` 主线要求。
- `mcycle` 每周期递增。
- `mtime` 每周期递增，作为简单 MMIO/CSR 可观测计数。
- `prefetchCtrl[0]` 连接到 `NextLinePrefetcher.prefetchEn`。
- CSR 写回结果为写前旧值。

完成标准：

- `csrrw` 能读出旧值并写入新值。
- 软件可通过 CSR 打开/关闭预取器。

### P5：I/D Cache 与 10 周期内存模型

- 实现 D-Cache：
  - 8KB。
  - 64B cache line。
  - 4 路或 8 路组相联。
  - LRU 替换。
  - write-back + write-allocate。
  - 命中 1 周期返回。
  - miss 额外 10 周期 refill。
- 实现 I-Cache 或补齐 `FetchStage` 依赖的 `ICacheTop`。
- 实现统一内存模型，支持固定 10 周期响应。
- 暂不强制支持多个 inflight 请求，优先保证顺序性。
- printf 地址 store 走 uncacheable 路径，直接写内存或输出端口。

完成标准：

- load/store byte、half、word 访问正确。
- store mask 正确。
- cache hit/miss 行为可由测试观测。
- printf 地址不污染 cache。

### P6：next-line prefetch 集成和性能对比

- 在 I-Cache 或 D-Cache miss/refill 逻辑中接入 next-line prefetch 请求。
- 预取请求优先级低于真实 demand miss。
- 当 cache 或内存忙时，允许丢弃预取请求，避免阻塞主线。
- 通过 CSR 分别测试 prefetch on/off。

完成标准：

- 预取开关关闭时行为与无预取一致。
- 预取开启时功能正确，性能计数可对比。

### P7：Dhrystone 验收

- 准备 bare-metal 程序入口、链接脚本和内存镜像加载方式。
- 实现 `success` 退出信号。
- 实现 `printChar.valid/bits` 或等价 printf 输出通路。
- 统计周期数、提交指令数、cache miss 数、prefetch 请求数。

完成标准：

- Dhrystone 能完整运行并正常退出。
- 能记录 prefetch 开启/关闭下的性能数据。

## 4. 风险清单

### 4.1 双发槽间 RAW/WAW

slot1 依赖 slot0 时必须区分可旁路和不可旁路：

- ALU 结果可同周期旁路时允许双发。
- load、CSR 或其他延迟结果不可同周期旁路时，slot1 必须 bubble 或延后。

同周期两个 slot 写同一个 `rd` 时，必须保证程序序更年轻的 slot1 覆盖 slot0。

### 4.2 load-use stall

load 数据最早在 MEM/WB 可用。若下一拍 EX 需要该数据，必须插入 bubble，不能错误地从 EX/MEM 旁路 load 地址。

### 4.3 redirect 优先级

EX redirect 必须高于 ID JAL redirect 和 BPU 预测。flush 必须高于 stall，否则错误路径指令可能被保留下来。

### 4.4 cache miss 全流水停顿

基础实现可以在 D-Cache 或 I-Cache miss 时停住全流水，先保证正确性。后续若支持 inflight，再重新设计顺序性和仲裁。

### 4.5 printf uncacheable 地址

printf 地址 store 必须直接写外部内存或输出端口，不进入 cache，不触发 write-allocate。

### 4.6 CSR 与写回冲突

CSR 指令既要写 CSR，又要把旧 CSR 值写回 `rd`。双发时要限制同周期多个 CSR 写，或实现明确仲裁。

### 4.7 预取请求仲裁

预取请求不能阻塞 demand miss。真实取指/访存请求优先级必须高于 prefetch。

## 5. 验证清单

### 5.1 模块级测试

- ALU：所有 RV32I ALU op。
- Decoder：R/I/S/B/U/J/CSR 指令。
- CSR：`csrrw`、`mcycle`、`mtime`、`prefetchCtrl`。
- BPU：计数器更新、BTB 命中、预测更新 bypass。
- PcGen：EX/ID/stall/BPU/seq 优先级。
- Bypass/Hazard：RAW、WAW、load-use、slot1 stall。
- Cache：hit、miss、LRU、dirty write-back、write-allocate、store mask。
- Prefetcher：enable/disable、next-line 地址、cache busy 丢弃。

### 5.2 流水相关性测试

- ALU -> ALU RAW。
- ALU -> branch RAW。
- load -> ALU load-use。
- slot0 -> slot1 RAW。
- slot0/slot1 WAW。
- branch mispredict flush。
- JAL ID redirect。
- JALR EX redirect。
- store 后 load。

### 5.3 系统级测试

- RV32I 指令覆盖小程序。
- CSR 读写小程序。
- cache miss/refill 小程序。
- printf 输出小程序。
- prefetch on/off 对比小程序。

### 5.4 Benchmark

- 主线：Dhrystone。
- 后续加分：Coremark。

## 6. 默认假设

- 当前阶段只聚焦顺序双发核心统筹。
- 乱序核不进入当前主线。
- RV32M、TAGE/RAS、Coremark、参数化 cache、复杂预取器作为后续加分项。
- 基础 cache miss 先采用单 outstanding 请求，保证顺序性。
- 访存 miss 时可以先全流水停顿，后续再优化。
- 先保证正确性和可验证性，再考虑时序收敛和性能优化。
