#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RUN_ID="$(date +%Y%m%d-%H%M%S)"
REPORT_DIR="$ROOT_DIR/reports/run-$RUN_ID"
LOG_DIR="$REPORT_DIR/logs"
SUMMARY_TXT="$REPORT_DIR/summary.txt"
SUMMARY_CSV="$REPORT_DIR/summary.csv"
LATEST="$ROOT_DIR/reports/latest_summary.txt"

mkdir -p "$LOG_DIR"

MAKE_WRAP_DIR=""
if [[ "${PERF_SERIAL_MAKE:-1}" != "0" ]]; then
  MAKE_WRAP_DIR="$(mktemp -d /tmp/riscv-perf-make.XXXXXX)"
  cat > "$MAKE_WRAP_DIR/make" <<'EOF'
#!/usr/bin/env bash
set -e
args=()
argv=("$@")
i=0
while [[ "$i" -lt "${#argv[@]}" ]]; do
  arg="${argv[$i]}"
  case "$arg" in
    -j|--jobs)
      next_i=$((i + 1))
      if [[ "$next_i" -lt "${#argv[@]}" && "${argv[$next_i]}" =~ ^[0-9]+$ ]]; then
        i=$next_i
      fi
      ;;
    -j[0-9]*|--jobs=*)
      ;;
    *)
      args+=("$arg")
      ;;
  esac
  i=$((i + 1))
done
exec /usr/bin/make -j1 "${args[@]}"
EOF
  chmod +x "$MAKE_WRAP_DIR/make"
  export PATH="$MAKE_WRAP_DIR:$PATH"
fi

CSV_FIELDS="suite,program,bench,mode,iCacheKB,dCacheKB,iWay,dWay,status,metricSource,cycles,instRetired,IPC,CPI,simCycles,simInstRetired,simIPC,simCPI,icacheStall,dcacheStall,loadUse,branchInsts,branchPreds,branchCorrect,branchMispredicts,branchDirectionMispredicts,branchTargetMispredicts,wrongPathFlushInsts,jalrInsts,rasPushes,rasPops,rasPreds,rasCorrect,icacheAccesses,icacheHits,icacheMisses,icacheDemandRefills,dcacheLoads,dcacheStores,dcacheHits,dcacheMisses,dcacheWritebacks,dcacheDemandRefills,prefetchReqs,prefetchAccepted,prefetchDropped,prefetchRefills,prefetchUseful,icachePrefetchReqs,icachePrefetchAccepted,icachePrefetchDropped,icachePrefetchRefills,icachePrefetchUseful,dcachePrefetchReqs,dcachePrefetchAccepted,dcachePrefetchDropped,dcachePrefetchRefills,dcachePrefetchUseful"
echo "$CSV_FIELDS" > "$SUMMARY_CSV"
: > "$SUMMARY_TXT"

finish_report() {
  cp "$SUMMARY_TXT" "$LATEST"
  if [[ -n "$MAKE_WRAP_DIR" ]]; then
    rm -rf "$MAKE_WRAP_DIR"
  fi
  echo "reports/run-$RUN_ID"
}
trap finish_report EXIT

usage() {
  cat <<'EOF'
Usage: bash tests/run_perf_suite.sh [--quick] [--quick-cache] [--dhrystone] [--coremark] [--prefetch] [--branch] [--branch-synth] [--cache] [--cache-synth]

With no option, all suites are run.
EOF
}

if [[ $# -eq 0 ]]; then
  set -- --quick --dhrystone --coremark --prefetch --branch --cache
fi

append_perf_lines() {
  local log_file="$1"
  grep '^\[perf-' "$log_file" >> "$SUMMARY_TXT" || true
  grep '^\[perf-' "$log_file" | awk -v fields="$CSV_FIELDS" '
    BEGIN {
      n = split(fields, order, ",")
    }
    {
      delete kv
      tag = $1
      gsub(/^\[/, "", tag)
      gsub(/\]$/, "", tag)
      kv["suite"] = tag
      for (i = 2; i <= NF; i++) {
        eq = index($i, "=")
        if (eq > 0) {
          k = substr($i, 1, eq - 1)
          v = substr($i, eq + 1)
          kv[k] = v
        }
      }
      for (i = 1; i <= n; i++) {
        key = order[i]
        val = kv[key]
        gsub(/"/, "\"\"", val)
        printf "%s%s", (i == 1 ? "" : ","), val
      }
      printf "\n"
    }
  ' >> "$SUMMARY_CSV" || true
}

run_case() {
  local name="$1"
  local log_name="$2"
  shift 2
  local log_file="$LOG_DIR/$log_name"
  echo "== $name ==" | tee -a "$SUMMARY_TXT"
  set +e
  (
    cd "$ROOT_DIR"
    "$@"
  ) 2>&1 | tee "$log_file"
  local status=${PIPESTATUS[0]}
  set -e
  append_perf_lines "$log_file"
  return "$status"
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --quick)
      run_case "smoke" "smoke.txt" sbt "testOnly riscv.TopSpec -- -z smoke"
      ;;
    --quick-cache)
      run_case "quick cache" "cache_quick.txt" sbt "testOnly riscv.CacheSizeDhrystoneSpec -- -n quick-cache"
      ;;
    --dhrystone)
      run_case "dhrystone" "dhrystone.txt" sbt "testOnly riscv.DhrystoneSpec"
      ;;
    --coremark)
      run_case "coremark" "coremark.txt" sbt "testOnly riscv.CoreMarkSpec"
      ;;
    --prefetch)
      run_case "prefetch dhrystone" "prefetch_dhrystone.txt" sbt "testOnly riscv.PrefetchDhrystoneSpec"
      run_case "prefetch synthetic" "prefetch_synthetic.txt" sbt "testOnly riscv.PrefetchSpec"
      ;;
    --branch)
      run_case "branch predictor" "branch_predictor.txt" sbt "testOnly riscv.BranchPredictorDhrystoneSpec"
      run_case "branch synthetic" "branch_synthetic.txt" sbt "testOnly riscv.BranchPredictorSyntheticSpec"
      ;;
    --branch-synth)
      run_case "branch synthetic" "branch_synthetic.txt" sbt "testOnly riscv.BranchPredictorSyntheticSpec"
      ;;
    --cache)
      run_case "cache 4KB" "cache_4kb.txt" sbt "testOnly riscv.CacheSizeDhrystoneSpec -- -z 4KB"
      run_case "cache 8KB" "cache_8kb.txt" sbt "testOnly riscv.CacheSizeDhrystoneSpec -- -z 8KB"
      run_case "cache 16KB" "cache_16kb.txt" sbt "testOnly riscv.CacheSizeDhrystoneSpec -- -z 16KB"
      run_case "cache 32KB" "cache_32kb.txt" sbt "testOnly riscv.CacheSizeDhrystoneSpec -- -z 32KB"
      run_case "cache I32_D16" "cache_i32_d16.txt" sbt "testOnly riscv.CacheSizeDhrystoneSpec -- -z I32_D16"
      run_case "cache I16_D32" "cache_i16_d32.txt" sbt "testOnly riscv.CacheSizeDhrystoneSpec -- -z I16_D32"
      run_case "cache synthetic" "cache_synthetic.txt" sbt "testOnly riscv.CacheSizeSyntheticSpec"
      ;;
    --cache-synth)
      run_case "cache synthetic" "cache_synthetic.txt" sbt "testOnly riscv.CacheSizeSyntheticSpec"
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
  shift
done
