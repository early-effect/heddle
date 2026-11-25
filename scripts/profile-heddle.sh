#!/usr/bin/env bash
# Record a 30s JFR of HeddlePlain under bombardier/wrk.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"
PORT="${PORT:-8088}"
OUT="${OUT:-$ROOT/target/profile}"
mkdir -p "$OUT"
# shellcheck source=bench-run.sh
source "$ROOT/scripts/bench-run.sh"
bench_start bench.HeddlePlain "$PORT"
pid="$(bench_listener "$PORT")"
# Port ownership alone is not proof: match the unique property of this invocation.
if [[ -z "$pid" ]] || ! jcmd "$pid" VM.system_properties | grep -Fx "heddle.bench.owner=$BENCH_OWNER" >/dev/null; then
  echo "owned HeddlePlain JVM not found. log: $BENCH_LOG" >&2
  exit 1
fi
echo "optional: asprof -d 30 -e cpu -f $OUT/cpu.html $pid  (while the server is up)"
jcmd "$pid" JFR.start name=heddle settings=profile dumponexit=true filename="$OUT/heddle.jfr" duration=30s
if command -v bombardier >/dev/null 2>&1; then
  bombardier -c 256 -d 30s --http1 "http://127.0.0.1:${PORT}/plaintext"
elif command -v wrk >/dev/null 2>&1; then
  wrk -t4 -c256 -d30s "http://127.0.0.1:${PORT}/plaintext"
else
  echo "no bombardier/wrk; JFR still recording idle" >&2
  sleep 30
fi
sleep 2
jcmd "$pid" JFR.dump name=heddle filename="$OUT/heddle.jfr" || true
bench_stop
echo "JFR: $OUT/heddle.jfr"
