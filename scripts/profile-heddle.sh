#!/usr/bin/env bash
# Record a 30s JFR of HeddlePlain under bombardier/wrk.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"
PORT="${PORT:-8088}"
OUT="${OUT:-$ROOT/target/profile}"
mkdir -p "$OUT"
export JAVA_OPTS="${JAVA_OPTS:--Xms1g -Xmx1g -XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints}"

sbt --no-server bench/compile
sbt --no-server "bench/runMain bench.HeddlePlain $PORT" >/tmp/heddle-profile.log 2>&1 &
sbt_pid=$!
for _ in $(seq 1 80); do
  curl -sf -o /dev/null --max-time 0.2 "http://127.0.0.1:${PORT}/plaintext" && break
  sleep 0.25
done
pid="$(jps -l | awk '/bench.HeddlePlain/{print $1; exit}')"
if [[ -z "${pid:-}" ]]; then
  echo "HeddlePlain JVM not found" >&2
  kill "$sbt_pid" 2>/dev/null || true
  exit 1
fi
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
kill -INT "$pid" 2>/dev/null || true
sleep 1
kill -9 "$pid" 2>/dev/null || true
kill "$sbt_pid" 2>/dev/null || true
echo "JFR: $OUT/heddle.jfr"
echo "optional: asprof -d 30 -e cpu -f $OUT/cpu.html $pid  (while the server is up)"
