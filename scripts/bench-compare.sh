#!/usr/bin/env bash
# Compare heddle vs zio-http 3 plaintext RPS on loopback.
# Local / manual only. Not a PR gate.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

DURATION="${DURATION:-15s}"
CONNECTIONS="${CONNECTIONS:-256}"
MARGIN="${MARGIN:-10}"
HEDDLE_PORT="${HEDDLE_PORT:-8088}"
ZIO_PORT="${ZIO_PORT:-8089}"
JAVA_OPTS="${JAVA_OPTS:--Xms1g -Xmx1g -XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints}"

if command -v bombardier >/dev/null 2>&1; then
  load() {
    local url="$1"
    bombardier -c "$CONNECTIONS" -d "$DURATION" --http1 -l -p r "$url"
  }
  parse_rps() {
    grep -Eo 'Reqs/sec[[:space:]]+[0-9.]+' | tail -1 | awk '{print $2}'
  }
elif command -v wrk >/dev/null 2>&1; then
  load() {
    local url="$1"
    wrk -t"$(sysctl -n hw.ncpu 2>/dev/null || nproc)" -c "$CONNECTIONS" -d "$DURATION" --latency "$url"
  }
  parse_rps() {
    grep -Eo 'Requests/sec:[[:space:]]+[0-9.]+' | awk '{print $2}'
  }
else
  echo "install bombardier (brew install bombardier) or wrk" >&2
  exit 1
fi

wait_port() {
  local port="$1"
  for _ in $(seq 1 80); do
    if curl -sf -o /dev/null --max-time 0.2 "http://127.0.0.1:${port}/plaintext"; then
      return 0
    fi
    sleep 0.25
  done
  echo "server on :$port never became ready" >&2
  return 1
}

run_one() {
  local main="$1" port="$2" url="$3"
  export JAVA_OPTS
  sbt --no-server "bench/runMain $main $port" >/tmp/heddle-bench-$port.log 2>&1 &
  local sbt_pid=$!
  if ! wait_port "$port"; then
    kill "$sbt_pid" 2>/dev/null || true
    return 1
  fi
  echo "warmup $url" >&2
  load "$url" >/dev/null || true
  echo "measure $url" >&2
  local out rps
  out="$(load "$url")"
  echo "$out" >&2
  rps="$(printf '%s\n' "$out" | parse_rps)"
  local java_pid
  java_pid="$(jps -l | awk -v m="$main" '$0 ~ m {print $1; exit}')"
  if [[ -n "${java_pid:-}" ]]; then
    kill -INT "$java_pid" 2>/dev/null || true
    sleep 1
    kill -9 "$java_pid" 2>/dev/null || true
  fi
  kill "$sbt_pid" 2>/dev/null || true
  wait "$sbt_pid" 2>/dev/null || true
  echo "$rps"
}

echo "building bench..."
sbt --no-server bench/compile

heddle_plain="$(run_one bench.HeddlePlain "$HEDDLE_PORT" "http://127.0.0.1:${HEDDLE_PORT}/plaintext")"
zio_plain="$(run_one bench.ZioHttpPlain "$ZIO_PORT" "http://127.0.0.1:${ZIO_PORT}/plaintext")"

echo
echo "plaintext keep-alive  c=$CONNECTIONS  d=$DURATION"
echo "  heddle     ${heddle_plain} req/s"
echo "  zio-http 3 ${zio_plain} req/s"

python3 - "$heddle_plain" "$zio_plain" "$MARGIN" <<'PY'
import sys
h, z, m = float(sys.argv[1] or 0), float(sys.argv[2] or 0), float(sys.argv[3])
if z <= 0:
    print("zio-http RPS missing", file=sys.stderr)
    sys.exit(1)
floor = z * (1 - m / 100)
print(f"  margin     {m}%  (heddle must be >= {floor:.0f})")
if h + 1e-6 < floor:
    print(f"FAIL: heddle {h:.0f} < zio-http {z:.0f} by more than {m}%", file=sys.stderr)
    sys.exit(2)
print("PASS")
PY
