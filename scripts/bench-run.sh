#!/usr/bin/env bash
# Shared by bench-heddle.sh and bench-zio-http.sh. Not a benchmark on its own.
if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
  echo "Run scripts/bench-heddle.sh or scripts/bench-zio-http.sh." >&2
  exit 1
fi

DURATION="${DURATION:-15s}"
CONNECTIONS="${CONNECTIONS:-256}"
JAVA_OPTS="${JAVA_OPTS:--Xms1g -Xmx1g -XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints}"

if command -v bombardier >/dev/null 2>&1; then
  bench_load() {
    bombardier -c "$CONNECTIONS" -d "$DURATION" --http1 -l -p r "$1"
  }
  bench_parse_rps() {
    grep -Eo 'Reqs/sec[[:space:]]+[0-9.]+' | tail -1 | awk '{print $2}'
  }
elif command -v wrk >/dev/null 2>&1; then
  bench_load() {
    wrk -t"$(sysctl -n hw.ncpu 2>/dev/null || nproc)" -c "$CONNECTIONS" -d "$DURATION" --latency "$1"
  }
  bench_parse_rps() {
    grep -Eo 'Requests/sec:[[:space:]]+[0-9.]+' | awk '{print $2}'
  }
else
  bench_load() {
    echo "install bombardier (brew install bombardier) or wrk" >&2
    return 1
  }
  bench_parse_rps() {
    echo "install bombardier (brew install bombardier) or wrk" >&2
    return 1
  }
fi

# One measurement at a time. A second server on this machine splits the cores.
bench_lock() {
  local lock=/tmp/heddle-bench.lock old
  if [[ -f "$lock" ]]; then
    old="$(cat "$lock" 2>/dev/null || true)"
    if [[ -n "$old" ]] && kill -0 "$old" 2>/dev/null; then
      echo "bench pid $old is still running. Run one server, and wait until it exits." >&2
      exit 1
    fi
  fi
  echo $$ >"$lock"
  trap 'rm -f /tmp/heddle-bench.lock' EXIT
}

bench_listener() {
  lsof -nP -iTCP:"$1" -sTCP:LISTEN -t 2>/dev/null | awk 'NR==1 { print; exit }' || true
}

bench_wait_port() {
  local port="$1"
  local _
  for _ in $(seq 1 80); do
    if curl -sf -o /dev/null --max-time 0.2 "http://127.0.0.1:${port}/plaintext"; then
      return 0
    fi
    sleep 0.25
  done
  echo "server on :$port never became ready. log: /tmp/heddle-bench-$port.log" >&2
  return 1
}

bench_wait_closed() {
  local port="$1"
  local _
  for _ in $(seq 1 40); do
    if [[ -z "$(bench_listener "$port")" ]]; then
      return 0
    fi
    sleep 0.25
  done
  echo "port $port is still listening" >&2
  return 1
}

bench_stop() {
  local port="$1" sbt_pid="$2" pid
  pid="$(bench_listener "$port")"
  if [[ -n "$pid" ]]; then
    kill -INT "$pid" 2>/dev/null || true
    local _
    for _ in 1 2 3 4 5 6 7 8; do
      kill -0 "$pid" 2>/dev/null || break
      sleep 0.25
    done
    kill -9 "$pid" 2>/dev/null || true
  fi
  kill "$sbt_pid" 2>/dev/null || true
  wait "$sbt_pid" 2>/dev/null || true
  bench_wait_closed "$port"
}

# main class, port, short name. Prints the load-tool report and one req/s line.
bench_run() {
  local main="$1" port="$2" name="$3" url="http://127.0.0.1:${2}/plaintext"
  local busy sbt_pid out rps
  cd "$ROOT"
  bench_lock
  busy="$(bench_listener "$port")"
  if [[ -n "$busy" ]]; then
    echo "port $port is already in use by pid $busy" >&2
    exit 1
  fi
  export JAVA_OPTS
  echo "building bench..." >&2
  sbt --no-server bench/compile
  sbt --no-server "bench/runMain $main $port" >/tmp/heddle-bench-"$port".log 2>&1 &
  sbt_pid=$!
  if ! bench_wait_port "$port"; then
    bench_stop "$port" "$sbt_pid" || true
    exit 1
  fi
  echo "warmup $url" >&2
  bench_load "$url" >/dev/null || true
  echo "measure $url" >&2
  out="$(bench_load "$url")"
  rps="$(printf '%s\n' "$out" | bench_parse_rps)"
  bench_stop "$port" "$sbt_pid" || echo "warning: server did not release :$port" >&2
  printf '%s\n' "$out"
  echo
  echo "plaintext keep-alive  $name  c=$CONNECTIONS  d=$DURATION  :$port"
  echo "  ${rps} req/s"
}

# main class, port, short name. One server, then the safety probe. No request-rate run.
bench_serve() {
  local main="$1" port="$2" name="$3"
  local busy sbt_pid log probe
  cd "$ROOT"
  bench_lock
  busy="$(bench_listener "$port")"
  if [[ -n "$busy" ]]; then
    echo "port $port is already in use by pid $busy" >&2
    exit 1
  fi
  export JAVA_OPTS
  log="/tmp/heddle-bench-$port.log"
  echo "building bench..." >&2
  sbt --no-server bench/compile
  sbt --no-server "bench/runMain $main $port" >"$log" 2>&1 &
  sbt_pid=$!
  if ! bench_wait_port "$port"; then
    bench_stop "$port" "$sbt_pid" || true
    exit 1
  fi
  echo "safety $name :$port"
  echo "profile from server log:"
  grep -E 'safety profile|maxConnections=' "$log" || echo "profile line missing from $log"
  set +e
  python3 "$ROOT/scripts/bench-safety-probe.py" "$port" "$name"
  probe=$?
  set -e
  bench_stop "$port" "$sbt_pid" || echo "warning: server did not release :$port" >&2
  return "$probe"
}
