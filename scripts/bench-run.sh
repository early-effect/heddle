#!/usr/bin/env bash
# Shared by benchmark, safety, and profile scripts. Not a benchmark on its own.
if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
  echo "Run scripts/bench-heddle.sh or scripts/bench-zio-http.sh." >&2
  exit 1
fi

DURATION="${DURATION:-15s}"
CONNECTIONS="${CONNECTIONS:-256}"
JAVA_OPTS="${JAVA_OPTS:--Xms1g -Xmx1g}"
BENCH_CLIENT=""
BENCH_PORT=""
BENCH_LOCK=""
BENCH_LOG=""

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

# Atomic exclusion. A stale lock needs manual removal after checking its owner.
bench_lock() {
  local lock=/tmp/heddle-bench.lock.d
  if ! mkdir "$lock" 2>/dev/null; then
    echo "bench lock $lock exists. Check $lock/owner before removing a stale lock." >&2
    return 1
  fi
  BENCH_LOCK="$lock"
  trap 'bench_cleanup' EXIT
  trap 'exit 130' INT
  trap 'exit 143' TERM
  echo $$ >"$lock/owner"
}

bench_listener() {
  lsof -nP -iTCP:"$1" -sTCP:LISTEN -t 2>/dev/null | awk 'NR==1 { print; exit }' || true
}

bench_wait_port() {
  local port="$1" _
  for _ in $(seq 1 80); do
    if ! kill -0 "$BENCH_CLIENT" 2>/dev/null; then
      echo "Histos exited before readiness. log: $BENCH_LOG" >&2
      return 1
    fi
    if curl -sf -o /dev/null --max-time 0.2 "http://127.0.0.1:${port}/plaintext"; then
      return 0
    fi
    sleep 0.25
  done
  echo "server on :$port never became ready. log: $BENCH_LOG" >&2
  return 1
}

bench_wait_closed() {
  local port="$1" _
  for _ in $(seq 1 40); do
    if [[ -z "$(bench_listener "$port")" ]]; then
      return 0
    fi
    sleep 0.25
  done
  echo "port $port is still listening; no unowned process was killed" >&2
  return 1
}

# New selection flags can use the JVM CLI fallback. Its sibling `server` is not owned.
bench_cli_child() {
  ps -axo pid=,ppid=,args= | awk -v parent="$BENCH_CLIENT" \
    '$2 == parent && / -jar / && (/ run bench --platform jvm --main / || / compile bench --platform jvm/) { print $1 }'
}

bench_stop() {
  [[ -n "$BENCH_CLIENT" ]] || return 0
  local run pid _ owned=()
  run="$(awk '/^run [0-9a-f-]+$/ { print $2; exit }' "$BENCH_LOG")"
  if [[ -n "$run" ]]; then
    "$ROOT/histos" --no-color cancel "$run" >&2 || true
  fi
  # Freeze before inspecting children so early startup cannot fork a CLI behind cleanup.
  kill -STOP "$BENCH_CLIENT" 2>/dev/null || true
  while read -r pid; do
    [[ -n "$pid" ]] && owned+=("$pid")
  done < <(bench_cli_child)
  if [[ ${#owned[@]} -eq 0 ]]; then
    kill -TERM "$BENCH_CLIENT" 2>/dev/null || true
  else
    # Socket EOF cancels the scoped run. Let the native parent reap its JVM fallback.
    for pid in "${owned[@]}"; do kill -TERM "$pid" 2>/dev/null || true; done
  fi
  kill -CONT "$BENCH_CLIENT" 2>/dev/null || true
  for _ in 1 2 3 4 5 6 7 8; do
    kill -0 "$BENCH_CLIENT" 2>/dev/null || break
    sleep 0.25
  done
  if kill -0 "$BENCH_CLIENT" 2>/dev/null; then
    if [[ ${#owned[@]} -gt 0 ]]; then
      for pid in "${owned[@]}"; do kill -KILL "$pid" 2>/dev/null || true; done
    fi
    for _ in 1 2 3 4 5 6 7 8; do
      kill -0 "$BENCH_CLIENT" 2>/dev/null || break
      sleep 0.25
    done
    kill -KILL "$BENCH_CLIENT" 2>/dev/null || true
  fi
  wait "$BENCH_CLIENT" 2>/dev/null || true
  BENCH_CLIENT=""
  [[ -z "$BENCH_PORT" ]] || bench_wait_closed "$BENCH_PORT"
}

bench_cleanup() {
  local status=$?
  trap - EXIT
  trap '' INT TERM
  bench_stop || status=1
  if [[ -n "$BENCH_LOCK" ]]; then
    rm -f "$BENCH_LOCK/owner"
    rmdir "$BENCH_LOCK"
    BENCH_LOCK=""
  fi
  exit "$status"
}

bench_start() {
  local main="$1" port="$2" busy
  cd "$ROOT" || return 1
  bench_lock || return 1
  busy="$(bench_listener "$port")"
  if [[ -n "$busy" ]]; then
    echo "port $port is already in use by pid $busy" >&2
    return 1
  fi
  BENCH_LOG="/tmp/heddle-bench-$port.log"
  BENCH_OWNER="heddle-bench-$$-$RANDOM"
  # The JVM parses JAVA_OPTS through JAVA_TOOL_OPTIONS, including quotes, without shell evaluation.
  # Histos passes the caller environment to the child. Typed JvmOptions supply diagnostics.
  export JAVA_TOOL_OPTIONS="${JAVA_TOOL_OPTIONS:+$JAVA_TOOL_OPTIONS }$JAVA_OPTS -Dheddle.bench.owner=$BENCH_OWNER"
  echo "building bench..." >&2
  "$ROOT/histos" --no-color compile bench --platform jvm >"$BENCH_LOG" 2>&1 &
  BENCH_CLIENT=$!
  wait "$BENCH_CLIENT" || {
    local status=$?
    echo "bench compilation failed. log: $BENCH_LOG" >&2
    return "$status"
  }
  BENCH_CLIENT=""
  busy="$(bench_listener "$port")"
  if [[ -n "$busy" ]]; then
    echo "port $port became occupied by pid $busy during compilation" >&2
    return 1
  fi
  BENCH_PORT="$port"
  echo "starting bench..." >&2
  "$ROOT/histos" --no-color run bench --platform jvm --main "$main" -- "$port" >"$BENCH_LOG" 2>&1 &
  BENCH_CLIENT=$!
  bench_wait_port "$port"
}

# main class, port, short name. Prints the load-tool report and one req/s line.
bench_run() {
  local main="$1" port="$2" name="$3" url="http://127.0.0.1:${2}/plaintext" out rps
  bench_start "$main" "$port"
  echo "warmup $url" >&2
  bench_load "$url" >/dev/null || true
  echo "measure $url" >&2
  out="$(bench_load "$url")"
  rps="$(printf '%s\n' "$out" | bench_parse_rps)"
  bench_stop
  printf '%s\n' "$out"
  echo
  echo "plaintext keep-alive  $name  c=$CONNECTIONS  d=$DURATION  :$port"
  echo "  ${rps} req/s"
}

# main class, port, short name. One server, then the safety probe. No request-rate run.
bench_serve() {
  local main="$1" port="$2" name="$3" probe=0
  bench_start "$main" "$port"
  echo "safety $name :$port"
  echo "profile from server log:"
  grep -E 'safety profile|maxConnections=' "$BENCH_LOG" || echo "profile line missing from $BENCH_LOG"
  python3 "$ROOT/scripts/bench-safety-probe.py" "$port" "$name" || probe=$?
  bench_stop
  return "$probe"
}
