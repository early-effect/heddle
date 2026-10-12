#!/usr/bin/env bash
# Matched-config safety probe for the heddle server alone.
# Run scripts/bench-safety-zio-http.sh only after this process has exited.
# Local / manual. Not a PR gate. Not a request-rate comparison.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
# shellcheck source=bench-run.sh
source "$ROOT/scripts/bench-run.sh"
bench_serve bench.HeddleSafety "${PORT:-${HEDDLE_PORT:-8088}}" heddle
