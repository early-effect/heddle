#!/usr/bin/env bash
# Plaintext keep-alive for the heddle bench server alone.
# Run scripts/bench-zio-http.sh only after this process has exited.
# Local / manual. Not a PR gate.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
# shellcheck source=bench-run.sh
source "$ROOT/scripts/bench-run.sh"
bench_run bench.HeddlePlain "${PORT:-${HEDDLE_PORT:-8088}}" heddle
