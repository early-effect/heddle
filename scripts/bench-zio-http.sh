#!/usr/bin/env bash
# Plaintext keep-alive for the zio-http 3 bench server alone.
# Run scripts/bench-heddle.sh only after this process has exited.
# Local / manual. Not a PR gate.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
# shellcheck source=bench-run.sh
source "$ROOT/scripts/bench-run.sh"
bench_run bench.ZioHttpPlain "${PORT:-${ZIO_PORT:-8089}}" zio-http
