#!/usr/bin/env bash
# Matched-config safety probe for the zio-http server alone.
# Run this only after scripts/bench-safety-heddle.sh has exited.
# Local / manual. Not a PR gate. Not a request-rate comparison.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
# shellcheck source=bench-run.sh
source "$ROOT/scripts/bench-run.sh"
bench_serve bench.ZioHttpSafety "${PORT:-${ZIO_HTTP_PORT:-8089}}" zio-http
