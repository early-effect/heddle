#!/usr/bin/env bash
# Retired. One process measured both servers, and the second result tracked whichever ran first.
echo "bench-compare.sh is retired. It ran both servers in one process." >&2
echo "Run scripts/bench-heddle.sh and scripts/bench-zio-http.sh separately." >&2
echo "Start the second only after the first process has exited." >&2
exit 1
