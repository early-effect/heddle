#!/usr/bin/env bash
# Linux system libraries for the Playwright browsers that `chekhovInstall` pinned. The CLI comes from the Chekhov
# cache, so the Playwright version is named once, by sbt-chekhov.
set -euo pipefail
# The Chekhov cache, resolved as Chekhov resolves it on Linux: CHEKHOV_CACHE, else $XDG_CACHE_HOME/chekhov, else
# ~/.cache/chekhov.
cache="${CHEKHOV_CACHE:-${XDG_CACHE_HOME:-${HOME}/.cache}/chekhov}"
cli="$(find "${cache}/playwright" -path '*/node_modules/playwright/cli.js' -print -quit 2>/dev/null || true)"
if [ -z "${cli}" ]; then
  echo "no Chekhov-pinned Playwright CLI under ${cache}; run 'sbt appsBrowser/chekhovInstall' first" >&2
  exit 1
fi
sudo node "${cli}" install-deps chromium firefox webkit
