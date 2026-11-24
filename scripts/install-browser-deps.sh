#!/usr/bin/env bash
# Linux system libraries, using the same pinned CLI as Histos CI and Chekhov.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
if [[ "$(uname -s)" != Linux ]]; then
  echo "browser system dependency installation is Linux-only" >&2
  exit 1
fi
# RunnerTools lists Node, LLVM, then Playwright. Read the Playwright pin, not a cached arbitrary version.
version="$(awk '
  /RunnerTools\(/ { tools = 1 }
  tools && /browsers[[:space:]]*=/ { exit }
  tools && /Some\(ToolRevision\("/ {
    count++
    if (count == 2) {
      sub(/.*ToolRevision\("/, "")
      sub(/".*/, "")
      print
      exit
    }
  }
' "$ROOT/histos.scala")"
if [[ ! "$version" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "cannot read the Playwright RunnerTools pin from histos.scala" >&2
  exit 1
fi
cache="${CHEKHOV_CACHE:-${XDG_CACHE_HOME:-${HOME}/.cache}/chekhov}"
cli="${PLAYWRIGHT_DRIVER_CLI:-$cache/playwright/$version/node_modules/playwright/cli.js}"
node="${PLAYWRIGHT_NODEJS_PATH:-node}"
if [[ ! -f "$cli" ]]; then
  echo "no Playwright $version CLI at $cli" >&2
  echo "Histos browser CI installs that pin in $cache/playwright/$version." >&2
  echo "For local setup, install that pin there or set PLAYWRIGHT_DRIVER_CLI to its cli.js." >&2
  exit 1
fi
"$node" -e '
  const fs = require("fs"), path = require("path");
  const pkg = JSON.parse(fs.readFileSync(path.join(path.dirname(process.argv[1]), "package.json"), "utf8"));
  if (pkg.name !== "playwright" || pkg.version !== process.argv[2]) {
    console.error("Playwright CLI does not match Histos pin " + process.argv[2]);
    process.exit(1);
  }
' "$cli" "$version"
sudo "$node" "$cli" install-deps chromium firefox webkit
