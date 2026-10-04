#!/usr/bin/env bash
# Prints per-module expression coverage of the library as a Markdown
# table, least covered first. The HTML report `stack test --coverage`
# writes shows top-level declarations per module, which says little
# about modules made of a few large functions; this is the number to
# watch instead.
#
# Usage: stack test --coverage && tools/coverage-report.sh
#
# Only code the test process itself runs is counted. Modules reached
# solely through the `lask` binary that CliSpec spawns (Entry, Options,
# ArgCodec, ...) record nothing and are absent from the table.
set -euo pipefail

cd "$(dirname "$0")/.."

TIX="$(stack path --local-hpc-root 2>/dev/null)/lask/lask-test/lask-test.tix"
HPCDIR="$(stack path --dist-dir 2>/dev/null)/hpc"
[ -f "$TIX" ] || { echo "no coverage data: run 'stack test --coverage' first" >&2; exit 1; }

# The tix also covers the test modules themselves, so the total is
# summed over the library rows rather than taken from hpc.
ROWS=$(stack exec -- hpc report "$TIX" --per-module --hpcdir="$HPCDIR" 2>/dev/null | awk '
  /^-----<module/ { m = $2; sub(/^[^\/]*\//, "", m); sub(/>-----$/, "", m) }
  /expressions used/ && m !~ /Spec$/ && m != "Paths_lask" && m != "Main" {
    print $1, $4, m
  }
' | sort -n)

awk '{ split($2, f, /[(\/)]/); used += f[2]; all += f[3] }
  END { printf "Library expressions: %d%% (%d/%d)\n\n", used * 100 / all, used, all }' <<<"$ROWS"
echo "| Expressions | Module |"
echo "|---:|---|"
awk '{ printf "| %s %s | %s |\n", $1, $2, $3 }' <<<"$ROWS"
