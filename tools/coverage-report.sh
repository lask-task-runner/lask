#!/usr/bin/env bash
# Prints per-module expression coverage of the library as a Markdown
# table, least covered first. The HTML report `stack test --coverage`
# writes shows top-level declarations per module, which says little
# about modules made of a few large functions; this is the number to
# watch instead.
#
# Usage: LASK_TEST_TIX_DIR=<dir> stack test --coverage \
#          && LASK_TEST_TIX_DIR=<dir> tools/coverage-report.sh
#
# The code the test process runs is counted, and with LASK_TEST_TIX_DIR
# the code the `lask` binary runs when the tests start it as well: each
# run leaves its own tix there (see findLask in test/Command/Lask/
# Harness.hs), and they are summed with the test process's. Without it,
# modules reached only through the binary (Entry, Options, ArgCodec,
# ...) record nothing.
set -euo pipefail

cd "$(dirname "$0")/.."

TIX="$(stack path --local-hpc-root 2>/dev/null)/lask/lask-test/lask-test.tix"
HPCDIR="$(stack path --dist-dir 2>/dev/null)/hpc"
[ -f "$TIX" ] || { echo "no coverage data: run 'stack test --coverage' first" >&2; exit 1; }

# The binary's runs, summed with the test process's over every module
# either holds. The binary's Main is left out: its mix is not the test
# suite's Main, which is excluded below anyway.
if [ -n "${LASK_TEST_TIX_DIR:-}" ] && compgen -G "$LASK_TEST_TIX_DIR/*.tix" >/dev/null; then
  SUMMED="$LASK_TEST_TIX_DIR/summed.tix"
  rm -f "$SUMMED"
  stack exec -- hpc sum --union --exclude=Main --output="$SUMMED" "$TIX" "$LASK_TEST_TIX_DIR"/lask-*.tix
  TIX="$SUMMED"
fi

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
