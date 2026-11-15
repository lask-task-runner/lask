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
# The code the two test suites (lask-unit, lask-e2e) run is counted,
# and with LASK_TEST_TIX_DIR the code the `lask` binary runs when the
# tests start it as well: each run leaves its own tix there (see
# findLask in test/support/Support/Harness.hs). Without it, modules
# reached only through the binary (Entry, Options, ArgCodec, ...)
# record nothing.
set -euo pipefail

cd "$(dirname "$0")/.."

HPCROOT="$(stack path --local-hpc-root 2>/dev/null)"
HPCDIR="$(stack path --dist-dir 2>/dev/null)/hpc"

# What each suite that ran left, and the binary's runs.
INPUTS=()
for suite in lask-unit lask-e2e; do
  if [ -f "$HPCROOT/lask/$suite/$suite.tix" ]; then
    INPUTS+=("$HPCROOT/lask/$suite/$suite.tix")
  fi
done
[ ${#INPUTS[@]} -gt 0 ] || { echo "no coverage data: run 'stack test --coverage' first" >&2; exit 1; }
if [ -n "${LASK_TEST_TIX_DIR:-}" ] && compgen -G "$LASK_TEST_TIX_DIR/lask-*.tix" >/dev/null; then
  INPUTS+=("$LASK_TEST_TIX_DIR"/lask-*.tix)
fi

# Summed over every module any of them holds. Each Main is left out:
# the suites' and the binary's are different modules of one name.
TIX="$HPCROOT/summed.tix"
rm -f "$TIX"
stack exec -- hpc sum --union --exclude=Main --output="$TIX" "${INPUTS[@]}"

# The tix also covers the test modules themselves, so the total is
# summed over the library rows rather than taken from hpc.
ROWS=$(stack exec -- hpc report "$TIX" --per-module --hpcdir="$HPCDIR" 2>/dev/null | awk '
  /^-----<module/ { m = $2; sub(/^[^\/]*\//, "", m); sub(/>-----$/, "", m) }
  /expressions used/ && m !~ /Spec$/ && m !~ /^Support\./ && m != "Paths_lask" && m != "Main" {
    print $1, $4, m
  }
' | sort -n)

awk '{ split($2, f, /[(\/)]/); used += f[2]; all += f[3] }
  END { printf "Library expressions: %d%% (%d/%d)\n\n", used * 100 / all, used, all }' <<<"$ROWS"
echo "| Expressions | Module |"
echo "|---:|---|"
awk '{ printf "| %s %s | %s |\n", $1, $2, $3 }' <<<"$ROWS"
