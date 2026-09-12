#!/usr/bin/env bash
# Drives the completion scripts `lask completion` generates through the
# shells themselves. Quoting and word-splitting bugs live in the shell
# half of the feature, where the Haskell tests cannot see them.
#
# Usage: tools/completion-smoke.sh [path/to/lask]
# A shell that is not installed is skipped, not failed.
set -uo pipefail

LASK=${1:-lask}
command -v "$LASK" >/dev/null || { echo "not found: $LASK" >&2; exit 1; }
LASK=$(cd "$(dirname "$LASK")" && pwd)/$(basename "$LASK")
ROOT=$(cd "$(dirname "$0")/.." && pwd)
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

failures=0
ok()   { printf '  ok   %s\n' "$1"; }
fail() { printf '  FAIL %s\n' "$1"; failures=$((failures + 1)); }
skip() { printf '  skip %s\n' "$1"; }

# Every case runs against the repository's own main.lask, which defines
# `doctest` and, on `install`, the keyword parameter `--output`.
expect_in() { # <label> <needle> <haystack>
  case "$3" in
    *"$2"*) ok "$1" ;;
    *) fail "$1 (no '$2' in: $(echo "$3" | tr '\n' ' '))" ;;
  esac
}

"$LASK" completion bash > "$TMP/lask.bash"
"$LASK" completion zsh  > "$TMP/_lask"
"$LASK" completion fish > "$TMP/lask.fish"

echo "bash"
if command -v bash >/dev/null; then
  bash -n "$TMP/lask.bash" && ok "syntax" || fail "syntax"
  cat > "$TMP/bash-drive.sh" <<'DRIVE'
source "$1"; shift
COMP_WORDS=("$@")
COMP_CWORD=$(( $# - 1 ))
_lask
printf '%s\n' "${COMPREPLY[@]}"
DRIVE
  out=$(cd "$ROOT" && PATH="$(dirname "$LASK"):$PATH" bash --norc "$TMP/bash-drive.sh" "$TMP/lask.bash" lask run "" 2>&1)
  expect_in "completes function names" "doctest" "$out"
  out=$(cd "$ROOT" && PATH="$(dirname "$LASK"):$PATH" bash --norc "$TMP/bash-drive.sh" "$TMP/lask.bash" lask run install -- 2>&1)
  expect_in "completes keyword parameters" "--output" "$out"
  # bash splits --opt=value at the '='; the script has to put it back.
  out=$(cd "$ROOT" && PATH="$(dirname "$LASK"):$PATH" bash --norc "$TMP/bash-drive.sh" "$TMP/lask.bash" lask check --module = ma 2>&1)
  expect_in "rejoins --opt=value" "main.lask" "$out"
else
  skip "bash not installed"
fi

echo "zsh"
if command -v zsh >/dev/null; then
  zsh -n "$TMP/_lask" && ok "syntax" || fail "syntax"
  cat > "$TMP/zsh-drive.zsh" <<'DRIVE'
emulate -L zsh
compdef() { : }
_describe() { shift 3; local arr=$1; print -r -- "${(P)arr}" }
_message() { print -r -- "$2" }
_files() { print -r -- "FILES" }
source $1; shift
typeset -ga words; words=(lask "$@")
typeset -gi CURRENT=$#words
_lask
DRIVE
  out=$(cd "$ROOT" && PATH="$(dirname "$LASK"):$PATH" zsh -f "$TMP/zsh-drive.zsh" "$TMP/_lask" run "" 2>&1)
  expect_in "completes function names with descriptions" "doctest:Run the doctests" "$out"
  out=$(cd "$ROOT" && PATH="$(dirname "$LASK"):$PATH" zsh -f "$TMP/zsh-drive.zsh" "$TMP/_lask" run install -- 2>&1)
  expect_in "completes keyword parameters" "--output" "$out"
  out=$(cd "$ROOT" && PATH="$(dirname "$LASK"):$PATH" zsh -f "$TMP/zsh-drive.zsh" "$TMP/_lask" run install "" 2>&1)
  expect_in "falls back to files where nothing is known" "FILES" "$out"
else
  skip "zsh not installed"
fi

echo "fish"
if command -v fish >/dev/null; then
  fish --no-execute "$TMP/lask.fish" && ok "syntax" || fail "syntax"
  # `complete -C` runs the completion machinery without a terminal.
  out=$(cd "$ROOT" && PATH="$(dirname "$LASK"):$PATH" fish --no-config \
          -c "source $TMP/lask.fish; complete -C'lask run '" 2>&1)
  expect_in "completes function names" "doctest" "$out"
  out=$(cd "$ROOT" && PATH="$(dirname "$LASK"):$PATH" fish --no-config \
          -c "source $TMP/lask.fish; complete -C'lask run install --'" 2>&1)
  expect_in "completes keyword parameters" "--output" "$out"
else
  skip "fish not installed"
fi

echo
if [ "$failures" -eq 0 ]; then
  echo "completion smoke test: all checks passed"
else
  echo "completion smoke test: $failures check(s) failed" >&2
fi
exit "$failures"
