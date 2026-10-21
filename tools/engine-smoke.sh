#!/usr/bin/env bash
# Drives lask on one container engine (spec 10.10): pulling and pinning,
# building a recipe, running with run options, the filesystem built-ins,
# passing a variable by name, and stopping an abandoned container. The
# fake client of the Haskell tests can only say what lask asked; this
# says what the engine did with it.
#
# Usage: tools/engine-smoke.sh <docker|podman|nerdctl> [path/to/lask]
set -uo pipefail

ENGINE=${1:?usage: tools/engine-smoke.sh <docker|podman|nerdctl> [path/to/lask]}
LASK=${2:-lask}
command -v "$LASK" >/dev/null || { echo "not found: $LASK" >&2; exit 1; }
command -v "$ENGINE" >/dev/null || { echo "not found: $ENGINE" >&2; exit 1; }
LASK=$(cd "$(dirname "$LASK")" && pwd)/$(basename "$LASK")
ROOT=$(cd "$(dirname "$0")/.." && pwd)
TMP=$(mktemp -d)
trap 'rm -rf "$TMP" 2>/dev/null' EXIT
export LASK_CONTAINER_ENGINE=$ENGINE

failures=0
ok()   { printf '  ok   %s\n' "$1"; }
fail() { printf '  FAIL %s\n' "$1"; failures=$((failures + 1)); }

# <label> <needle> <command...>: run the command, and expect it to
# succeed with its stdout and stderr together holding the needle.
expect() {
  local label=$1 needle=$2; shift 2
  local out status
  out=$("$@" 2>&1)
  status=$?
  case "$status:$out" in
    0:*"$needle"*) ok "$label" ;;
    *) fail "$label (exit $status)"; printf '%s\n' "$out" | sed 's/^/       /' ;;
  esac
}

echo "$ENGINE"

# A registry image: pulled and pinned by sync, then run.
cp -R "$ROOT/example/01-projects/01-hello-world" "$TMP/hello"
cd "$TMP/hello" || exit 1
expect "sync pulls and pins" "1 image (1 " "$LASK" sync
expect "a pinned image runs" "< Hello, $ENGINE! >" "$LASK" run cowsay-hello "$ENGINE"
expect "envs list finds it" "present" "$LASK" envs list

# A recipe, built by sync, and run options of every kind.
cp -R "$ROOT/example/02-language/04-environments" "$TMP/envs"
cd "$TMP/envs" || exit 1
expect "sync pulls and builds" "3 images (3 " "$LASK" sync
expect "the recipe image runs" '"built":"jq-1.8.2"' "$LASK" eval tool-versions
expect "lask cmd runs a declared command" "jq-1.8.2" "$LASK" cmd jq --version

mkdir -p "$TMP/own"
cd "$TMP/own" || exit 1
cat > main.lask <<'LASK'
command { "busybox" } on #busybox:1.36.1

sealed = #busybox:1.36.1{network: "none", read_only: true, tmpfs: ["/tmp"], cap_drop: ["ALL"]}

files(): String = do {
  write_file("note.txt", "kept", #busybox:1.36.1)
  read_file("note.txt", #busybox:1.36.1)
}

sealed_write(): String = $[sealed] sh -c 'touch /tmp/ok && touch /x 2>&1 || true'

token(): String = $[#busybox:1.36.1{env: {"TOKEN": get_env("SMOKE_TOKEN")}}] sh -c 'echo "token=$TOKEN"'

slow(): String = $ busybox sleep 60

quick(): String = do {
  $ busybox sleep 3
  "quick"
}

raced(): String = race([spawn(\() -> slow()), spawn(\() -> quick())])
LASK
expect "sync pins a fresh image" "lask.lock.json updated" "$LASK" sync
expect "write_file and read_file in a container" '"kept"' "$LASK" eval files
[ -f note.txt ] && ok "the file is in the base directory" || fail "the file is in the base directory"
expect "run options reach the engine" "Read-only file system" "$LASK" eval sealed-write
expect "a variable passed by name arrives" "token=s3cr3t" env SMOKE_TOKEN=s3cr3t "$LASK" eval token
expect "the loser of a race is killed" "killed" "$LASK" eval raced
sleep 2
left=$("$ENGINE" ps --format '{{.Names}}' 2>/dev/null | grep '^lask-' || true)
[ -z "$left" ] && ok "no container is left running" || fail "no container is left running: $left"

if [ "$failures" -gt 0 ]; then
  echo "$failures failed"
  exit 1
fi
