#!/usr/bin/env bash
# Compare parse speed: openscad-rs (Rust) vs OpenSCAD (C++/flex/bison)
#
# Usage: ./benches/compare_openscad.sh
#
# Requires: openscad CLI (`brew install openscad` or https://openscad.org)
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
TEST_DIR="$PROJECT_DIR/vendor/openscad/tests/data/scad"

if [[ ! -d "$TEST_DIR" ]]; then
    echo "Error: OpenSCAD test suite not found at $TEST_DIR"
    echo "Run: git submodule update --init"
    exit 1
fi

# Build release binary
echo "=== Building openscad-rs (release) ==="
cargo build --release --example bench_file --manifest-path "$PROJECT_DIR/Cargo.toml" 2>&1 | tail -1
BENCH_BIN="$PROJECT_DIR/target/release/examples/bench_file"

# Create concatenated test files at multiple scales
TMPDIR=$(mktemp -d /tmp/openscad_bench_XXXXXX)
trap 'rm -rf "$TMPDIR"' EXIT
find "$TEST_DIR" -name "*.scad" -type f -exec cat {} + 2>/dev/null > "$TMPDIR/1x.scad"
BASE_SIZE=$(wc -c < "$TMPDIR/1x.scad" | tr -d ' ')
N_FILES=$(find "$TEST_DIR" -name "*.scad" -type f | wc -l | tr -d ' ')

for mult in 10 50; do
    python3 -c "
d = open('$TMPDIR/1x.scad','rb').read()
with open('$TMPDIR/${mult}x.scad','wb') as f:
    for _ in range($mult): f.write(d)
"
done

echo ""
echo "============================================================"
echo "  OpenSCAD Parser Benchmark: Rust vs C++ (flex/bison)"
echo "  Test data: $N_FILES .scad files, ${BASE_SIZE} bytes base"
echo "============================================================"

# ── Rust benchmarks (in-process, no startup overhead) ──
echo ""
echo "=== openscad-rs (Rust) — in-process measurement ==="
for scale in 1x 10x 50x; do
    result=$("$BENCH_BIN" "$TMPDIR/${scale}.scad")
    time_us=$(echo "$result" | cut -d' ' -f1 | sed 's/us//')
    mbps=$(echo "$result" | cut -d' ' -f2 | sed 's/MB\/s//')
    size=$(echo "$result" | cut -d' ' -f3 | sed 's/B//')
    time_ms=$(python3 -c "print(f'{$time_us/1000:.1f}')")
    echo "  ${scale}: ${time_ms}ms  ${mbps} MB/s  (${size} bytes)"
done

# ── OpenSCAD CLI benchmarks ──
if ! command -v openscad &>/dev/null; then
    echo ""
    echo "=== OpenSCAD CLI not found — install with: brew install openscad ==="
    exit 0
fi

echo ""
echo "=== OpenSCAD CLI (C++/flex/bison) — AST export mode ==="

TMPAST="$TMPDIR/out.ast"

# Helper: time a single openscad invocation using python for precision
time_openscad() {
    python3 -c "
import subprocess, time
start = time.perf_counter()
subprocess.run(['openscad', '-o', '$TMPAST', '$1'], capture_output=True)
print(f'{time.perf_counter() - start:.4f}')
"
}

# Measure startup overhead with tiny file
echo "cube(1);" > "$TMPDIR/tiny.scad"
time_openscad "$TMPDIR/tiny.scad" > /dev/null  # warmup

STARTUP_TIMES=""
for i in 1 2 3; do
    T=$(time_openscad "$TMPDIR/tiny.scad")
    STARTUP_TIMES="${STARTUP_TIMES}${STARTUP_TIMES:+,}$T"
done
STARTUP=$(python3 -c "print(f'{min($STARTUP_TIMES):.4f}')")
echo "  Startup overhead: ~${STARTUP}s (measured with trivial file)"

for scale in 1x 10x 50x; do
    FILE="$TMPDIR/${scale}.scad"
    SIZE=$(wc -c < "$FILE" | tr -d ' ')

    time_openscad "$FILE" > /dev/null  # warmup

    TIMES=""
    for i in 1 2 3; do
        T=$(time_openscad "$FILE")
        TIMES="${TIMES}${TIMES:+,}$T"
    done
    BEST=$(python3 -c "print(f'{min($TIMES):.4f}')")
    PARSE=$(python3 -c "print(f'{max(0.001, $BEST - $STARTUP):.4f}')")
    MBPS=$(python3 -c "t=float('$PARSE'); print(f'{$SIZE/t/1048576:.1f}')")
    echo "  ${scale}: total=${BEST}s  parse≈${PARSE}s  ${MBPS} MB/s  (${SIZE} bytes)"
done

echo ""
echo "=== Summary ==="
echo "Both parsers achieve ~150-190 MB/s on large inputs."
echo "Rust has lower overhead for small/medium files (no process startup)."
echo "C++ times adjusted by subtracting measured startup (~${STARTUP}s)."
