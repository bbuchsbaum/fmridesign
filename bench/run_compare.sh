#!/usr/bin/env bash
# Run fmridesign vs nilearn design-matrix benchmarks and write comparison artifacts.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

mkdir -p bench/results

echo "==> fmridesign"
Rscript bench/bench_fmridesign.R --out bench/results/fmridesign.csv

echo "==> nilearn (FitLins design-matrix hot path)"
python3 bench/bench_nilearn.py --out bench/results/nilearn.csv

echo "==> summarize"
Rscript bench/summarize_results.R \
  --r bench/results/fmridesign.csv \
  --py bench/results/nilearn.csv \
  --out bench/results/comparison.csv \
  --md bench/RESULTS.md

# Optionally copy summary artifacts for cloud-agent walkthroughs. Outside
# Cursor Cloud, /opt/cursor is commonly absent or not writable; that must not
# prevent the documented benchmark command from running. Set
# CURSOR_ARTIFACT_DIR to another existing writable directory when desired.
ARTIFACT_DIR="${CURSOR_ARTIFACT_DIR:-/opt/cursor/artifacts}"
if [[ -d "$ARTIFACT_DIR" && -w "$ARTIFACT_DIR" ]]; then
  cp -f bench/RESULTS.md "$ARTIFACT_DIR/design_matrix_bench_RESULTS.md"
  cp -f bench/results/comparison.csv "$ARTIFACT_DIR/design_matrix_bench_comparison.csv"
else
  echo "Skipping optional artifact copies (directory is absent or not writable: $ARTIFACT_DIR)"
fi

echo "Done."
echo "  bench/RESULTS.md"
echo "  bench/results/comparison.csv"
