#!/usr/bin/env bash
# Run fmridesign vs nilearn design-matrix benchmarks and write comparison artifacts.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

mkdir -p bench/results /opt/cursor/artifacts

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

# Also copy summary artifacts for the cloud agent walkthrough.
cp -f bench/RESULTS.md /opt/cursor/artifacts/design_matrix_bench_RESULTS.md
cp -f bench/results/comparison.csv /opt/cursor/artifacts/design_matrix_bench_comparison.csv

echo "Done."
echo "  bench/RESULTS.md"
echo "  bench/results/comparison.csv"
