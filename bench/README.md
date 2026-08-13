# Design-matrix hot-path benchmarks

Compare **fmridesign** design-matrix construction to the equivalent Python path used by **nilearn** and **FitLins**.

## Why nilearn ≈ FitLins here

FitLins builds first-level design matrices through Nilearn
(`nilearn.glm.first_level.make_first_level_design_matrix` / `FirstLevelModel`).
Timing Nilearn on synthetic event tables therefore measures the FitLins
design-matrix hot path without requiring a full BIDS dataset or FitLins CLI run.

Downstream GLM fitting (voxelwise AR/robust solvers) lives in **fmrireg** and is
covered by `fmrireg/bench/glm_efficiency_benchmark.R`. This suite focuses on the
shared design-construction work that feeds those fits.

## Workloads

Defined in [`workloads.json`](workloads.json):

| ID | What it stresses |
|---|---|
| `categorical_spm_dense` | Standard multi-condition SPM HRF |
| `categorical_spmg3` | SPM + derivative + dispersion (3 bases) |
| `fir_12` | FIR with 12 delays |
| `modulated_spm` | Condition + parametric modulator |
| `block_design_spm` | Non-zero durations (boxcar → HRF) |
| `trialwise_lss_medium/large` | One regressor per trial (LSS / beta-series) |
| `multi_term_realistic` | Multi-term event model + poly baseline |

## Run

```bash
# Python deps (once)
pip install -r bench/requirements.txt

# From repo root, with fmridesign deps installed
bash bench/run_compare.sh
```

Outputs:

- `bench/results/fmridesign.csv` — per-rep R timings
- `bench/results/nilearn.csv` — per-rep Python timings
- `bench/results/comparison.csv` — median comparison + speedup
- `bench/RESULTS.md` — human-readable summary

Focused harness validation:

```bash
python3 bench/test_bench_nilearn.py
```

## Fairness notes

- Same TR, run lengths, event counts, and (seeded) onset generators.
- Nilearn `drift_model=None` so event/HRF work is isolated; fmridesign baseline
  is only included in the multi-term workload.
- The multi-term workload gives both libraries two polynomial drift terms per
  run plus an equivalent runwise-intercept span. Its categorical/interaction
  terms use SPM and its separate modulator uses SPMG3 in both libraries.
- Multi-run designs use **global onsets** on a concatenated time axis in both libs.
- Column counts can differ (intercept columns, interaction encoding, basis naming);
  compare wall-clock within a workload, not absolute column counts.
- Numeric identity is out of scope (different HRF oversampling / discretization).
