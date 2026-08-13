# Design-matrix benchmark: fmridesign vs nilearn (FitLins hot path)

Generated: 2026-08-13 10:09:32 UTC

## Scope

- **fmridesign**: `event_model()` + `design_matrix()` (plus `baseline_model()` for the multi-term workload).
- **nilearn / FitLins**: `nilearn.glm.first_level.make_first_level_design_matrix`.
  FitLins uses this (or `FirstLevelModel`) for first-level design construction, so these
  numbers represent the FitLins design-matrix hot path for equivalent event/FIR/modulated/trialwise models.
- Drift/baseline disabled on the nilearn side for event-only isolation (`drift_model=None`).
- The multi-term workload uses two polynomial drift terms per run plus an equivalent runwise-intercept span in both libraries.
  Its categorical/interaction terms use SPM and its separate modulator uses SPMG3 on both sides.
- Multi-run designs use a concatenated global onset axis in both libraries.
- Times are median wall-clock seconds over non-warmup reps (see `bench/workloads.json`).

## Results

| Workload | Events | fmridesign cols | nilearn cols | fmridesign (s) | nilearn (s) | fmridesign / nilearn |
|---|---:|---:|---:|---:|---:|---:|
| Block design (duration=8s, SPM) | 64 | 2 | 3 | 0.0040 | 0.0104 | **2.60x** |
| Categorical SPM (dense, 4 conditions) | 160 | 4 | 5 | 0.0040 | 0.0358 | **8.96x** |
| Categorical SPM + deriv + dispersion | 160 | 12 | 13 | 0.0060 | 0.0819 | **13.66x** |
| FIR (12 bins) | 160 | 48 | 49 | 0.0190 | 0.1088 | **5.73x** |
| Parametric modulator (SPM) | 160 | 3 | 4 | 0.0050 | 0.0314 | **6.28x** |
| Multi-term realistic (cat + interaction + modulator) | 400 | 35 | 35 | 0.0160 | 0.2127 | **13.29x** |
| Trialwise / LSS (480 trials) | 480 | 480 | 481 | 0.0900 | 9.1017 | **101.13x** |
| Trialwise / LSS (240 trials) | 240 | 240 | 241 | 0.0410 | 2.2910 | **55.88x** |

Ratio column: `nilearn_time / fmridesign_time`. Values **> 1** mean fmridesign is faster.

## Interpretation notes

- Column counts are not always identical across libraries (naming, constant/intercept columns,
  interaction encoding, SPMG3 vs nilearn's three SPM bases). Compare timings within each workload,
  not across mismatched column counts.
- Dense categorical / FIR / modulated designs: fmridesign is ahead after the shared-HRF C++
  eval + metadata opts (see table; FIR/SPMG3/multi-term show the largest dense wins).
- Trialwise/LSS: fmridesign's largest advantage. Per-block zero-column skip + shared-HRF
  evaluation avoid empty regressors and per-column `Reg`/`prep_reg_inputs` overhead; nilearn
  evaluates every trial column over the full concatenated series.
- Remaining fmridesign time is dominated by `fmrihrf`'s C++ evaluate kernel
  (see `OPTIMIZATION_NOTES.md` for next cross-repo batch-evaluate targets).
- Numeric equivalence is intentionally out of scope for this harness (different HRF discretizations
  / oversampling). This suite is for **hot-path wall-clock** comparison of equivalent operations.

## Re-run

```bash
bash bench/run_compare.sh
```

