# Design-matrix benchmark: fmridesign vs nilearn (FitLins hot path)

Generated: 2026-08-09 18:53:20 UTC

## Scope

- **fmridesign**: `event_model()` + `design_matrix()` (plus `baseline_model()` for the multi-term workload).
- **nilearn / FitLins**: `nilearn.glm.first_level.make_first_level_design_matrix`.
  FitLins uses this (or `FirstLevelModel`) for first-level design construction, so these
  numbers represent the FitLins design-matrix hot path for equivalent event/FIR/modulated/trialwise models.
- Drift/baseline disabled on the nilearn side for event-only isolation (`drift_model=None`).
- Multi-run designs use a concatenated global onset axis in both libraries.
- Times are median wall-clock seconds over non-warmup reps (see `bench/workloads.json`).

## Results

| Workload | Events | fmridesign cols | nilearn cols | fmridesign (s) | nilearn (s) | fmridesign / nilearn |
|---|---:|---:|---:|---:|---:|---:|
| Block design (duration=8s, SPM) | 64 | 2 | 3 | 0.0070 | 0.0083 | **1.18x** |
| Categorical SPM (dense, 4 conditions) | 160 | 4 | 5 | 0.0080 | 0.0147 | **1.84x** |
| Categorical SPM + deriv + dispersion | 160 | 12 | 13 | 0.0090 | 0.0391 | **4.34x** |
| FIR (12 bins) | 160 | 48 | 49 | 0.0120 | 0.0707 | **5.89x** |
| Parametric modulator (SPM) | 160 | 3 | 4 | 0.0100 | 0.0122 | **1.22x** |
| Multi-term realistic (cat + interaction + modulator) | 400 | 35 | 30 | 0.0310 | 0.1936 | **6.24x** |
| Trialwise / LSS (480 trials) | 480 | 480 | 481 | 0.1070 | 3.6545 | **34.15x** |
| Trialwise / LSS (240 trials) | 240 | 240 | 241 | 0.0500 | 1.1208 | **22.42x** |

Ratio column: `nilearn_time / fmridesign_time`. Values **> 1** mean fmridesign is faster.

## Interpretation notes

- Column counts are not always identical across libraries (naming, constant/intercept columns,
  interaction encoding, SPMG3 vs nilearn's three SPM bases). Compare timings within each workload,
  not across mismatched column counts.
- Dense categorical / FIR / modulated designs: fmridesign is typically competitive or ahead
  after the metadata + sparse-block opts (see table; FIR/SPMG3 show the largest dense wins).
- Trialwise/LSS: fmridesign's largest advantage. The per-block zero-column skip avoids building
  empty regressors for trials absent from a run; nilearn evaluates every trial column over the
  full concatenated series.
- Remaining fmridesign time is still dominated by `fmrihrf::evaluate` / `prep_reg_inputs`
  (see `OPTIMIZATION_NOTES.md` for next cross-repo targets).
- Numeric equivalence is intentionally out of scope for this harness (different HRF discretizations
  / oversampling). This suite is for **hot-path wall-clock** comparison of equivalent operations.

## Re-run

```bash
bash bench/run_compare.sh
```

