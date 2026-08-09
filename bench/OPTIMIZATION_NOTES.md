# Hot-path optimization notes (fmridesign → fmrireg)

Design-matrix construction in **fmridesign** is on the critical path for
**fmrireg** first-level fits (`fmri_lm()` builds / consumes these matrices every
run). This note tracks what shipped, what the cross-library bench shows, and
where the next wins are.

## Shipped (this branch / recent main)

| Opt | Change | Win |
|---|---|---|
| Metadata fast path | `.new_meta_tibble()` replaces per-term `tibble()` / `bind_rows()` | ~15% end-to-end on multi-term models |
| Sparse-block convolution | `convolve.event_term()` skips all-zero columns per block | ~2.2× on 360-col trialwise/LSS |

## Cross-library snapshot (`bash bench/run_compare.sh`)

See [`RESULTS.md`](RESULTS.md). Headline pattern (quiet machine, post-opts):

- **Dense categorical / FIR / modulated**: fmridesign is competitive or ahead of
  nilearn (FitLins’ design-matrix engine), with larger wins on FIR / SPMG3.
- **Trialwise / LSS**: fmridesign’s biggest advantage (often >20× on large
  single-trial designs) once the sparse-block path is active.

FitLins is not timed as a separate binary: its first-level design matrices are
built via `nilearn.glm.first_level.make_first_level_design_matrix`.

## Remaining hotspots (post-opts profiling)

Trialwise / LSS profile (`event_model` + `design_matrix`, large single-trial):

| Component | Share | Owner |
|---|---|---|
| `convolve.event_term` | ~86% | fmridesign |
| `fmrihrf::evaluate` / `evaluate.Reg` | ~46% | **fmrihrf** |
| `prep_reg_inputs` / `.memo_hrf` | ~33% | **fmrihrf** |
| `evaluate_regressor_cpp` | ~12% self | **fmrihrf** |
| `matrixToDataFrame` (tibble materialization) | ~4% self | fmridesign |

Dense designs are dominated by the same `fmrihrf` evaluate path; fmridesign-side
bookkeeping is no longer the primary dense-design cost after the metadata opt.

## Next candidates (ordered)

1. **fmrihrf evaluate / prep_reg_inputs** (cross-repo) — vectorize or batch
   multi-condition evaluate; reduce per-regressor `prep_reg_inputs` overhead.
   Largest remaining gap vs nilearn’s numpy convolution.
2. **Defer tibble materialization** — keep an internal matrix through
   `build_event_model_design_matrix()` and convert once (or on demand). Small
   but measurable on wide trialwise matrices (`matrixToDataFrame`).
3. **Reduce `tryCatch` in the term/block loop** — profile shows non-trivial
   time in tryCatch wrappers around convolution; fail-fast for the common path.
4. **Batch `convolve_design` for dense blocks** — when `all(col_has)`, avoid
   per-column R lists and call a multi-column evaluate API if fmrihrf grows one.
5. **fmrireg GLM path** — separate from this suite; see
   `fmrireg/bench/glm_efficiency_benchmark.R` and the GLM efficiency PRD
   (AR estimation, `solve_glm_core` RSS-only mode, whitening reuse).

## Correctness gates for any further opt

- Golden `all.equal(tol=0)` on design values + exact colnames /
  `col_indices` / `term_spans` / `design_meta` across categorical, SPMG3,
  FIR, modulated, trialwise, multi-run, NaN-fallback, empty-block cases.
- Full `devtools::test()` green.
- Re-run `bash bench/run_compare.sh` and update `RESULTS.md`.
