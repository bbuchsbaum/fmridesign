# Hot-path optimization notes (fmridesign → fmrireg)

Design-matrix construction in **fmridesign** is on the critical path for
**fmrireg** first-level fits (`fmri_lm()` builds / consumes these matrices every
run). This note tracks what shipped, what the cross-library bench shows, and
where the next wins are.

## Shipped

| Opt | Change | Win |
|---|---|---|
| Metadata fast path | `.new_meta_tibble()` replaces per-term `tibble()` / `bind_rows()` | ~15% end-to-end on multi-term models |
| Sparse-block convolution | `convolve.event_term()` skips all-zero columns per block | ~2.2× on 360-col trialwise/LSS |
| Shared-HRF C++ eval | One fine-grid HRF matrix + `evaluate_regressor_cpp` per live column (no per-column `Reg` / `prep_reg_inputs`) | ~5× on the evaluate loop; ~2–3.5× end-to-end trialwise / multi-term |
| Global matrix assembly | Single `T × p` output matrix; scatter live block columns once | Removes per-block zero-alloc + `rbind` |
| Deferred tibble | `.convolve_event_term_matrix()` keeps matrices through `cbind`; one `as_tibble` at the end | Removes per-term `matrixToDataFrame` |

## Cross-library snapshot (`bash bench/run_compare.sh`)

See [`RESULTS.md`](RESULTS.md). Headline pattern (quiet machine, post-opts):

- **Dense categorical / FIR / modulated**: fmridesign is ahead of nilearn
  (FitLins’ design-matrix engine), with larger wins on FIR / SPMG3 / multi-term.
- **Trialwise / LSS**: fmridesign’s biggest advantage (often ≫20× on large
  single-trial designs) from sparse-block skip + shared-HRF eval.

FitLins is not timed as a separate binary: its first-level design matrices are
built via `nilearn.glm.first_level.make_first_level_design_matrix`.

## Remaining hotspots (post-opts profiling)

Trialwise / LSS profile (`event_model` + `design_matrix`, large single-trial):

| Component | Share | Owner |
|---|---|---|
| `evaluate_regressor_cpp` (shared-HRF loop) | dominant remaining | **fmrihrf** |
| `design_matrix.event_term` / `model.matrix` | secondary | fmridesign |
| Term realisation / event construction | secondary | fmridesign |

Dense designs are also dominated by the C++ evaluate call; fmridesign-side
bookkeeping is no longer the primary cost after the shared-HRF + metadata opts.

## Next candidates (ordered)

1. **fmrihrf multi-column / batched evaluate** (cross-repo) — accept a list of
   onset trains (or a sparse amplitude matrix) and return a dense design in one
   C++ call, sharing fine-grid construction. Largest remaining gap vs nilearn’s
   numpy convolution for dense multi-condition blocks.
2. **Trialwise one-hot fast path** — skip `model.matrix` for pure trialwise
   terms (identity coding is known a priori) and map events → columns directly.
3. **Export a public batch API from fmrihrf** — replace the
   `getFromNamespace("evaluate_regressor_cpp", "fmrihrf")` bridge with a
   supported entry point.
4. **fmrireg GLM path** — separate from this suite; see
   `fmrireg/bench/glm_efficiency_benchmark.R` and the GLM efficiency PRD
   (AR estimation, `solve_glm_core` RSS-only mode, whitening reuse).

## Correctness gates for any further opt

- Golden `all.equal(tol=0)` on design values + exact colnames /
  `col_indices` / `term_spans` / `design_meta` across categorical, SPMG3,
  FIR, modulated, trialwise, multi-run, NaN-fallback, empty-block cases.
- Full `devtools::test()` / `testthat::test_local()` green.
- Re-run `bash bench/run_compare.sh` and update `RESULTS.md`.
