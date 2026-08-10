# Changelog

## fmridesign 0.6.0

### New features

- [`baseline_model()`](https://bbuchsbaum.github.io/fmridesign/reference/baseline_model.md)
  now checks `nuisance_list` inputs during construction for
  zero-variance columns, duplicate or near-duplicate columns, non-finite
  values, nuisance rank deficiency, and columns aliased with baseline
  terms.
- Added `nuisance_check = c("warn", "error", "drop", "none")` to control
  whether nuisance problems warn, stop, are dropped with an audit
  warning, or are skipped.
- Added
  [`check_nuisance()`](https://bbuchsbaum.github.io/fmridesign/reference/check_nuisance.md)
  and
  [`clean_nuisance()`](https://bbuchsbaum.github.io/fmridesign/reference/clean_nuisance.md)
  helpers for inspecting and repairing block-wise nuisance regressors
  before model construction.

### Performance

- Sped up
  [`event_model()`](https://bbuchsbaum.github.io/fmridesign/reference/event_model.md)
  design-matrix construction by replacing the per-term
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  /
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
  calls used to assemble column metadata with a lightweight, validated
  `tibble` constructor. This removes the metadata-building hotspot (~15%
  faster end-to-end on a representative multi-term, multi-run model)
  with byte-identical design matrices, column names,
  `col_indices`/`term_spans`, and metadata values.
- [`convolve.event_term()`](https://bbuchsbaum.github.io/fmridesign/reference/convolve.md)
  now skips columns that are all-zero within a block instead of building
  and evaluating an empty `fmrihrf` regressor for each. For
  block-diagonal-ish designs (trialwise/LSS single-trial models, or
  factor levels present only in some runs) this is a large speedup
  (~2.2x faster on a representative 360-column trialwise model) while
  producing bit-identical output. Designs where every column is
  populated in every block are unaffected (a fast-exit keeps the
  original path), and blocks containing `NA`/`NaN` fall back to the
  previous full-column path so filtering semantics are unchanged.
- Convolution hot path now shares one fine-grid HRF matrix across all
  columns and blocks and calls `fmrihrf`’s C++ evaluator directly,
  skipping per-column `Reg` construction / `prep_reg_inputs` overhead.
  Combined with a single global output matrix (no per-block zero-alloc +
  `rbind`) and deferred tibble materialization in
  `build_event_model_design_matrix()`, this is ~2–3.5× faster end-to-end
  on trialwise/LSS and multi-term workloads while remaining
  bit-identical to `fmrihrf::evaluate(regressor(...))`. Per-onset
  `hrf_fun` lists and NA-misaligned designs keep the previous path.
- Added `bench/` cross-library design-matrix benchmarks against nilearn
  (the FitLins first-level design-matrix hot path). Run with
  `bash bench/run_compare.sh`; see `bench/RESULTS.md` and
  `bench/OPTIMIZATION_NOTES.md`.

### Bug fixes

- [`convolve_design()`](https://bbuchsbaum.github.io/fmridesign/reference/convolve_design.md)
  now extracts each condition column with `dmat[[i]]`, so it produces
  correct regressors for base `data.frame` inputs (its documented
  example). The previous `dmat[, i][[1]]` collapsed a data frame column
  to its first element; the tibble-based internal call path was
  unaffected.
- [`contrast_weights()`](https://bbuchsbaum.github.io/fmridesign/reference/contrast_weights.md)
  and
  [`Fcontrasts()`](https://bbuchsbaum.github.io/fmridesign/reference/Fcontrasts.md)
  for `event_model` objects now name interaction-term contrasts with the
  same term tags used by design-matrix `col_indices`, preventing
  downstream consumers from dropping crossed-term contrasts because of
  `:`/`_` key mismatches
  ([\#9](https://github.com/bbuchsbaum/fmridesign/issues/9)).
- [`event_model()`](https://bbuchsbaum.github.io/fmridesign/reference/event_model.md)
  now warns when continuous parametric modulators are all-zero or have
  zero variance, catching degenerate design columns before model fitting
  while preserving the existing design-matrix shape
  ([\#8](https://github.com/bbuchsbaum/fmridesign/issues/8)).
- Fixed list-based
  [`event_model()`](https://bbuchsbaum.github.io/fmridesign/reference/event_model.md)
  specifications so `hrf(..., subset = )` expressions can use base
  operators and helper functions from the calling environment, matching
  formula-interface subset behavior.
- Fixed event-model column metadata construction for factor and
  multi-basis HRF terms that expand to multiple design columns.
- Event terms whose subsets select zero events now retain their
  canonical condition-column names and metadata on the resulting
  all-zero design matrix, rather than falling back to generic `col_1`,
  `col_2`, … names.
- Suppressed exact, known false-positive metadata warnings produced when
  decorated HRFs are reconstructed by `fmrihrf` 0.3.0, while continuing
  to surface unrelated warnings.
