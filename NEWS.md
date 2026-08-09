# fmridesign 0.6.0

## New features

- `baseline_model()` now checks `nuisance_list` inputs during construction for
  zero-variance columns, duplicate or near-duplicate columns, non-finite values,
  nuisance rank deficiency, and columns aliased with baseline terms.
- Added `nuisance_check = c("warn", "error", "drop", "none")` to control whether
  nuisance problems warn, stop, are dropped with an audit warning, or are skipped.
- Added `check_nuisance()` and `clean_nuisance()` helpers for inspecting and
  repairing block-wise nuisance regressors before model construction.

## Performance

- Sped up `event_model()` design-matrix construction by replacing the per-term
  `tibble::tibble()` / `dplyr::bind_rows()` calls used to assemble column
  metadata with a lightweight, validated `tibble` constructor. This removes the
  metadata-building hotspot (~15% faster end-to-end on a representative
  multi-term, multi-run model) with byte-identical design matrices, column
  names, `col_indices`/`term_spans`, and metadata values.

## Bug fixes

- `convolve_design()` now extracts each condition column with `dmat[[i]]`, so it
  produces correct regressors for base `data.frame` inputs (its documented
  example). The previous `dmat[, i][[1]]` collapsed a data frame column to its
  first element; the tibble-based internal call path was unaffected.
- `contrast_weights()` and `Fcontrasts()` for `event_model` objects now name
  interaction-term contrasts with the same term tags used by design-matrix
  `col_indices`, preventing downstream consumers from dropping crossed-term
  contrasts because of `:`/`_` key mismatches (#9).
- `event_model()` now warns when continuous parametric modulators are all-zero
  or have zero variance, catching degenerate design columns before model fitting
  while preserving the existing design-matrix shape (#8).
- Fixed list-based `event_model()` specifications so `hrf(..., subset = )`
  expressions can use base operators and helper functions from the calling
  environment, matching formula-interface subset behavior.
- Fixed event-model column metadata construction for factor and multi-basis HRF
  terms that expand to multiple design columns.
