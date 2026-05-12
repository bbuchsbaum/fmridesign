# fmridesign 0.6.0

## New features

- `baseline_model()` now checks `nuisance_list` inputs during construction for
  zero-variance columns, duplicate or near-duplicate columns, non-finite values,
  nuisance rank deficiency, and columns aliased with baseline terms.
- Added `nuisance_check = c("warn", "error", "drop", "none")` to control whether
  nuisance problems warn, stop, are dropped with an audit warning, or are skipped.
- Added `check_nuisance()` and `clean_nuisance()` helpers for inspecting and
  repairing block-wise nuisance regressors before model construction.

## Bug fixes

- Fixed list-based `event_model()` specifications so `hrf(..., subset = )`
  expressions can use base operators and helper functions from the calling
  environment, matching formula-interface subset behavior.
- Fixed event-model column metadata construction for factor and multi-basis HRF
  terms that expand to multiple design columns.
