# Check nuisance regressors for rank and column problems

Inspects a block-wise nuisance regressor list before it is added to a
baseline model. The check is run per block and compares nuisance columns
against the baseline terms that would be constructed from `basis`,
`degree`, and `intercept`.

## Usage

``` r
check_nuisance(
  nuisance_list,
  sframe,
  basis = c("constant", "poly", "bs", "ns"),
  degree = 1,
  intercept = c("runwise", "global", "none"),
  tol = sqrt(.Machine$double.eps),
  duplicate_threshold = 1 - sqrt(.Machine$double.eps),
  na_action = c("drop", "zero", "median")
)
```

## Arguments

- nuisance_list:

  A list of numeric matrices or data frames, one per block.

- sframe:

  A sampling frame.

- basis, degree, intercept:

  Baseline model settings used to construct the comparison baseline
  terms.

- tol:

  Numeric tolerance passed to QR rank checks.

- duplicate_threshold:

  Absolute correlation threshold used to flag duplicate or
  near-duplicate columns.

- na_action:

  Character; how to handle `NA` values in `nuisance_list` columns before
  the diagnostics run. `"drop"` (default) leaves `NA`s in place so any
  column containing one is treated as non-finite. `"zero"` replaces `NA`
  with `0` (matching the fMRIPrep leading-row convention) and `"median"`
  replaces `NA` with the column median; both repair an isolated leading
  `NA` (e.g. in DVARS or framewise displacement) so the regressor is
  retained. `NaN` and `Inf` are never repaired and remain non-finite.

## Value

A `nuisance_check` object with `ok`, `problems`, `by_block`, and
normalized `nuisance_list` elements.
