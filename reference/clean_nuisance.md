# Clean nuisance regressors by dropping rank-useless columns

Drops nuisance columns that are non-finite, zero-variance, or fail to
increase QR rank after the block's baseline terms and earlier nuisance
columns. Column order is respected, so when two columns are aliased the
earlier column is kept.

## Usage

``` r
clean_nuisance(
  nuisance_list,
  sframe,
  basis = c("constant", "poly", "bs", "ns"),
  degree = 1,
  intercept = c("runwise", "global", "none"),
  tol = sqrt(.Machine$double.eps),
  duplicate_threshold = 1 - sqrt(.Machine$double.eps)
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

## Value

A list with `nuisance_list` and `report` elements. Pass
`result$nuisance_list` to
[`baseline_model()`](https://bbuchsbaum.github.io/fmridesign/reference/baseline_model.md).
