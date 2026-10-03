# Check full-design estimability and contrast precision

Diagnoses dependencies involving many regressors, including event
regressors and the baseline. Unlike
[`check_collinearity()`](https://bbuchsbaum.github.io/fmridesign/reference/check_collinearity.md),
this uses the singular values of the full design rather than pairwise
correlations.

## Usage

``` r
check_estimability(
  x,
  contrasts = NULL,
  baseline = NULL,
  absolute = TRUE,
  condition_threshold = 10,
  coverage_threshold = 0.9,
  tol = 1e-08
)

# S3 method for class 'estimability_check'
print(x, ...)
```

## Arguments

- x:

  An `event_model`, or a finite numeric matrix/data frame (including a
  sparse `Matrix`) with observations in rows.

- contrasts:

  A numeric vector, a matrix with contrast vectors in columns, or a list
  of these. Names on vectors or row names on matrices are matched
  strictly to design columns; omitted columns receive zero weight.
  Unnamed weights must span the input design or the full design
  including appended baseline columns. For an event model, `NULL` uses
  the declared contrasts returned by
  [`contrast_weights()`](https://bbuchsbaum.github.io/fmridesign/reference/contrast_weights.md).

- baseline:

  `NULL` (default) adds run intercepts to an event model and leaves a
  matrix input unchanged. `TRUE` requests run intercepts for an event
  model, or a global intercept for a matrix (reusing an existing
  constant nonzero column). `FALSE` adds nothing. Alternatively, supply
  a `baseline_model` or numeric matrix of the actual baseline/nuisance
  columns, with rows in the same order as `x`. Supplied columns are
  never dropped.

- absolute:

  Include diagnostics for individual coefficients of the input design
  (before appending a baseline). Default `TRUE`.

- condition_threshold:

  Condition-number warning threshold, default 10. This is a screening
  heuristic, not a universal precision cutoff.

- coverage_threshold:

  Flag runs whose event coverage exceeds this fraction, default 0.9.
  Coverage is context, not an estimability test.

- tol:

  Relative singular-value cutoff for numerical rank and relative
  null-space tolerance for contrast estimability. Default `1e-8`.

- ...:

  Unused.

## Value

An `estimability_check` list with `ok`, `rank`, `n_columns`,
`n_observations`, `full_rank`, `condition_number`,
`condition_threshold`, `ill_conditioned`, `singular_values`,
`column_norms`, `weakest_direction`, `baseline_pattern`, `baseline`
(source and column names), `contrasts` (name, quantity, estimable,
variance_factor, null_fraction), `coverage` (run, duration, covered,
fraction, high_coverage), and explanatory `messages`. `ok` means full
numerical rank and condition number within the threshold; it does not
certify adequate precision for a scientific question.

## Details

Columns are scaled to unit Euclidean norm without centring, retaining
the intercept. `condition_number` is the exact spectral ratio
(largest/smallest singular value), or `Inf` for a numerically
rank-deficient design. It is not the default approximation returned by
[`kappa()`](https://rdrr.io/r/base/kappa.html). Zero columns are
retained.

`weakest_direction` gives named loadings in the scaled coordinates and
the corresponding coefficient direction in the original units. Its sign
is arbitrary; when the smallest singular value is repeated, the
direction is not unique. `baseline_pattern` labels a possible
baseline-versus-regressors dependency when all loadings outside the
baseline have one sign and baseline loadings have the opposite sign, in
an ill-conditioned or deficient design. This is a heuristic
interpretation, not proof of a particular task structure.

A contrast is estimable when it is orthogonal to the design's null
space. Estimable contrasts receive a `variance_factor`, equal to
\\c'(X'X)^{-1}c\\ for full-rank designs and the corresponding
generalized inverse quadratic form otherwise. Non-estimable contrasts
receive `NA`, never a misleading finite pseudoinverse variance. These
are variances per unit independent residual variance in the original
coefficient units; rescaling a contrast rescales its variance
quadratically. F-contrast columns are assessed separately, not as an
omnibus F statistic. For inference with temporal filtering or correlated
errors, supply the actual filtered/whitened full design and matching
contrasts.

Event coverage is the union of modelled event intervals, clipped to each
run's `[0, n_scans * TR)` window. Overlapping events or repeated terms
count once; zero-duration events occupy no time. Coverage uses retained
event terms, including term-specific subsets/durations, and is
unavailable (`NULL`) for raw matrices or models without event timing.
High coverage suggests inspecting scientifically appropriate centred or
reference-condition contrasts, which estimate different quantities from
absolute coefficients.

## See also

[`validate_contrasts()`](https://bbuchsbaum.github.io/fmridesign/reference/validate_contrasts.md),
[`baseline_model()`](https://bbuchsbaum.github.io/fmridesign/reference/baseline_model.md)

## Examples

``` r
# Every observation belongs to one task: absolute effects alias the intercept.
X <- cbind(task_A = c(1, 1, 0, 0), task_B = c(0, 0, 1, 1), intercept = 1)
check_collinearity(X)$ok
#> [1] FALSE
check_estimability(X, contrasts = list(A_vs_B = c(1, -1, 0)))
#> Design estimability: rank 2/3; condition number Inf
#> Baseline: supplied design 
#> - Design is rank deficient (2 of 3 columns). 
#> - Weakest direction trades the baseline against same-sign event/regressor loadings (baseline_minus_regressors). 
#> - Non-estimable contrasts have no identifiable coefficient variance (variance_factor = NA). 
#>            name quantity estimable variance_factor null_fraction
#>          A_vs_B contrast      TRUE               1  9.476346e-17
#>     beta:task_A absolute     FALSE              NA  5.000000e-01
#>     beta:task_B absolute     FALSE              NA  5.000000e-01
#>  beta:intercept absolute     FALSE              NA  7.071068e-01
```
