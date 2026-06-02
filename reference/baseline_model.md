# Construct a Baseline Model

Builds a baseline model to account for noise and non-event-related
variance. This model may include a drift term, a block intercept term,
and nuisance regressors.

## Usage

``` r
baseline_model(
  basis = c("constant", "poly", "bs", "ns"),
  degree = 1,
  sframe,
  intercept = c("runwise", "global", "none"),
  nuisance_list = NULL,
  nuisance_check = c("warn", "error", "drop", "none"),
  na_action = c("drop", "zero", "median")
)
```

## Arguments

- basis:

  Character; type of basis function ("constant", "poly", "bs", or "ns").

- degree:

  Integer; degree of the spline/polynomial function.

- sframe:

  A sampling_frame object.

- intercept:

  Character; whether to include an intercept ("runwise", "global", or
  "none"). Ignored when `basis == "constant"` because the drift term
  already provides the constant baseline.

- nuisance_list:

  Optional list of nuisance matrices or data frames (one per fMRI
  block).

- nuisance_check:

  Character; how to handle nuisance diagnostics. `"warn"` warns on
  construction-time problems, `"error"` stops, `"drop"` removes
  non-finite, zero-variance, and rank-aliased columns with a warning,
  and `"none"` skips these checks.

- na_action:

  Character; how to handle `NA` values in `nuisance_list` columns before
  the diagnostics run. `"drop"` (default) leaves `NA`s in place so any
  column containing one is treated as non-finite and removed by
  `nuisance_check`. `"zero"` replaces `NA` with `0` (matching the
  fMRIPrep leading-row convention) and `"median"` replaces `NA` with the
  column median; both repair an isolated leading `NA` (e.g. in DVARS or
  framewise displacement) so the regressor is retained. `NaN` and `Inf`
  are never repaired and remain subject to the non-finite drop. Repair
  is applied even when `nuisance_check = "none"`, preventing `NA`s from
  leaking into the design matrix.

## Value

An object of class "baseline_model".

## Examples

``` r
sframe <- fmrihrf::sampling_frame(blocklens = c(100, 100), TR = 2)
bmod <- baseline_model(basis = "bs", degree = 3, sframe = sframe)
bmod_global <- baseline_model(basis = "bs", degree = 3, sframe = sframe, intercept = "global")
bmod_nointercept <- baseline_model(basis = "bs", degree = 3, sframe = sframe, intercept = "none")
stopifnot(ncol(design_matrix(bmod)) == 8)
```
