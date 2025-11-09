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
  nuisance_list = NULL
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
