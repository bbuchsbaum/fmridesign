# Continuous Feature Term

Specify a continuously sampled feature (for example RMS energy) to be
convolved with an HRF via
[`fmrihrf::feature_regressor()`](https://bbuchsbaum.github.io/fmrihrf/reference/feature_regressor.html).
Unlike
[`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md),
this is a time series, not a list of trials.

## Usage

``` r
feature(
  x,
  dt = NULL,
  times = NULL,
  start = 0,
  center = TRUE,
  scale = c("none", "sd"),
  mask = NULL,
  basis = "spmg1",
  nbasis = 1,
  lag = 0,
  span = NULL,
  precision = NULL,
  id = NULL,
  name = NULL,
  prefix = NULL
)
```

## Arguments

- x:

  Numeric vector, matrix (samples x features), or a list of those (one
  per run).

- dt:

  Positive sampling interval in seconds. Mutually exclusive with
  `times`.

- times:

  Sample times in seconds, or a list of per-run time vectors. Mutually
  exclusive with `dt`.

- start:

  Start time in seconds used only when `dt` is supplied. Defaults to 0.

- center, scale, mask, span:

  Passed to
  [`fmrihrf::feature_regressor()`](https://bbuchsbaum.github.io/fmrihrf/reference/feature_regressor.html).
  Centering and scaling are applied per run.

- basis, nbasis, lag:

  HRF specification, as in
  [`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md).

- precision:

  Optional evaluation precision in seconds. When `NULL` (default), the
  event-model precision is used, then tightened to `min(precision, dt)`.

- id, name, prefix:

  Term identifier used for column names. If omitted, the name of `x` is
  used.

## Value

A `featurespec` object for use on the RHS of an
[`event_model()`](https://bbuchsbaum.github.io/fmridesign/reference/event_model.md)
formula or in the list interface.

## Details

`x` may be a numeric vector or a samples-by-features matrix (one run),
or a list of those with one element per `sampling_frame` block. Supply
exactly one of `dt` or `times`. Feature-only models are allowed:
`event_model(~ feature(x, dt = 0.1, id = "rms"), sampling_frame = sframe)`.

## See also

[`fmrihrf::feature_regressor()`](https://bbuchsbaum.github.io/fmrihrf/reference/feature_regressor.html),
[`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md),
[`event_model()`](https://bbuchsbaum.github.io/fmridesign/reference/event_model.md)

## Examples

``` r
dt <- 0.5
rms <- abs(sin(seq(0, 20, by = dt)))
sframe <- sampling_frame(blocklens = 15, TR = 2)
emod <- event_model(
  ~ feature(rms, dt = dt, id = "rms", center = FALSE, scale = "none"),
  sampling_frame = sframe
)
ncol(design_matrix(emod))
#> [1] 1
```
