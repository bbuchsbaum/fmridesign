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
  prefix = NULL,
  durations = NULL,
  band = NULL,
  subband = NULL,
  contrasts = NULL
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

- durations:

  Optional event durations in seconds, only valid with `times`: a single
  number, a vector with one entry per sample, or a list of those with
  one element per run (parallel to `times`). `NULL` (default) keeps the
  zero-order hold. Requires a version of fmrihrf whose
  [`feature_regressor()`](https://bbuchsbaum.github.io/fmrihrf/reference/feature_regressor.html)
  has a `durations` argument.

- band:

  Optional single string naming the feature band this term belongs to.
  Defaults to the term's tag (its `id`).

- subband:

  Optional sub-band labels: `NULL` (none), a single string (every
  feature column), or a character vector with one entry per feature
  column of `x`.

- contrasts:

  Optional contrast specification(s) for the term: a `contrast_spec`, a
  [`contrast_set()`](https://bbuchsbaum.github.io/fmridesign/reference/contrast_set.md),
  or a list of `contrast_spec` objects, as in
  [`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md).

## Value

A `featurespec` object for use on the RHS of an
[`event_model()`](https://bbuchsbaum.github.io/fmridesign/reference/event_model.md)
formula or in the list interface.

## Details

`x` may be a numeric vector or a samples-by-features matrix (one run),
or a list of those with one element per `sampling_frame` block. Supply
exactly one of `dt` or `times`. Feature-only models are allowed:
`event_model(~ feature(x, dt = 0.1, id = "rms"), sampling_frame = sframe)`.

A series given with `dt` or `times` alone is a zero-order hold: each
value lasts until the next sample. If such a series stops more than one
TR before the run does, the scans after its end get no contribution from
it (as if the feature were 0 there, after centering), and a warning says
so.

## Event-timed features

For a sparse feature tied to events (word surprisal, per-phone
features), supply `times` (onsets) and `durations`. Row `i` of `x` is
then an event starting at `times[i]` and lasting `durations[i]` seconds
(`0` is an impulse) with amplitude `x[i, ]`, and no value is held across
the gap to the next event. `times` need only be non-decreasing, and
events may overlap (their responses add). `center` and `scale` act
across the events of a run, the parametric-modulator convention. See
[`fmrihrf::feature_regressor()`](https://bbuchsbaum.github.io/fmrihrf/reference/feature_regressor.html).

## Bands, sub-bands and contrasts

`band` and `subband` label the columns of the term for banded (grouped)
models; they are recorded in the `band` and `subband` columns of
[`design_colmap()`](https://bbuchsbaum.github.io/fmridesign/reference/design_colmap.md)
and collected by
[`design_bands()`](https://bbuchsbaum.github.io/fmridesign/reference/design_bands.md).
Several terms may share a band.

[`Fcontrasts()`](https://bbuchsbaum.github.io/fmridesign/reference/Fcontrasts.md)
gives a feature term an omnibus F-contrast over all of its columns, and
one F-contrast per declared sub-band. Each tests its feature columns
jointly in every basis function. `contrasts` attaches contrast
specifications to the term, as in
[`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md).
Feature terms have no categorical cells, so use
[`column_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/column_contrast.md),
whose patterns select design-matrix columns by name (for example
`column_contrast("^env_alpha$", "^env_beta$", name = "a_vs_b")`).

## See also

[`fmrihrf::feature_regressor()`](https://bbuchsbaum.github.io/fmrihrf/reference/feature_regressor.html),
[`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md),
[`event_model()`](https://bbuchsbaum.github.io/fmridesign/reference/event_model.md),
[`design_bands()`](https://bbuchsbaum.github.io/fmridesign/reference/design_bands.md)

## Examples

``` r
dt <- 0.5
rms <- abs(sin(seq(0, 29.5, by = dt)))
sframe <- sampling_frame(blocklens = 15, TR = 2)
emod <- event_model(
  ~ feature(rms, dt = dt, id = "rms", center = FALSE, scale = "none"),
  sampling_frame = sframe
)
ncol(design_matrix(emod))
#> [1] 1

# Two-column feature with sub-bands and a column contrast
env <- cbind(low = rms, high = rev(rms))
emod2 <- event_model(
  ~ feature(env, dt = dt, id = "env", band = "acoustic",
            subband = c("low", "high"),
            contrasts = column_contrast("^env_low$", "^env_high$",
                                        name = "low_vs_high")),
  sampling_frame = sframe
)
names(Fcontrasts(emod2))
#> [1] "env#env"  "env#low"  "env#high"
names(contrast_weights(emod2))
#> [1] "env#low_vs_high"
design_bands(emod2)$families
#> $acoustic
#> [1] "low"  "high"
#> 
```
