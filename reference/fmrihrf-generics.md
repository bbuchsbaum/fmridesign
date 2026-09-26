# Accessor generics shared with fmrihrf

[`onsets()`](https://bbuchsbaum.github.io/fmrihrf/reference/onsets.html),
[`durations()`](https://bbuchsbaum.github.io/fmrihrf/reference/durations.html),
[`blockids()`](https://bbuchsbaum.github.io/fmrihrf/reference/blockids.html)
and
[`nbasis()`](https://bbuchsbaum.github.io/fmrihrf/reference/nbasis.html)
are fmrihrf's generics, re-exported by fmridesign. fmridesign registers
methods on them for its own classes, so `fmrihrf::onsets(term)` and
`fmridesign::onsets(term)` are the same function and dispatch to the
same methods, whichever package is attached first.

fmridesign provides methods for:

- [`onsets()`](https://bbuchsbaum.github.io/fmrihrf/reference/onsets.html),
  [`durations()`](https://bbuchsbaum.github.io/fmrihrf/reference/durations.html):
  `event_term`, `convolved_term`

- [`blockids()`](https://bbuchsbaum.github.io/fmrihrf/reference/blockids.html):
  `event_term`, `convolved_term`, `event_model`

- [`nbasis()`](https://bbuchsbaum.github.io/fmrihrf/reference/nbasis.html):
  `hrfspec`, `convolved_term` subclasses, `feature_term` and the
  parametric basis classes (`Poly`, `BSpline`, `Scale`, ...)

Methods for fmrihrf's own classes (`Reg`, `HRF`, `sampling_frame`) are
documented in fmrihrf.

## Usage

``` r
# S3 method for class 'covariate_convolved_term'
nbasis(x, ...)

# S3 method for class 'feature_term'
nbasis(x, ...)
```

## Arguments

- x:

  An object.

- ...:

  Passed to methods.

## Value

See
[`fmrihrf::onsets()`](https://bbuchsbaum.github.io/fmrihrf/reference/onsets.html),
[`fmrihrf::durations()`](https://bbuchsbaum.github.io/fmrihrf/reference/durations.html),
[`fmrihrf::blockids()`](https://bbuchsbaum.github.io/fmrihrf/reference/blockids.html)
and
[`fmrihrf::nbasis()`](https://bbuchsbaum.github.io/fmrihrf/reference/nbasis.html).

## Examples

``` r
term <- event_term(
  list(condition = factor(c("A", "B", "A"))),
  onsets = c(0, 10, 20),
  blockids = c(1, 1, 1),
  durations = c(2, 2, 2)
)
onsets(term)
#> [1]  0 10 20
durations(term)
#> [1] 2 2 2
blockids(term)
#> [1] 1 1 1
identical(onsets, fmrihrf::onsets)
#> [1] TRUE
```
