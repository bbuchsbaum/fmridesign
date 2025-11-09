# Convert an `event_term` to a per-condition basis list

A lightweight wrapper around
[`convolve()`](https://bbuchsbaum.github.io/fmridesign/reference/convolve.md)
that post-processes the resulting design matrix into a named list of T x
d matrices - one per experimental condition ("base condition tag"). This
keeps **all** of the heavy lifting inside **fmrireg** while exposing a
minimal, pipe-friendly API that can be used anywhere a condition -\>
basis split is required (e.g. for CFALS).

## Usage

``` r
condition_basis_list(
  x,
  hrf,
  sampling_frame,
  ...,
  output = c("condition_list", "matrix")
)
```

## Arguments

- x:

  An
  [`event_term`](https://bbuchsbaum.github.io/fmridesign/reference/event_term.md)
  object.

- hrf:

  An
  [`HRF`](https://bbuchsbaum.github.io/fmrihrf/reference/HRF-class.html)
  object to apply.

- sampling_frame:

  A
  [`sampling_frame`](https://bbuchsbaum.github.io/fmrihrf/reference/sampling_frame.html)
  object defining the temporal grid.

- ...:

  Further arguments passed on to
  [`convolve()`](https://bbuchsbaum.github.io/fmridesign/reference/convolve.md)
  (e.g. `drop.empty = FALSE`).

- output:

  Either "matrix" (default) for the ordinary design matrix or
  "condition_list" for the split-by-condition list.

## Value

A numeric *matrix* or a named *list* of matrices, depending on `output`.

## Examples

``` r
term <- event_term(
  list(condition = factor(c("A", "B", "A"))),
  onsets = c(0, 10, 20),
  blockids = c(1, 1, 1)
)
sf <- fmrihrf::sampling_frame(blocklens = 30, TR = 1)
condition_basis_list(term, fmrihrf::HRF_SPMG1, sf)
#> list()
```
