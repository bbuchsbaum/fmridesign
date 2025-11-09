# Convolve events with a hemodynamic response function

Convolve events with a hemodynamic response function

## Usage

``` r
convolve(
  x,
  hrf,
  sampling_frame,
  drop.empty = TRUE,
  summate = TRUE,
  precision = 0.1,
  ...
)

# S3 method for class 'event_term'
convolve(
  x,
  hrf,
  sampling_frame,
  drop.empty = TRUE,
  summate = TRUE,
  precision = 0.3,
  ...
)
```

## Arguments

- x:

  The events to convolve.

- hrf:

  The hemodynamic response function.

- sampling_frame:

  The sampling frame.

- drop.empty:

  Logical indicating whether to drop columns with all zeros.

- summate:

  Logical indicating whether to sum convolved signals.

- precision:

  Numeric specifying the temporal precision for convolution.

- ...:

  Additional arguments.

## Value

A matrix-like (often tibble) of convolved regressors.

## Examples

``` r
term <- event_term(
  list(condition = factor(c("A", "B", "A"))),
  onsets = c(0, 5, 10),
  blockids = c(1, 1, 1)
)
sf <- fmrihrf::sampling_frame(blocklens = 20, TR = 1)
conv <- convolve(term, fmrihrf::HRF_SPMG1, sf)
names(conv)
#> [1] "condition.A" "condition.B"
```
