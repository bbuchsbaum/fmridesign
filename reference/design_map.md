# Compute design map

Compute design map

## Usage

``` r
design_map(x, ...)
```

## Arguments

- x:

  The object.

- ...:

  Additional arguments.

## Value

A ggplot2 object visualizing the design matrix.

## Examples

``` r
des <- data.frame(
  onset = c(0, 10, 20, 30),
  run = 1,
  cond = factor(c("A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
design_map(emod)
```
