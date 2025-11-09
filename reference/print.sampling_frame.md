# Print and Plot for sampling_frame

Custom print and plot methods for `sampling_frame` objects (from
fmrihrf). The print method provides a concise summary focused on
runs/blocks, TR and total scans without mentioning lower-level
evaluation precision. The plot method visualizes runs over time.

## Usage

``` r
# S3 method for class 'sampling_frame'
print(x, ...)

# S3 method for class 'sampling_frame'
plot(x, style = c("timeline", "grid"), show_ticks = FALSE, tick_every = 5, ...)
```

## Arguments

- x:

  A `sampling_frame` object created by
  [`fmrihrf::sampling_frame()`](https://bbuchsbaum.github.io/fmrihrf/reference/sampling_frame.html).

- ...:

  Unused.

- style:

  Plot style for
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html). One of
  `"timeline"` (default) or `"grid"`. `"timeline"` draws a horizontal
  bar per run; `"grid"` shows a scan grid by run.

- show_ticks:

  Logical; for [`plot()`](https://rdrr.io/r/graphics/plot.default.html),
  whether to show per-TR tick marks along each run (timeline style
  only). Default `FALSE`.

- tick_every:

  Integer; draw a tick every `tick_every` TRs when `show_ticks = TRUE`.
  Default `5`.

## Value

For [`print()`](https://rdrr.io/r/base/print.html), returns `x`
invisibly. For [`plot()`](https://rdrr.io/r/graphics/plot.default.html),
a ggplot object.

## Examples

``` r
sf <- fmrihrf::sampling_frame(blocklens = c(60, 120), TR = 2)
print(sf)
#> Sampling frame
#> - Blocks: 2 
#> - Scans: 180 (per block: 60, 120 )
#> - TR: 2 s
#> - Duration: 359 s
plot(sf)
#> Warning: ‘-’ not meaningful for factors
#> Warning: reverse transformation introduced infinite values.
#> Warning: ‘-’ not meaningful for factors
#> Warning: reverse transformation introduced infinite values.
#> Warning: Position guide is perpendicular to the intended axis.
#> ℹ Did you mean to specify a different guide `position`?
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_segment()`).

```
