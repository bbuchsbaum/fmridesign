# Print and Plot for sampling_frame

Custom print and plot methods for `sampling_frame` objects (from
fmrihrf). The print method provides a concise summary focused on
runs/blocks, TR and total scans without mentioning lower-level
evaluation precision. The plot method visualizes runs over time.

## Usage

``` r
# S3 method for class 'sampling_frame'
plot(
  x,
  style = c("timeline", "grid", "lane"),
  show_ticks = FALSE,
  tick_every = 5,
  events = NULL,
  title = NULL,
  subtitle = NULL,
  ...
)

# S3 method for class 'sampling_frame'
print(x, ...)
```

## Arguments

- x:

  A `sampling_frame` object created by
  [`fmrihrf::sampling_frame()`](https://bbuchsbaum.github.io/fmrihrf/reference/sampling_frame.html).

- style:

  Plot style for
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html). One of
  `"timeline"` (default), `"grid"` or `"lane"`. `"timeline"` draws one
  bar per run on a shared time axis; `"grid"` shows the scans of each
  run as cells on a within-run scan axis; `"lane"` is a compact single
  row of adjacent run segments, annotated above each segment.

- show_ticks:

  Logical; for [`plot()`](https://rdrr.io/r/graphics/plot.default.html),
  whether to show per-TR tick marks along each run (timeline style
  only). Default `FALSE`.

- tick_every:

  Integer; draw a tick every `tick_every` TRs when `show_ticks = TRUE`
  (timeline), or alternate cell shading every `tick_every` scans (grid;
  defaults to 10 for runs over 100 scans). Default `5`.

- events:

  For [`plot()`](https://rdrr.io/r/graphics/plot.default.html), optional
  events to overlay as a coverage check: an `event_model`, or a data
  frame with an `onset` column (seconds, relative to the start of each
  run) and a `run` (or `block`) column. Each onset is drawn as a small
  tick under its run, so runs without events or with an empty tail stand
  out; onsets that fall outside their run are counted in the caption.

- title, subtitle:

  For [`plot()`](https://rdrr.io/r/graphics/plot.default.html), optional
  title and subtitle; the defaults describe the acquisition (runs,
  scans, TR, total duration).

- ...:

  Unused.

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

```
