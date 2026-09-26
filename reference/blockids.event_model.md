# Run (block) ids of an event model's events

[`blockids()`](https://bbuchsbaum.github.io/fmrihrf/reference/blockids.html)
on an `event_model` returns one run id per **event**, in event order:
the run each onset belongs to. It does not return one id per scan. For
per-scan run ids use `blockids(x$sampling_frame)`. `blocklens(x)` gives
the number of scans in each run.

## Usage

``` r
# S3 method for class 'event_model'
blockids(x, ...)
```

## Arguments

- x:

  An `event_model`.

- ...:

  Unused.

## Value

Integer vector with one run id per event.

## Details

fmrireg releases before its switch to fmridesign's method returned
per-scan ids here whenever fmrireg was loaded. To flag the change, the
first call in a session prints a one-time message (class
`fmridesign_blockids_per_event`).

## See also

[`fmrihrf::blockids()`](https://bbuchsbaum.github.io/fmrihrf/reference/blockids.html),
[`fmrihrf::blocklens()`](https://bbuchsbaum.github.io/fmrihrf/reference/blocklens.html)

## Examples

``` r
sf <- fmrihrf::sampling_frame(blocklens = c(20, 20), TR = 2)
des <- data.frame(onset = c(0, 10, 0, 10, 20), run = c(1, 1, 2, 2, 2),
                  cond = factor(c("A", "B", "A", "B", "A")))
em <- event_model(onset ~ hrf(cond), data = des, block = ~run,
                  sampling_frame = sf)
blockids(em)                  # per event: 1 1 2 2 2
#> blockids(<event_model>) returns per-event block ids; for per-scan ids use blockids(x$sampling_frame).
#> [1] 1 1 2 2 2
blockids(em$sampling_frame)   # per scan: 20 x 1, then 20 x 2
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [39] 2 2
blocklens(em)                 # scans per run: 20 20
#> [1] 20 20
```
