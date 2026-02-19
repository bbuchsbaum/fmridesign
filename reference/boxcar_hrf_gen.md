# Create duration-based boxcar HRF generator

Creates a generator function for use with the `hrf_fun` parameter in
[`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md). The
generator produces boxcar HRFs where each event's duration determines
the boxcar width.

## Usage

``` r
boxcar_hrf_gen(normalize = TRUE, min_duration = 0.1)
```

## Arguments

- normalize:

  Logical; whether to normalize the boxcar HRF. Default TRUE.

- min_duration:

  Numeric; minimum duration to use (prevents zero-width boxcars).
  Default 0.1.

## Value

A function that takes an event data frame and returns a list of HRF
objects.

## See also

[`weighted_hrf_gen()`](https://bbuchsbaum.github.io/fmridesign/reference/weighted_hrf_gen.md)
for weighted impulse HRFs

## Examples

``` r
# \donttest{
# Events with variable durations
trial_data <- data.frame(
  onset = c(0, 10, 25),
  duration = c(2, 5, 3),
  condition = c("A", "B", "A"),
  run = 1
)
sf <- fmrihrf::sampling_frame(blocklens = 50, TR = 2)

emod <- event_model(
  onset ~ hrf(condition, hrf_fun = boxcar_hrf_gen()),
  data = trial_data, block = ~run, sampling_frame = sf, durations = trial_data$duration
)
#> Warning: Parameters width, amplitude, normalize are not arguments to function boxcar[2] and will be ignored
#> Warning: Parameters width, amplitude, normalize are not arguments to function boxcar[5] and will be ignored
#> Warning: Parameters width, amplitude, normalize are not arguments to function boxcar[3] and will be ignored
print(emod)
#> 
#> ── fMRI Event Model ────────────────────────────────────────────────────────────
#> Number of Terms: 1
#> Number of Events: 3
#> Number of Blocks: 1
#> Total Scans: 50
#> Design Matrix Dimensions: 50 x 2
#> 
#> ── Terms ──
#> 
#> • condition (<event_term>)
#> 
#> ── Design Matrix Preview ──
#> 
#>           condition_condition.A condition_condition.B
#>    Scan 1   2.333                 0.000              
#>    Scan 2   1.833                 0.000              
#>    Scan 3   0.000                 0.000              
#> ...
# }
```
