# Create duration-aware normalized HRF generator

Generator for use with the `hrf_fun` parameter in
[`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md).
Creates per-onset HRFs by convolving a base HRF with a boxcar of each
event's duration, then normalizing to peak = 1. This preserves the
temporal shape of the duration-modulated response while equalizing
amplitude across events.

## Usage

``` r
duration_hrf_gen(base = fmrihrf::HRF_SPMG1, min_duration = 0)
```

## Arguments

- base:

  Base HRF object. Default
  [`fmrihrf::HRF_SPMG1`](https://bbuchsbaum.github.io/fmrihrf/reference/HRF_objects.html).

- min_duration:

  Minimum duration (prevents zero-width events). Default 0.

## Value

A function for use with `hrf_fun` in
[`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md).

## See also

[`boxcar_hrf_gen()`](https://bbuchsbaum.github.io/fmridesign/reference/boxcar_hrf_gen.md)
for duration-based boxcar HRFs

## Examples

``` r
# Events with variable durations
trial_data <- data.frame(
  onset = c(0, 10, 25),
  duration = c(2, 5, 3),
  condition = c("A", "B", "A"),
  run = 1
)
sf <- fmrihrf::sampling_frame(blocklens = 50, TR = 2)

emod <- event_model(
  onset ~ hrf(condition, hrf_fun = duration_hrf_gen()),
  data = trial_data, block = ~run, sampling_frame = sf,
  durations = trial_data$duration
)
#> Warning: Parameters P1, P2, A1 are not arguments to function SPMG1_block(w=2) and will be ignored
#> Warning: Parameters P1, P2, A1 are not arguments to function SPMG1_block(w=5) and will be ignored
#> Warning: Parameters P1, P2, A1 are not arguments to function SPMG1_block(w=3) and will be ignored
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
#>    Scan 1   0.005                 0.000              
#>    Scan 2   0.760                 0.000              
#>    Scan 3   4.731                 0.000              
#> ...
```
