# Create weighted HRF generator from list columns

Creates a generator function for use with the `hrf_fun` parameter in
[`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md). The
generator produces weighted impulse HRFs from columns containing lists
of sub-event times and weights.

## Usage

``` r
weighted_hrf_gen(
  times_col = "sub_times",
  weights_col = "sub_weights",
  relative = FALSE,
  method = "constant",
  normalize = FALSE
)
```

## Arguments

- times_col:

  Character; name of the column containing sub-event times (relative or
  absolute).

- weights_col:

  Character; name of the column containing sub-event weights.

- relative:

  Logical; if TRUE, times are relative to event onset; if FALSE, times
  are absolute and will be converted to relative by subtracting the
  onset. Default FALSE (absolute times).

- method:

  Character; interpolation method for `hrf_weighted()`. Default
  "constant".

- normalize:

  Logical; whether to normalize the weighted HRF. Default FALSE.

## Value

A function that takes an event data frame and returns a list of HRF
objects.

## See also

[`boxcar_hrf_gen()`](https://bbuchsbaum.github.io/fmridesign/reference/boxcar_hrf_gen.md)
for duration-based boxcar HRFs

## Examples

``` r
# \donttest{
# Events with internal temporal structure
trial_data <- data.frame(
  onset = c(0, 20),
  sub_times = I(list(c(0, 1, 2), c(0, 3, 6))),  # Times relative to onset
  sub_weights = I(list(c(0.2, 0.5, 0.3), c(0.1, 0.6, 0.3))),
  run = 1
)
sf <- fmrihrf::sampling_frame(blocklens = 50, TR = 2)

emod <- event_model(
  onset ~ hrf(onset, hrf_fun = weighted_hrf_gen("sub_times", "sub_weights", relative = TRUE)),
  data = trial_data, block = ~run, sampling_frame = sf
)
#> Warning: Parameters times, weights, width, method, normalize are not arguments to function weighted[3 pts, constant] and will be ignored
#> Warning: Parameters times, weights, width, method, normalize are not arguments to function weighted[3 pts, constant] and will be ignored
print(emod)
#> 
#> ── fMRI Event Model ────────────────────────────────────────────────────────────
#> Number of Terms: 1
#> Number of Events: 2
#> Number of Blocks: 1
#> Total Scans: 50
#> Design Matrix Dimensions: 50 x 1
#> 
#> ── Terms ──
#> 
#> • onset (<event_term>)
#> 
#> ── Design Matrix Preview ──
#> 
#>           onset_onset
#>    Scan 1   0.000    
#>    Scan 2   0.000    
#>    Scan 3   0.000    
#> ...
# }
```
