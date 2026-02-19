# trialwise

Generate one regressor per trial (plus an optional grand-mean column) by
delegating everything to
[`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md).

## Usage

``` r
trialwise(
  basis = "spmg1",
  lag = 0,
  nbasis = 1,
  add_sum = FALSE,
  label = "trial",
  durations = NULL,
  normalize = FALSE
)
```

## Arguments

- basis, lag, nbasis:

  Passed straight to
  [`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md).

- add_sum:

  If TRUE, append a column that is the average of all trialwise columns
  (useful as a conventional main effect).

- label:

  Term label / prefix for the generated columns.

- durations:

  Optional durations override (passed to
  [`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md)).
  If NULL, uses the durations argument from `event_model`.

- normalize:

  logical; if TRUE, each trialwise regressor column is peak-normalized
  so that `max(abs(column)) == 1`. When events have different durations,
  longer events produce taller peaks after convolution. Normalizing
  equalizes amplitudes so that beta estimates are comparable across
  trials regardless of duration. Particularly useful for MVPA and RSA
  analyses. Default FALSE.

## Value

An `hrfspec` term to be used on the RHS of an event-model formula.

## Details

Use it **only on the RHS** of an event-model formula:

    onset ~ trialwise(basis = "spmg1", add_sum = TRUE)

## Examples

``` r
# Create example trial data for beta-series analysis
trial_data <- data.frame(
  onset = c(2, 8, 14, 20, 26),
  run = c(1, 1, 1, 1, 1)
)

# Create sampling frame (30 TRs, TR=2s)
sframe <- fmrihrf::sampling_frame(blocklens = 30, TR = 2)

# Basic trialwise model - creates one regressor per trial
emod_trials <- event_model(onset ~ trialwise(),
                          data = trial_data,
                          block = ~run,
                          sampling_frame = sframe)
print(emod_trials)
#> 
#> ── fMRI Event Model ────────────────────────────────────────────────────────────
#> Number of Terms: 1
#> Number of Events: 5
#> Number of Blocks: 1
#> Total Scans: 30
#> Design Matrix Dimensions: 30 x 5
#> 
#> ── Terms ──
#> 
#> • trial (<event_term>)
#> 
#> ── Design Matrix Preview ──
#> 
#>           trial_.trial_factor.length.onsets...1
#>    Scan 1   0.000                              
#>    Scan 2   0.048                              
#>    Scan 3   1.073                              
#>           trial_.trial_factor.length.onsets...2
#>    Scan 1   0.000                              
#>    Scan 2   0.000                              
#>    Scan 3   0.000                              
#>           trial_.trial_factor.length.onsets...3
#>    Scan 1   0.000                              
#>    Scan 2   0.000                              
#>    Scan 3   0.000                              
#>           trial_.trial_factor.length.onsets...4
#>    Scan 1   0.000                              
#>    Scan 2   0.000                              
#>    Scan 3   0.000                              
#> ...

# Trialwise with different basis and grand mean
emod_trials_mean <- event_model(onset ~ trialwise(basis = "spmg2", add_sum = TRUE),
                               data = trial_data,
                               block = ~run,
                               sampling_frame = sframe)
print(emod_trials_mean)
#> 
#> ── fMRI Event Model ────────────────────────────────────────────────────────────
#> Number of Terms: 1
#> Number of Events: 5
#> Number of Blocks: 1
#> Total Scans: 30
#> Design Matrix Dimensions: 30 x 11
#> 
#> ── Terms ──
#> 
#> • trial (<event_term>)
#> 
#> ── Design Matrix Preview ──
#> 
#>           trial_.trial_factor.length.onsets...1_b01
#>    Scan 1   0.000                                  
#>    Scan 2   0.048                                  
#>    Scan 3   1.073                                  
#>           trial_.trial_factor.length.onsets...2_b01
#>    Scan 1   0.000                                  
#>    Scan 2   0.162                                  
#>    Scan 3   0.654                                  
#>           trial_.trial_factor.length.onsets...3_b01
#>    Scan 1   0.000                                  
#>    Scan 2   0.000                                  
#>    Scan 3   0.000                                  
#>           trial_.trial_factor.length.onsets...4_b01
#>    Scan 1   0.000                                  
#>    Scan 2   0.000                                  
#>    Scan 3   0.000                                  
#> ...
```
