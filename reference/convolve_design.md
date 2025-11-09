# Convolve HRF with Design Matrix.

Convolves a HRF with a design matrix (one column per condition) to
produce a list of regressors.

## Usage

``` r
convolve_design(hrf, dmat, globons, durations, summate = TRUE)
```

## Arguments

- hrf:

  A function representing the HRF.

- dmat:

  Design matrix (with named columns).

- globons:

  Numeric vector of global onsets.

- durations:

  Numeric vector of event durations.

- summate:

  Logical; if TRUE, summate the convolved HRF (default: TRUE).

## Value

A list of regressors (one for each column).

## Examples

``` r
hrf <- fmrihrf::HRF_SPMG1
dmat <- data.frame(A = c(1, 0, 1), B = c(0, 1, 0))
globons <- c(0, 10, 20)
durations <- rep(0, 3)
regs <- convolve_design(hrf, dmat, globons, durations)
length(regs)
#> [1] 2
```
