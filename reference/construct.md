# Construct method

Construct method

## Usage

``` r
# S3 method for class 'baselinespec'
construct(x, model_spec, ...)

# S3 method for class 'covariatespec'
construct(x, model_spec, sampling_frame = NULL, ...)

construct(x, ...)
```

## Arguments

- x:

  The object.

- model_spec:

  A model specification object (used by some methods). For baselinespec:
  typically a sampling_frame or list containing one. For
  hrfspec/covariatespec: contains data and other model information.

- ...:

  Additional arguments.

- sampling_frame:

  A sampling_frame object (used by covariatespec method).

## Value

A constructed object; return type depends on method.

## Examples

``` r
sframe <- fmrihrf::sampling_frame(blocklens = 5, TR = 1)
drift_spec <- baseline(degree = 2, basis = "poly")
construct(drift_spec, sframe)
#> fmri_term:  baseline_term 
#>   Term Name:  baseline_poly_2 
#>   Num Rows:  5 
#>   Num Columns:  2 
```
