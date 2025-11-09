# Extract baseline terms

Extract baseline terms

## Usage

``` r
# S3 method for class 'baseline_model'
baseline_terms(x, ...)

baseline_terms(x, ...)
```

## Arguments

- x:

  The object.

- ...:

  Additional arguments.

## Value

A named list of baseline term objects.

## Examples

``` r
sframe <- fmrihrf::sampling_frame(blocklens = 6, TR = 1)
bmod <- baseline_model(sframe = sframe)
baseline_terms(bmod)
#> $drift
#> fmri_term:  baseline_term 
#>   Term Name:  baseline_constant_1 
#>   Num Rows:  6 
#>   Num Columns:  1 
#> 
```
