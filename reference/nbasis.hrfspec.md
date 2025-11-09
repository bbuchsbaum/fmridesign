# Get number of basis functions from hrfspec

Get number of basis functions from hrfspec

## Usage

``` r
# S3 method for class 'hrfspec'
nbasis(x, ...)
```

## Arguments

- x:

  An hrfspec object

- ...:

  Additional arguments (unused)

## Value

The number of basis functions

## Examples

``` r
# Create hrfspec with canonical HRF (1 basis function)
spec1 <- hrf(condition, basis = "spmg1")
nbasis(spec1)
#> [1] 1

# Create hrfspec with derivative HRF (2 basis functions)
spec2 <- hrf(condition, basis = "spmg2")
nbasis(spec2)
#> [1] 2

# Create hrfspec with FIR basis (custom number of basis functions)
spec3 <- hrf(condition, basis = "fir", nbasis = 10)
nbasis(spec3)
#> [1] 10
```
