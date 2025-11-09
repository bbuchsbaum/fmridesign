# Extract columns

Extract columns

## Usage

``` r
# S3 method for class 'Scale'
columns(x, ...)

# S3 method for class 'ScaleWithin'
columns(x, ...)

# S3 method for class 'RobustScale'
columns(x, ...)

columns(x, ...)
```

## Arguments

- x:

  The object.

- ...:

  Additional arguments.

## Value

Character vector of column names produced by the object.

## Examples

``` r
bs_basis <- BSpline(seq(0, 1, length.out = 5), degree = 3)
columns(bs_basis)
#> [1] "X01" "X02" "X03"
```
