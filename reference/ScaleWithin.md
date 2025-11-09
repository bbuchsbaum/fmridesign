# Z-score within groups

Z-score within groups

## Usage

``` r
ScaleWithin(x, g)
```

## Arguments

- x:

  numeric vector

- g:

  grouping factor / character / integer of same length as x

## Value

An object of class `ScaleWithin` (a `ParametricBasis`).

## Examples

``` r
# Create a within-group z-score transformed basis
x_vals <- c(1, 2, 3, 10, 11, 12)
groups <- c("A", "A", "A", "B", "B", "B")
scale_within_basis <- ScaleWithin(x_vals, groups)
print(scale_within_basis$y)
#>      z_x_vals_by_groups
#> [1,]                 -1
#> [2,]                  0
#> [3,]                  1
#> [4,]                 -1
#> [5,]                  0
#> [6,]                  1
print(scale_within_basis$means)
#>  A  B 
#>  2 11 
print(scale_within_basis$sds)
#> A B 
#> 1 1 
```
