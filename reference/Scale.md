# Z-score (global) basis

Z-score (global) basis

## Usage

``` r
Scale(x)
```

## Arguments

- x:

  numeric vector (NAs allowed)

## Value

object of class c("Scale","ParametricBasis")

## Examples

``` r
# Create a z-score transformed basis
x_vals <- c(1, 3, 5, 7, 9, 11)
scale_basis <- Scale(x_vals)
print(scale_basis$y)
#>        z_x_vals
#> [1,] -1.3363062
#> [2,] -0.8017837
#> [3,] -0.2672612
#> [4,]  0.2672612
#> [5,]  0.8017837
#> [6,]  1.3363062
print(scale_basis$mean)
#> [1] 6
print(scale_basis$sd)
#> [1] 3.741657
```
