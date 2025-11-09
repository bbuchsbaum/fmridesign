# Robust Scaling (Median/MAD)

Robust Scaling (Median/MAD)

## Usage

``` r
RobustScale(x)
```

## Arguments

- x:

  numeric vector (NAs allowed)

## Value

object of class c("RobustScale","ParametricBasis")

## Examples

``` r
# Create a robust scale transformed basis using median and MAD
x_vals <- c(1, 2, 3, 4, 100)  # Note the outlier
robust_basis <- RobustScale(x_vals)
print(robust_basis$y)
#>      robz_x_vals
#> [1,]  -1.3489815
#> [2,]  -0.6744908
#> [3,]   0.0000000
#> [4,]   0.6744908
#> [5,]  65.4256037
print(robust_basis$median)
#> [1] 3
print(robust_basis$mad)
#> [1] 1.4826
```
