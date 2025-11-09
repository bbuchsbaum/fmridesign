# Standardized basis

Standardize a numeric vector by centering and scaling, handling NAs
appropriately. If the computed standard deviation is `NA` or zero, a
small constant (`1e-6`) is used instead to avoid division by zero. The
returned basis matrix has one column with this standardized name.

## Usage

``` r
Standardized(x)
```

## Arguments

- x:

  a numeric vector to standardize. Missing values are allowed and will
  be replaced with 0 after standardization.

## Value

an instance of class `Standardized` extending `ParametricBasis`

## Examples

``` r
# Standardize a numeric vector
x_vals <- c(10, 20, 30, 40, 50)
std_basis <- Standardized(x_vals)
print(std_basis$y)
#>      std_x_vals
#> [1,] -1.2649111
#> [2,] -0.6324555
#> [3,]  0.0000000
#> [4,]  0.6324555
#> [5,]  1.2649111
print(std_basis$mean)
#> [1] 30
print(std_basis$sd)
#> [1] 15.81139
```
