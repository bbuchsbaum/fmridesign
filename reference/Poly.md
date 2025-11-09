# Polynomial basis

Orthogonal polynomial expansion of a linear term based on
[`poly`](https://rdrr.io/r/stats/poly.html)

## Usage

``` r
Poly(x, degree)
```

## Arguments

- x:

  a numeric vector at which to evaluate the polynomial. Missing values
  are not allowed in x.

- degree:

  the degree of the polynomial. Must be less than the number of unique
  points.

## Value

an instance of class `Poly` extending `ParametricBasis`

## See also

[poly](https://rdrr.io/r/stats/poly.html)

## Examples

``` r
# Create a 3rd degree polynomial basis
x_vals <- c(1, 2, 3, 4, 5, 6)
poly_basis <- Poly(x_vals, degree = 3)
print(poly_basis$y)
#>               1          2          3
#> [1,] -0.5976143  0.5455447 -0.3726780
#> [2,] -0.3585686 -0.1091089  0.5217492
#> [3,] -0.1195229 -0.4364358  0.2981424
#> [4,]  0.1195229 -0.4364358 -0.2981424
#> [5,]  0.3585686 -0.1091089 -0.5217492
#> [6,]  0.5976143  0.5455447  0.3726780
#> attr(,"coefs")
#> attr(,"coefs")$alpha
#> [1] 3.5 3.5 3.5
#> 
#> attr(,"coefs")$norm2
#> [1]  1.00000  6.00000 17.50000 37.33333 64.80000
#> 
#> attr(,"degree")
#> [1] 1 2 3
#> attr(,"class")
#> [1] "poly"   "matrix"
```
