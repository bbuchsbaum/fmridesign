# Ident

A basis that applies identity transform to a set of raw variables.

## Usage

``` r
Ident(...)
```

## Arguments

- ...:

  a list of variable names

## Value

an instance of class `Ident` extending `ParametricBasis`

## Examples

``` r
# Create identity basis from numeric vectors
x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 6, 8, 10)
ident_basis <- Ident(x, y)
print(ident_basis$y)
#>      x  y
#> [1,] 1  2
#> [2,] 2  4
#> [3,] 3  6
#> [4,] 4  8
#> [5,] 5 10
```
