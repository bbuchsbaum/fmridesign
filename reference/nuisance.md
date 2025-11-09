# Create a Nuisance Specification

Returns a nuisance term specification from a numeric matrix.

## Usage

``` r
nuisance(x)
```

## Arguments

- x:

  A matrix.

## Value

An object of class "nuisancespec".

## Examples

``` r
mat <- matrix(rnorm(10), nrow = 5)
nuisance(mat)
#> $name
#> mat
#> 
#> attr(,"class")
#> [1] "nuisancespec"
```
