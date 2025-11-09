# Residualize Data Against a Design

Projects the data onto the orthogonal complement of the design,
returning residuals from an OLS fit. Specialized for `event_model`,
`baseline_model`, or a raw numeric design matrix.

## Usage

``` r
residualize(x, data, cols = NULL, ...)
```

## Arguments

- x:

  A design object (`event_model`, `baseline_model`) or a numeric matrix
  (design matrix X).

- data:

  A numeric vector/matrix/data.frame of observations Y with rows
  matching `nrow(design_matrix(x))`.

- cols:

  Optional integer or character vector selecting columns of the design
  to project out.

- ...:

  Additional arguments passed to methods.

## Value

Residuals with the same dimensions as `data`.

## Examples

``` r
# Simple example with a raw design matrix
X <- cbind(1, 1:5)
Y <- cbind(1:5, 2:6)
R <- residualize(X, Y)
dim(R)
#> [1] 5 2
```
