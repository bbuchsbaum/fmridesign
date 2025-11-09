# B-spline basis

Generate the B-spline basis matrix for a polynomial spline.

## Usage

``` r
BSpline(x, degree)
```

## Arguments

- x:

  a numeric vector at which to evaluate the spline. Missing values are
  not allowed in x

- degree:

  the degree of the piecewise polynomial

## Value

an `BSpline` list instance

## See also

[bs](https://rdrr.io/r/splines/bs.html)

## Examples

``` r
x_vals <- seq(0, 1, length.out = 6)
bs_obj <- BSpline(x_vals, degree = 3)
dim(bs_obj$y)
#> [1] 6 3
bs_obj$name
#> [1] "bs_x_vals"
```
