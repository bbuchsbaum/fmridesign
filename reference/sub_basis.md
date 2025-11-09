# sub_basis

Subset a parametric basis regressor.

## Usage

``` r
sub_basis(x, subset)

# S3 method for class 'Scale'
sub_basis(x, subset)

# S3 method for class 'ScaleWithin'
sub_basis(x, subset)

# S3 method for class 'RobustScale'
sub_basis(x, subset)
```

## Arguments

- x:

  the object

- subset:

  the subset (logical or integer indices)

## Value

An object of the same class as `x` with subset applied.

## Examples

``` r
# Create some sample data
x_vals <- 1:10
rt_vals <- rnorm(10, 500, 50)

# Create different basis objects
poly_basis <- Poly(x_vals, degree = 3)
scale_basis <- Scale(rt_vals)
bspline_basis <- BSpline(x_vals, degree = 2)

# Subset with integer indices
poly_sub <- sub_basis(poly_basis, 1:5)
scale_sub <- sub_basis(scale_basis, c(1, 3, 5, 7, 9))

# Subset with logical indices
logical_idx <- x_vals <= 5
bspline_sub <- sub_basis(bspline_basis, logical_idx)

# Check dimensions
nrow(poly_basis$y)  # 10
#> [1] 10
nrow(poly_sub$y)    # 5
#> [1] 5
nrow(scale_sub$y)   # 5
#> [1] 5
nrow(bspline_sub$y) # 5
#> [1] 5
```
