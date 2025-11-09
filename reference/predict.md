# Predict from a ParametricBasis object

Dispatch to the appropriate method for transforming new data according
to a specific parametric basis.

## Usage

``` r
# S3 method for class 'ParametricBasis'
predict(object, newdata, ...)

# S3 method for class 'Standardized'
predict(object, newdata, ...)

# S3 method for class 'Poly'
predict(object, newdata, ...)

# S3 method for class 'BSpline'
predict(object, newdata, ...)

# S3 method for class 'Ident'
predict(object, newdata, ...)

# S3 method for class 'Scale'
predict(object, newdata, ...)

# S3 method for class 'ScaleWithin'
predict(object, newdata, newgroup, ...)

# S3 method for class 'RobustScale'
predict(object, newdata, ...)
```

## Arguments

- object:

  ParametricBasis object.

- newdata:

  Numeric vector to transform.

- ...:

  Additional arguments.

- newgroup:

  Optional factor for group-dependent bases.

## Value

A numeric matrix with transformed values (one column per basis
component).

## Examples

``` r
# Create polynomial basis from training data
train_x <- 1:10
poly_basis <- Poly(train_x, degree = 2)

# Predict on new data
new_x <- c(5.5, 7.3, 11.2)
predict(poly_basis, new_x)
#>              1          2
#> [1,] 0.0000000 -0.3590352
#> [2,] 0.1981735 -0.2180323
#> [3,] 0.6275493  1.0549106

# Create scaling basis
train_vals <- c(10, 20, 30, 40, 50)
scale_basis <- Scale(train_vals)

# Apply same scaling to new data
new_vals <- c(15, 25, 35)
predict(scale_basis, new_vals)
#>      z_train_vals
#> [1,]   -0.9486833
#> [2,]   -0.3162278
#> [3,]    0.3162278
```
