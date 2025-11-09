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
