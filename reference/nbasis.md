# Number of Basis Functions

Get the number of basis functions for various basis objects.

## Usage

``` r
# S3 method for class 'BSpline'
nbasis(x, ...)

# S3 method for class 'Poly'
nbasis(x, ...)

# S3 method for class 'Scale'
nbasis(x, ...)

# S3 method for class 'ScaleWithin'
nbasis(x, ...)

# S3 method for class 'RobustScale'
nbasis(x, ...)

# S3 method for class 'Standardized'
nbasis(x, ...)

# S3 method for class 'Ident'
nbasis(x, ...)

# S3 method for class 'covariate_convolved_term'
nbasis(x, ...)
```

## Arguments

- x:

  A basis object (e.g., BSpline, Poly, Ident, etc.)

- ...:

  Additional arguments (currently unused)

## Value

An integer representing the number of basis functions
