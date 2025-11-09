# Check design matrix for multicollinearity

Convenience helper to quickly flag highly correlated regressors.

## Usage

``` r
check_collinearity(X, threshold = 0.9)
```

## Arguments

- X:

  A numeric design matrix (or an `event_model`).

- threshold:

  Absolute correlation above which a pair is flagged. Default 0.9.

## Value

A list with elements: `ok` (logical), `pairs` (data.frame with offending
pairs and their correlations). Invisibly returns the same list.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- check_collinearity(design_matrix(emodel), threshold = 0.95)
if (!res$ok) print(res$pairs)
} # }
```
