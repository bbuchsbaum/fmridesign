# Validate contrast weights against a design matrix or event model

Provides basic diagnostics for t- and F-contrasts once the design matrix
is available. You can either pass an `event_model` (to validate all
attached contrasts) or a design matrix plus custom weights.

## Usage

``` r
validate_contrasts(x, weights = NULL, tol = 1e-08)
```

## Arguments

- x:

  An `event_model` or a numeric matrix/data.frame design matrix.

- weights:

  Optional contrast weights. May be a numeric vector (t-contrast), a
  numeric matrix (F-contrast with columns as contrast vectors), or a
  named list mapping names to vectors/matrices. If `NULL` and `x` is an
  `event_model`, all attached t- and F-contrasts are validated.

- tol:

  Numeric tolerance for zero checks. Default `1e-8`.

## Value

A data.frame with one row per validated contrast column and the
following columns: `name`, `type` ("t" or "F"), `estimable`,
`sum_to_zero`, `orthogonal_to_intercept`, `full_rank` (F only), and
`nonzero_weights`.

## Details

Checks include:

- Estimability: whether each contrast column lies in the row space of
  `X`.

- Sum-to-zero: whether the weights sum to ~0 (t-contrasts only).

- Intercept orthogonality: whether weights on intercept-like columns are
  ~0.

- Full-rank (F only): whether an F-contrast matrix has full column rank.

## Examples

``` r
if (FALSE) { # \dontrun{
# Validate all attached contrasts on a model
res <- validate_contrasts(emodel)

# Validate a custom vector against a model
v <- rep(0, ncol(design_matrix(emodel))); v[1] <- 1; v[2] <- -1
res2 <- validate_contrasts(emodel, weights = v)

# Validate a custom matrix against a design matrix
X <- as.matrix(design_matrix(emodel))
C <- cbind(c(1,-1,rep(0, ncol(X)-2)), c(0,1,-1,rep(0, ncol(X)-3)))
res3 <- validate_contrasts(X, weights = C)
} # }
```
