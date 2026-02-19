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
# Create a simple event model
des <- data.frame(
  onset = c(0, 10, 20, 30),
  run = 1,
  cond = factor(c("A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
emodel <- event_model(onset ~ hrf(cond), data = des, block = ~run,
                      sampling_frame = sframe)

# Validate all attached contrasts on a model
res <- validate_contrasts(emodel)

# Validate a custom vector against a model
v <- rep(0, ncol(design_matrix(emodel)))
v[1] <- 1
v[2] <- -1
res2 <- validate_contrasts(emodel, weights = v)
```
