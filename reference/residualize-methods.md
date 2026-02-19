# Residualize methods

S3 methods for the
[`residualize()`](https://bbuchsbaum.github.io/fmridesign/reference/residualize.md)
generic. These methods project data onto the orthogonal complement of a
design, returning OLS residuals.

## Usage

``` r
# S3 method for class 'matrix'
residualize(x, data, cols = NULL, ...)

# S3 method for class 'event_model'
residualize(x, data, cols = NULL, ...)

# S3 method for class 'baseline_model'
residualize(x, data, cols = NULL, ...)
```

## Arguments

- x:

  A design object: matrix, event_model, or baseline_model.

- data:

  Numeric vector/matrix/data.frame of observations Y.

- cols:

  Optional integer or character vector selecting columns to project out.

- ...:

  Additional arguments (currently unused).

## Value

A numeric matrix of residuals with the same dimensions as `data`.

## Examples

``` r
# Residualize with a raw matrix
X <- cbind(1, 1:10)
Y <- matrix(rnorm(20), ncol = 2)
R <- residualize(X, Y)
dim(R)  # 10 x 2
#> [1] 10  2

# Residualize with an event model
des <- data.frame(
  onset = c(0, 10, 20, 30),
  run = 1,
  cond = factor(c("A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
emod <- event_model(onset ~ hrf(cond), data = des, block = ~run,
                    sampling_frame = sframe)
Y_sim <- matrix(rnorm(40 * 2), ncol = 2)
R_emod <- residualize(emod, Y_sim)
dim(R_emod)  # 40 x 2
#> [1] 40  2
```
