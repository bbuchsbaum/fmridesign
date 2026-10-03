# Check design matrix for multicollinearity

Convenience helper to quickly flag highly correlated regressors. This is
a pairwise check only: constant columns (including intercepts) are
excluded, and `ok = TRUE` does not establish full rank, estimability or
good contrast precision. Dependencies involving several columns, such as
task regressors summing to an intercept in a no-rest design, can pass
this check. Use
[`check_estimability()`](https://bbuchsbaum.github.io/fmridesign/reference/check_estimability.md)
on the full design, including baseline/nuisance regressors, to inspect
those dependencies.

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
# Create a simple event model
des <- data.frame(
  onset = c(0, 10, 20, 30),
  run = 1,
  cond = factor(c("A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
emodel <- event_model(onset ~ hrf(cond), data = des, block = ~run,
                      sampling_frame = sframe)

# Check for multicollinearity
res <- check_collinearity(design_matrix(emodel), threshold = 0.95)
if (!res$ok) print(res$pairs)
```
