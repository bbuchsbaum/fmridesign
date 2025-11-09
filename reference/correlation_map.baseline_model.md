# correlation_map.baseline_model

Generates a correlation heatmap of the columns in a `baseline_model`'s
design matrix.

## Usage

``` r
# S3 method for class 'baseline_model'
correlation_map(
  x,
  method = c("pearson", "spearman"),
  half_matrix = FALSE,
  absolute_limits = TRUE,
  ...
)
```

## Arguments

- x:

  A `baseline_model`.

- method:

  Correlation method (e.g., "pearson", "spearman").

- half_matrix:

  Logical; if TRUE, display only the lower triangle of the matrix.

- absolute_limits:

  Logical; if TRUE, set color scale limits from -1 to 1.

- ...:

  Additional arguments passed to internal plotting functions.

## Value

A ggplot2 plot object.

## Examples

``` r
sframe <- fmrihrf::sampling_frame(blocklens = 5, TR = 1)
bmod <- baseline_model(sframe = sframe)
if (requireNamespace("ggplot2", quietly = TRUE)) correlation_map(bmod)
#> Warning: the standard deviation is zero
```
