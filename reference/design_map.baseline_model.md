# Heatmap visualization of the baseline_model design matrix

Produces a heatmap of all columns in the design matrix for a
`baseline_model` object, with rows corresponding to scans and columns
corresponding to regressors. By default, it draws horizontal lines
separating runs (blocks), and rotates the column labels diagonally.

## Usage

``` r
# S3 method for class 'baseline_model'
design_map(
  x,
  block_separators = TRUE,
  rotate_x_text = TRUE,
  fill_midpoint = NULL,
  fill_limits = NULL,
  ...
)
```

## Arguments

- x:

  A `baseline_model` object.

- block_separators:

  Logical; if `TRUE`, draw white horizontal lines between blocks.

- rotate_x_text:

  Logical; if `TRUE`, rotate x-axis labels by 45 degrees.

- fill_midpoint:

  Numeric or `NULL`; if not `NULL`, used as the `midpoint` in
  [`ggplot2::scale_fill_gradient2()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html)
  to center the color scale (for example at 0).

- fill_limits:

  Numeric vector of length 2 or `NULL`; passed to the fill scale
  `limits` argument. Can clip or expand the color range.

- ...:

  Additional arguments forwarded to
  [`ggplot2::geom_tile()`](https://ggplot2.tidyverse.org/reference/geom_tile.html).

## Value

A ggplot2 plot object.

## Examples

``` r
sframe <- fmrihrf::sampling_frame(blocklens = 5, TR = 1)
bmod <- baseline_model(sframe = sframe)
if (requireNamespace("ggplot2", quietly = TRUE)) design_map(bmod)
```
