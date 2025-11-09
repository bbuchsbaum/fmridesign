# plot_contrasts.event_model

Produces a heatmap of all contrasts defined for an `event_model`. Rows =
each contrast (or column of an F-contrast), columns = each regressor in
the full design matrix, and the fill color = the contrast weight.

## Usage

``` r
# S3 method for class 'event_model'
plot_contrasts(
  x,
  absolute_limits = FALSE,
  rotate_x_text = TRUE,
  scale_mode = c("auto", "diverging", "one_sided"),
  coord_fixed = TRUE,
  ...
)
```

## Arguments

- x:

  An `event_model` with (lazily) defined contrasts.

- absolute_limits:

  Logical; if `TRUE`, the color scale is fixed at (-1,1). If `FALSE`,
  the range is set to (min, max) of the weights.

- rotate_x_text:

  Logical; if `TRUE`, rotate x-axis labels for readability.

- scale_mode:

  Character; 'auto', 'diverging', or 'one_sided' color scaling.

- coord_fixed:

  Logical; if TRUE, use fixed aspect ratio.

- ...:

  Further arguments passed to `geom_tile`, e.g. `color="grey80"`.

## Value

A `ggplot2` object (a heatmap).
