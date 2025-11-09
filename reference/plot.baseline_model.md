# Plot a Baseline Model

Creates a detailed ggplot2 visualization of the baseline model design
matrix. Each non-constant term is plotted over time. The plot includes
separate panels for each block and supports customization of titles,
axis labels, line size, and color palette.

## Usage

``` r
# S3 method for class 'baseline_model'
plot(
  x,
  term_name = NULL,
  title = NULL,
  xlab = "Time",
  ylab = "Design Matrix Value",
  line_size = 1,
  color_palette = "Set1",
  ...
)
```

## Arguments

- x:

  A baseline_model object.

- term_name:

  Optional term name (a character string) specifying which term to plot.
  If omitted, the first non-constant term is plotted.

- title:

  Optional title for the plot. If not provided, a default title is
  generated.

- xlab:

  Label for the x-axis (default: "Time").

- ylab:

  Label for the y-axis (default: "Design Matrix Value").

- line_size:

  Numeric value for line thickness (default: 1).

- color_palette:

  A palette name for the line colors (default: "Set1").

- ...:

  Additional arguments passed to ggplot2::geom_line.

## Value

A ggplot2 plot object.

## Examples

``` r
sframe <- fmrihrf::sampling_frame(blocklens = 5, TR = 1)
bmod <- baseline_model(sframe = sframe)
if (requireNamespace("ggplot2", quietly = TRUE)) plot(bmod)
#> No term_name specified, plotting the first available non-constant term: drift
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the fmridesign package.
#>   Please report the issue at <https://github.com/bbuchsbaum/fmridesign/issues>.

```
