# plot_contrasts

Generic function for plotting contrasts.

## Usage

``` r
plot_contrasts(x, ...)
```

## Arguments

- x:

  Object containing contrast information

- ...:

  Additional arguments passed to methods

## Value

A plot object (typically ggplot2) displaying the contrasts. The exact
type depends on the method used.

## Examples

``` r
# Create example data
des <- data.frame(
  onset = c(1, 3, 5, 7),
  cond = factor(c("A", "B", "A", "B")),
  run = c(1, 1, 1, 1)
)

# Create sampling frame and event model
sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)

# Create contrast set
cset <- contrast_set(
  main_A = unit_contrast(~ cond == "A", name = "A_vs_baseline"),
  diff = pair_contrast(~ cond == "A", ~ cond == "B", name = "A_vs_B")
)

# Create event model with contrasts
emod <- event_model(onset ~ hrf(cond, contrasts = cset),
                    data = des, block = ~run, sampling_frame = sframe)

# Plot the contrasts
plot_contrasts(emod)

```
