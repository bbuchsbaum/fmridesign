# Retrieve contrast definitions for an event term

This accessor returns the list of contrast specifications attached to
the term's originating hrfspec, if any.

## Usage

``` r
# S3 method for class 'event_term'
contrasts(x, ...)
```

## Arguments

- x:

  An `event_term` object.

- ...:

  Unused.

## Value

A list of contrast specifications or `NULL` when none are defined.

## Examples

``` r
des <- data.frame(
  onset = c(0, 10, 20, 30),
  run = 1,
  cond = factor(c("A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
cset <- contrast_set(
  diff = column_contrast(pattern_A = "cond.A", pattern_B = "cond.B", name = "diff")
)
emod <- event_model(onset ~ hrf(cond, contrasts = cset),
                    data = des, block = ~run, sampling_frame = sframe)
contrasts(terms(emod)[[1]])
#> 
#> === Contrast Set ===
#> 
#>  Overview:
#>   * Number of contrasts: 1 
#>   * Types of contrasts:
#>     - column_contrast_spec : 1 
#> 
#>   Individual Contrasts:
#> 
#> [1] diff (column_contrast_spec)
#>     Formula: 
#> 
```
