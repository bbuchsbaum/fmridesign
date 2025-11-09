# Extract contrasts

Extract contrasts

## Usage

``` r
contrasts(x, ...)
```

## Arguments

- x:

  The object.

- ...:

  Additional arguments.

## Value

A named list of contrast specifications.

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
contrasts(emod)
#> $cond.diff
#> contrast: diff 
#>  A:  NULL 
#> 
```
