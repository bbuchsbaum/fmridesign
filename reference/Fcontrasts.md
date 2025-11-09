# Compute F-contrasts

Compute F-contrasts

## Usage

``` r
Fcontrasts(x, ...)

# S3 method for class 'convolved_term'
Fcontrasts(x, ...)

# S3 method for class 'event_model'
Fcontrasts(x, ...)
```

## Arguments

- x:

  The object.

- ...:

  Additional arguments.

## Value

A named list of matrices with F-contrast weights.

## Details

Row names of the contrast matrices can specify which levels of the term
are tested. Any matching is done against the design matrix column names.

## Examples

``` r
des <- data.frame(
  onset = c(0, 10, 20, 30),
  run = 1,
  cond = factor(c("A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
names(Fcontrasts(emod))
#> [1] "cond#cond"
```
