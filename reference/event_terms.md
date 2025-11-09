# Extract event terms

Extract event terms

## Usage

``` r
event_terms(x, ...)
```

## Arguments

- x:

  The object.

- ...:

  Additional arguments.

## Value

A named list of event term objects.

## Examples

``` r
# Create a simple experimental design
des <- data.frame(
  onset = c(0, 10, 20, 30),
  run = 1,
  cond = factor(c("A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)

# Extract event terms (named list of event term objects)
terms_list <- event_terms(emod)
names(terms_list)
#> [1] "cond"
```
