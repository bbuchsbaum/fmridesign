# Split by block

Split by block

## Usage

``` r
split_by_block(x, ...)
```

## Arguments

- x:

  The object.

- ...:

  Additional arguments.

## Value

A list split by block/run.

## Examples

``` r
des <- data.frame(
  onset = c(0, 10, 20, 30, 5, 15, 25, 35),
  run = c(1, 1, 1, 1, 2, 2, 2, 2),
  cond = factor(c("A", "B", "A", "B", "A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 1)
emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
block_list <- split_by_block(emod)
#> Error in UseMethod("split_by_block"): no applicable method for 'split_by_block' applied to an object of class "c('event_model', 'list')"
length(block_list)
#> Error: object 'block_list' not found
```
