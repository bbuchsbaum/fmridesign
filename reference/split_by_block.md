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
# Create experimental design with multiple runs
des <- data.frame(
  onset = c(0, 10, 20, 30, 5, 15, 25, 35),
  run = c(1, 1, 1, 1, 2, 2, 2, 2),
  cond = factor(c("A", "B", "A", "B", "A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 1)

# Create an event model
emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)

# Example usage (when methods are implemented):
# block_list <- split_by_block(emod)
# length(block_list)  # Should return 2 (for 2 runs)
```
