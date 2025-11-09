# Split onsets

Split onsets

## Usage

``` r
split_onsets(x, sframe, global = FALSE, blocksplit = FALSE, ...)

# S3 method for class 'event_term'
split_onsets(x, sframe, global = FALSE, blocksplit = FALSE, ...)
```

## Arguments

- x:

  The object.

- sframe:

  The sampling frame object containing timing information.

- global:

  Whether onsets are in global time units (across all runs).

- blocksplit:

  Whether to split onsets by blocks.

- ...:

  Additional arguments.

## Value

A list of onset vectors, one per block (unless `global=TRUE`).

## Examples

``` r
# Create an event term with mixed conditions across blocks
conditions <- factor(c("A", "B", "A", "B", "A", "B"))
onsets <- c(5, 15, 25, 105, 115, 125)  # Events in blocks 1 and 2
blockids <- c(1, 1, 1, 2, 2, 2)

term <- event_term(
  list(condition = conditions),
  onsets = onsets,
  blockids = blockids
)

# Create sampling frame for two blocks of 50 TRs each
sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)

# Split onsets by condition (default behavior)
onset_list <- split_onsets(term, sframe)
names(onset_list)  # Shows condition names
#> [1] "condition.A" "condition.B"
onset_list$condition.A  # Onsets for condition A
#> [1]   5  25 115

# Split with global timing (onsets relative to start of experiment)
global_onsets <- split_onsets(term, sframe, global = TRUE)

# Split by both condition and block
block_split <- split_onsets(term, sframe, blocksplit = TRUE)
```
