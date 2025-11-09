# Extract term matrices

Extract term matrices

## Usage

``` r
term_matrices(x, ...)
```

## Arguments

- x:

  The object.

- ...:

  Additional arguments.

## Value

A list of matrices/tibbles, one per term.

## Examples

``` r
# Create a simple experimental design with event model
des <- data.frame(
  onset = c(0, 10, 20, 30),
  run = 1,
  cond = factor(c("A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)

# Extract term matrices - returns list with one matrix per term
term_mats <- term_matrices(emod)
names(term_mats)     # Shows term names
#> [1] "cond"
ncol(term_mats[[1]]) # Number of columns for first term
#> [1] 2

# Create baseline model and extract its term matrices
bmod <- baseline_model(sframe = sframe)
baseline_mats <- term_matrices(bmod)
names(baseline_mats) # Shows baseline term names
#> [1] "drift"
```
