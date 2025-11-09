# Extract term indices

Extract term indices

## Usage

``` r
term_indices(x, ...)

# Default S3 method
term_indices(x, ...)
```

## Arguments

- x:

  The object.

- ...:

  Additional arguments.

## Value

Integer vector or list mapping term(s) to column indices.

## Examples

``` r
# Create a sampling frame and event model
sf <- fmrihrf::sampling_frame(blocklens = c(100, 100), TR = 2)
events <- data.frame(
  onset = c(10, 30, 50, 70),
  condition = c("A", "B", "A", "B"),
  block = c(1, 1, 2, 2)
)
model <- event_model(onset ~ hrf(condition), events, ~ block, sf)

# Get design matrix and extract term indices
dm <- design_matrix(model)
indices <- term_indices(dm)
print(indices)
#> $condition
#> [1] 1 2
#> 

# Access indices for specific term
condition_indices <- indices[["condition"]]
print(condition_indices)
#> [1] 1 2
```
