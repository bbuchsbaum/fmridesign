# Extract cells from a design object

Extract cells from a design object

## Usage

``` r
# S3 method for class 'baseline_model'
cells(x, drop.empty = TRUE, ...)

cells(x, drop.empty = TRUE, ...)

# S3 method for class 'event'
cells(x, drop.empty = TRUE, ...)

# S3 method for class 'event_term'
cells(x, drop.empty = TRUE, ...)

# S3 method for class 'covariate_convolved_term'
cells(x, ...)
```

## Arguments

- x:

  The object to extract cells from.

- drop.empty:

  Logical indicating whether to drop empty cells (default: TRUE).

- ...:

  Additional arguments (e.g., exclude_basis for convolved_term method).

## Value

A data.frame/tibble of cells (categorical combinations) relevant to `x`.

## Examples

``` r
sframe <- fmrihrf::sampling_frame(blocklens = 6, TR = 1)
bmod <- baseline_model(sframe = sframe)
head(cells(bmod))
#> # A tibble: 1 × 4
#>   term                level                  basis  index
#>   <chr>               <chr>                  <chr>  <int>
#> 1 baseline_constant_1 base_constant1_block_1 basis1     1
```
