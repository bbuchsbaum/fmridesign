# Extract or construct a design matrix

Extract or construct a design matrix

## Usage

``` r
# S3 method for class 'baseline_model'
design_matrix(x, blockid = NULL, allrows = FALSE, ...)

# S3 method for class 'baseline_term'
design_matrix(x, blockid = NULL, allrows = FALSE, ...)

design_matrix(x, ...)

# S3 method for class 'event_model'
design_matrix(x, blockid = NULL, ...)

# S3 method for class 'event_term'
design_matrix(x, drop.empty = TRUE, ...)
```

## Arguments

- x:

  The object to extract design matrix from.

- blockid:

  Block ID(s) to extract (for baseline_term method).

- allrows:

  Whether to return all rows (for baseline_term method).

- ...:

  Additional arguments.

- drop.empty:

  Whether to drop empty columns (for event_term method).

## Value

A matrix-like object (often tibble) with rows = scans, cols =
regressors.

## Examples

``` r
sframe <- fmrihrf::sampling_frame(blocklens = 6, TR = 1)
bmod <- baseline_model(sframe = sframe)
head(design_matrix(bmod))
#> # A tibble: 6 × 1
#>   base_constant1_block_1
#>                    <dbl>
#> 1                      1
#> 2                      1
#> 3                      1
#> 4                      1
#> 5                      1
#> 6                      1
```
