# Retrieve contrast specifications from an hrfspec

Retrieve contrast specifications from an hrfspec

## Usage

``` r
# S3 method for class 'hrfspec'
contrasts(x, ...)
```

## Arguments

- x:

  An `hrfspec` object.

- ...:

  Unused.

## Value

The list of contrast specifications attached to the hrfspec, or `NULL`.

## Examples

``` r
condition <- factor(c("A", "B"))
cset <- contrast_set(
  diff = column_contrast(pattern_A = "condition.A", pattern_B = "condition.B", name = "diff")
)
spec <- hrf(condition, contrasts = cset)
contrasts(spec)
#> 
#> === Contrast Set ===
#> 
#>  Overview:
#>   * Number of contrasts: 1 
#>   * Types of contrasts:
#>     - column_contrast_spec : 1 
#> 
#>   Individual Contrasts:
#> 
#> [1] diff (column_contrast_spec)
#>     Formula: 
#> 
```
