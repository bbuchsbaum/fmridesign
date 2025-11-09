# Create a Set of Contrasts

Construct a list of contrast_spec objects.

## Usage

``` r
contrast_set(...)
```

## Arguments

- ...:

  A variable-length list of contrast_spec objects.

## Value

A list of contrast_spec objects with class "contrast_set".

## Examples

``` r
c1 <- contrast(~ A - B, name="A_B")
c2 <- contrast(~ B - C, name="B_C")
contrast_set(c1,c2)
#> 
#> === Contrast Set ===
#> 
#>  Overview:
#>   * Number of contrasts: 2 
#>   * Types of contrasts:
#>     - contrast_formula_spec : 2 
#> 
#>   Individual Contrasts:
#> 
#> [1] A_B (contrast_formula_spec)
#>     Formula: ~A - B
#> 
#> [2] B_C (contrast_formula_spec)
#>     Formula: ~B - C
#> 
```
