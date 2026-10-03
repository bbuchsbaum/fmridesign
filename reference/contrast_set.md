# Create a Set of Contrasts

Construct a list of contrast_spec objects. Nested `contrast_set`
arguments, including results from
[`one_against_all_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/one_against_all_contrast.md)
or
[`pairwise_contrasts()`](https://bbuchsbaum.github.io/fmridesign/reference/pairwise_contrasts.md),
are recursively flattened in argument order. Individual specifications
and their list names are preserved; names on enclosing sets are not used
as prefixes. Empty sets contribute no specifications. Ordinary lists are
not flattened; splice those explicitly with
`do.call(contrast_set, specs)`.

## Usage

``` r
contrast_set(...)
```

## Arguments

- ...:

  Individual `contrast_spec` objects or nested `contrast_set` objects.

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
contrast_set(c1, one_against_all_contrast(c("A", "B", "C"), "condition"))
#> 
#> === Contrast Set ===
#> 
#>  Overview:
#>   * Number of contrasts: 4 
#>   * Types of contrasts:
#>     - contrast_formula_spec : 1 
#>     - pair_contrast_spec : 3 
#> 
#>   Individual Contrasts:
#> 
#> [1] A_B (contrast_formula_spec)
#>     Formula: ~A - B
#> 
#> [2] con_A_vs_other (pair_contrast_spec)
#>     Formula: ~condition == "A" vs  ~condition != "A"
#> 
#> [3] con_B_vs_other (pair_contrast_spec)
#>     Formula: ~condition == "B" vs  ~condition != "B"
#> 
#> [4] con_C_vs_other (pair_contrast_spec)
#>     Formula: ~condition == "C" vs  ~condition != "C"
#> 
```
