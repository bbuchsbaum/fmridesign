# Pairwise Contrasts

Construct pairwise contrasts for all combinations of factor levels.

## Usage

``` r
pairwise_contrasts(levels, facname, where = NULL, name_prefix = "con")
```

## Arguments

- levels:

  A vector of factor levels to be compared.

- facname:

  The name of the factor variable (column name in the design) these
  levels belong to.

- where:

  An optional formula specifying the subset over which the contrast is
  computed.

- name_prefix:

  A character string to prefix the generated contrast names (default:
  "con").

## Value

A contrast_set object containing pairwise contrasts for all combinations
of factor levels.

## Examples

``` r
# Assuming 'my_factor' is a column name
pairwise_contrasts(c("A", "B", "C"), facname = "my_factor")
#> 
#> === Contrast Set ===
#> 
#>  Overview:
#>   * Number of contrasts: 3 
#>   * Types of contrasts:
#>     - pair_contrast_spec : 3 
#> 
#>   Individual Contrasts:
#> 
#> [1] con_A_B (pair_contrast_spec)
#>     Formula: ~my_factor == "A" vs  ~my_factor == "B"
#> 
#> [2] con_A_C (pair_contrast_spec)
#>     Formula: ~my_factor == "A" vs  ~my_factor == "C"
#> 
#> [3] con_B_C (pair_contrast_spec)
#>     Formula: ~my_factor == "B" vs  ~my_factor == "C"
#> 
pairwise_contrasts(c("A", "B", "C"), facname = "my_factor", name_prefix = "pair")
#> 
#> === Contrast Set ===
#> 
#>  Overview:
#>   * Number of contrasts: 3 
#>   * Types of contrasts:
#>     - pair_contrast_spec : 3 
#> 
#>   Individual Contrasts:
#> 
#> [1] pair_A_B (pair_contrast_spec)
#>     Formula: ~my_factor == "A" vs  ~my_factor == "B"
#> 
#> [2] pair_A_C (pair_contrast_spec)
#>     Formula: ~my_factor == "A" vs  ~my_factor == "C"
#> 
#> [3] pair_B_C (pair_contrast_spec)
#>     Formula: ~my_factor == "B" vs  ~my_factor == "C"
#> 
```
