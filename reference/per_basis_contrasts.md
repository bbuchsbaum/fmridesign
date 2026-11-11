# One Contrast per Basis Function

Turn a contrast into a set of contrasts, one for each basis function of
a multi-basis HRF. With an FIR basis this gives the contrast's time
course, one contrast per time bin.

## Usage

``` r
per_basis_contrasts(con, basis)
```

## Arguments

- con:

  A contrast specification that has a `basis` argument:
  [`pair_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/pair_contrast.md),
  [`unit_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/unit_contrast.md),
  [`oneway_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/oneway_contrast.md)
  or
  [`poly_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/poly_contrast.md).

- basis:

  Integer vector of basis-function indices, e.g. `1:8` for an HRF with
  eight basis functions.

## Value

A
[`contrast_set()`](https://bbuchsbaum.github.io/fmridesign/reference/contrast_set.md)
with one copy of `con` per index, restricted to that basis function and
named `<name>_b01`, `<name>_b02`, ...

## See also

[`pair_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/pair_contrast.md)
for averaging over or weighting basis functions.

## Examples

``` r
# A - B in each of the 8 bins of an FIR basis
per_basis_contrasts(
  pair_contrast(~ cond == "A", ~ cond == "B", name = "AvB"),
  basis = 1:8
)
#> 
#> === Contrast Set ===
#> 
#>  Overview:
#>   * Number of contrasts: 8 
#>   * Types of contrasts:
#>     - pair_contrast_spec : 8 
#> 
#>   Individual Contrasts:
#> 
#> [1] AvB_b01 (pair_contrast_spec)
#>     Formula: ~cond == "A" vs  ~cond == "B"
#> 
#> [2] AvB_b02 (pair_contrast_spec)
#>     Formula: ~cond == "A" vs  ~cond == "B"
#> 
#> [3] AvB_b03 (pair_contrast_spec)
#>     Formula: ~cond == "A" vs  ~cond == "B"
#> 
#> [4] AvB_b04 (pair_contrast_spec)
#>     Formula: ~cond == "A" vs  ~cond == "B"
#> 
#> [5] AvB_b05 (pair_contrast_spec)
#>     Formula: ~cond == "A" vs  ~cond == "B"
#> 
#> [6] AvB_b06 (pair_contrast_spec)
#>     Formula: ~cond == "A" vs  ~cond == "B"
#> 
#> [7] AvB_b07 (pair_contrast_spec)
#>     Formula: ~cond == "A" vs  ~cond == "B"
#> 
#> [8] AvB_b08 (pair_contrast_spec)
#>     Formula: ~cond == "A" vs  ~cond == "B"
#> 
```
