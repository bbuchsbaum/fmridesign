# Create Basis Function Suffix

Generates the `_b##` suffix for HRF basis functions.

## Usage

``` r
basis_suffix(j, nb)
```

## Arguments

- j:

  Integer vector of basis indices (1-based).

- nb:

  Total number of basis functions.

## Value

Character vector of suffixes (e.g., `_b01`, `_b02`).

## Examples

``` r
basis_suffix(1:3, 5)
#> [1] "_b01" "_b02" "_b03"
basis_suffix(1:10, 10)
#>  [1] "_b01" "_b02" "_b03" "_b04" "_b05" "_b06" "_b07" "_b08" "_b09" "_b10"
```
