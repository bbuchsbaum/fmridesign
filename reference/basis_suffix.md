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
if (FALSE) { # \dontrun{
basis_suffix(1:3, 5)
basis_suffix(1:10, 10)
} # }
```
