# Create Feature Suffix

Generates the `f##` suffix for multi-column continuous events.

## Usage

``` r
feature_suffix(j, nf)
```

## Arguments

- j:

  Integer vector of feature indices (1-based).

- nf:

  Total number of features.

## Value

Character vector of suffixes (e.g., `f01`, `f02`).

## Examples

``` r
feature_suffix(1:3, 5)
#> [1] "f01" "f02" "f03"
```
