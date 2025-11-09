# Get All External HRF Function Names

Returns all function names that should be recognized in formulas from
registered external packages.

## Usage

``` r
get_all_external_hrf_functions()
```

## Value

Character vector of function names

## Examples

``` r
register_hrfspec_extension(
  spec_class = "demo_hrfspec",
  package = "demoPkg",
  formula_functions = "demo_hrf"
)
get_all_external_hrf_functions()
#> [1] "demo_hrf"
```
