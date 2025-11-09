# Get the HRF Function Name for External Specifications

Returns the function name(s) that should be recognized in formulas for a
given external HRF specification class.

## Usage

``` r
get_external_hrfspec_functions(spec_class)
```

## Arguments

- spec_class:

  Character string naming the class

## Value

Character vector of function names, or NULL if not registered

## Examples

``` r
register_hrfspec_extension(
  spec_class = "demo_hrfspec",
  package = "demoPkg",
  formula_functions = c("demo_hrf", "demo_trialwise")
)
get_external_hrfspec_functions("demo_hrfspec")
#> [1] "demo_hrf"       "demo_trialwise"
```
