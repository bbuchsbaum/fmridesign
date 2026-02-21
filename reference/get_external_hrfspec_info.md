# Get Information About a Registered External HRF Specification

Get Information About a Registered External HRF Specification

## Usage

``` r
get_external_hrfspec_info(spec_class)
```

## Arguments

- spec_class:

  Character string naming the class

## Value

A list with registration information, or NULL if not registered

## Examples

``` r
register_hrfspec_extension(
  spec_class = "demo_hrfspec",
  package = "demoPkg",
  requires_external_processing = TRUE,
  formula_functions = "demo_hrf"
)
get_external_hrfspec_info("demo_hrfspec")
#> $spec_class
#> [1] "demo_hrfspec"
#> 
#> $package
#> [1] "demoPkg"
#> 
#> $convolved_class
#> NULL
#> 
#> $requires_external_processing
#> [1] TRUE
#> 
#> $formula_functions
#> [1] "demo_hrf"
#> 
#> $registered_at
#> [1] "2026-02-21 23:11:09 UTC"
#> 
```
