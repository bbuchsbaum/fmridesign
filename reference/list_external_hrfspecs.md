# List All Registered External HRF Specifications

List All Registered External HRF Specifications

## Usage

``` r
list_external_hrfspecs()
```

## Value

A character vector of registered class names

## Examples

``` r
register_hrfspec_extension(
  spec_class = "demo_hrfspec",
  package = "demoPkg"
)
list_external_hrfspecs()
#> [1] "demo_hrfspec"
```
