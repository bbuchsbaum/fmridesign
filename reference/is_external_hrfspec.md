# Check if a Class is a Registered External HRF Specification

Check if a Class is a Registered External HRF Specification

## Usage

``` r
is_external_hrfspec(x)
```

## Arguments

- x:

  An object or character string class name

## Value

Logical indicating if the class is registered as an external HRF spec

## Examples

``` r
register_hrfspec_extension(
  spec_class = "demo_hrfspec",
  package = "demoPkg"
)
is_external_hrfspec("demo_hrfspec")
#> [1] TRUE
```
