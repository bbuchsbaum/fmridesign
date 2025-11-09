# Check if an Object Requires External Processing

Determines if an HRF specification or convolved term should be handled
by external tools rather than R's standard convolution.

## Usage

``` r
requires_external_processing(x)
```

## Arguments

- x:

  An object to check

## Value

Logical indicating if external processing is required

## Examples

``` r
register_hrfspec_extension(
  spec_class = "demo_hrfspec",
  package = "demoPkg",
  requires_external_processing = TRUE
)
requires_external_processing("demo_hrfspec")
#> [1] FALSE
```
