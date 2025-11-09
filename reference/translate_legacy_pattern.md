# Translate legacy contrast regex patterns

Convert older column-naming patterns to the current naming scheme.

## Usage

``` r
translate_legacy_pattern(pattern)
```

## Arguments

- pattern:

  Character string with the legacy regex.

## Value

Updated regex string.

## Examples

``` r
if (FALSE) { # \dontrun{
# Convert old bracket notation to dot notation
translate_legacy_pattern("condition[A]") # Returns "condition.A"

# Convert basis notation
translate_legacy_pattern("term:basis[2]") # Returns "term_b2"
} # }
```
