# Sanitize Strings for Use in R Names

Wraps `make.names` but allows control over dot replacement.

## Usage

``` r
sanitize(x, allow_dot = TRUE)
```

## Arguments

- x:

  A character vector.

- allow_dot:

  Logical, if `FALSE`, dots (`.`) are replaced with underscores (`_`).

## Value

A sanitized character vector.

## Examples

``` r
if (FALSE) { # \dontrun{
sanitize("a.b c")
sanitize("a.b c", allow_dot = FALSE)
} # }
```
