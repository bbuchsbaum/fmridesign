# Extract conditions from a design object

Extract conditions from a design object

## Usage

``` r
conditions(x, drop.empty = TRUE, expand_basis = FALSE, ...)

# S3 method for class 'event_term'
conditions(x, drop.empty = TRUE, expand_basis = FALSE, ...)
```

## Arguments

- x:

  The object to extract conditions from.

- drop.empty:

  Logical whether to drop conditions with no events (default: TRUE).

- expand_basis:

  Logical whether to expand basis functions (default: FALSE).

- ...:

  Additional arguments.

## Value

A character vector of condition names.

## Examples

``` r
term <- event_term(
  list(condition = factor(c("A", "B", "A"))),
  onsets = c(0, 10, 20),
  blockids = c(1, 1, 1)
)
conditions(term)
#> [1] "condition.A" "condition.B"
conditions(term, expand_basis = TRUE)
#> [1] "condition.A" "condition.B"
```
