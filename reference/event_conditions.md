# Retrieve per-event condition assignments

Retrieve per-event condition assignments

## Usage

``` r
event_conditions(x, drop.empty = FALSE, ...)
```

## Arguments

- x:

  The object of interest.

- drop.empty:

  Logical; drop unused levels when `TRUE`.

- ...:

  Additional arguments passed to methods.

## Value

Typically a factor aligned with the events of `x`.

## Examples

``` r
term <- event_term(
  list(condition = factor(c("A", "B", "A"))),
  onsets = c(0, 10, 20),
  blockids = c(1, 1, 1)
)
event_conditions(term)
#> [1] condition.A condition.B condition.A
#> Levels: condition.A condition.B
```
