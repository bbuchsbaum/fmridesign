# Retrieve canonical event information

Retrieve canonical event information

## Usage

``` r
events(x, drop.empty = FALSE, ...)
```

## Arguments

- x:

  The object to summarise.

- drop.empty:

  Logical; whether to drop empty conditions in the resulting factor.

- ...:

  Additional arguments passed to methods.

## Value

A data.frame (or tibble) with onset, duration, block, and condition
columns.

## Examples

``` r
# Create an event term with condition factor
term <- event_term(
  list(condition = factor(c("A", "B", "A"))),
  onsets = c(0, 10, 20),
  blockids = c(1, 1, 1)
)

# Extract canonical event information
evt_info <- events(term)
print(evt_info)
#>   onset duration block   condition
#> 1     0        0     1 condition.A
#> 2    10        0     1 condition.B
#> 3    20        0     1 condition.A
```
