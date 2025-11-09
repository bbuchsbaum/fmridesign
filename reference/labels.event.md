# Get Formatted Labels for a Single Event

Returns a character vector of formatted labels for an event object,
using the `Variable[Level]` style for categorical events,
`Variable[Index]` for multi-column continuous events, or just `Variable`
for single continuous events. Useful for getting consistent labels for
individual event components. This is distinct from
[`levels()`](https://rdrr.io/r/base/levels.html) which returns the raw
level names or column names. Relies on the internal `.level_vector`
helper function.

## Usage

``` r
# S3 method for class 'event'
labels(object, ...)
```

## Arguments

- object:

  An object of class `event`.

- ...:

  Additional arguments (unused).

## Value

A character vector of formatted labels, or `character(0)` if not
applicable.

## Examples

``` r
fac <- factor(rep(c("A", "B"), 3))
onsets <- 1:6
ev_fac <- event_factor(fac, "Condition", onsets, blockids = rep(1, length(onsets)))
labels(ev_fac) # Should return c("Condition[A]", "Condition[B]")
#>              A              B 
#> "Condition[A]" "Condition[B]" 

vals <- 1:6
ev_num <- event_variable(vals, "Modulator", onsets, blockids = rep(1, length(onsets)))
labels(ev_num) # Should return "Modulator"
#> [1] "Modulator"
#> attr(,"orig_names")
#> [1] "Modulator"

mat <- matrix(1:12, 6, 2)
colnames(mat) <- c("C1", "C2")
ev_mat <- event_matrix(mat, "MatrixVar", onsets, blockids = rep(1, length(onsets)))
labels(ev_mat) # Should return c("MatrixVar[1]", "MatrixVar[2]") 
#>              C1              C2 
#> "MatrixVar[C1]" "MatrixVar[C2]" 
```
