# Group column indices by condition for a term/basis pair

Group column indices by condition for a term/basis pair

## Usage

``` r
column_groups_by_condition(term, basis, sampling_frame)
```

## Arguments

- term:

  An `event_term`.

- basis:

  An `HRF` object.

- sampling_frame:

  Unused; present for future compatibility.

## Value

A named list mapping condition tags to integer indices.

## Examples

``` r
term <- event_term(
  list(condition = factor(c("A", "B"))),
  onsets = c(0, 5),
  blockids = c(1, 1)
)
column_groups_by_condition(term, fmrihrf::HRF_SPMG1, NULL)
#> $condition.A
#> [1] 1
#> 
#> $condition.B
#> [1] 2
#> 
```
