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
