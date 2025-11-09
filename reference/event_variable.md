# Create a continuous event sequence from a numeric vector.

This is a user-facing wrapper around the internal `event()` constructor,
specifically for creating continuous event sequences from numeric
vectors.

## Usage

``` r
event_variable(vec, name, onsets, blockids = 1, durations = 0, subset = NULL)
```

## Arguments

- vec:

  Numeric vector representing continuous event values.

- name:

  Name of the event variable.

- onsets:

  Numeric vector of event onsets (seconds).

- blockids:

  Numeric vector of block IDs.

- durations:

  Numeric vector of event durations (seconds), or a scalar.

- subset:

  Optional logical vector indicating which events to keep. If provided,
  the vector must match `onsets` in length and contain no `NA` values.

## Value

An S3 object of class `event` and `event_seq`.

## See also

[`event_factor`](https://bbuchsbaum.github.io/fmridesign/reference/event_factor.md)

## Examples

``` r
ev_onsets <- seq(1, 100, length.out = 6)
evar <- event_variable(c(1, 2, 3, 4, 5, 6), "example_var", 
                       onsets = ev_onsets, blockids = rep(1, length(ev_onsets)))
print(evar)
#> 
#> ── Event Sequence: example_var ─────────────────────────────────────────────────
#> * Type: Continuous
#> * Columns: example_var
#> * Events: 6
#> 
#> ── Timing ──
#> 
#> * Onset Range: 1.00 - 100.00 sec
#> * Duration Range: 0.00 - 0.00 sec
#> 
#> ── Blocks ──
#> 
#> * Number of Blocks: 1
#> * Block IDs: 1
#> 
#> ── Values ──
#> 
#> * Value Range: 1.00 - 6.00
is_continuous(evar)
#> [1] TRUE
```
