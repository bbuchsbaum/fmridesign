# Create a categorical event sequence from a factor.

This is a user-facing wrapper around the internal `event()` constructor,
specifically for creating categorical event sequences from factors or
characters.

## Usage

``` r
event_factor(fac, name, onsets, blockids = 1, durations = 0, subset = NULL)
```

## Arguments

- fac:

  A factor or something coercible to a factor.

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

  Column names are sanitized using `.sanitizeName()` if provided. If
  column names are missing or not unique, deterministic feature suffixes
  (`f01`, `f02`, ...) are generated instead. The resulting names are
  returned by [`levels()`](https://rdrr.io/r/base/levels.html) for the
  event object.

## Value

An S3 object of class `event` and `event_seq`.

## See also

[`event_model`](https://bbuchsbaum.github.io/fmridesign/reference/event_model.md),
[`event_variable`](https://bbuchsbaum.github.io/fmridesign/reference/event_variable.md),
[`event_matrix`](https://bbuchsbaum.github.io/fmridesign/reference/event_matrix.md),
[`event_basis`](https://bbuchsbaum.github.io/fmridesign/reference/event_basis.md)

## Examples

``` r
ef_onsets <- seq(1, 100, length.out = 6)
efac <- event_factor(factor(c("a", "b", "c", "a", "b", "c")), "abc", 
        onsets = ef_onsets, blockids = rep(1, length(ef_onsets)))
print(efac)
#> 
#> ── Event Sequence: abc ─────────────────────────────────────────────────────────
#> * Type: Categorical
#> * Levels: a, b, c
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
levels(efac)
#> [1] "a" "b" "c"
```
