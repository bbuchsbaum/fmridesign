# Create a continuous event set from a matrix.

This is a user-facing wrapper around the internal `event()` constructor,
specifically for creating continuous event sequences from numeric
matrices.

## Usage

``` r
event_matrix(mat, name, onsets, blockids = 1, durations = 0, subset = NULL)
```

## Arguments

- mat:

  A numeric matrix of continuous event values (one row per event).

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

  If `mat` has column names and more than one column, those names are
  sanitized using `.sanitizeName()` before being stored. The sanitized
  column names are returned by
  [`levels()`](https://rdrr.io/r/base/levels.html) for the resulting
  event object.

## Value

An S3 object of class `event` and `event_seq`.

## Examples

``` r
mat <- matrix(rnorm(20), 10, 2, dimnames=list(NULL, c("Val1", "Val2")))
onsets <- seq(1, 100, length.out = 10)
durations <- rep(1, 10)
blockids <- rep(1, 10)
eset <- event_matrix(mat, "eset", onsets, blockids, durations)
print(eset)
#> 
#> ── Event Sequence: eset ────────────────────────────────────────────────────────
#> * Type: Continuous
#> * Columns: Val1, Val2
#> * Events: 10
#> 
#> ── Timing ──
#> 
#> * Onset Range: 1.00 - 100.00 sec
#> * Duration Range: 1.00 - 1.00 sec
#> 
#> ── Blocks ──
#> 
#> * Number of Blocks: 1
#> * Block IDs: 1
#> 
#> ── Values ──
#> 
#> * Value Range: -1.43 - 1.46
columns(eset) # Alias for levels
#> [1] "Val1" "Val2"
```
