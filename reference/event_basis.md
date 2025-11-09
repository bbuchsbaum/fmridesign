# Create an event set from a ParametricBasis object.

This is a user-facing wrapper around the internal `event()` constructor,
specifically for creating event sequences modulated by a basis set.

## Usage

``` r
event_basis(
  basis,
  name = NULL,
  onsets,
  blockids = 1,
  durations = 0,
  subset = NULL
)
```

## Arguments

- basis:

  A `ParametricBasis` object (e.g., from `BSpline`, `PolynomialBasis`).

- name:

  Optional name for the event variable. If NULL, uses `basis$name`.

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

## Examples

``` r
basis <- BSpline(1:21, 3)
onsets <- seq(0, 20, length.out = 21)
blockids <- rep(1, length(onsets))
ebasis <- event_basis(basis, onsets=onsets, blockids=blockids)
print(ebasis)
#> 
#> ── Event Sequence: bs_. ────────────────────────────────────────────────────────
#> * Type: Continuous
#> * Columns: 01, 02, 03
#> * Events: 21
#> 
#> ── Timing ──
#> 
#> * Onset Range: 0.00 - 20.00 sec
#> * Duration Range: 0.00 - 0.00 sec
#> 
#> ── Blocks ──
#> 
#> * Number of Blocks: 1
#> * Block IDs: 1
levels(ebasis)
#> [1] "01" "02" "03"
```
