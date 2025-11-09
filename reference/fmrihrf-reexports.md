# fmrihrf reexports

Re-exported functions from the fmrihrf package for convenience. See the
upstream fmrihrf documentation for details on usage and return values of
each function.

These functions are re-exported from the fmrihrf package. Note: When
both packages are loaded, R will show masking warnings. This is expected
and harmless - the functions work identically.

## Usage

``` r
onsets(x, ...)

durations(x, ...)

blockids(x, ...)

nbasis(x, ...)
```

## Value

See the corresponding fmrihrf function documentation.

## Details

The following generics are re-exported:

- [`onsets`](https://bbuchsbaum.github.io/fmrihrf/reference/onsets.html):
  Extract onset times from event objects

- [`durations`](https://bbuchsbaum.github.io/fmrihrf/reference/durations.html):
  Extract durations from event objects

- [`blockids`](https://bbuchsbaum.github.io/fmrihrf/reference/blockids.html):
  Extract block identifiers

- [`nbasis`](https://bbuchsbaum.github.io/fmrihrf/reference/nbasis.html):
  Get number of basis functions

fmridesign adds S3 methods for these generics to work with:

- event_term objects

- convolved_term objects

- event_model objects

## Examples

``` r
# Create an event term
term <- event_term(
  list(condition = factor(c("A", "B", "A"))),
  onsets = c(0, 10, 20),
  blockids = c(1, 1, 1),
  durations = c(2, 2, 2)
)

# Extract onset times
onsets(term)
#> [1]  0 10 20

# Extract durations
durations(term)
#> [1] 2 2 2

# Extract block IDs
blockids(term)
#> [1] 1 1 1
```
