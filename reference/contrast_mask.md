# Build a base contrast mask

`contrast_mask()` is the extension point for new contrast types: it
returns a *base* contrast matrix with one row per base condition (no
basis expansion) and one column per contrast column. The unified driver
[`contrast_from_mask()`](https://bbuchsbaum.github.io/fmridesign/reference/contrast_from_mask.md)
handles basis expansion, basis filtering and packaging into a `contrast`
object compatible with
[`contrast_weights.event_model()`](https://bbuchsbaum.github.io/fmridesign/reference/contrast_weights.md).

## Usage

``` r
contrast_mask(x, term, ...)
```

## Arguments

- x:

  A `contrast_spec` object.

- term:

  An `event_term` against which the mask is computed.

- ...:

  Additional arguments passed to methods.

## Value

A list with elements:

- `weights` : numeric matrix, `n_base_conditions x n_contrast_cols`, row
  names = base condition names.

- `condnames` : character vector of base condition names.

## Details

To register a custom contrast type:

1.  Define a constructor that produces a list with class
    `c("my_spec", "contrast_spec", "list")`.

2.  Implement `contrast_mask.my_spec()` returning the base weights.

3.  Implement `contrast_weights.my_spec()` as a one-liner:

        contrast_weights.my_spec <- function(x, term, ...) {
          contrast_from_mask(contrast_mask(x, term, ...), x, term)
        }

The built-in spec classes (`pair_contrast_spec`, `oneway_contrast_spec`,
`poly_contrast_spec`, ...) implement
[`contrast_weights()`](https://bbuchsbaum.github.io/fmridesign/reference/contrast_weights.md)
directly, so they do not require `contrast_mask()` methods.
