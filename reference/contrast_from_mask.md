# Package a base mask into a `contrast` object

Drives the unified post-processing pipeline: expands the base mask
across basis functions (when applicable), applies basis filtering,
attaches the standard `contrast` list class, and returns an object
compatible with downstream
[`contrast_weights.event_model()`](https://bbuchsbaum.github.io/fmridesign/reference/contrast_weights.md).

## Usage

``` r
contrast_from_mask(mask, spec, term, classes = character())
```

## Arguments

- mask:

  Output of
  [`contrast_mask()`](https://bbuchsbaum.github.io/fmridesign/reference/contrast_mask.md)
  (or compatible list).

- spec:

  The originating contrast spec (stored on the result).

- term:

  The `event_term` the contrast is being computed against.

- classes:

  Character vector of extra S3 classes prepended to the default
  `c("cell_contrast", "contrast", "list")`.

## Value

A `contrast` object.

## Details

Custom contrast types implement
[`contrast_mask()`](https://bbuchsbaum.github.io/fmridesign/reference/contrast_mask.md)
and call this driver.
