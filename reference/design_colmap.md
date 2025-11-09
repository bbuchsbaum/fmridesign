# Column Metadata Map

Returns a tibble with one row per column of the design matrix and
structured metadata (term, condition, basis, role, run, etc.).

## Usage

``` r
design_colmap(x, ...)
```

## Arguments

- x:

  An object containing or producing a design matrix (e.g., event_model,
  baseline_model).

- ...:

  Additional arguments passed to methods.

## Value

A tibble with per-column metadata.
