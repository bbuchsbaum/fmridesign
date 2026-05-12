# Register a Parametric Basis Class

Register a basis class so that the term-naming pipeline produces a
well-formed term tag (`<prefix>_<varname>`) and so that design-matrix
metadata identifies columns built from this basis as `parametric`.
Built-in bases (`Poly`, `BSpline`, `Scale`, `Standardized`,
`ScaleWithin`, `RobustScale`, `Ident`) are registered automatically.

## Usage

``` r
register_basis(
  class_name,
  prefix = NULL,
  modulation = c("parametric", "amplitude"),
  description = NULL
)
```

## Arguments

- class_name:

  Character string naming the S3 class extending `ParametricBasis`.

- prefix:

  Character string used as the term-tag prefix for terms whose sole
  variable is `class_name(var, ...)`. Use `NULL` (or omit) if the basis
  should not contribute a tag prefix (e.g., `Ident`).

- modulation:

  Character string describing the modulation kind for columns generated
  by this basis. Defaults to `"parametric"`. Use `"amplitude"` for an
  unmodulated identity-style basis.

- description:

  Optional human-readable description.

## Value

Invisibly returns the registration entry.

## Examples

``` r
# \donttest{
# Register a custom orthogonal-polynomial basis with prefix "ortho"
register_basis("OrthoPoly", prefix = "ortho")
# }
```
