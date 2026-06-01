# Internal registry environment for parametric basis classes

Holds registration data for parametric basis classes that participate in
the term-naming and column-metadata pipelines (e.g., `Poly`, `BSpline`,
`Scale`). Built-in classes are registered automatically at package load.
External packages can register their own basis classes via
[`register_basis()`](https://bbuchsbaum.github.io/fmridesign/reference/register_basis.md).

## Usage

``` r
.fmridesign_basis_registry
```

## Format

An object of class `environment` of length 7.

## Value

An environment used internally as a registry.
