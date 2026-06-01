# Extension Registry for External HRF Specifications and Parametric Bases

This file provides an extension mechanism for packages to register their
own HRF specification types and parametric basis classes with
fmridesign. The registries are queried by the term-naming pipeline and
the design-metadata layer so user-defined types integrate without
requiring edits to package internals.

## Usage

``` r
.fmridesign_extensions
```

## Format

An object of class `environment` of length 0.

## Value

An environment used internally as a registry.

## Details

Internal registry environment for external HRF specs

Holds registration data for external HRF specification classes.
