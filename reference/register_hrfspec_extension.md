# Register an External HRF Specification Type

Register a new HRF specification class that can be used in event models.
This allows external packages to extend fmridesign with their own HRF
types.

## Usage

``` r
register_hrfspec_extension(
  spec_class,
  package,
  convolved_class = NULL,
  requires_external_processing = FALSE,
  formula_functions = NULL
)
```

## Arguments

- spec_class:

  Character string naming the class to register

- package:

  Character string naming the package providing the class

- convolved_class:

  Optional character string naming the associated convolved term class

- requires_external_processing:

  Logical indicating if this spec should be skipped during standard
  convolution (e.g., for AFNI terms that are processed externally)

- formula_functions:

  Optional character vector of function names that should be recognised
  in formulas and mapped to this HRF specification class.

## Value

Invisible NULL

## Examples

``` r
if (FALSE) { # \dontrun{
# In an external package's .onLoad function:
register_hrfspec_extension(
  spec_class = "afni_hrfspec",
  package = "afnireg",
  convolved_class = "afni_hrf_convolved_term",
  requires_external_processing = TRUE,
  formula_functions = "afni_hrf"
)
} # }
```
