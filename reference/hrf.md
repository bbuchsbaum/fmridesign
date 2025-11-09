# hemodynamic regressor specification function for model formulas.

This function is to be used in formulas for fitting functions, e.g.
onsets ~ hrf(fac1,fac2) ... It captures the variables/expressions
provided and packages them with HRF/contrast information into an
`hrfspec` object, which is then processed by `event_model`.

## Usage

``` r
hrf(
  ...,
  basis = "spmg1",
  onsets = NULL,
  durations = NULL,
  prefix = NULL,
  subset = NULL,
  precision = 0.3,
  nbasis = 1,
  contrasts = NULL,
  id = NULL,
  name = NULL,
  lag = 0,
  summate = TRUE
)
```

## Arguments

- ...:

  One or more variable names (bare or character) or expressions
  involving variables present in the `data` argument of `event_model`.

- basis:

  the impulse response function or the name of a pre-supplied function,
  one of: "gamma", "spmg1", "spmg2", "spmg3", "bspline", "gaussian",
  "tent", "bs". Can also be an `HRF` object.

- onsets:

  optional onsets override. If missing, onsets will be taken from the
  LHS of the main model formula.

- durations:

  optional durations override. If missing, durations argument from
  `event_model` is used.

- prefix:

  a character string that is prepended to the variable names and used to
  identify the term. Can be used to disambiguate two `hrf` terms with
  the same variable(s) but different onsets or basis functions.

- subset:

  an expression indicating the subset of 'onsets' to keep.

- precision:

  sampling precision in seconds.

- nbasis:

  number of basis functions – only used for hemodynamic response
  functions (e.g. bspline) that take a variable number of bases.

- contrasts:

  one or more `contrast_spec` objects created with the `contrast`,
  `pair_contrast` etc. functions. Must be NULL, a single contrast spec,
  or a *named* list of contrast specs.

- id:

  a unique `character` identifier used to refer to term, otherwise will
  be determined from variable names.

- name:

  Optional human-readable name for the term.

- lag:

  a temporal offset in seconds which is added to onset before
  convolution

- summate:

  whether impulse amplitudes sum up when duration is greater than 0.

## Value

an `hrfspec` instance

## Examples

``` r
## 'hrf' is typically used in the context of \code{formula}s passed to `event_model`.

# Simple model with one factor
form1 <- onsets ~ hrf(condition, basis="spmg1")

# Model with factor and continuous modulator, using default SPMG1 for both terms
form2 <- onsets ~ hrf(condition) + hrf(RT)

# Model with interaction term and SPMG3 basis
form3 <- onsets ~ hrf(condition, RT, basis="spmg3")

# Model with an expression and contrasts
library(rlang)
con1 <- pair_contrast(~ condition == "A", ~ condition == "B", name="AvB")
form4 <- onsets ~ hrf(condition, Poly(RT, 2), contrasts=con1)
```
