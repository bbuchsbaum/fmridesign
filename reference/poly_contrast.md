# Polynomial Contrast

Create polynomial contrasts for testing trends across ordered factor
levels. This is particularly useful for analyzing factors with a natural
ordering (e.g., time, dose).

## Usage

``` r
poly_contrast(
  A,
  name,
  where = NULL,
  degree = 1,
  value_map = NULL,
  basis = NULL,
  basis_weights = NULL
)
```

## Arguments

- A:

  A formula specifying the ordered factor.

- name:

  A character string identifying the contrast.

- where:

  An optional formula for subsetting the data.

- degree:

  An integer specifying the degree of the polynomial (default: 1).

- value_map:

  An optional list mapping factor levels to numeric values.

- basis:

  NULL (default: use all basis functions), an integer vector specifying
  which basis function indices to include, or `"all"`. See
  [`pair_contrast`](https://bbuchsbaum.github.io/fmridesign/reference/pair_contrast.md)
  for details on basis filtering.

- basis_weights:

  NULL (default: equal weights), or a numeric vector of weights to apply
  to the selected basis functions. Must have the same length as `basis`
  selection and will be normalized to sum to 1. See
  [`pair_contrast`](https://bbuchsbaum.github.io/fmridesign/reference/pair_contrast.md)
  for details on basis weighting.

## Value

A poly_contrast_spec object containing the specification for generating
polynomial contrast weights.

## Details

The function creates orthogonal polynomial contrasts up to the specified
degree. These contrasts can test for linear, quadratic, cubic, and
higher-order trends in the data. The value_map parameter allows for
non-uniform spacing between levels.

## See also

[`oneway_contrast`](https://bbuchsbaum.github.io/fmridesign/reference/oneway_contrast.md)
for categorical contrasts,
[`interaction_contrast`](https://bbuchsbaum.github.io/fmridesign/reference/interaction_contrast.md)
for interaction effects

## Examples

``` r
# Linear trend across time points
pcon <- poly_contrast(~ time, name = "linear_time", degree = 1)

# Cubic trend with custom spacing
pcon <- poly_contrast(~ dose, name = "dose_cubic",
                     degree = 3,
                     value_map = list("low" = 0, "med" = 2, "high" = 5))

# Linear trend for only first basis function
pcon <- poly_contrast(~ dose, name = "dose_linear_basis1",
                     degree = 1, basis = 1)
```
