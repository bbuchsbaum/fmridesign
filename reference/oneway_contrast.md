# One-way Contrast

Create a one-way contrast specification

## Usage

``` r
oneway_contrast(A, name, where = NULL, basis = NULL, basis_weights = NULL)
```

## Arguments

- A:

  A formula specifying the contrast

- name:

  The name of the contrast

- where:

  An optional formula specifying the subset over which the contrast is
  computed.

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

A oneway_contrast_spec object that can be used to generate contrast
weights

## See also

[`interaction_contrast`](https://bbuchsbaum.github.io/fmridesign/reference/interaction_contrast.md)
for testing interactions,
[`pair_contrast`](https://bbuchsbaum.github.io/fmridesign/reference/pair_contrast.md)
for pairwise comparisons

## Examples

``` r
# Create a one-way contrast for a factor 'basis'
con <- oneway_contrast(~ basis, name = "Main_basis")

# Create a one-way contrast with a 'where' clause
con <- oneway_contrast(~ basis, name = "Main_basis",
                      where = ~ block == 1)

# Test only first two basis functions
con <- oneway_contrast(~ condition, name = "Main_early", basis = 1:2)
```
