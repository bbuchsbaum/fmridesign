# Interaction Contrast

Create an interaction contrast specification

## Usage

``` r
interaction_contrast(A, name, where = NULL)
```

## Arguments

- A:

  A formula specifying the interaction contrast

- name:

  The name of the contrast

- where:

  An optional formula specifying the subset over which the contrast is
  computed.

## Value

An interaction_contrast_spec object containing the specification for
generating interaction contrast weights

## See also

[`oneway_contrast`](https://bbuchsbaum.github.io/fmridesign/reference/oneway_contrast.md)
for main effects,
[`pair_contrast`](https://bbuchsbaum.github.io/fmridesign/reference/pair_contrast.md)
for pairwise comparisons

## Examples

``` r
# Create an interaction contrast for factors A and B
con <- interaction_contrast(~ A * B, name = "A_by_B")

# Create an interaction contrast with a 'where' clause
con <- interaction_contrast(~ A * B, name = "A_by_B",
                          where = ~ block == 1)
```
