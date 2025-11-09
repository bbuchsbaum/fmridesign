# One Against All Contrast

Construct contrasts comparing each factor level against the average of
the other levels.

## Usage

``` r
one_against_all_contrast(levels, facname, where = NULL)
```

## Arguments

- levels:

  A vector of factor levels to be compared.

- facname:

  A character string specifying the name of the factor containing the
  supplied levels.

- where:

  An optional formula specifying the subset over which the contrast is
  computed.

## Value

A contrast_set object containing contrasts comparing each factor level
against the average of the other levels.

## Examples

``` r
fac <- factor(rep(c("A", "B", "C"), 2))
con <- one_against_all_contrast(levels(fac), "fac")
```
