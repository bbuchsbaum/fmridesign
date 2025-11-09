# Contrast Specification

Define a linear contrast using a formula expression.

## Usage

``` r
contrast(form, name, where = NULL)
```

## Arguments

- form:

  A formula describing the contrast.

- name:

  A character label for the contrast.

- where:

  An expression defining the subset over which the contrast is applied
  (default: NULL).

## Value

A list containing the contrast specification.

## Examples

``` r
# A minus B contrast
contrast(~ A - B, name="A_B")
#> contrast: A_B 
#>  A:  ~A - B 

# With subsetting
contrast(~ A - B, name="A_B_block1", where = ~ block == 1)
#> contrast: A_B_block1 
#>  A:  ~A - B 
#>  where:  ~block == 1 
```
