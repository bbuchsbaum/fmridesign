# Unit Contrast

Construct a contrast that sums to 1 and is used to define contrasts
against the baseline.

## Usage

``` r
unit_contrast(A, name, where = NULL)
```

## Arguments

- A:

  A formula selecting the cells to average. A logical expression (e.g.
  `~ cond == "A"`) selects the matching cells; a bare factor name (e.g.
  `~ cond`) selects every cell. For a multi-basis HRF the weights are
  repeated on every basis function of the selected cells.

- name:

  A character string specifying the name of the contrast.

- where:

  An optional formula specifying the subset of conditions to apply the
  contrast to.

## Value

A unit_contrast_spec object containing the contrast that sums to 1.

## Examples

``` r
# Test main effect of Face against baseline
con <- unit_contrast(~ Face, name="Main_face")

# Test main effect within specific blocks
con2 <- unit_contrast(~ Face, name="Face_early", where = ~ block <= 3)
```
