# Column Contrast Specification

Define a contrast by directly targeting design matrix columns using
regex patterns. This is useful for contrasts involving continuous
variables or specific basis functions.

## Usage

``` r
column_contrast(pattern_A, pattern_B = NULL, name, where = NULL)
```

## Arguments

- pattern_A:

  A character string containing a regex pattern to identify the columns
  for the positive (+) part of the contrast.

- pattern_B:

  Optional character string containing a regex pattern for the negative
  (-) part (for A-B type contrasts). If NULL, creates a contrast testing
  the average of columns matching `pattern_A` against baseline (0).

- name:

  A character string name for the contrast (mandatory).

- where:

  Currently unused for column_contrast, but kept for API consistency.

## Value

A `column_contrast_spec` object containing the specification.

## Details

This contrast type operates by finding design matrix columns whose names
match the provided patterns (`pattern_A`, `pattern_B`). It calculates
weights such that the average effect of the 'A' columns is compared to
the average effect of the 'B' columns (or baseline if `pattern_B` is
NULL). Weights are assigned as +1/nA for 'A' columns and -1/nB for 'B'
columns, ensuring the contrast sums to zero if both A and B groups are
present.

Use standard R regex syntax for the patterns. Remember to escape special
characters (e.g., `\\[`, `\\.`, `\\*`).

## Examples

``` r
# Test the main effect of a continuous modulator 'RT'
# Assumes RT is a column name, e.g., from columns(Scale(RT))
cc1 <- column_contrast(pattern_A = "^z_RT$", name = "Main_RT")

# Compare Condition.A vs Condition.B for the 'RT' modulator effect
# Assumes condition names like "Condition.A_z_RT", "Condition.B_z_RT"
cc2 <- column_contrast(pattern_A = "^Condition\\.A_z_RT$",
                       pattern_B = "^Condition\\.B_z_RT$",
                       name = "CondA_vs_CondB_for_RT")

# Test a specific basis function (e.g., basis spline #3)
# Assumes column names like "TermName_Condition.Tag_b03"
cc3 <- column_contrast(pattern_A = "_b03$", name = "Basis_3_Effect")
```
