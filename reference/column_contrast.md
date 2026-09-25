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

Weights are assigned as +1/nA to the nA columns matched by `pattern_A`
and -1/nB to the nB columns matched by `pattern_B`, so the contrast
compares the average effect of the 'A' columns with the average of the
'B' columns (or with baseline if `pattern_B` is NULL) and sums to zero
when both groups are present. A contrast is evaluated within the single
term it is attached to (via `hrf(..., contrasts = )`); only that term's
columns are candidates.

**What the patterns are matched against.** Each pattern is matched, with
[`grepl()`](https://rdrr.io/r/base/grep.html) semantics, against two
names for every column of the term:

1.  the *design-matrix column name*, exactly as shown by
    `colnames(design_matrix(model))`: `term_tag_condition_tag`, plus a
    `_b##` basis suffix for multi-basis HRFs (e.g. `cond_cond.A`,
    `task_cond_task.face_cond.A`, `cond_cond.A_b02`). See
    [`event_model()`](https://bbuchsbaum.github.io/fmridesign/reference/event_model.md)
    for the naming scheme and how the term tag is chosen;

2.  the *term-level condition name*, i.e. the same name without the
    `term_tag_` prefix (e.g. `cond.A`, `cond.A_b02`), as returned by
    `conditions(term, expand_basis = TRUE)`. This is accepted for
    backward compatibility.

The design-matrix names take precedence: a pattern that matches at least
one design-matrix column name selects exactly those columns, and the
term-level names are consulted only when it matches none. Both routes
therefore refer to the same columns (`"^cond_cond\\.A$"` and
`"^cond\\.A$"` select the same column). If a pattern matches in both
namespaces but selects *different* columns (for example, an unanchored
`"h"` against `hf_task.face` and `hf_task.house`, where only
`task.house` contains "h" at term level), the pattern is ambiguous and
an error is raised; anchor it on the design-matrix names
(`"^hf_task\\.house$"`). If a pattern matches nothing in either
namespace a warning lists the available column names, and an error
follows if neither pattern selected any column.

Use standard R regex syntax for the patterns. Remember to escape special
characters (e.g., `\\[`, `\\.`, `\\*`), and anchor patterns with `^` and
`$` to avoid accidental partial matches.

## See also

[`event_model()`](https://bbuchsbaum.github.io/fmridesign/reference/event_model.md)
for the column naming scheme,
[`pair_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/pair_contrast.md)
with `basis = ` for basis-restricted condition contrasts.

## Examples

``` r
des <- data.frame(
  onset = seq(0, 70, by = 10),
  run = 1,
  cond = factor(rep(c("A", "B"), 4))
)
sframe <- fmrihrf::sampling_frame(blocklens = 50, TR = 2)

# Patterns written against the design-matrix column names
# (colnames are "cond_cond.A", "cond_cond.B")
cset <- contrast_set(
  column_contrast(pattern_A = "^cond_cond\\.A$",
                  pattern_B = "^cond_cond\\.B$", name = "A_vs_B")
)
emod <- event_model(onset ~ hrf(cond, contrasts = cset),
                    data = des, block = ~run, sampling_frame = sframe)
colnames(design_matrix(emod))
#> [1] "cond_cond.A" "cond_cond.B"
contrast_weights(emod)[["cond#A_vs_B"]]$offset_weights
#>             A_vs_B
#> cond_cond.A      1
#> cond_cond.B     -1

# Multi-basis HRF: select only the first basis function (_b01) of each
# condition. Column names are "cond_cond.A_b01", "cond_cond.A_b02", ...
cset_mb <- contrast_set(
  column_contrast(pattern_A = "^cond_cond\\.A_b01$",
                  pattern_B = "^cond_cond\\.B_b01$", name = "A_vs_B_b01"),
  column_contrast(pattern_A = "_b01$", name = "canonical_mean")
)
emod_mb <- event_model(onset ~ hrf(cond, basis = "spmg3", contrasts = cset_mb),
                       data = des, block = ~run, sampling_frame = sframe)
colnames(design_matrix(emod_mb))
#> [1] "cond_cond.A_b01" "cond_cond.A_b02" "cond_cond.A_b03" "cond_cond.B_b01"
#> [5] "cond_cond.B_b02" "cond_cond.B_b03"
contrast_weights(emod_mb)[["cond#A_vs_B_b01"]]$offset_weights
#>                 A_vs_B_b01
#> cond_cond.A_b01          1
#> cond_cond.A_b02          0
#> cond_cond.A_b03          0
#> cond_cond.B_b01         -1
#> cond_cond.B_b02          0
#> cond_cond.B_b03          0

# Legacy term-level patterns select the same columns
cc_legacy <- column_contrast(pattern_A = "^cond\\.A_b01$",
                             pattern_B = "^cond\\.B_b01$", name = "legacy")
```
