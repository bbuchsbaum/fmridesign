# Extract longnames

Superseded by `conditions(x, style = "canonical")`.

## Usage

``` r
# S3 method for class 'covariate_term'
longnames(x, ...)

# S3 method for class 'covariate_convolved_term'
longnames(x, ...)

longnames(x, ...)

# S3 method for class 'event_model'
longnames(x, drop.empty = TRUE, expand_basis = FALSE, ...)

# S3 method for class 'convolved_term'
longnames(x, ...)

# S3 method for class 'event_seq'
longnames(x, ...)

# S3 method for class 'feature_term'
longnames(x, ...)
```

## Arguments

- x:

  The object.

- ...:

  Additional arguments.

- drop.empty:

  Logical; leave out cells with no events (default `TRUE`).

- expand_basis:

  Logical; add a basis suffix for multi-basis HRFs (default `FALSE`).

## Value

Character vector of long (fully qualified) names.

## Details

Long names are the canonical condition names: each factor level is
written `variable.level`, and the factors of an interaction are joined
with `_` (for example `condition.A_attn.x`). With `expand_basis = TRUE`
a `_bNN` suffix is added for each HRF basis function.

They are not the design-matrix column names. A term's columns are
`paste0(term_tag, "_", longnames(term, expand_basis = TRUE))`, so the
column for level `A` of `hrf(condition)` is `condition_condition.A`, and
`longnames(term)` returns `condition.A`. Use
[`columns()`](https://bbuchsbaum.github.io/fmridesign/reference/columns.md)
(or `colnames(design_matrix(x))`) for the column names, and
[`condition_map()`](https://bbuchsbaum.github.io/fmridesign/reference/condition_map.md)
for a table that lines the two up.

With `drop.empty = TRUE` (the default) a cell of an interaction that has
no events is left out, as it is from the design matrix, so the names
line up one-to-one with the term's columns. `drop.empty = FALSE` lists
the full grid of factor levels, which is what
[`conditions()`](https://bbuchsbaum.github.io/fmridesign/reference/conditions.md)
always returns.

Methods exist for `event_term`, `event_model` (all terms in order),
`convolved_term`, `feature_term`, covariate terms, and bare events from
[`event_factor()`](https://bbuchsbaum.github.io/fmridesign/reference/event_factor.md)
and friends (`event_seq`), which are named as the single-variable
`event_term` built from them would be.

## Examples

``` r
# Create a simple event term with one condition factor
term <- event_term(
  list(condition = factor(c("A", "B", "A"))),
  onsets = c(0, 10, 20),
  blockids = c(1, 1, 1)
)
longnames(term)  # Returns: "condition.A" "condition.B"
#> [1] "condition.A" "condition.B"

# Create event term with multiple factors
term2 <- event_term(
  list(
    category = factor(c("face", "scene", "face")),
    attention = factor(c("attend", "attend", "ignore"))
  ),
  onsets = c(0, 10, 20),
  blockids = c(1, 1, 1)
)
longnames(term2)
#> [1] "category.face_attention.attend"  "category.scene_attention.attend"
#> [3] "category.face_attention.ignore" 
# Returns: "category.face_attention.attend"
#          "category.scene_attention.attend"
#          "category.face_attention.ignore"

# Long names versus design-matrix column names
sf <- fmrihrf::sampling_frame(blocklens = 30, TR = 2)
des <- data.frame(onset = c(0, 10, 20), run = 1,
                  condition = factor(c("A", "B", "A")))
em <- event_model(onset ~ hrf(condition), data = des, block = ~run,
                  sampling_frame = sf)
longnames(em)  # "condition.A" "condition.B"
#> [1] "condition.A" "condition.B"
columns(em)    # "condition_condition.A" "condition_condition.B"
#> [1] "condition_condition.A" "condition_condition.B"
```
