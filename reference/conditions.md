# Extract conditions from a design object

Extract conditions from a design object

## Usage

``` r
# S3 method for class 'baseline_model'
conditions(x, ...)

# S3 method for class 'covariate_term'
conditions(
  x,
  drop.empty = TRUE,
  expand_basis = FALSE,
  style = c("canonical", "display"),
  ...
)

# S3 method for class 'covariate_convolved_term'
conditions(
  x,
  drop.empty = TRUE,
  expand_basis = FALSE,
  style = c("canonical", "display"),
  ...
)

conditions(
  x,
  drop.empty = TRUE,
  expand_basis = FALSE,
  style = c("canonical", "display"),
  ...
)

# S3 method for class 'event_term'
conditions(
  x,
  drop.empty = TRUE,
  expand_basis = FALSE,
  style = c("canonical", "display"),
  ...
)

# S3 method for class 'convolved_term'
conditions(x, ...)

# S3 method for class 'feature_term'
conditions(
  x,
  drop.empty = TRUE,
  expand_basis = FALSE,
  style = c("canonical", "display"),
  ...
)
```

## Arguments

- x:

  The object to extract conditions from.

- ...:

  Additional arguments.

- drop.empty:

  Logical whether to drop conditions with no events (default: TRUE). The
  `event_term` and `event_model` methods ignore it and always return the
  full grid of factor levels, including cells with no events;
  [`longnames()`](https://bbuchsbaum.github.io/fmridesign/reference/longnames.md),
  [`shortnames()`](https://bbuchsbaum.github.io/fmridesign/reference/shortnames.md)
  and
  [`condition_map()`](https://bbuchsbaum.github.io/fmridesign/reference/condition_map.md)
  honour it.

- expand_basis:

  Logical whether to expand basis functions (default: FALSE).

- style:

  Naming style. `"canonical"` returns fully qualified internal names,
  while `"display"` returns shorter user-facing labels.

## Value

A character vector of condition names.

## Examples

``` r
term <- event_term(
  list(condition = factor(c("A", "B", "A"))),
  onsets = c(0, 10, 20),
  blockids = c(1, 1, 1)
)
conditions(term)
#> [1] "condition.A" "condition.B"
conditions(term, style = "display")
#> [1] "A" "B"
conditions(term, expand_basis = TRUE)
#> [1] "condition.A" "condition.B"
```
