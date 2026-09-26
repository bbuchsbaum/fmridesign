# Extract shortnames

Superseded by `conditions(x, style = "display")`.

## Usage

``` r
# S3 method for class 'covariate_term'
shortnames(x, ...)

# S3 method for class 'covariate_convolved_term'
shortnames(x, ...)

shortnames(x, ...)

# S3 method for class 'event_model'
shortnames(x, drop.empty = TRUE, ...)

# S3 method for class 'convolved_term'
shortnames(x, ...)

# S3 method for class 'event_seq'
shortnames(x, ...)

# S3 method for class 'feature_term'
shortnames(x, ...)
```

## Arguments

- x:

  The object.

- ...:

  Additional arguments.

- drop.empty:

  Logical; leave out cells with no events (default `TRUE`).

## Value

Character vector of short names.

## Details

Short names give the factor levels only, joined with `:` for
interactions (for example `A:x`). See
[`longnames()`](https://bbuchsbaum.github.io/fmridesign/reference/longnames.md)
for the qualified names and how they relate to design-matrix columns.

## Examples

``` r
# Create a simple event term with one condition factor
term <- event_term(
  list(condition = factor(c("A", "B", "A"))),
  onsets = c(0, 10, 20),
  blockids = c(1, 1, 1)
)
shortnames(term)  # Returns: "A" "B"
#> [1] "A" "B"

# Create event term with multiple factors
term2 <- event_term(
  list(
    category = factor(c("face", "scene", "face")),
    attention = factor(c("attend", "attend", "ignore"))
  ),
  onsets = c(0, 10, 20),
  blockids = c(1, 1, 1)
)
shortnames(term2)  # Returns: "face:attend" "scene:attend" "face:ignore"
#> [1] "face:attend"  "scene:attend" "face:ignore" 
```
