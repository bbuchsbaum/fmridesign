# Map Display and Canonical Condition Names

For an `event_model` the result also has `term` and `column_name`: the
design-matrix column of each condition, found by its canonical name
(columns are `<term tag>_<canonical>`). A condition with no column (an
empty cell when `drop.empty = FALSE`, or a multi-basis term when
`expand_basis = FALSE`) has `column_name = NA`.

## Usage

``` r
# S3 method for class 'covariate_term'
condition_map(x, drop.empty = TRUE, expand_basis = FALSE, ...)

# S3 method for class 'covariate_convolved_term'
condition_map(x, drop.empty = TRUE, expand_basis = FALSE, ...)

condition_map(x, drop.empty = TRUE, expand_basis = FALSE, ...)

# S3 method for class 'feature_term'
condition_map(x, drop.empty = TRUE, expand_basis = FALSE, ...)
```

## Arguments

- x:

  The object to inspect.

- drop.empty:

  Logical whether to drop empty conditions (default: TRUE).

- expand_basis:

  Logical whether to expand basis functions (default: FALSE).

- ...:

  Additional arguments.

## Value

A tibble mapping display names to canonical names.

## Examples

``` r
term <- event_term(
  list(
    category = factor(c("face", "scene", "face")),
    attention = factor(c("attend", "attend", "ignore"))
  ),
  onsets = c(0, 10, 20),
  blockids = c(1, 1, 1)
)
condition_map(term)
#> # A tibble: 3 × 2
#>   display      canonical                      
#>   <chr>        <chr>                          
#> 1 face:attend  category.face_attention.attend 
#> 2 scene:attend category.scene_attention.attend
#> 3 face:ignore  category.face_attention.ignore 
```
