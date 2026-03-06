# Map Display and Canonical Condition Names

Map display and canonical condition names.

## Usage

``` r
condition_map(x, drop.empty = TRUE, expand_basis = FALSE, ...)

# S3 method for class 'event_term'
condition_map(x, drop.empty = TRUE, expand_basis = FALSE, ...)

# S3 method for class 'event_model'
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

A tibble mapping display labels to canonical names. For event models,
the result also includes the term name and, when available, the
corresponding design-matrix column name.

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
#> # A tibble: 4 × 2
#>   display      canonical                      
#>   <chr>        <chr>                          
#> 1 face:attend  category.face_attention.attend 
#> 2 scene:attend category.scene_attention.attend
#> 3 face:ignore  category.face_attention.ignore 
#> 4 scene:ignore category.scene_attention.ignore
```
