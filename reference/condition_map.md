# Map Display and Canonical Condition Names

Map Display and Canonical Condition Names

## Usage

``` r
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
#> # A tibble: 4 × 2
#>   display      canonical                      
#>   <chr>        <chr>                          
#> 1 face:attend  category.face_attention.attend 
#> 2 scene:attend category.scene_attention.attend
#> 3 face:ignore  category.face_attention.ignore 
#> 4 scene:ignore category.scene_attention.ignore
```
