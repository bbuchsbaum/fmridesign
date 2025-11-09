# Sliding-Window Contrasts (Disjoint)

Generate a set of A-vs-B contrasts where A and B are adjacent, equally
sized and disjoint windows over an ordered factor. For window size k,
contrast i compares A = `levels[i:(i+k-1)]` against B =
`levels[(i+k):(i+2k-1)]`. This yields `length(levels) - 2*k + 1`
contrasts that detect local changes across the sequence without
overlapping masks.

## Usage

``` r
sliding_window_contrasts(
  levels,
  facname,
  window_size = 2,
  where = NULL,
  name_prefix = "win"
)
```

## Arguments

- levels:

  Character vector of ordered factor levels.

- facname:

  Name of the factor (column in the design).

- window_size:

  Positive integer window size (default 2).

- where:

  Optional formula to subset events used when computing weights.

- name_prefix:

  Prefix for generated contrast names (default "win").

## Value

A `contrast_set` of `pair_contrast` specifications.

## Examples

``` r
# For levels 1..5, generate 2 disjoint adjacent-window contrasts (k=2)
sliding_window_contrasts(as.character(1:5), facname = "intensity", window_size = 2)
#> 
#> === Contrast Set ===
#> 
#>  Overview:
#>   * Number of contrasts: 2 
#>   * Types of contrasts:
#>     - pair_contrast_spec : 2 
#> 
#>   Individual Contrasts:
#> 
#> [1] win_1-2_vs_3-4 (pair_contrast_spec)
#>     Formula: ~intensity %in% c("1", "2") vs  ~intensity %in% c("3", "4")
#> 
#> [2] win_2-3_vs_4-5 (pair_contrast_spec)
#>     Formula: ~intensity %in% c("2", "3") vs  ~intensity %in% c("4", "5")
#> 

# For k=3 with 7 levels (disjoint windows):
# A=[1,2,3] vs B=[4,5,6], then A=[2,3,4] vs B=[5,6,7]
sliding_window_contrasts(LETTERS[1:7], facname = "difficulty", window_size = 3)
#> 
#> === Contrast Set ===
#> 
#>  Overview:
#>   * Number of contrasts: 2 
#>   * Types of contrasts:
#>     - pair_contrast_spec : 2 
#> 
#>   Individual Contrasts:
#> 
#> [1] win_A-B-C_vs_D-E-F (pair_contrast_spec)
#>     Formula: ~difficulty %in% c("A", "B", "C") vs  ~difficulty %in% c("D", "E", "F")
#> 
#> [2] win_B-C-D_vs_E-F-G (pair_contrast_spec)
#>     Formula: ~difficulty %in% c("B", "C", "D") vs  ~difficulty %in% c("E", "F", "G")
#> 
```
