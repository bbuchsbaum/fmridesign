# Accessor: column metadata for a design matrix

Returns the `col_metadata` tibble attached to a design matrix produced
by
[`event_model()`](https://bbuchsbaum.github.io/fmridesign/reference/event_model.md).
If the attribute is missing (e.g., on a manually constructed matrix)
returns `NULL`.

## Usage

``` r
design_meta(x)
```

## Arguments

- x:

  A design matrix or any object with a
  [`design_matrix()`](https://bbuchsbaum.github.io/fmridesign/reference/design_matrix.md)
  method.

## Value

A tibble, or `NULL`.

## Examples

``` r
des <- data.frame(
  onset = c(0, 10, 20, 30),
  run = 1,
  cond = factor(c("A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
emod <- event_model(onset ~ hrf(cond), data = des,
                    block = ~run, sampling_frame = sframe)
design_meta(emod)
#> # A tibble: 2 × 15
#>     col name   term_tag term_index condition   run role  model_source basis_name
#>   <int> <chr>  <chr>         <int> <chr>     <int> <chr> <chr>        <chr>     
#> 1     1 cond_… cond              1 cond.A       NA task  event        HRF       
#> 2     2 cond_… cond              1 cond.B       NA task  event        HRF       
#> # ℹ 6 more variables: basis_ix <int>, basis_total <int>, basis_label <chr>,
#> #   is_block_diagonal <lgl>, modulation_type <chr>, modulation_id <chr>
```
