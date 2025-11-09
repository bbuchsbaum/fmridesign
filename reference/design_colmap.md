# Column Metadata Map

Returns a tibble with one row per column of the design matrix and
structured metadata (term, condition, basis, role, run, etc.).

## Usage

``` r
design_colmap(x, ...)
```

## Arguments

- x:

  An object containing or producing a design matrix (e.g., event_model,
  baseline_model).

- ...:

  Additional arguments passed to methods.

## Value

A tibble with per-column metadata.

## Examples

``` r
# Create a simple event model
des <- data.frame(
  onset = c(0, 10, 20, 30),
  run = 1,
  cond = factor(c("A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)

# Extract column metadata
colmap <- design_colmap(emod)
head(colmap)
#> # A tibble: 2 × 15
#>     col name   term_tag term_index condition   run role  model_source basis_name
#>   <int> <chr>  <chr>         <int> <chr>     <int> <chr> <chr>        <chr>     
#> 1     1 cond_… cond              1 cond.A       NA task  event        HRF       
#> 2     2 cond_… cond              1 cond.B       NA task  event        HRF       
#> # ℹ 6 more variables: basis_ix <int>, basis_total <int>, basis_label <chr>,
#> #   is_block_diagonal <lgl>, modulation_type <chr>, modulation_id <chr>

# Query specific columns
colmap[colmap$condition == "A", ]
#> # A tibble: 0 × 15
#> # ℹ 15 variables: col <int>, name <chr>, term_tag <chr>, term_index <int>,
#> #   condition <chr>, run <int>, role <chr>, model_source <chr>,
#> #   basis_name <chr>, basis_ix <int>, basis_total <int>, basis_label <chr>,
#> #   is_block_diagonal <lgl>, modulation_type <chr>, modulation_id <chr>
```
