# Column metadata for a design matrix

Returns a tibble with one row per column of the design matrix and
structured metadata derived from existing package attributes and naming
conventions. This avoids ad-hoc string parsing in user code and provides
a consistent, queryable view of the design.

## Usage

``` r
# S3 method for class 'event_model'
design_colmap(x, ...)
```

## Arguments

- x:

  An object containing or producing a design matrix (e.g.,
  `event_model`, `baseline_model`).

- ...:

  Unused; reserved for future extensions.

## Value

A tibble with columns:

- `col` (integer): 1-based column index

- `name` (character): column name

- `term_tag` (character): term identifier (event tag or baseline term
  name)

- `term_index` (integer): position within `terms(x)`

- `condition` (character): condition/event label without basis suffix
  (if applicable)

- `run` (integer): block/run id when the column is block-specific; `NA`
  if pooled

- `role` (character): e.g., "task", "drift", "intercept", "nuisance"

- `model_source` (character): "event" or "baseline"

- `basis_name` (character): HRF or baseline basis identifier when
  available

- `basis_ix` (integer): within-condition basis index (HRF component);
  `NA` if not applicable

- `basis_total` (integer): total number of basis components for the
  column's term; `NA` if not applicable

- `basis_label` (character): human-readable label for the basis
  component when known

- `is_block_diagonal` (logical): TRUE when the regressor is
  per-run/block

- `modulation_type` (character): "amplitude", "parametric", or
  "covariate"

- `modulation_id` (character): modulator identifier when applicable
  (e.g., "RT", "RT_by_group")

## Examples

``` r
# Create event model
des <- data.frame(
  onset = c(0, 10, 20, 30),
  run = 1,
  cond = factor(c("A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)

# Get column metadata
colmap <- design_colmap(emod)
head(colmap)
#> # A tibble: 2 × 15
#>     col name   term_tag term_index condition   run role  model_source basis_name
#>   <int> <chr>  <chr>         <int> <chr>     <int> <chr> <chr>        <chr>     
#> 1     1 cond_… cond              1 cond.A       NA task  event        HRF       
#> 2     2 cond_… cond              1 cond.B       NA task  event        HRF       
#> # ℹ 6 more variables: basis_ix <int>, basis_total <int>, basis_label <chr>,
#> #   is_block_diagonal <lgl>, modulation_type <chr>, modulation_id <chr>

# Query columns by condition
colmap[colmap$condition == "A", ]
#> # A tibble: 0 × 15
#> # ℹ 15 variables: col <int>, name <chr>, term_tag <chr>, term_index <int>,
#> #   condition <chr>, run <int>, role <chr>, model_source <chr>,
#> #   basis_name <chr>, basis_ix <int>, basis_total <int>, basis_label <chr>,
#> #   is_block_diagonal <lgl>, modulation_type <chr>, modulation_id <chr>
```
