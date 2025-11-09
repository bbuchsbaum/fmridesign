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
