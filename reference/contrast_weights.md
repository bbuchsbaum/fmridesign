# Unit Contrast Weights

Compute the contrast weights for a unit_contrast_spec object.

Compute the contrast weights for an oneway_contrast_spec object.

Compute the contrast weights for an interaction_contrast_spec object.

Compute the contrast weights for a poly_contrast_spec object.

Compute the contrast weights for a pair_contrast_spec object.

Compute contrast weights for a `column_contrast_spec` object by
targeting design matrix columns based on regex patterns.

Compute the contrast weights for a contrast_formula_spec object.

Compute the contrast weights for a contrast_diff_spec object.

Compute the contrast weights for each contrast specification within a
contrast_set object.

## Usage

``` r
# S3 method for class 'unit_contrast_spec'
contrast_weights(x, term, ...)

# S3 method for class 'oneway_contrast_spec'
contrast_weights(x, term, ...)

# S3 method for class 'interaction_contrast_spec'
contrast_weights(x, term, ...)

# S3 method for class 'poly_contrast_spec'
contrast_weights(x, term, ...)

# S3 method for class 'pair_contrast_spec'
contrast_weights(x, term, ...)

# S3 method for class 'column_contrast_spec'
contrast_weights(x, term, ...)

# S3 method for class 'contrast_formula_spec'
contrast_weights(x, term, ...)

# S3 method for class 'contrast_diff_spec'
contrast_weights(x, term, ...)

# S3 method for class 'contrast_set'
contrast_weights(x, term, ...)

contrast_weights(x, ...)

# S3 method for class 'convolved_term'
contrast_weights(x, ...)

# S3 method for class 'event_model'
contrast_weights(x, ...)
```

## Arguments

- x:

  The object.

- term:

  A term object against which weights should be computed.

- ...:

  Additional arguments.

## Value

A list containing the term, name, weights, condition names, and contrast
specification.

A list containing the term, name, weights, condition names, and contrast
specification.

A list containing the term, name, weights, condition names, and contrast
specification.

A list containing the term, name, weights, condition names, and contrast
specification.

A list containing the term, name, weights, condition names, and contrast
specification.

A list containing the contrast details:

- term:

  The original `event_term` object.

- name:

  The name of the contrast.

- weights:

  A numeric matrix where rows correspond to the full design matrix
  columns (from `.condnames(term, expanded = TRUE)`) and columns
  represent the contrast(s). Usually one column.

- condnames:

  Character vector of all potential *expanded* condition names from
  `term`.

- contrast_spec:

  The original `column_contrast_spec` object.

A list containing the term, name, weights, condition names, and contrast
specification.

A list containing the term, name, weights, condition names, and contrast
specification.

A named list where each element is the result of calling
contrast_weights on the corresponding contrast_spec in the set. The list
names are the names of the individual contrasts.

A named list of contrast weight objects or matrices.

## Details

If the weight matrices returned by a contrast specification contain row
names, these are matched to the column names of the corresponding term
in the design matrix. This allows contrasts to target only a subset of
term levels.

## Examples

``` r
des <- data.frame(
  onset = c(0, 10, 20, 30),
  run = 1,
  cond = factor(c("A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
cset <- contrast_set(
  diff = column_contrast(pattern_A = "cond.A", pattern_B = "cond.B", name = "diff")
)
emod <- event_model(onset ~ hrf(cond, contrasts = cset),
                    data = des, block = ~run, sampling_frame = sframe)
contrast_weights(emod)
#> $`cond#diff`
#> contrast: diff 
#>  A:  NULL 
#>  term:  cond 
#>  weights:  
#>        diff
#> cond.A    1
#> cond.B   -1
#>  conditions:  cond_cond.A cond_cond.B
```
