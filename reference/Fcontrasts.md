# Compute F-contrasts

Compute F-contrasts

## Usage

``` r
Fcontrasts(x, ...)

# S3 method for class 'convolved_term'
Fcontrasts(x, ...)

# S3 method for class 'event_model'
Fcontrasts(x, ...)

# S3 method for class 'feature_term'
Fcontrasts(x, ...)
```

## Arguments

- x:

  The object.

- ...:

  Additional arguments.

## Value

A named list of matrices with F-contrast weights.

## Details

Row names of the contrast matrices can specify which levels of the term
are tested. Any matching is done against the design matrix column names.

For terms with a multi-basis HRF (e.g. `"spmg2"`, `"spmg3"`, FIR), the
term-level contrast \\C\\ (one row per condition) is expanded to \\C
\otimes I\_{nb}\\: the condition effect is tested jointly in every basis
function, so a term with \\k\\ conditions and \\nb\\ basis functions
yields a \\(k \cdot nb) \times ((k-1) \cdot nb)\\ matrix for its main
effect. Columns are named `<contrast column>_b##`.

Terms with no categorical variable (e.g. `hrf(rt)`, covariates, feature
terms) contribute no F-contrasts; a model made only of such terms
returns an empty list. Calling `Fcontrasts()` directly on such an
`event_term` still signals an error.

## Examples

``` r
des <- data.frame(
  onset = c(0, 10, 20, 30),
  run = 1,
  cond = factor(c("A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
names(Fcontrasts(emod))
#> [1] "cond#cond"
```
