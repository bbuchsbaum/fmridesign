# Design matrix grouped into feature bands

Collects the event-model design matrix together with the band and
sub-band of every column, in the form banded (grouped) regression needs:
one matrix and one group label per column. The labels come from
[`design_colmap()`](https://bbuchsbaum.github.io/fmridesign/reference/design_colmap.md),
which remains the single source of truth. A
[`feature()`](https://bbuchsbaum.github.io/fmridesign/reference/feature.md)
term declares them with `band` and `subband`; any other term is a band
of its own, named by its term tag.

## Usage

``` r
design_bands(x, ...)

# S3 method for class 'event_model'
design_bands(x, terms = NULL, bands = NULL, ...)
```

## Arguments

- x:

  The object, typically an `event_model`.

- ...:

  Additional arguments passed to methods.

- terms:

  Optional character vector of term tags (`names(terms(x))`); only
  columns of these terms are returned.

- bands:

  Optional character vector of band names; only columns in these bands
  are returned. With both `terms` and `bands`, a column must satisfy
  both.

## Value

A list with elements:

- `X`: numeric design matrix (scans by columns) with column names

- `band`: character vector, the band of each column of `X`

- `subband`: character vector, the sub-band of each column; equal to
  `band` where no sub-band was declared

- `families`: named list with one element for each band that has more
  than one distinct sub-band, holding the character vector of its
  sub-bands in column order

- `colmap`: the rows of
  [`design_colmap()`](https://bbuchsbaum.github.io/fmridesign/reference/design_colmap.md)
  for the columns of `X`. Its `col` column still indexes the full design
  matrix.

## See also

[`design_colmap()`](https://bbuchsbaum.github.io/fmridesign/reference/design_colmap.md),
[`feature()`](https://bbuchsbaum.github.io/fmridesign/reference/feature.md)

## Examples

``` r
dt <- 0.5
t <- seq(0, 39.5, by = dt)
phon <- cbind(stop = abs(sin(t)), nasal = abs(cos(t)), voiced = abs(sin(2 * t)))
des <- data.frame(onset = c(0, 10, 20, 30), run = 1,
                  cond = factor(c("A", "B", "A", "B")))
sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
emod <- event_model(
  onset ~ hrf(cond) +
    feature(phon, dt = dt, id = "phon", band = "phonological",
            subband = c("manner", "manner", "voicing")),
  data = des, block = ~run, sampling_frame = sframe
)
b <- design_bands(emod)
table(b$band, b$subband)
#>               
#>                cond manner voicing
#>   cond            2      0       0
#>   phonological    0      2       1
b$families
#> $phonological
#> [1] "manner"  "voicing"
#> 

# Only the phonological band
colnames(design_bands(emod, bands = "phonological")$X)
#> [1] "phon_stop"   "phon_nasal"  "phon_voiced"
```
