# Pair Contrast

Construct a sum-to-zero contrast between two logical expressions. This
function is particularly useful for comparing specific conditions or
combinations of conditions.

## Usage

``` r
pair_contrast(A, B, name, where = NULL, basis = NULL, basis_weights = NULL)
```

## Arguments

- A:

  A formula representing the first logical expression in the contrast.

- B:

  A formula representing the second logical expression in the contrast.

- name:

  A character string specifying the name of the contrast (mandatory).

- where:

  An optional formula specifying the subset over which the contrast is
  computed.

- basis:

  NULL (default: use all basis functions), an integer vector specifying
  which basis function indices to include (e.g., `1`, `2:3`, `c(1,3)`),
  or `"all"`. Only relevant when the HRF uses multiple basis functions
  (e.g., bspline, FIR, Fourier). Basis indices are 1-based (1 = first
  basis function, 2 = second, etc.).

- basis_weights:

  NULL (default: equal weights), or a numeric vector of weights to apply
  to the selected basis functions. Must have the same length as `basis`
  selection and will be normalized to sum to 1. Use this to emphasize
  specific temporal components (e.g., `c(.1, .2, .4, .2, .1)` for
  Gaussian-like weighting emphasizing the peak).

## Value

A pair_contrast_spec object containing:

- A:

  First logical expression

- B:

  Second logical expression

- where:

  Subsetting formula (if provided)

- basis:

  Basis function specification (if provided)

- basis_weights:

  Basis weights (if provided)

- name:

  Contrast name

## Details

The contrast is constructed as (A - B), where A and B are logical
expressions that evaluate to TRUE/FALSE for each observation. The
resulting contrast weights sum to zero.

When using multi-basis HRFs (e.g., bspline with 5 basis functions), the
`basis` argument allows you to test specific temporal components of the
response. For example:

- `basis = 1`: Test only the first basis function (often the
  canonical/early response)

- `basis = 2:3`: Test the second and third basis functions together

- `basis = NULL` or `basis = "all"`: Test all basis functions (default
  behavior)

The `basis_weights` argument allows non-uniform weighting across
selected basis functions:

- `basis_weights = c(.1, .2, .4, .2, .1)`: Gaussian-like emphasis on
  peak

- `basis_weights = c(1, 0, 0, 0, 0)`: Isolate first basis (equivalent to
  `basis = 1`)

- Weights are applied within each condition, maintaining contrast
  sum-to-zero property

## See also

[`pairwise_contrasts`](https://bbuchsbaum.github.io/fmridesign/reference/pairwise_contrasts.md)
for all pairwise comparisons,
[`contrast_set`](https://bbuchsbaum.github.io/fmridesign/reference/contrast_set.md)
for creating sets of contrasts

## Examples

``` r
# Compare faces vs scenes (all basis functions)
pair_contrast(~ category == "face", ~ category == "scene", name = "face_vs_scene")
#> contrast: face_vs_scene 
#>  A:  ~category == "face" 
#>  B:  ~category == "scene" 

# Test only the second basis function (e.g., linear component for polynomial HRF)
pair_contrast(~ category == "face", ~ category == "scene",
             basis = 2, name = "face_vs_scene_basis2")
#> contrast: face_vs_scene_basis2 
#>  A:  ~category == "face" 
#>  B:  ~category == "scene" 

# Test early response components (first 3 basis functions)
pair_contrast(~ category == "face", ~ category == "scene",
             basis = 1:3, name = "face_vs_scene_early")
#> contrast: face_vs_scene_early 
#>  A:  ~category == "face" 
#>  B:  ~category == "scene" 

# Compare with subsetting
pair_contrast(~ category == "face", ~ category == "scene",
             name = "face_vs_scene_block1",
             where = ~ block == 1)
#> contrast: face_vs_scene_block1 
#>  A:  ~category == "face" 
#>  B:  ~category == "scene" 
#>  where:  ~block == 1 

# Complex logical expressions
pair_contrast(~ stimulus == "face" & emotion == "happy",
             ~ stimulus == "face" & emotion == "sad",
             name = "happy_vs_sad_faces")
#> contrast: happy_vs_sad_faces 
#>  A:  ~stimulus == "face" & emotion == "happy" 
#>  B:  ~stimulus == "face" & emotion == "sad" 
```
