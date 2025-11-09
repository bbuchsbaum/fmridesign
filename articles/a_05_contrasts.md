# Contrasts in fmridesign

``` r
library(fmridesign)
library(ggplot2)
```

## Introduction to Contrasts

Contrasts specify linear combinations of GLM parameters (β) to test
hypotheses about conditions or trends. In `fmridesign`, define contrasts
inside
[`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md)
calls using helper functions, then extract weights with
[`contrast_weights()`](https://bbuchsbaum.github.io/fmridesign/reference/contrast_weights.md)
(and F-sets with
[`Fcontrasts()`](https://bbuchsbaum.github.io/fmridesign/reference/Fcontrasts.md)).

## Types of Contrasts

T-contrasts use a single contrast vector for directional questions
(e.g., A \> B). F-contrasts use a matrix to test sets of effects (e.g.,
any difference among levels), returning omnibus statistics.

## Defining Contrasts in Event Models

Contrasts can be specified directly within the
[`event_model()`](https://bbuchsbaum.github.io/fmridesign/reference/event_model.md)
formula using the `contrasts` argument in
[`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md)
terms:

``` r
# Create a simple two-condition experiment
set.seed(123)
sframe <- sampling_frame(blocklens = 200, TR = 2)

# Generate events
onsets <- sort(runif(40, 0, 350))
conditions <- rep(c("left", "right"), 20)

# Define contrasts within the model
emodel_with_contrast <- event_model(
  onset ~ hrf(hand, contrasts = pair_contrast(~ hand == "left", ~ hand == "right", name = "left_vs_right")),
  data = data.frame(
    onset = onsets,
    hand = factor(conditions),
    block = factor(rep(1, 40))
  ),
  block = ~ block,
  sampling_frame = sframe
)

# Extract contrast weights
contrast_weights(emodel_with_contrast)
#> $`hand#left_vs_right`
#> contrast: left_vs_right 
#>  A:  ~hand == "left" 
#>  B:  ~hand == "right" 
#>  term:  hand 
#>  weights:  
#>            left_vs_right
#> hand.left              1
#> hand.right            -1
#>  conditions:  hand_hand.left hand_hand.right
```

### Quick Validation

Validate all attached contrasts for a model in one call:

``` r
validate_contrasts(emodel_with_contrast)
#>                 name type estimable sum_to_zero orthogonal_to_intercept
#> 2          hand#hand    t      TRUE        TRUE                    TRUE
#> 1 hand#left_vs_right    t      TRUE        TRUE                    TRUE
#>   full_rank nonzero_weights
#> 2        NA               2
#> 1        NA               2
```

## Advanced Contrast Specifications

### Pairwise Contrasts

For designs with multiple levels, you can specify all pairwise
comparisons:

``` r
# Three-condition experiment
set.seed(456)
conditions_3 <- rep(c("easy", "medium", "hard"), each = 15)
onsets_3 <- sort(runif(45, 0, 350))

emodel_pairwise <- event_model(
  onset ~ hrf(difficulty,
              contrasts = pairwise_contrasts(c("easy","medium","hard"), facname = "difficulty", name_prefix = "pair")),
  data = data.frame(
    onset = onsets_3,
    difficulty = factor(conditions_3, levels = c("easy", "medium", "hard")),
    block = factor(rep(1, 45))
  ),
  block = ~ block,
  sampling_frame = sframe
)

# View all contrasts
contrasts_list <- contrast_weights(emodel_pairwise)
names(contrasts_list)
#> [1] "difficulty#pair_easy_medium" "difficulty#pair_easy_hard"  
#> [3] "difficulty#pair_medium_hard"
```

### Polynomial Contrasts

Test for linear, quadratic, or higher-order trends across ordered
conditions:

``` r
# Parametric design with 4 levels
set.seed(789)
intensity_levels <- rep(1:4, each = 12)
onsets_param <- sort(runif(48, 0, 350))

# Create polynomial contrasts using poly_contrast()
emodel_polynomial <- event_model(
  onset ~ hrf(intensity, 
              contrasts = list(
                linear = poly_contrast(~ intensity, name = "linear", degree = 1),
                quadratic = poly_contrast(~ intensity, name = "quadratic", degree = 2),
                cubic = poly_contrast(~ intensity, name = "cubic", degree = 3)
              )),
  data = data.frame(
    onset = onsets_param,
    intensity = factor(intensity_levels),
    block = factor(rep(1, 48))
  ),
  block = ~ block,
  sampling_frame = sframe
)

# Extract polynomial contrast weights
poly_contrasts <- contrast_weights(emodel_polynomial)
lapply(poly_contrasts, function(x) round(x$weights, 3))
#> $`intensity#linear`
#>             linear_1
#> intensity.1   -0.671
#> intensity.2   -0.224
#> intensity.3    0.224
#> intensity.4    0.671
#> 
#> $`intensity#quadratic`
#>             quadratic_1 quadratic_2
#> intensity.1      -0.671         0.5
#> intensity.2      -0.224        -0.5
#> intensity.3       0.224        -0.5
#> intensity.4       0.671         0.5
#> 
#> $`intensity#cubic`
#>             cubic_1 cubic_2 cubic_3
#> intensity.1  -0.671     0.5  -0.224
#> intensity.2  -0.224    -0.5   0.671
#> intensity.3   0.224    -0.5  -0.671
#> intensity.4   0.671     0.5   0.224
```

## Factorial Design Contrasts

### Main Effects and Interactions

For factorial designs, specify contrasts for main effects and
interactions:

``` r
# 2x2 factorial design
set.seed(234)
n_trials <- 60
factor_A <- rep(c("A1", "A2"), each = 30)
factor_B <- rep(rep(c("B1", "B2"), each = 15), 2)
factorial_onsets <- sort(runif(n_trials, 0, 350))

emodel_factorial <- event_model(
  onset ~ hrf(A, B, contrasts = contrast_set(
    oneway_contrast(~ A, name = "main_A"),
    oneway_contrast(~ B, name = "main_B"),
    interaction_contrast(~ A * B, name = "A_by_B")
  )),
  data = data.frame(
    onset = factorial_onsets,
    A = factor(factor_A),
    B = factor(factor_B),
    block = factor(rep(1, n_trials))
  ),
  block = ~ block,
  sampling_frame = sframe
)

interaction_contrasts <- contrast_weights(emodel_factorial)
#> Warning: Contrast 'A_by_B' for term 'A_B' has unmatched row names: A1_B1,
#> A2_B1, A1_B2, A2_B2
lapply(interaction_contrasts, function(x) round(x$weights, 3))
#> $`A:B#main_A`
#>           main_A_1
#> A.A1_B.B1       -1
#> A.A2_B.B1        1
#> A.A1_B.B2       -1
#> A.A2_B.B2        1
#> 
#> $`A:B#main_B`
#>           main_B_1
#> A.A1_B.B1       -1
#> A.A2_B.B1       -1
#> A.A1_B.B2        1
#> A.A2_B.B2        1
#> 
#> $`A:B#A_by_B`
#>       A_by_B_1
#> A1_B1        1
#> A2_B1       -1
#> A1_B2       -1
#> A2_B2        1
```

## Contrasts with Parametric Modulators

When using parametric modulators, contrasts can test both categorical
and continuous effects:

``` r
# Design with conditions and RT modulation
set.seed(567)
n_events <- 50
pm_conditions <- rep(c("congruent", "incongruent"), each = 25)
pm_onsets <- sort(runif(n_events, 0, 350))
pm_RT <- rnorm(n_events, mean = ifelse(pm_conditions == "congruent", 0.5, 0.7), sd = 0.1)

emodel_parametric <- event_model(
  onset ~ hrf(condition, 
              contrasts = pair_contrast(~ condition == "incongruent", ~ condition == "congruent", name = "incongruent_gt_congruent")) + 
          hrf(RT),
  data = data.frame(
    onset = pm_onsets,
    condition = factor(pm_conditions),
    RT = scale(pm_RT)[,1],
    block = factor(rep(1, n_events))
  ),
  block = ~ block,
  sampling_frame = sframe
)

# Get contrasts - includes both categorical and parametric terms
param_contrasts <- contrast_weights(emodel_parametric)
print(param_contrasts)
#> $`condition#incongruent_gt_congruent`
#> contrast: incongruent_gt_congruent 
#>  A:  ~condition == "incongruent" 
#>  B:  ~condition == "congruent" 
#>  term:  condition 
#>  weights:  
#>                       incongruent_gt_congruent
#> condition.congruent                         -1
#> condition.incongruent                        1
#>  conditions:  condition_condition.congruent condition_condition.incongruent RT_RT
```

## F-contrasts for Omnibus Tests

F-contrasts test multiple parameters simultaneously:

``` r
# Four-condition design for omnibus test
set.seed(890)
conditions_4 <- rep(c("A", "B", "C", "D"), each = 12)
onsets_4 <- sort(runif(48, 0, 350))

# Using oneway_contrast for main effect
emodel_omnibus <- event_model(
  onset ~ hrf(condition, 
              contrasts = oneway_contrast(~ condition, name = "main_effect")),
  data = data.frame(
    onset = onsets_4,
    condition = factor(conditions_4),
    block = factor(rep(1, 48))
  ),
  block = ~ block,
  sampling_frame = sframe
)

# Extract F-contrast
f_contrasts <- Fcontrasts(emodel_omnibus)
print(f_contrasts)
#> $`condition#condition`
#>                       c1 c2 c3
#> condition_condition.A  1  0  0
#> condition_condition.B  0  1  0
#> condition_condition.C  0  0  1
#> condition_condition.D -1 -1 -1
#> attr(,"term_indices")
#> [1] 1 2 3 4
```

## Working with Contrast Weights

### Extracting and Manipulating Weights

``` r
# Create a model
simple_model <- event_model(
  onset ~ hrf(stim),
  data = data.frame(
    onset = c(10, 30, 50, 70, 90),
    stim = factor(c("A", "B", "A", "B", "A")),
    block = factor(rep(1, 5))
  ),
  block = ~ block,
  sampling_frame = sampling_frame(60, TR = 2)
)

# Manually create contrast weights
design_mat <- design_matrix(simple_model)
n_cols <- ncol(design_mat)

# Create a custom contrast vector
custom_contrast <- rep(0, n_cols)
# Find columns for condition A and B
# Note: column names use dot notation for factor levels (e.g., "stim.A")
col_names <- colnames(design_mat)
# Match by suffix to handle term-tag prefixes like "stim_stim.A"
a_cols <- grep("stim\\.A$", col_names)
b_cols <- grep("stim\\.B$", col_names)

# A > B contrast
custom_contrast[a_cols] <- 1/length(a_cols)
custom_contrast[b_cols] <- -1/length(b_cols)

print(custom_contrast)
#> [1]  1 -1
```

### Contrast Validation

Validate contrasts once the model (and design matrix) is constructed.
You can validate built-in contrast specs or your own custom vectors
using
[`validate_contrasts()`](https://bbuchsbaum.github.io/fmridesign/reference/validate_contrasts.md):

``` r
# Validate the custom contrast against the design implied by the model
validate_contrasts(simple_model, weights = custom_contrast)
#>       name type estimable sum_to_zero orthogonal_to_intercept full_rank
#> 1 contrast    t      TRUE        TRUE                    TRUE        NA
#>   nonzero_weights
#> 1               2
```

## Contrasts for Multi-Run Designs

When working with multiple runs, contrasts can be specified to test
within-run or across-run effects:

``` r
# Two-run experiment with potential run differences
set.seed(345)
run1_onsets <- sort(runif(20, 0, 200))
run2_onsets <- sort(runif(20, 0, 200))
all_onsets <- c(run1_onsets, run2_onsets)
all_conditions <- rep(c("stim", "control"), 20)
all_blocks <- rep(1:2, each = 20)

emodel_multirun <- event_model(
  onset ~ hrf(condition, block,
              contrasts = list(
                overall   = pair_contrast(~ condition == "stim", ~ condition == "control", name = "overall"),
                run1_only = pair_contrast(~ condition == "stim", ~ condition == "control", name = "run1", where = ~ block == "1"),
                run2_only = pair_contrast(~ condition == "stim", ~ condition == "control", name = "run2", where = ~ block == "2")
              )),
  data = data.frame(
    onset = all_onsets,
    condition = factor(all_conditions),
    block = factor(all_blocks)
  ),
  block = ~ block,
  sampling_frame = sampling_frame(c(120, 120), TR = 2)
)

multirun_contrasts <- contrast_weights(emodel_multirun)
names(multirun_contrasts)
#> [1] "condition:block#overall" "condition:block#run1"   
#> [3] "condition:block#run2"
```

## Contrast Specification Best Practices

Plan contrasts a priori, keep them simple and interpretable, and prefer
orthogonal sets when possible.

### 2. Scaling and Normalization

``` r
# Properly scaled contrasts sum to zero
create_scaled_contrast <- function(n_positive, n_negative) {
  c(rep(1/n_positive, n_positive), rep(-1/n_negative, n_negative))
}

# Example: 3 vs 2 conditions
scaled_contrast <- create_scaled_contrast(3, 2)
print(scaled_contrast)
#> [1]  0.3333333  0.3333333  0.3333333 -0.5000000 -0.5000000
print(sum(scaled_contrast))  # Should be ~0
#> [1] -5.551115e-17
```

### 3. Multiple Comparison Correction

When testing multiple contrasts, consider correction for multiple
comparisons:

``` r
# Design with multiple planned contrasts
emodel_multiple <- event_model(
  onset ~ hrf(condition,
              contrasts = contrast_set(
                pair_contrast(~ condition == "C1", ~ condition == "C2", name = "C1_vs_C2"),
                pair_contrast(~ condition == "C1", ~ condition == "C3", name = "C1_vs_C3"),
                pair_contrast(~ condition == "C2", ~ condition == "C3", name = "C2_vs_C3")
              )),
  data = data.frame(
    onset = sort(runif(60, 0, 350)),
    condition = factor(rep(c("C1", "C2", "C3"), 20)),
    block = factor(rep(1, 60))
  ),
  block = ~ block,
  sampling_frame = sframe
)

# Number of contrasts to correct for
n_contrasts <- length(contrast_weights(emodel_multiple))
bonferroni_alpha <- 0.05 / n_contrasts
print(paste("Bonferroni-corrected alpha:", bonferroni_alpha))
#> [1] "Bonferroni-corrected alpha: 0.0166666666666667"
```

## Integration with Statistical Analysis

Once contrasts are defined, they can be used in the statistical
analysis:

``` r
# Assuming you have:
# - Y: fMRI time series data
# - X: design matrix from event_model
# - C: contrast vector

# Standard GLM analysis
# fit <- lm(Y ~ X - 1)
# beta <- coef(fit)
# contrast_estimate <- t(C) %*% beta
# contrast_se <- sqrt(t(C) %*% vcov(fit) %*% C)
# t_stat <- contrast_estimate / contrast_se

# Using the fmridesign contrast
design_mat <- design_matrix(emodel_with_contrast)
contrasts <- contrast_weights(emodel_with_contrast)

# Apply contrast to parameter estimates
# contrast_result <- apply_contrast(fit, contrasts[[1]])
```

## Advanced Topics

### Custom Contrast Functions

You can generate complex patterns via helper generators that return
contrast specifications. For example, a sliding-window set that compares
adjacent, disjoint windows across an ordered factor:

``` r
# Five ordered levels
lvl <- paste0("L", 1:5)

# Build a small model with an ordered factor
set.seed(111)
emod_sliding <- event_model(
  onset ~ hrf(level, contrasts = sliding_window_contrasts(levels = lvl, facname = "level", window_size = 1)),
  data = data.frame(
    onset = sort(runif(50, 0, 350)),
    level = factor(sample(lvl, 50, replace = TRUE), levels = lvl, ordered = TRUE),
    block = factor(rep(1, 50))
  ),
  block = ~ block,
  sampling_frame = sframe
)

# Inspect the generated contrasts
names(contrast_weights(emod_sliding))
#> [1] "level#win_L1_vs_L2" "level#win_L2_vs_L3" "level#win_L3_vs_L4"
#> [4] "level#win_L4_vs_L5"
```

For more targeted patterns (e.g., specific basis functions or continuous
terms), use
[`column_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/column_contrast.md)
with regex to match design-matrix columns.

## Troubleshooting Common Issues

### 1. Rank-Deficient Contrasts

``` r
# This will cause issues - contrast is not estimable
bad_data <- data.frame(
  onset = c(10, 30),
  condition = factor(c("A", "A")),  # Only one level!
  block = factor(c(1, 1))
)

# This will warn about the issue
tryCatch({
  bad_model <- event_model(
    onset ~ hrf(condition, contrasts = pair_contrast(~ condition == "A", ~ condition == "B", name = "A_vs_B")),
    data = bad_data,
    block = ~ block,
    sampling_frame = sampling_frame(50, TR = 2)
  )
}, error = function(e) {
  print("Error: Cannot create contrast for single-level factor")
})
```

### 2. Multicollinearity in Contrasts

``` r
# Check for multicollinearity in your design matrix
cc <- check_collinearity(design_matrix(emodel_with_contrast), threshold = 0.9)
if (!cc$ok) cc$pairs
```

## Summary

Define contrasts inline, extract weights cleanly, and validate before
analysis. Use t-contrasts for directional tests and F-contrasts for
omnibus effects. 3. **Complex designs**: Handle factorial, parametric,
and multi-run contrasts 4. **Validation tools**: Ensure contrasts are
properly specified and estimable

Remember to: - Plan contrasts based on your hypotheses - Validate
contrast properties before analysis - Consider multiple comparison
corrections - Document your contrast specifications for reproducibility

For more information on event and baseline models, see: -
[`vignette("a_01_introduction")`](https://bbuchsbaum.github.io/fmridesign/articles/a_01_introduction.md) -
[`vignette("a_03_baseline_model")`](https://bbuchsbaum.github.io/fmridesign/articles/a_03_baseline_model.md) -
[`vignette("a_04_event_models")`](https://bbuchsbaum.github.io/fmridesign/articles/a_04_event_models.md)
