## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  dev = "svg",
  fig.retina = 1
)

## ----packages-----------------------------------------------------------------
library(fmridesign)
library(ggplot2)

## ----basic_contrast-----------------------------------------------------------
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

## ----validate_attached_contrasts----------------------------------------------
validate_contrasts(emodel_with_contrast)

## ----pairwise_contrasts-------------------------------------------------------
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

## ----polynomial_contrasts-----------------------------------------------------
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

## ----factorial_contrasts------------------------------------------------------
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
lapply(interaction_contrasts, function(x) round(x$weights, 3))

## ----parametric_contrasts-----------------------------------------------------
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

## ----f_contrasts--------------------------------------------------------------
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

## ----manipulate_weights-------------------------------------------------------
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

## ----validate_contrasts-------------------------------------------------------
# Validate the custom contrast against the design implied by the model
validate_contrasts(simple_model, weights = custom_contrast)

## ----multirun_contrasts-------------------------------------------------------
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

## ----contrast_scaling---------------------------------------------------------
# Properly scaled contrasts sum to zero
create_scaled_contrast <- function(n_positive, n_negative) {
  c(rep(1/n_positive, n_positive), rep(-1/n_negative, n_negative))
}

# Example: 3 vs 2 conditions
scaled_contrast <- create_scaled_contrast(3, 2)
print(scaled_contrast)
print(sum(scaled_contrast))  # Should be ~0

## ----multiple_contrasts-------------------------------------------------------
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

## ----analysis_integration, eval=FALSE-----------------------------------------
# # Assuming you have:
# # - Y: fMRI time series data
# # - X: design matrix from event_model
# # - C: contrast vector
# 
# # Standard GLM analysis
# # fit <- lm(Y ~ X - 1)
# # beta <- coef(fit)
# # contrast_estimate <- t(C) %*% beta
# # contrast_se <- sqrt(t(C) %*% vcov(fit) %*% C)
# # t_stat <- contrast_estimate / contrast_se
# 
# # Using the fmridesign contrast
# design_mat <- design_matrix(emodel_with_contrast)
# contrasts <- contrast_weights(emodel_with_contrast)
# 
# # Apply contrast to parameter estimates
# # contrast_result <- apply_contrast(fit, contrasts[[1]])

## ----custom_contrast_function-------------------------------------------------
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

## ----rank_deficient-----------------------------------------------------------
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

## ----multicollinearity--------------------------------------------------------
# Check for multicollinearity in your design matrix
cc <- check_collinearity(design_matrix(emodel_with_contrast), threshold = 0.9)
if (!cc$ok) cc$pairs

