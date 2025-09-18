## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  fig.retina = 1
)

## ----packages-----------------------------------------------------------------
library(fmridesign)
library(ggplot2)

## ----complete_example, fig.alt="Predicted BOLD regressors over time for an example event model (two runs)."----
# 1. Define the temporal structure (2 runs, 150 scans each, TR = 2s)
TR <- 2
sframe <- sampling_frame(blocklens = c(150, 150), TR = TR)

# 2. Create experimental events
set.seed(123)
# Simple two-condition design with 20 events per run
conditions <- rep(c("A", "B"), each = 10, times = 2)
onsets <- c(
  sort(runif(20, 0, 150 * TR - 10)),  # Run 1
  sort(runif(20, 0, 150 * TR - 10))   # Run 2
)
blockids <- rep(1:2, each = 20)

# 3. Build the event model
emodel <- event_model(
  onset ~ hrf(Condition, basis = "spmg1"),
  data = data.frame(
    onset = onsets,
    Condition = factor(conditions),
    block = factor(blockids)
  ),
  block = ~ block,
  sampling_frame = sframe
)

# 4. Build the baseline model
bmodel <- baseline_model(
  basis = "poly",
  degree = 3,
  sframe = sframe
)

# 5. Extract design matrices
X_task <- design_matrix(emodel)
X_baseline <- design_matrix(bmodel)

# 6. Combine into full design matrix
X_full <- cbind(X_task, X_baseline)
dim(X_full)

# 7. Visualize the complete design
plot(emodel)

## ----sampling_frame-----------------------------------------------------------
# Single run: 200 scans, TR = 2 seconds
sframe_single <- sampling_frame(blocklens = 200, TR = 2)
print(sframe_single)

# Multiple runs: 3 runs with different lengths
sframe_multi <- sampling_frame(blocklens = c(150, 200, 150), TR = 2)
print(sframe_multi)

## ----event_specification------------------------------------------------------
# Formula interface (recommended)
emodel_formula <- event_model(
  onset ~ hrf(condition) + hrf(RT, basis = "gaussian"),
  data = data.frame(
    onset = c(10, 30, 50, 70),
    condition = factor(c("easy", "hard", "easy", "hard")),
    RT = c(0.5, 0.8, 0.4, 0.9),
    block = factor(c(1, 1, 1, 1))
  ),
  block = ~ block,
  sampling_frame = sampling_frame(100, TR = 2)
)

## ----block_design, fig.alt="Block design regressors (task and rest blocks) over time."----
# Block design with 20-second blocks
block_onsets <- seq(0, 280, by = 40)
block_conditions <- rep(c("task", "rest"), length.out = length(block_onsets))
block_durations <- rep(20, length(block_onsets))

emodel_block <- event_model(
  onset ~ hrf(condition),
  data = data.frame(
    onset = block_onsets,
    condition = factor(block_conditions),
    block = factor(rep(1, length(block_onsets)))
  ),
  block = ~ block,
  durations = block_durations,
  sampling_frame = sampling_frame(150, TR = 2)
)

plot(emodel_block)

## ----rapid_event--------------------------------------------------------------
# Rapid event-related with jittered ISI
set.seed(456)
n_events <- 60
rapid_onsets <- cumsum(runif(n_events, 2, 6))  # ISI between 2-6s
rapid_conditions <- sample(c("face", "house", "object"), n_events, replace = TRUE)

emodel_rapid <- event_model(
  onset ~ hrf(stimulus),
  data = data.frame(
    onset = rapid_onsets,
    stimulus = factor(rapid_conditions),
    block = factor(rep(1, n_events))
  ),
  block = ~ block,
  sampling_frame = sampling_frame(ceiling(max(rapid_onsets)/2) + 20, TR = 2)
)

# Check design efficiency
cor(design_matrix(emodel_rapid))

## ----parametric, fig.alt="Parametric modulator (RT) regressor and conditions plotted over time."----
# Event-related design with RT modulation
set.seed(789)
n_trials <- 30
pm_onsets <- sort(runif(n_trials, 0, 200))
pm_conditions <- rep(c("congruent", "incongruent"), length.out = n_trials)
pm_RT <- rnorm(n_trials, mean = ifelse(pm_conditions == "congruent", 0.5, 0.7), sd = 0.1)

emodel_parametric <- event_model(
  onset ~ hrf(condition) + hrf(RT),
  data = data.frame(
    onset = pm_onsets,
    condition = factor(pm_conditions),
    RT = scale(pm_RT)[,1],  # Center the parametric modulator
    block = factor(rep(1, n_trials))
  ),
  block = ~ block,
  sampling_frame = sampling_frame(120, TR = 2)
)

# Visualize the parametric modulator
plot(emodel_parametric, term_name = "RT")

## ----integration, eval=FALSE--------------------------------------------------
# # Example: Export for use with external software
# X <- cbind(design_matrix(emodel), design_matrix(bmodel))
# 
# # Save as text file for SPM, FSL, or AFNI
# write.table(X, "design_matrix.txt", row.names = FALSE, col.names = TRUE)
# 
# # For use with R-based analysis
# # Assuming Y is your fMRI time series data
# # fit <- lm(Y ~ X - 1)  # -1 removes intercept as it's in the design
# 
# # Or use with specialized fMRI packages
# # library(fmri)
# # results <- fmri_glm(Y, X)

