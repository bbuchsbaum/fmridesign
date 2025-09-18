## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, 
  fig.height = 5,
  dev = "svg",
  fig.retina = 1,
  message = FALSE,
  warning = FALSE
)

# Load packages
library(fmridesign)
library(fmrihrf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)

# No longer need workaround - fmrihrf bug has been fixed

## ----setup_simple_design------------------------------------------------------
TR <- 2
cond <- c("face", "scene", "tool", "object")
NSTIM <- length(cond) * 4

# Construct the design table
set.seed(123) # for reproducibility
simple_design <- data.frame(
  stim = factor(sample(rep(cond, 4))),
  ISI = sample(10:20, NSTIM, replace = TRUE),  # Increased ISI range for better spacing
  run = rep(1, NSTIM),
  trial = factor(1:NSTIM)
)

# Calculate onsets (cumulative sum of duration (2s) + ISI)
simple_design$onset <- cumsum(c(0, simple_design$ISI[-NSTIM] + 2))

# Define the sampling frame (temporal structure of the scan)
sframe_single_run <- sampling_frame(blocklens = 140, TR = TR)

head(simple_design)

## ----create_simple_model------------------------------------------------------
emodel_simple <- event_model(onset ~ hrf(stim), 
                             data = simple_design, 
                             block = ~ run, 
                             sampling_frame = sframe_single_run)

# Print the model summary
print(emodel_simple)

## ----plot_simple_model, fig.alt="Event model regressors for a simple single-factor design plotted over time."----
# Static plot (ggplot2)
plot(emodel_simple)

# Interactive plot via plotly
plotly::ggplotly(plot(emodel_simple))

## ----setup_multi_block--------------------------------------------------------
# Construct a design table with two runs
design_list <- lapply(1:2, function(run_idx) {
  df <- data.frame(
    stim = factor(sample(rep(cond, 4))),
    ISI = sample(10:20, NSTIM, replace = TRUE),  # Increased ISI range for better spacing
    run = rep(run_idx, NSTIM)
  )
  df$onset <- cumsum(c(0, df$ISI[-NSTIM] + 2))
  df
})
design_multi_run <- bind_rows(design_list)

# Sampling frame for two runs of 140s each
sframe_multi_run <- sampling_frame(blocklens = c(140, 140), TR = TR)

head(design_multi_run)

## ----create_multi_block_model, fig.alt="Event model regressors across multiple runs plotted over time."----
emodel_multi_run <- event_model(onset ~ hrf(stim), 
                                data = design_multi_run, 
                                block = ~ run, 
                                sampling_frame = sframe_multi_run)

print(emodel_multi_run)
# Plot without faceting - the plot method will handle multiple blocks
plot(emodel_multi_run)

## ----setup_two_factor---------------------------------------------------------
cond1 <- c("face", "scene", "tool", "object")
cond2 <- c("attend", "ignore")
comb <- expand.grid(stim = cond1, task = cond2)
NSTIM_TF <- nrow(comb) * 4 # 8 conditions * 4 reps per run

# Design for two runs
design_two_factor_list <- lapply(1:2, function(run_idx) {
  ind <- sample(rep(1:nrow(comb), length.out = NSTIM_TF))
  df <- data.frame(
    stim = factor(comb$stim[ind]),
    task = factor(comb$task[ind]),
    ISI = sample(6:15, NSTIM_TF, replace = TRUE),  # Increased ISI range for better spacing
    run = rep(run_idx, NSTIM_TF)
  )
  df$onset <- cumsum(c(0, df$ISI[-NSTIM_TF] + 2))
  df
})
design_two_factor <- bind_rows(design_two_factor_list)

# Sampling frame for two runs, potentially longer
sframe_two_factor <- sampling_frame(blocklens = c(200, 200), TR = TR)

head(design_two_factor)

## ----create_two_factor_model, fig.alt="Interaction term regressors from a two-factor design plotted over time."----
emodel_two_factor <- event_model(onset ~ hrf(stim, task), 
                                 data = design_two_factor, 
                                 block = ~ run, 
                                 sampling_frame = sframe_two_factor)
print(emodel_two_factor)
# Column names will be like stim_task_stim.face_task.attend

# Plotting all interaction terms can be busy; consider plotly
plot(emodel_two_factor)
plotly::ggplotly(plot(emodel_two_factor))

## ----setup_pmod---------------------------------------------------------------
# Use the simple single-run design and add a simulated RT column
simple_design$RT <- rnorm(nrow(simple_design), mean = 700, sd = 100)

# It's often recommended to center parametric modulators
simple_design$RT_centered <- scale(simple_design$RT, center = TRUE, scale = FALSE)[,1]
head(simple_design)

## ----create_pmod_model, fig.alt="Parametric modulator regressors (and main effects) plotted over time."----
emodel_pmod <- event_model(onset ~ hrf(stim) + hrf(RT_centered), 
                           data = simple_design, 
                           block = ~ run, 
                           sampling_frame = sframe_single_run)
print(emodel_pmod)
# Column names: stim_stim.face, ..., stim_stim.object, RT_centered_RT_centered

# Plot the RT parametric modulator term (using its term tag)
plot(emodel_pmod, term_name = "RT_centered") 
plotly::ggplotly(plot(emodel_pmod, term_name = "RT_centered"))

## ----create_pmod_interaction, fig.alt="Interaction of factor levels with parametric modulator plotted over time."----
emodel_pmod_int <- event_model(onset ~ hrf(stim) + hrf(stim, RT_centered), 
                               data = simple_design, 
                               block = ~ run, 
                               sampling_frame = sframe_single_run)
print(emodel_pmod_int)
# Columns: stim_stim.face, ..., stim_RT_centered_stim.face_RT_centered, ...

# Plot the interaction term (using its term tag)
plot(emodel_pmod_int, term_name = "stim_RT_centered")
plotly::ggplotly(plot(emodel_pmod_int, term_name = "stim_RT_centered"))

## ----list_hrfs----------------------------------------------------------------
# List basic information about available HRFs
list_available_hrfs()

# Get detailed descriptions
list_available_hrfs(details = TRUE)

## ----specify_hrf, fig.alt="Regressors generated using different HRF bases (SPMG, Gaussian, spline/tent)."----
# Example 1: Using basis name string "gaussian"
# Term tags: "stim", "RT_centered"
emodel_diff_hrf <- event_model(onset ~ hrf(stim, basis="spmg1") + hrf(RT_centered, basis="gaussian"), 
                               data = simple_design, 
                               block = ~ run, 
                               sampling_frame = sframe_single_run)
print(emodel_diff_hrf)
plot(emodel_diff_hrf, term_name = "RT_centered") # Plot the Gaussian RT regressor

# Example 2: Using a pre-defined HRF object (SPMG3)
# Term tag: "stim"
emodel_spmg3 <- event_model(onset ~ hrf(stim, basis=HRF_SPMG3), 
                            data = simple_design, 
                            block = ~ run, 
                            sampling_frame = sframe_single_run)
print(emodel_spmg3) # Note nbasis=3 for the hrf
# Columns: stim_stim.face_b01, stim_stim.face_b02, ...
plot(emodel_spmg3, term_name = "stim") # Plotting shows the 3 basis functions for one condition
plotly::ggplotly(plot(emodel_spmg3)) # Better for exploring many conditions

# Example 3: Using a custom function (simple linear ramp)
# Term tag: "stim"
linear_ramp_hrf <- function(t) { ifelse(t > 0 & t < 10, t/10, 0) }
emodel_custom_hrf <- event_model(onset ~ hrf(stim, basis=linear_ramp_hrf), 
                                 data = simple_design, 
                                 block = ~ run, 
                                 sampling_frame = sframe_single_run)
print(emodel_custom_hrf)
plot(emodel_custom_hrf, term_name = "stim")

# Example 4: Using gen_hrf() to create a lagged Gaussian
# Term tag: "stim"
lagged_gauss <- gen_hrf(hrf_gaussian, lag = 2, name = "Lagged Gaussian")
emodel_gen_hrf <- event_model(onset ~ hrf(stim, basis = lagged_gauss), 
                              data = simple_design, 
                              block = ~ run, 
                              sampling_frame = sframe_single_run)
print(emodel_gen_hrf)
plot(emodel_gen_hrf, term_name = "stim")

## ----create_trialwise, fig.alt="Trialwise regressors (one per trial) plotted over time with optional mean column."----
emodel_trialwise <- event_model(onset ~ trialwise(), 
                                data = simple_design, 
                                block = ~ run, 
                                sampling_frame = sframe_single_run)
print(emodel_trialwise)
# Term tag: "trialwise"
# Columns: trialwise_trial.1, trialwise_trial.2, ...

# Plotting trialwise models can be very dense!
# It generates one condition per trial.
plot(emodel_trialwise)
plotly::ggplotly(plot(emodel_trialwise)) # Use plotly to explore

## ----covariates_example-------------------------------------------------------

# Create covariates aligned to the sampling frame
n_scans <- sum(blocklens(sframe_single_run))
motion <- data.frame(mx = rnorm(n_scans), my = rnorm(n_scans))

# Build a model with both convolved events and non-convolved covariates
emodel_cov <- event_model(onset ~ hrf(stim) + covariate(mx, my, data = motion, id = "motion"),
                          data = simple_design,
                          block = ~ run,
                          sampling_frame = sframe_single_run)
print(emodel_cov)

# Inspect columns; motion terms are added as-is
head(colnames(design_matrix(emodel_cov)))

## ----access_components--------------------------------------------------------
# List the terms in the model (names are now term tags)
terms_list <- terms(emodel_pmod_int)
print(names(terms_list))
# Expected: "stim", "stim_RT_centered"

# Get all condition names (full column names)
conds <- conditions(emodel_pmod_int)
# Example: stim_stim.face, stim_stim.scene, ..., 
#          stim_RT_centered_stim.face_RT_centered, ...
cat("\nFirst 6 column names:", head(colnames(design_matrix(emodel_pmod_int)), 6), "...\n")

# Extract the full design matrix
dmat_events <- design_matrix(emodel_pmod_int)
cat("\nEvent design matrix dimensions:", dim(dmat_events), "\n")
head(dmat_events[, 1:6])

# Contrast weights and F-contrasts can also be extracted (see contrasts vignette)
contrast_weights(emodel_pmod_int)
Fcontrasts(emodel_pmod_int)

## ----matrix_viz, fig.alt="Heatmap visualization of event-model design matrix columns across scans."----
# Heatmap of the design matrix for the 2-factor model
design_map(emodel_two_factor, rotate_x_text = TRUE) +
  labs(title = "Design Matrix Heatmap (Two-Factor Model)")

# Correlation map for the same model
correlation_map(emodel_two_factor, rotate_x_text = TRUE) +
  labs(title = "Regressor Correlation Map (Two-Factor Model)")

# The column names in the plots will now reflect the new naming scheme.

## ----contrasts_quickstart-----------------------------------------------------
# Pairwise contrast between two stimulus levels
con_face_vs_scene <- pair_contrast(~ stim == "face", ~ stim == "scene", name = "face_vs_scene")

emodel_con <- event_model(onset ~ hrf(stim, contrasts = contrast_set(con_face_vs_scene)),
                          data = simple_design,
                          block = ~ run,
                          sampling_frame = sframe_single_run)

# List available contrast specs and weights
contr_specs <- contrasts(emodel_con)
cw <- contrast_weights(emodel_con)
names(cw)

# One-way contrast (main effect over levels of a factor)
con_main_stim <- oneway_contrast(~ stim, name = "main_stim")
emodel_con2 <- event_model(onset ~ hrf(stim, contrasts = contrast_set(con_main_stim)),
                           data = simple_design,
                           block = ~ run,
                           sampling_frame = sframe_single_run)

fcons <- Fcontrasts(emodel_con2)
names(fcons)

## ----combine_designs----------------------------------------------------------
# Baseline model for the same sampling frame
bmodel <- baseline_model(basis = "poly", degree = 5, sframe = sframe_two_factor)

# Combine columns (order can matter downstream; keep consistent)
DM_full <- dplyr::bind_cols(design_matrix(emodel_two_factor), design_matrix(bmodel))
dim(DM_full)
colnames(DM_full)[1:8]

