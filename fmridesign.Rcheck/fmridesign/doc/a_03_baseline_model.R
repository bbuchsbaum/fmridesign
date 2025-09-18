## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, 
  fig.height = 5,
  fig.retina = 1,
  message = FALSE,
  warning = FALSE
)
library(fmridesign)
library(dplyr)
library(ggplot2)
library(tidyr)
library(Matrix) # May be needed for sparse matrix ops internally

## ----basis_sets---------------------------------------------------------------
# Define a sampling frame for two runs of 100 scans each, TR=2s
TR <- 2
sframe <- sampling_frame(blocklens = c(100, 100), TR = TR)

# 1. Polynomial Basis (degree 5)
bmodel_poly <- baseline_model(basis = "poly", degree = 5, sframe = sframe)
print(bmodel_poly)

# 2. B-spline Basis (degree 5)
bmodel_bs <- baseline_model(basis = "bs", degree = 5, sframe = sframe)
print(bmodel_bs)

# 3. Natural Spline Basis (degree 5)
bmodel_ns <- baseline_model(basis = "ns", degree = 5, sframe = sframe)
print(bmodel_ns)

# 4. Constant Basis (Intercept only per run)
bmodel_const <- baseline_model(basis = "constant", sframe = sframe)
print(bmodel_const)

## ----plot_basis, fig.alt="Baseline drift regressors (poly, bs, ns) plotted per run over time."----
# Plot the polynomial regressors (term is named "drift")
plot(bmodel_poly, term_name = "drift")

# Plot the B-spline regressors
plot(bmodel_bs, term_name = "drift")

# Plot the Natural spline regressors
plot(bmodel_ns, term_name = "drift")

# Plotting the Constant (Intercept) term is generally not informative
print(bmodel_const) # Shows it has a 'constant' term

## ----nuisance_list, fig.alt="Example nuisance regressors (e.g., motion) plotted per run over time."----
# Simulate nuisance regressors (e.g., 6 motion parameters)
n_scans_run1 <- blocklens(sframe)[1]
n_scans_run2 <- blocklens(sframe)[2]

# Create nuisance data frames for each run
nuis_run1 <- as.data.frame(matrix(rnorm(n_scans_run1 * 6), n_scans_run1, 6))
names(nuis_run1) <- paste0("motion_", 1:6)

nuis_run2 <- as.data.frame(matrix(rnorm(n_scans_run2 * 6), n_scans_run2, 6))
names(nuis_run2) <- paste0("motion_", 1:6)

# Combine into a list
nuisance_regressors <- list(nuis_run1, nuis_run2)

# Create a baseline model including only these nuisance regressors
# (Set basis = NULL, degree = 0 to exclude drift terms)
bmodel_nuis_only <- baseline_model(basis = NULL, degree = 0, sframe = sframe, 
                                 nuisance_list = nuisance_regressors)
print(bmodel_nuis_only)

# Plot the nuisance regressors (term_name = "nuisance")
plot(bmodel_nuis_only, term_name = "nuisance") + 
  labs(title = "Nuisance Regressors Only (e.g., Motion)")

## ----combined_baseline, fig.alt="Combined baseline model plots: polynomial drift and nuisance terms per run."----
bmodel_combined <- baseline_model(basis = "poly", degree = 5, sframe = sframe, 
                                  nuisance_list = nuisance_regressors)
print(bmodel_combined)

# Check the terms included
term_names <- names(terms(bmodel_combined))
print(term_names) # e.g., constant, baseline_poly_5, nuisance
terms(bmodel_combined) # List all terms in the model

# Plot the drift terms (polynomial basis)
plot(bmodel_combined, term_name = "drift") + 
  labs(title = "Polynomial Drift Terms (from Combined Model)")

# Plot the nuisance terms (using exact match "nuisance")
plot(bmodel_combined, term_name = "nuisance") + 
  labs(title = "Nuisance Terms (from Combined Model)")

## ----design_matrix_baseline---------------------------------------------------
dmat_baseline <- design_matrix(bmodel_combined)

cat("Dimensions of baseline design matrix:", dim(dmat_baseline), "\n")
cat("Column names:", paste(colnames(dmat_baseline)[1:10], "..."), "\n")

head(dmat_baseline[, 1:8]) # Show first few columns

# Visualize the full baseline matrix as a heatmap (optional)
# design_map.baseline_model(bmodel_combined, rotate_x_text = TRUE)

