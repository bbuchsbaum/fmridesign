# ====================================================================
# Demonstration of Basis Function Filtering in Contrasts
# ====================================================================
#
# This script demonstrates the new `basis` argument in contrast specifications,
# which allows users to selectively test specific basis functions when using
# multi-basis HRFs (e.g., bspline, FIR, Fourier, polynomial).
#
# Author: Claude Code
# Date: 2025-01-19

# Load the development version with basis filtering support
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".")
} else {
  library(fmridesign)
}
library(fmrihrf)

# ====================================================================
# Setup: Create test data with multiple conditions
# ====================================================================

test_data <- data.frame(
  onset = seq(2, 120, by = 4),
  condition = rep(c("Face", "Scene", "Object"), 10),
  run = rep(1:2, each = 15)
)

# Create sampling frame (100 TRs, TR = 2s)
sframe <- sampling_frame(blocklens = c(50, 50), TR = 2)

# ====================================================================
# Example 1: Bspline HRF with 5 basis functions
# ====================================================================

cat("\n=== Example 1: Bspline HRF (5 basis functions) ===\n\n")

# Create event model with 5-basis bspline HRF
em_bspline <- event_model(
  onset ~ hrf(condition, basis = "bspline", nbasis = 5),
  data = test_data,
  block = ~ run,
  sampling_frame = sframe
)

# Show all expanded condition names
cat("All condition names (basis-expanded):\n")
print(conditions(em_bspline, expand_basis = TRUE))

# Test 1a: Compare Face vs Scene using ALL basis functions (default)
con_all <- pair_contrast(
  ~ condition == "Face",
  ~ condition == "Scene",
  name = "Face_vs_Scene_AllBasis"
)

cat("\nContrast 1a: Face vs Scene (all basis functions)\n")
weights_all <- contrast_weights(con_all, terms(em_bspline)[[1]])
print(weights_all$weights[weights_all$weights != 0, , drop = FALSE])

# Test 1b: Compare Face vs Scene using ONLY basis function 2
con_basis2 <- pair_contrast(
  ~ condition == "Face",
  ~ condition == "Scene",
  basis = 2,
  name = "Face_vs_Scene_Basis2"
)

cat("\nContrast 1b: Face vs Scene (basis 2 only)\n")
weights_b2 <- contrast_weights(con_basis2, terms(em_bspline)[[1]])
cat("Filtered condition names:", weights_b2$condnames, "\n")
print(weights_b2$weights[weights_b2$weights != 0, , drop = FALSE])

# Test 1c: Compare Face vs Scene using early response (basis 1-3)
con_early <- pair_contrast(
  ~ condition == "Face",
  ~ condition == "Scene",
  basis = 1:3,
  name = "Face_vs_Scene_Early"
)

cat("\nContrast 1c: Face vs Scene (early response: basis 1-3)\n")
weights_early <- contrast_weights(con_early, terms(em_bspline)[[1]])
cat("Filtered condition names:", weights_early$condnames, "\n")
print(weights_early$weights[weights_early$weights != 0, , drop = FALSE])

# Test 1d: Compare Face vs Scene using late response (basis 4-5)
con_late <- pair_contrast(
  ~ condition == "Face",
  ~ condition == "Scene",
  basis = 4:5,
  name = "Face_vs_Scene_Late"
)

cat("\nContrast 1d: Face vs Scene (late response: basis 4-5)\n")
weights_late <- contrast_weights(con_late, terms(em_bspline)[[1]])
cat("Filtered condition names:", weights_late$condnames, "\n")
print(weights_late$weights[weights_late$weights != 0, , drop = FALSE])

# ====================================================================
# Example 2: FIR (Finite Impulse Response) with 10 time bins
# ====================================================================

cat("\n\n=== Example 2: FIR HRF (10 time bins) ===\n\n")

# Create event model with 10-bin FIR
em_fir <- event_model(
  onset ~ hrf(condition, basis = "fir", nbasis = 10),
  data = test_data,
  block = ~ run,
  sampling_frame = sframe
)

cat("All condition names (FIR basis-expanded):\n")
print(head(conditions(em_fir, expand_basis = TRUE), 12))

# Test 2a: Test peak response window (bins 3-5, around 4-10 seconds post-stimulus)
con_peak <- pair_contrast(
  ~ condition == "Face",
  ~ condition == "Object",
  basis = 3:5,
  name = "Face_vs_Object_Peak"
)

cat("\nContrast 2a: Face vs Object (peak response: bins 3-5)\n")
weights_peak <- contrast_weights(con_peak, terms(em_fir)[[1]])
cat("Filtered condition names:", weights_peak$condnames, "\n")
print(weights_peak$weights[weights_peak$weights != 0, , drop = FALSE])

# ====================================================================
# Example 3: Using basis filtering with oneway_contrast
# ====================================================================

cat("\n\n=== Example 3: One-way contrast with basis filtering ===\n\n")

# Test main effect of condition using only first basis function
con_oneway_b1 <- oneway_contrast(
  ~ condition,
  basis = 1,
  name = "Main_Condition_Basis1"
)

cat("One-way contrast for condition (basis 1 only)\n")
# Note: oneway_contrast generates F-contrast with multiple columns
weights_oneway <- contrast_weights(con_oneway_b1, terms(em_bspline)[[1]])
cat("This is an F-contrast with", ncol(weights_oneway$weights), "columns\n")
cat("Filtered condition names (first 10):\n")
print(head(weights_oneway$condnames, 10))
cat("\nSample of weights (non-zero rows):\n")
non_zero_rows <- apply(weights_oneway$weights, 1, function(x) any(x != 0))
print(head(weights_oneway$weights[non_zero_rows, , drop = FALSE], 10))

# Test main effect using early response (basis 1-3)
con_oneway_early <- oneway_contrast(
  ~ condition,
  basis = 1:3,
  name = "Main_Condition_Early"
)

cat("\nOne-way contrast for condition (early response: basis 1-3)\n")
weights_oneway_early <- contrast_weights(con_oneway_early, terms(em_bspline)[[1]])
cat("Filtered condition names (first 15):\n")
print(head(weights_oneway_early$condnames, 15))

# ====================================================================
# Example 4: Practical use case - testing temporal dynamics
# ====================================================================

cat("\n\n=== Example 4: Testing temporal dynamics ===\n\n")

# Hypothesis: Face processing has a sustained response while Scene processing
# has a transient response. Test this by comparing early vs late components.

# Early response for Face
con_face_early <- pair_contrast(
  ~ condition == "Face",
  ~ condition == "Object",  # Use Object as baseline
  basis = 1:2,
  name = "Face_vs_Object_Early"
)

# Late response for Face
con_face_late <- pair_contrast(
  ~ condition == "Face",
  ~ condition == "Object",
  basis = 4:5,
  name = "Face_vs_Object_Late"
)

cat("Face vs Object (early response):\n")
w_early <- contrast_weights(con_face_early, terms(em_bspline)[[1]])
cat("Conditions tested:", w_early$condnames, "\n")

cat("\nFace vs Object (late response):\n")
w_late <- contrast_weights(con_face_late, terms(em_bspline)[[1]])
cat("Conditions tested:", w_late$condnames, "\n")

cat("\nInterpretation: If both contrasts are significant with similar effect sizes,\n")
cat("this suggests a sustained response. If only early is significant, this\n")
cat("suggests a transient response.\n")

# ====================================================================
# Example 5: Polynomial contrast with basis filtering
# ====================================================================

cat("\n\n=== Example 5: Polynomial contrast with basis filtering ===\n\\n")

# Create test data with ordinal stimulus levels
test_data_ord <- data.frame(
  onset = seq(2, 120, by = 4),
  intensity = rep(c("low", "medium", "high"), 10),
  run = rep(1:2, each = 15)
)

# Create event model with bspline HRF
em_ord <- event_model(
  onset ~ hrf(intensity, basis = "bspline", nbasis = 5),
  data = test_data_ord,
  block = ~ run,
  sampling_frame = sframe
)

# Test linear trend across intensity levels using only first basis function
con_poly_b1 <- poly_contrast(
  ~ intensity,
  degree = 1,  # Linear trend
  value_map = list(low = 1, medium = 2, high = 3),
  basis = 1,
  name = "Intensity_Linear_Basis1"
)

cat("Polynomial contrast (linear trend, basis 1 only):\n")
weights_poly <- contrast_weights(con_poly_b1, terms(em_ord)[[1]])
cat("Filtered condition names:", weights_poly$condnames, "\n")
cat("\nNon-zero weights:\n")
non_zero_rows_poly <- apply(weights_poly$weights, 1, function(x) any(x != 0))
print(weights_poly$weights[non_zero_rows_poly, , drop = FALSE])

# Test quadratic trend using early response (basis 1-2)
con_poly_early <- poly_contrast(
  ~ intensity,
  degree = 2,  # Quadratic trend
  value_map = list(low = 1, medium = 2, high = 3),
  basis = 1:2,
  name = "Intensity_Quadratic_Early"
)

cat("\nPolynomial contrast (quadratic trend, early response: basis 1-2):\n")
weights_poly_early <- contrast_weights(con_poly_early, terms(em_ord)[[1]])
cat("Filtered condition names:", weights_poly_early$condnames, "\n")

# ====================================================================
# Example 6: Using basis_weights for non-uniform weighting
# ====================================================================

cat("\n\n=== Example 6: Basis Weighting (basis_weights) ===\n\n")

# Basis weighting allows you to apply non-uniform weights to selected basis functions
# This is useful for emphasizing specific temporal components

# Test 6a: Gaussian-like weighting emphasizing peak response
con_weighted_peak <- pair_contrast(
  ~ condition == "Face",
  ~ condition == "Scene",
  basis = 1:5,
  basis_weights = c(0.1, 0.2, 0.4, 0.2, 0.1),  # Peak at basis 3
  name = "Face_vs_Scene_PeakWeighted"
)

cat("Contrast 6a: Face vs Scene with Gaussian weighting (peak at basis 3)\n")
weights_peak <- contrast_weights(con_weighted_peak, terms(em_bspline)[[1]])
cat("Weights sum to 1:", sum(c(0.1, 0.2, 0.4, 0.2, 0.1)), "\n")
cat("Filtered condition names (first 6):", head(weights_peak$condnames), "\n")

# Test 6b: Early response weighting
con_weighted_early <- pair_contrast(
  ~ condition == "Face",
  ~ condition == "Object",
  basis = 1:3,
  basis_weights = c(0.5, 0.3, 0.2),  # Emphasize early basis
  name = "Face_vs_Object_EarlyWeighted"
)

cat("\nContrast 6b: Face vs Object with early response weighting\n")
weights_early_w <- contrast_weights(con_weighted_early, terms(em_bspline)[[1]])
cat("Filtered condition names:", weights_early_w$condnames, "\n")

# Test 6c: Auto-normalization (weights that don't sum to 1)
cat("\nContrast 6c: Auto-normalization demo\n")
con_unnorm <- pair_contrast(
  ~ condition == "Face",
  ~ condition == "Scene",
  basis = 1:2,
  basis_weights = c(3, 1),  # Will be normalized to c(0.75, 0.25)
  name = "Face_vs_Scene_Unnormalized"
)

cat("Input weights: c(3, 1)\n")
cat("Will be normalized to: c(0.75, 0.25)\n")
# This should trigger a warning
suppressWarnings(weights_unnorm <- contrast_weights(con_unnorm, terms(em_bspline)[[1]]))
cat("(Warning about normalization would appear above)\n")

# ====================================================================
# Summary
# ====================================================================

cat("\n\n=== SUMMARY ===\n\n")
cat("The `basis` and `basis_weights` arguments in contrast specifications allow you to:\n")
cat("1. Test specific temporal components of the HRF (basis)\n")
cat("2. Apply non-uniform weights to basis functions (basis_weights)\n")
cat("3. Separate early vs late response components\n")
cat("4. Focus on theoretically-motivated time windows\n")
cat("5. Emphasize peak or sustained responses\n")
cat("6. Reduce dimensionality when testing hypotheses\n\n")

cat("Usage patterns for basis:\n")
cat("  basis = NULL      # Default: use all basis functions\n")
cat("  basis = 1         # Test only first basis function\n")
cat("  basis = 2:4       # Test basis functions 2, 3, and 4\n")
cat("  basis = c(1,5)    # Test basis functions 1 and 5\n")
cat("  basis = 'all'     # Explicit: use all basis functions\n\n")

cat("Usage patterns for basis_weights:\n")
cat("  basis_weights = NULL               # Default: equal weights (uniform)\n")
cat("  basis_weights = c(0.5, 0.3, 0.2)  # Custom weights (auto-normalized to sum to 1)\n")
cat("  basis_weights = c(0.1, 0.8, 0.1)  # Emphasize middle basis\n")
cat("  basis_weights = c(0.7, 0.2, 0.1)  # Emphasize early basis\n\n")

cat("Both arguments work with:\n")
cat("  - pair_contrast()    ✓\n")
cat("  - oneway_contrast()  ✓\n")
cat("  - poly_contrast()    ✓\n\n")

cat("Demo complete!\n")
