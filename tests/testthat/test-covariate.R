# test-covariate.R
#
# Tests for covariate(), covariatespec, covariate_term, and
# construct.covariatespec in covariate.R.

library(testthat)

testthat::local_edition(3)

# ── Standard setup ──────────────────────────────────────────────────────────

des <- data.frame(
  onset = c(0, 10, 20, 30, 5, 15, 25, 35),
  run   = c(1, 1, 1, 1, 2, 2, 2, 2),
  cond  = factor(c("A", "B", "A", "B", "A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 1)

# ── covariate() constructor ────────────────────────────────────────────────

test_that("covariate() creates a covariatespec with correct class", {
  cov_data <- data.frame(x = rnorm(80), y = rnorm(80))
  cv <- covariate(x, y, data = cov_data)

  expect_s3_class(cv, "covariatespec")
  expect_s3_class(cv, "hrfspec")
  expect_s3_class(cv, "list")
})

test_that("covariate() stores correct variable names", {
  cov_data <- data.frame(motion_x = rnorm(80), motion_y = rnorm(80))
  cv <- covariate(motion_x, motion_y, data = cov_data)

  expect_equal(cv$vars, c("motion_x", "motion_y"))
  expect_equal(cv$varnames, c("motion_x", "motion_y"))
  expect_equal(cv$label, "covariate(motion_x,motion_y)")
})

test_that("covariate() with single variable", {
  cov_data <- data.frame(x = rnorm(80))
  cv <- covariate(x, data = cov_data)

  expect_equal(cv$vars, "x")
  expect_equal(cv$varnames, "x")
  expect_equal(cv$name, "cov")
  expect_equal(cv$id, "cov")
})

test_that("covariate() uses prefix as its term tag", {
  cov_data <- data.frame(x = rnorm(80), y = rnorm(80))
  cv <- covariate(x, y, data = cov_data, prefix = "motion")

  expect_equal(cv$varnames, c("x", "y"))
  expect_equal(cv$name, "motion")
  expect_null(cv$id)
  expect_equal(cv$prefix, "motion")
})

test_that("covariate() with id sets custom identifier", {
  cov_data <- data.frame(x = rnorm(80))
  cv <- covariate(x, data = cov_data, id = "my_covariate")

  expect_equal(cv$id, "my_covariate")
})

test_that("covariate() uses a compact default term id", {
  cov_data <- data.frame(x = rnorm(80), y = rnorm(80))
  cv <- covariate(x, y, data = cov_data)

  expect_equal(cv$id, "cov")
  expect_equal(cv$name, "cov")
})

test_that("covariate() stores data reference", {
  cov_data <- data.frame(x = rnorm(80))
  cv <- covariate(x, data = cov_data)

  expect_true(is.data.frame(cv$data))
  expect_equal(nrow(cv$data), 80)
})

# ── construct.covariatespec() ──────────────────────────────────────────────

test_that("construct.covariatespec() produces covariate_convolved_term", {
  cov_data <- data.frame(x = rnorm(80), y = rnorm(80))
  cv <- covariate(x, y, data = cov_data)

  model_spec <- list(
    sampling_frame = sframe
  )

  result <- construct(cv, model_spec)
  expect_s3_class(result, "covariate_convolved_term")
  expect_s3_class(result, "convolved_term")
  expect_s3_class(result, "fmri_term")
})

test_that("construct.covariatespec() design matrix has correct dimensions", {
  cov_data <- data.frame(x = rnorm(80), y = rnorm(80))
  cv <- covariate(x, y, data = cov_data)

  model_spec <- list(sampling_frame = sframe)
  result <- construct(cv, model_spec)

  dm <- result$design_matrix
  expect_equal(nrow(dm), 80)
  expect_equal(ncol(dm), 2)
  expect_equal(names(dm), c("cov_x", "cov_y"))
})

test_that("construct.covariatespec() with prefix has correct column names", {
  cov_data <- data.frame(x = rnorm(80), y = rnorm(80))
  cv <- covariate(x, y, data = cov_data, prefix = "motion")

  model_spec <- list(sampling_frame = sframe)
  result <- construct(cv, model_spec)

  dm <- result$design_matrix
  expect_equal(names(dm), c("motion_x", "motion_y"))
})

test_that("construct.covariatespec() expands a named matrix column", {
  Mx <- cbind(
    lag_m2 = seq_len(80),
    lag_0 = seq_len(80) + 100,
    lag_p2 = seq_len(80) + 200
  )
  cov_data <- data.frame(row = seq_len(80))
  cov_data$Mx <- Mx

  result <- construct(
    covariate(Mx, data = cov_data),
    list(sampling_frame = sframe)
  )

  expect_equal(names(result$design_matrix),
               c("cov_lag_m2", "cov_lag_0", "cov_lag_p2"))
  expect_equal(unname(as.matrix(result$design_matrix)), unname(Mx))
  expect_equal(result$condition_tags, c("lag_m2", "lag_0", "lag_p2"))
  expect_equal(result$source_map$source_arg, rep("Mx", 3))
  expect_equal(result$source_map$source_column,
               c("lag_m2", "lag_0", "lag_p2"))
})

test_that("construct.covariatespec() names an unnamed matrix with feature suffixes", {
  Mx <- matrix(seq_len(240), nrow = 80, ncol = 3)
  cov_data <- data.frame(row = seq_len(80))
  cov_data$Mx <- Mx

  result <- construct(
    covariate(Mx, data = cov_data),
    list(sampling_frame = sframe)
  )

  expect_equal(names(result$design_matrix), c("cov_f01", "cov_f02", "cov_f03"))
  expect_equal(result$condition_tags, c("f01", "f02", "f03"))
})

test_that("construct.covariatespec() expands a nested numeric data frame", {
  features <- data.frame(low = seq_len(80), high = seq_len(80) + 80)
  cov_data <- data.frame(row = seq_len(80))
  cov_data$features <- I(features)

  result <- construct(
    covariate(features, data = cov_data),
    list(sampling_frame = sframe)
  )

  expect_equal(names(result$design_matrix), c("cov_low", "cov_high"))
  expect_equal(unname(as.matrix(result$design_matrix)), unname(as.matrix(features)))
})

test_that("duplicate matrix column names use deterministic feature suffixes", {
  Mx <- matrix(seq_len(160), nrow = 80, ncol = 2,
               dimnames = list(NULL, c("dup", "dup")))
  cov_data <- data.frame(row = seq_len(80))
  cov_data$Mx <- Mx

  result <- construct(
    covariate(Mx, data = cov_data),
    list(sampling_frame = sframe)
  )

  expect_equal(names(result$design_matrix), c("cov_f01", "cov_f02"))
})

test_that("matrix covariates obey id and prefix term-tag conventions", {
  Mx <- cbind(alpha = seq_len(80), beta = seq_len(80) + 80)
  cov_data <- data.frame(row = seq_len(80))
  cov_data$Mx <- Mx

  with_id <- construct(
    covariate(Mx, data = cov_data, id = "alignment"),
    list(sampling_frame = sframe)
  )
  with_prefix <- construct(
    covariate(Mx, data = cov_data, prefix = "acoustic"),
    list(sampling_frame = sframe)
  )

  expect_equal(names(with_id$design_matrix), c("alignment_alpha", "alignment_beta"))
  expect_equal(names(with_prefix$design_matrix), c("acoustic_alpha", "acoustic_beta"))
})

test_that("vectors and matrices can be mixed in one covariate term", {
  Mx <- cbind(pc1 = seq_len(80), pc2 = seq_len(80) + 80)
  cov_data <- data.frame(drift = seq_len(80) / 80)
  cov_data$Mx <- Mx

  result <- construct(
    covariate(drift, Mx, data = cov_data),
    list(sampling_frame = sframe)
  )

  expect_equal(names(result$design_matrix), c("cov_drift", "cov_pc1", "cov_pc2"))
  expect_equal(result$condition_tags, c("drift", "pc1", "pc2"))
})

test_that("construct.covariatespec() errors on row count mismatch", {
  cov_data <- data.frame(x = rnorm(50))  # 50 rows != 80 expected
  cv <- covariate(x, data = cov_data)

  model_spec <- list(sampling_frame = sframe)
  expect_error(construct(cv, model_spec), "sampling_frame expects")
})

test_that("construct.covariatespec() respects sampling_frame override", {
  sf_short <- fmrihrf::sampling_frame(blocklens = c(30), TR = 1)
  cov_data <- data.frame(x = rnorm(30))
  cv <- covariate(x, data = cov_data)

  model_spec <- list(sampling_frame = sframe)  # 80 total

  # Override with shorter frame should succeed
  result <- construct(cv, model_spec, sampling_frame = sf_short)
  expect_equal(nrow(result$design_matrix), 30)
})

# ── Using covariate in event_model ─────────────────────────────────────────

test_that("covariate works in event_model formula interface", {
  cov_data <- data.frame(motion_x = rnorm(80), motion_y = rnorm(80))

  emod <- event_model(
    onset ~ hrf(cond) + covariate(motion_x, motion_y, data = cov_data),
    data = des,
    block = ~run,
    sampling_frame = sframe
  )

  expect_s3_class(emod, "event_model")
  dm <- design_matrix(emod)
  # Should have HRF columns for cond (A, B) plus 2 covariate columns
  expect_true(ncol(dm) >= 4)
  expect_true(all(c("cov_motion_x", "cov_motion_y") %in% names(dm)))
})

test_that("covariate with prefix in event_model", {
  cov_data <- data.frame(x = rnorm(80), y = rnorm(80))

  emod <- event_model(
    onset ~ hrf(cond) + covariate(x, y, data = cov_data, prefix = "nuisance"),
    data = des,
    block = ~run,
    sampling_frame = sframe
  )

  dm <- design_matrix(emod)
  cnames <- colnames(dm)
  expect_true(any(grepl("nuisance_x", cnames)))
  expect_true(any(grepl("nuisance_y", cnames)))
})

test_that("covariate with id in event_model", {
  cov_data <- data.frame(x = rnorm(80))

  emod <- event_model(
    onset ~ hrf(cond) + covariate(x, data = cov_data, id = "motion"),
    data = des,
    block = ~run,
    sampling_frame = sframe
  )

  expect_s3_class(emod, "event_model")
  dm <- design_matrix(emod)
  expect_true(ncol(dm) >= 3)
  expect_true("motion_x" %in% names(dm))
})

test_that("standalone covariate model (no hrf terms)", {
  cov_data <- data.frame(x = rnorm(80), y = rnorm(80))

  emod <- event_model(
    onset ~ covariate(x, y, data = cov_data),
    data = des,
    block = ~run,
    sampling_frame = sframe
  )

  dm <- design_matrix(emod)
  expect_equal(ncol(dm), 2)
  expect_equal(nrow(dm), 80)
  expect_equal(names(dm), c("cov_x", "cov_y"))
})

# ── nbasis.covariate_convolved_term ─────────────────────────────────────────

test_that("nbasis for covariate_convolved_term is one per regressor", {
  cov_data <- data.frame(x = rnorm(80), y = rnorm(80), z = rnorm(80))
  cv <- covariate(x, y, z, data = cov_data)

  model_spec <- list(sampling_frame = sframe)
  result <- construct(cv, model_spec)

  expect_equal(nbasis(result), 1L)
})

# ── event_table.covariate_convolved_term ────────────────────────────────────

test_that("event_table for covariate_convolved_term returns tibble", {
  cov_data <- data.frame(x = rnorm(80), y = rnorm(80))
  cv <- covariate(x, y, data = cov_data)

  model_spec <- list(sampling_frame = sframe)
  result <- construct(cv, model_spec)

  et <- event_table(result)
  expect_s3_class(et, "tbl_df")
  expect_equal(nrow(et), 80)
  expect_equal(ncol(et), 2)
  expect_equal(names(et), c("cov_x", "cov_y"))
})

test_that("covariate condition accessors expose base regressor tags", {
  cov_data <- data.frame(x = rnorm(80), y = rnorm(80))
  result <- construct(
    covariate(x, y, data = cov_data),
    list(sampling_frame = sframe)
  )

  expect_equal(conditions(result), c("x", "y"))
  expect_equal(conditions(result$evterm), c("x", "y"))
  expect_equal(shortnames(result), c("x", "y"))
  expect_equal(shortnames(result$evterm), c("x", "y"))
  expect_equal(longnames(result), c("x", "y"))
  expect_equal(longnames(result$evterm), c("x", "y"))
  expect_equal(
    condition_map(result),
    tibble::tibble(display = c("x", "y"), canonical = c("x", "y"))
  )
})

test_that("covariate design metadata separates term and regressor identity", {
  cov_data <- data.frame(x = rnorm(80), y = rnorm(80))
  emod <- event_model(
    onset ~ covariate(x, y, data = cov_data),
    data = des,
    block = ~run,
    sampling_frame = sframe
  )

  cmap <- design_colmap(emod)
  expect_equal(cmap$name, c("cov_x", "cov_y"))
  expect_equal(cmap$term_tag, c("cov", "cov"))
  expect_equal(cmap$condition, c("x", "y"))
  expect_equal(cmap$modulation_type, c("covariate", "covariate"))
  expect_equal(cmap$modulation_id, c("x", "y"))
  expect_equal(cmap$pretty_name, c("x", "y"))
})
