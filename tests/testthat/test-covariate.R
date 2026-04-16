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
  expect_equal(cv$name, "x")
})

test_that("covariate() with prefix prepends to variable names", {
  cov_data <- data.frame(x = rnorm(80), y = rnorm(80))
  cv <- covariate(x, y, data = cov_data, prefix = "motion")

  expect_equal(cv$varnames, c("motion_x", "motion_y"))
  expect_equal(cv$name, "motion_x::motion_y")
})

test_that("covariate() with id sets custom identifier", {
  cov_data <- data.frame(x = rnorm(80))
  cv <- covariate(x, data = cov_data, id = "my_covariate")

  expect_equal(cv$id, "my_covariate")
})

test_that("covariate() with default id uses termname", {
  cov_data <- data.frame(x = rnorm(80), y = rnorm(80))
  cv <- covariate(x, y, data = cov_data)

  expect_equal(cv$id, cv$name)
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
  expect_true(all(c("x", "y") %in% names(dm)))
})

test_that("construct.covariatespec() with prefix has correct column names", {
  cov_data <- data.frame(x = rnorm(80), y = rnorm(80))
  cv <- covariate(x, y, data = cov_data, prefix = "motion")

  model_spec <- list(sampling_frame = sframe)
  result <- construct(cv, model_spec)

  dm <- result$design_matrix
  expect_true(all(c("motion_x", "motion_y") %in% names(dm)))
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
})

# ── nbasis.covariate_convolved_term ─────────────────────────────────────────

test_that("nbasis for covariate_convolved_term returns column count", {
  cov_data <- data.frame(x = rnorm(80), y = rnorm(80), z = rnorm(80))
  cv <- covariate(x, y, z, data = cov_data)

  model_spec <- list(sampling_frame = sframe)
  result <- construct(cv, model_spec)

  expect_equal(nbasis(result), 3)
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
})
