library(testthat)
library(fmridesign)

# Tests for per-onset HRF specification via hrf_fun parameter

test_that("hrf_fun with function generates per-onset HRFs", {
  # Simple case: different HRFs per event using a function generator
  des <- data.frame(
    onset = c(0, 10, 20),
    duration = c(0, 0, 0),
    condition = factor(c("A", "B", "A")),
    run = 1
  )
  sf <- fmrihrf::sampling_frame(blocklens = 40, TR = 2)

  # Generator that returns different HRFs based on condition
  cond_hrf_gen <- function(d) {
    lapply(seq_len(nrow(d)), function(i) {
      if (d$condition[i] == "A") fmrihrf::HRF_SPMG1 else fmrihrf::HRF_GAMMA
    })
  }

  emod <- event_model(
    onset ~ hrf(condition, hrf_fun = cond_hrf_gen),
    data = des, block = ~run, sampling_frame = sf
  )

 expect_s3_class(emod, "event_model")
  dm <- design_matrix(emod)
  expect_true(ncol(dm) > 0)
  expect_equal(nrow(dm), 40)  # blocklens=40 samples
})


test_that("hrf_fun with formula extracts HRF column", {
  # Create data with a column containing HRF objects
  des <- data.frame(
    onset = c(0, 10, 20),
    condition = factor(c("A", "B", "A")),
    run = 1
  )
  # Add a list column containing HRF objects
  des$my_hrfs <- list(fmrihrf::HRF_SPMG1, fmrihrf::HRF_GAMMA, fmrihrf::HRF_SPMG1)

  sf <- fmrihrf::sampling_frame(blocklens = 40, TR = 2)

  # Use formula syntax to reference the HRF column
  emod <- event_model(
    onset ~ hrf(condition, hrf_fun = ~my_hrfs),
    data = des, block = ~run, sampling_frame = sf
  )

  expect_s3_class(emod, "event_model")
  dm <- design_matrix(emod)
  expect_true(ncol(dm) > 0)
  expect_equal(nrow(dm), 40)
})


test_that("hrf_fun respects subset parameter", {
  # Generator should only receive post-subset events
  n_received <- 0
  tracking_gen <- function(d) {
    n_received <<- nrow(d)
    lapply(seq_len(nrow(d)), function(i) fmrihrf::HRF_SPMG1)
  }

  des <- data.frame(
    onset = c(0, 10, 20),
    condition = c("A", "B", "A"),
    run = 1
  )
  sf <- fmrihrf::sampling_frame(blocklens = 40, TR = 2)

  emod <- event_model(
    onset ~ hrf(condition, hrf_fun = tracking_gen, subset = condition == "A"),
    data = des, block = ~run, sampling_frame = sf
  )

  # Only 2 "A" events should be received by generator
  expect_equal(n_received, 2)
})


test_that("boxcar_hrf_gen creates duration-based HRFs", {
  skip_if_not_installed("fmrihrf")

  if (!exists("hrf_boxcar", envir = asNamespace("fmrihrf"))) {
    skip("fmrihrf::hrf_boxcar not available")
  }

  des <- data.frame(
    onset = c(0, 15),
    duration = c(2, 5),
    condition = c("A", "B"),
    run = 1
  )
  sf <- fmrihrf::sampling_frame(blocklens = 40, TR = 2)

  emod <- event_model(
    onset ~ hrf(condition, hrf_fun = boxcar_hrf_gen()),
    data = des, block = ~run, sampling_frame = sf
  )

  expect_s3_class(emod, "event_model")
  dm <- design_matrix(emod)
  expect_true(ncol(dm) > 0)
})


test_that("weighted_hrf_gen creates weighted impulse HRFs", {
  skip_if_not_installed("fmrihrf")

  # Check if hrf_weighted exists in fmrihrf
  if (!exists("hrf_weighted", envir = asNamespace("fmrihrf"))) {
    skip("fmrihrf::hrf_weighted not available")
  }

  # Create data with list columns for sub-event times and weights
  # The hrf_fun generator accesses these via original_data attached to event_term
  des <- data.frame(
    onset = c(0, 20),
    condition = c("A", "B"),  # Simple term variable
    run = 1
  )
  # Add list columns for weighted HRF parameters
  des$sub_times <- I(list(c(0, 1, 2), c(0, 3, 6)))
  des$sub_weights <- I(list(c(0.2, 0.5, 0.3), c(0.1, 0.6, 0.3)))

  sf <- fmrihrf::sampling_frame(blocklens = 50, TR = 2)

  # Use a simple condition as the term variable, and let weighted_hrf_gen
  # access sub_times and sub_weights from the original_data attribute
  emod <- event_model(
    onset ~ hrf(condition, hrf_fun = weighted_hrf_gen("sub_times", "sub_weights", relative = TRUE)),
    data = des, block = ~run, sampling_frame = sf
  )

  expect_s3_class(emod, "event_model")
  dm <- design_matrix(emod)
  expect_true(ncol(dm) > 0)
  expect_equal(nrow(dm), 50)
})


test_that("hrf_fun validation catches incorrect return values", {
  des <- data.frame(
    onset = c(0, 10),
    condition = c("A", "B"),
    run = 1
  )
  sf <- fmrihrf::sampling_frame(blocklens = 30, TR = 2)

  # Generator that returns wrong type
  bad_gen <- function(d) {
    "not an HRF"
  }

  expect_error(
    event_model(
      onset ~ hrf(condition, hrf_fun = bad_gen),
      data = des, block = ~run, sampling_frame = sf
    ),
    "HRF"
  )
})


test_that("hrf_fun validation catches length mismatch", {
  des <- data.frame(
    onset = c(0, 10, 20),
    condition = c("A", "B", "C"),
    run = 1
  )
  sf <- fmrihrf::sampling_frame(blocklens = 40, TR = 2)

  # Generator that returns wrong number of HRFs
  wrong_len_gen <- function(d) {
    list(fmrihrf::HRF_SPMG1, fmrihrf::HRF_SPMG1)  # Only 2, but 3 events
  }

  expect_error(
    event_model(
      onset ~ hrf(condition, hrf_fun = wrong_len_gen),
      data = des, block = ~run, sampling_frame = sf
    ),
    "HRF"
  )
})


test_that("hrf_fun validation catches inconsistent nbasis", {
  des <- data.frame(
    onset = c(0, 10),
    condition = c("A", "B"),
    run = 1
  )
  sf <- fmrihrf::sampling_frame(blocklens = 30, TR = 2)

  # Generator that returns HRFs with different nbasis
  mixed_nbasis_gen <- function(d) {
    list(fmrihrf::HRF_SPMG1, fmrihrf::HRF_SPMG2)  # 1 basis vs 2 basis
  }

  expect_error(
    event_model(
      onset ~ hrf(condition, hrf_fun = mixed_nbasis_gen),
      data = des, block = ~run, sampling_frame = sf
    ),
    "nbasis"
  )
})


test_that("hrf_fun with single HRF return is recycled", {
  des <- data.frame(
    onset = c(0, 10, 20),
    condition = c("A", "B", "A"),
    run = 1
  )
  sf <- fmrihrf::sampling_frame(blocklens = 40, TR = 2)

  # Generator that returns a single HRF (should be recycled)
  single_hrf_gen <- function(d) {
    fmrihrf::HRF_GAMMA
  }

  emod <- event_model(
    onset ~ hrf(condition, hrf_fun = single_hrf_gen),
    data = des, block = ~run, sampling_frame = sf
  )

  expect_s3_class(emod, "event_model")
  dm <- design_matrix(emod)
  expect_true(ncol(dm) > 0)
})


test_that("hrf_fun with list of length 1 is recycled", {
  des <- data.frame(
    onset = c(0, 10, 20),
    condition = c("A", "B", "A"),
    run = 1
  )
  sf <- fmrihrf::sampling_frame(blocklens = 40, TR = 2)

  # Generator that returns list of length 1
  list_one_gen <- function(d) {
    list(fmrihrf::HRF_GAMMA)
  }

  emod <- event_model(
    onset ~ hrf(condition, hrf_fun = list_one_gen),
    data = des, block = ~run, sampling_frame = sf
  )

  expect_s3_class(emod, "event_model")
})


test_that("hrf_fun works with multi-block designs", {
  des <- data.frame(
    onset = c(0, 10, 0, 10),
    condition = c("A", "B", "A", "B"),
    run = c(1, 1, 2, 2)
  )
  sf <- fmrihrf::sampling_frame(blocklens = c(30, 30), TR = 2)

  # Generator that uses onset to vary HRF
  block_aware_gen <- function(d) {
    lapply(seq_len(nrow(d)), function(i) fmrihrf::HRF_SPMG1)
  }

  emod <- event_model(
    onset ~ hrf(condition, hrf_fun = block_aware_gen),
    data = des, block = ~run, sampling_frame = sf
  )

  expect_s3_class(emod, "event_model")
  dm <- design_matrix(emod)
  # 30 + 30 = 60 samples total
  expect_equal(nrow(dm), 60)
})


test_that("hrf_fun receives correct event data columns", {
  received_cols <- NULL

  col_check_gen <- function(d) {
    received_cols <<- names(d)
    lapply(seq_len(nrow(d)), function(i) fmrihrf::HRF_SPMG1)
  }

  des <- data.frame(
    onset = c(0, 10),
    myvar = c("x", "y"),
    run = 1
  )
  sf <- fmrihrf::sampling_frame(blocklens = 30, TR = 2)

  emod <- event_model(
    onset ~ hrf(myvar, hrf_fun = col_check_gen),
    data = des, block = ~run, sampling_frame = sf
  )

  # Should have onset, duration, blockid, and the term variable
  expect_true("onset" %in% received_cols)
  expect_true("duration" %in% received_cols)
  expect_true("blockid" %in% received_cols)
  expect_true("myvar" %in% received_cols)
})


test_that("hrf_fun warning when basis also specified", {
  des <- data.frame(
    onset = c(0, 10),
    condition = c("A", "B"),
    run = 1
  )
  sf <- fmrihrf::sampling_frame(blocklens = 30, TR = 2)

  simple_gen <- function(d) {
    lapply(seq_len(nrow(d)), function(i) fmrihrf::HRF_SPMG1)
  }

  # Should warn when both basis and hrf_fun specified
  expect_warning(
    event_model(
      onset ~ hrf(condition, basis = "gamma", hrf_fun = simple_gen),
      data = des, block = ~run, sampling_frame = sf
    ),
    "hrf_fun"
  )
})


test_that("hrf_fun with empty events (after subset) handles gracefully", {
  des <- data.frame(
    onset = c(0, 10),
    condition = c("A", "B"),
    run = 1
  )
  sf <- fmrihrf::sampling_frame(blocklens = 30, TR = 2)

  gen_called <- FALSE
  check_gen <- function(d) {
    gen_called <<- TRUE
    lapply(seq_len(nrow(d)), function(i) fmrihrf::HRF_SPMG1)
  }

  # Subset that results in no events
  emod <- event_model(
    onset ~ hrf(condition, hrf_fun = check_gen, subset = condition == "C"),
    data = des, block = ~run, sampling_frame = sf
  )

  # Generator should not be called for empty events
  expect_false(gen_called)
})
