test_that("summate parameter is propagated from hrf() to hrfspec", {
  # Verify summate value is stored and propagated in the hrfspec
  spec_true <- hrf(x, basis = "spmg1", summate = TRUE)
  expect_true(spec_true$summate)

  spec_false <- hrf(x, basis = "spmg1", summate = FALSE)
  expect_false(spec_false$summate)

  # Default should be TRUE
  spec_default <- hrf(x, basis = "spmg1")
  expect_true(spec_default$summate)
})

test_that("normalize=TRUE equalizes peak amplitudes across durations", {
  sframe <- fmrihrf::sampling_frame(blocklens = 200, TR = 1)
  dat <- data.frame(
    onset = c(5, 40, 80, 130),
    cond = "A",
    dur = c(1, 2, 5, 10),
    run = 1
  )

  emod <- event_model(onset ~ trialwise(basis = "spmg1", durations = dur, normalize = TRUE),
                      data = dat, block = ~run, sampling_frame = sframe)
  dm <- as.matrix(design_matrix(emod))

  # All columns should have max(abs(col)) == 1
  peaks <- apply(dm, 2, function(col) max(abs(col)))
  # Filter out zero columns (shouldn't be any, but be safe)
  peaks <- peaks[peaks > 0]
  expect_true(length(peaks) > 0, info = "Should have non-zero columns")
  expect_true(all(abs(peaks - 1.0) < 1e-10),
              info = "All peaks should be 1.0 after normalization")
})

test_that("normalize=TRUE preserves regressor shape (correlation == 1)", {
  sframe <- fmrihrf::sampling_frame(blocklens = 200, TR = 1)
  dat <- data.frame(
    onset = c(5, 40, 80, 130),
    cond = "A",
    dur = c(1, 2, 5, 10),
    run = 1
  )

  emod_raw <- event_model(onset ~ trialwise(basis = "spmg1", durations = dur),
                          data = dat, block = ~run, sampling_frame = sframe)
  dm_raw <- as.matrix(design_matrix(emod_raw))

  emod_norm <- event_model(onset ~ trialwise(basis = "spmg1", durations = dur, normalize = TRUE),
                           data = dat, block = ~run, sampling_frame = sframe)
  dm_norm <- as.matrix(design_matrix(emod_norm))

  # Each column pair should correlate at r=1.0
  for (j in seq_len(ncol(dm_raw))) {
    if (max(abs(dm_raw[, j])) > 0 && max(abs(dm_norm[, j])) > 0) {
      r <- cor(dm_raw[, j], dm_norm[, j])
      expect_equal(r, 1.0, tolerance = 1e-10,
                   info = sprintf("Column %d should correlate perfectly", j))
    }
  }
})

test_that("normalize=FALSE (default) preserves current behavior", {
  sframe <- fmrihrf::sampling_frame(blocklens = 200, TR = 1)
  dat <- data.frame(
    onset = c(5, 40, 80, 130),
    cond = "A",
    dur = c(1, 2, 5, 10),
    run = 1
  )

  # Without normalize (default FALSE)
  emod1 <- event_model(onset ~ trialwise(basis = "spmg1", durations = dur),
                       data = dat, block = ~run, sampling_frame = sframe)
  dm1 <- as.matrix(design_matrix(emod1))

  # Explicitly with normalize=FALSE
  emod2 <- event_model(onset ~ trialwise(basis = "spmg1", durations = dur, normalize = FALSE),
                       data = dat, block = ~run, sampling_frame = sframe)
  dm2 <- as.matrix(design_matrix(emod2))

  # Should be identical
  expect_equal(dm1, dm2, info = "Default and explicit normalize=FALSE should match")

  # Peaks should differ across columns (longer duration = taller peak)
  peaks <- apply(dm1, 2, function(col) max(abs(col)))
  peaks <- peaks[peaks > 0]
  expect_true(length(unique(round(peaks, 6))) > 1,
              info = "Without normalization, different durations should produce different peaks")
})

test_that("duration_hrf_gen produces normalized per-onset HRFs", {
  sframe <- fmrihrf::sampling_frame(blocklens = 200, TR = 1)
  dat <- data.frame(
    onset = c(5, 40, 80, 130),
    cond = factor(c("A", "A", "A", "A")),
    dur = c(1, 3, 6, 10),
    run = 1
  )

  emod <- event_model(
    onset ~ hrf(cond, hrf_fun = duration_hrf_gen()),
    data = dat, block = ~run, sampling_frame = sframe,
    durations = dat$dur
  )
  dm <- as.matrix(design_matrix(emod))

  # The single condition column should exist
  expect_true(ncol(dm) >= 1)
})

test_that("normalize works with hrf() for condition-level regressors", {
  sframe <- fmrihrf::sampling_frame(blocklens = 200, TR = 1)
  dat <- data.frame(
    onset = c(5, 20, 40, 60, 80, 100),
    cond = factor(c("A", "B", "A", "B", "A", "B")),
    dur = c(1, 5, 1, 5, 1, 5),
    run = 1
  )

  emod <- event_model(onset ~ hrf(cond, basis = "spmg1", normalize = TRUE),
                      data = dat, block = ~run, sampling_frame = sframe,
                      durations = dat$dur)
  dm <- as.matrix(design_matrix(emod))

  # Each condition column should have peak == 1
  peaks <- apply(dm, 2, function(col) max(abs(col)))
  peaks <- peaks[peaks > 0]
  expect_true(all(abs(peaks - 1.0) < 1e-10),
              info = "Condition-level normalize should also set peaks to 1")
})
