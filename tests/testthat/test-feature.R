skip_if_not(
  exists("feature_regressor", asNamespace("fmrihrf"), inherits = FALSE),
  "fmrihrf::feature_regressor is required"
)

local_edition(3)

test_that("feature-only model matches feature_regressor evaluation", {
  dt <- 0.2
  rms <- abs(sin(seq(0, 20, by = dt)))
  sframe <- fmrihrf::sampling_frame(blocklens = 15, TR = 2)

  emod <- event_model(
    ~ feature(rms, dt = dt, id = "rms", center = FALSE, scale = "none",
              basis = fmrihrf::HRF_SPMG1),
    sampling_frame = sframe,
    precision = dt
  )
  dm <- design_matrix(emod)
  expect_equal(nrow(dm), sum(fmrihrf::blocklens(sframe)))
  expect_equal(colnames(dm), "rms_rms")

  term <- terms(emod)[[1]]
  feat <- fmrihrf::feature_regressor(
    rms, dt = dt, hrf = term$hrf, center = FALSE, scale = "none"
  )
  grid <- fmrihrf::samples(sframe, global = FALSE)
  expect_equal(
    as.numeric(dm[[1]]),
    as.numeric(fmrihrf::evaluate(feat, grid, precision = dt)),
    tolerance = 1e-8
  )
})

test_that("impulse event encoding differs from ZOH feature by about 1/dt", {
  dt <- 0.2
  times <- seq(0, 8, by = dt)
  values <- sin(2 * pi * times / 2) + 1.5
  hrf <- fmrihrf::HRF_SPMG1
  grid <- seq(0, 12, by = 1)

  y_feat <- fmrihrf::evaluate(
    fmrihrf::feature_regressor(values, dt = dt, hrf = hrf,
                               center = FALSE, scale = "none"),
    grid, precision = dt
  )
  y_imp <- fmrihrf::evaluate(
    fmrihrf::regressor(times, hrf, duration = 0, amplitude = values),
    grid, precision = dt
  )
  ratio <- max(abs(y_imp)) / max(abs(y_feat))
  expect_lt(abs(ratio - 1 / dt) / (1 / dt), 0.05)
})

test_that("feature-only one-sided formula does not require data or block", {
  dt <- 0.5
  rms <- abs(sin(seq(0, 12, by = dt)))
  sframe <- fmrihrf::sampling_frame(blocklens = 10, TR = 2)
  emod <- event_model(
    ~ feature(rms, dt = dt, id = "rms", center = FALSE, scale = "none"),
    sampling_frame = sframe
  )
  expect_s3_class(emod, "event_model")
  expect_equal(length(emod$blockids), 0L)
  expect_equal(nrow(design_matrix(emod)), 10)
})

test_that("mixed hrf + feature model combines columns", {
  dt <- 0.5
  rms <- abs(sin(seq(0, 20, by = dt)))
  events <- data.frame(
    onset = c(2, 8, 14),
    run = 1,
    cond = factor(c("A", "B", "A"))
  )
  sframe <- fmrihrf::sampling_frame(blocklens = 15, TR = 2)
  emod <- event_model(
    onset ~ hrf(cond) + feature(rms, dt = dt, id = "rms",
                               center = FALSE, scale = "none"),
    data = events,
    block = ~ run,
    sampling_frame = sframe
  )
  dm <- design_matrix(emod)
  expect_equal(ncol(dm), 3)
  expect_true("rms_rms" %in% colnames(dm))
  expect_true(all(c("cond_cond.A", "cond_cond.B") %in% colnames(dm)))
})

test_that("multi-run feature evaluates each block separately", {
  dt <- 0.2
  t1 <- seq(0, 10, by = dt)
  rms1 <- abs(sin(2 * pi * t1 / 4)) + 2
  rms2 <- abs(cos(2 * pi * t1 / 3))
  sframe <- fmrihrf::sampling_frame(blocklens = c(8, 8), TR = 2)

  emod <- event_model(
    ~ feature(list(rms1, rms2), dt = dt, id = "rms",
              center = TRUE, scale = "none", basis = fmrihrf::HRF_SPMG1),
    sampling_frame = sframe,
    precision = dt
  )
  dm <- design_matrix(emod)
  bids <- fmrihrf::blockids(sframe)
  term <- terms(emod)[[1]]

  y2 <- fmrihrf::evaluate(
    fmrihrf::feature_regressor(rms2, dt = dt, hrf = term$hrf,
                               center = TRUE, scale = "none"),
    fmrihrf::samples(sframe, blockids = 2, global = FALSE),
    precision = dt
  )
  expect_equal(as.numeric(dm[[1]][bids == 2]), as.numeric(y2), tolerance = 1e-8)

  y1_on_b2 <- fmrihrf::evaluate(
    fmrihrf::feature_regressor(rms1, dt = dt, hrf = term$hrf,
                               center = TRUE, scale = "none"),
    fmrihrf::samples(sframe, blockids = 2, global = FALSE),
    precision = dt
  )
  expect_gt(max(abs(as.numeric(dm[[1]][bids == 2]) - as.numeric(y1_on_b2))), 0.05)
})

test_that("centering is per run, not global", {
  dt <- 0.5
  rms1 <- rep(10, 21)
  rms2 <- rep(0, 21)
  sframe <- fmrihrf::sampling_frame(blocklens = c(8, 8), TR = 2)

  emod <- event_model(
    ~ feature(list(rms1, rms2), dt = dt, id = "rms",
              center = TRUE, scale = "none", basis = fmrihrf::HRF_SPMG1),
    sampling_frame = sframe
  )
  # Constant series, centered per run, is identically zero.
  expect_equal(as.numeric(design_matrix(emod)[[1]]),
               rep(0, sum(fmrihrf::blocklens(sframe))))

  global_centered <- c(rms1, rms2) - mean(c(rms1, rms2))
  expect_false(isTRUE(all.equal(global_centered, rep(0, length(global_centered)))))
})

test_that("mask keeps off-period at 0 after centering", {
  dt <- 0.5
  vals <- c(rep(0, 10), rep(4, 10), rep(0, 11))
  mask <- vals > 0
  sframe <- fmrihrf::sampling_frame(blocklens = 20, TR = 2)
  emod <- event_model(
    ~ feature(vals, dt = dt, id = "rms", mask = mask,
              center = TRUE, scale = "none", basis = fmrihrf::HRF_SPMG1),
    sampling_frame = sframe
  )
  term <- terms(emod)[[1]]
  feat <- fmrihrf::feature_regressor(
    vals, dt = dt, hrf = term$hrf, mask = mask, center = TRUE, scale = "none"
  )
  grid <- fmrihrf::samples(sframe, global = FALSE)
  expect_equal(
    as.numeric(design_matrix(emod)[[1]]),
    as.numeric(fmrihrf::evaluate(feat, grid, precision = dt)),
    tolerance = 1e-8
  )
})

test_that("multi-basis and multi-column features are named correctly", {
  dt <- 0.5
  mat <- cbind(alpha = abs(sin(seq(0, 12, by = dt))),
               beta = abs(cos(seq(0, 12, by = dt))))
  sframe <- fmrihrf::sampling_frame(blocklens = 10, TR = 2)

  emod_named <- event_model(
    ~ feature(mat, dt = dt, id = "env", center = FALSE, scale = "none"),
    sampling_frame = sframe
  )
  expect_equal(colnames(design_matrix(emod_named)), c("env_alpha", "env_beta"))

  mat2 <- matrix(mat, ncol = 2)
  emod_suf <- event_model(
    ~ feature(mat2, dt = dt, id = "env", center = FALSE, scale = "none"),
    sampling_frame = sframe
  )
  expect_equal(colnames(design_matrix(emod_suf)), c("env_f01", "env_f02"))

  emod_b <- event_model(
    ~ feature(mat[, 1], dt = dt, id = "rms", basis = "spmg3",
              center = FALSE, scale = "none"),
    sampling_frame = sframe
  )
  expect_equal(
    colnames(design_matrix(emod_b)),
    paste0("rms_rms", basis_suffix(1:3, 3))
  )
})

test_that("model precision coarser than dt evaluates at dt", {
  dt <- 0.1
  rms <- abs(sin(seq(0, 8, by = dt)))
  sframe <- fmrihrf::sampling_frame(blocklens = 8, TR = 2)
  expect_warning(
    emod <- event_model(
      ~ feature(rms, dt = dt, id = "rms", center = FALSE, scale = "none",
                basis = fmrihrf::HRF_SPMG1),
      sampling_frame = sframe,
      precision = 0.3
    ),
    "coarser than dt"
  )
  term <- terms(emod)[[1]]
  feat <- fmrihrf::feature_regressor(
    rms, dt = dt, hrf = term$hrf, center = FALSE, scale = "none"
  )
  grid <- fmrihrf::samples(sframe, global = FALSE)
  expect_equal(
    as.numeric(design_matrix(emod)[[1]]),
    as.numeric(fmrihrf::evaluate(feat, grid, precision = dt)),
    tolerance = 1e-8
  )
})

test_that("feature input validation", {
  sframe2 <- fmrihrf::sampling_frame(blocklens = c(8, 8), TR = 2)
  rms <- abs(sin(seq(0, 8, by = 0.5)))

  expect_error(feature(rms), "dt")
  expect_error(feature(rms, dt = 0.5, times = seq_along(rms)), "exactly one")
  expect_error(
    event_model(~ feature(rms, dt = 0.5, id = "rms"), sampling_frame = sframe2),
    "list with one series per block"
  )
  expect_error(
    event_model(onset ~ hrf(cond) + feature(rms, dt = 0.5, id = "rms"),
                sampling_frame = fmrihrf::sampling_frame(blocklens = 10, TR = 2)),
    "data"
  )
})

test_that("feature metadata and list interface", {
  dt <- 0.5
  rms <- abs(sin(seq(0, 12, by = dt)))
  sframe <- fmrihrf::sampling_frame(blocklens = 10, TR = 2)
  spec <- feature(rms, dt = dt, id = "rms", center = FALSE, scale = "none")
  emod <- event_model(list(spec), sampling_frame = sframe)

  cm <- design_colmap(emod)
  expect_equal(cm$modulation_type, "feature")
  expect_equal(cm$modulation_id, "rms")
  expect_equal(conditions(emod), "rms")
  expect_true(is_continuous(terms(emod)[[1]]))
  expect_error(events(terms(emod)[[1]]), "not defined")
})
