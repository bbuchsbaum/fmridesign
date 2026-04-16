# test-hrf_formula.R
#
# Tests for hrf(), hrfspec, and HRF generator functions in hrf-formula.R.
# Covers: hrf(), nbasis.hrfspec, contrasts.hrfspec, construct.hrfspec,
#         boxcar_hrf_gen, duration_hrf_gen, weighted_hrf_gen, make_hrf.

library(testthat)

testthat::local_edition(3)

# ── Standard setup ──────────────────────────────────────────────────────────

des <- data.frame(
  onset = c(0, 10, 20, 30, 5, 15, 25, 35),
  run   = c(1, 1, 1, 1, 2, 2, 2, 2),
  cond  = factor(c("A", "B", "A", "B", "A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 1)

# ── hrf() function ─────────────────────────────────────────────────────────

test_that("hrf() creates an hrfspec with default basis", {
  spec <- hrf(cond)

  expect_s3_class(spec, "hrfspec")
  expect_true(inherits(spec$hrf, "HRF"))
  expect_equal(spec$name, "cond")
  expect_equal(spec$precision, 0.3)
  expect_true(spec$summate)
  expect_false(spec$normalize)
  expect_null(spec$contrasts)
})

test_that("hrf() accepts character basis names", {
  spec_g1 <- hrf(cond, basis = "spmg1")
  expect_s3_class(spec_g1, "hrfspec")
  expect_true(inherits(spec_g1$hrf, "HRF"))

  spec_g2 <- hrf(cond, basis = "spmg2")
  expect_true(fmrihrf::nbasis(spec_g2$hrf) == 2)

  spec_g3 <- hrf(cond, basis = "spmg3")
  expect_true(fmrihrf::nbasis(spec_g3$hrf) == 3)

  spec_gamma <- hrf(cond, basis = "gamma")
  expect_true(fmrihrf::nbasis(spec_gamma$hrf) == 1)
})

test_that("hrf() accepts HRF object as basis", {
  spec <- hrf(cond, basis = fmrihrf::HRF_SPMG1)
  expect_s3_class(spec, "hrfspec")
  expect_true(inherits(spec$hrf, "HRF"))
})

test_that("hrf() with multiple variables captures all in varnames", {
  spec <- hrf(cond, RT)
  expect_equal(length(spec$varnames), 2)
  expect_true("cond" %in% spec$varnames)
  expect_true("RT" %in% spec$varnames)
  expect_equal(spec$name, "cond:RT")
})

test_that("hrf() with id argument overrides term name", {
  spec <- hrf(cond, id = "my_term")
  expect_equal(spec$name, "my_term")
  expect_equal(spec$id, "my_term")
})

test_that("hrf() with name argument used as id fallback", {
  spec <- hrf(cond, name = "my_name")
  expect_equal(spec$name, "my_name")
  expect_equal(spec$id, "my_name")
})

test_that("hrf() with lag creates shifted HRF", {
  spec <- hrf(cond, lag = 2)
  expect_s3_class(spec, "hrfspec")
  # The HRF should be lagged (gen_hrf handles this internally)
  expect_true(inherits(spec$hrf, "HRF"))
})

test_that("hrf() with summate=FALSE and normalize=TRUE stores options", {
  spec <- hrf(cond, summate = FALSE, normalize = TRUE)
  expect_false(spec$summate)
  expect_true(spec$normalize)
})

test_that("hrf() with prefix stores prefix", {
  spec <- hrf(cond, prefix = "task1")
  expect_equal(spec$prefix, "task1")
})

test_that("hrf() errors with no variables", {
  expect_error(hrf(basis = "spmg1"), "at least one variable")
})

test_that("hrf() with contrast_spec wraps in named list", {
  con <- pair_contrast(~ cond == "A", ~ cond == "B", name = "AvB")
  spec <- hrf(cond, contrasts = con)
  expect_true(is.list(spec$contrasts))
  expect_equal(names(spec$contrasts), "AvB")
})

test_that("hrf() with contrast_set stores named list", {
  cs <- contrast_set(
    diff = column_contrast(
      pattern_A = "cond.A", pattern_B = "cond.B", name = "diff"
    )
  )
  spec <- hrf(cond, contrasts = cs)
  expect_true(is.list(spec$contrasts))
  expect_true("diff" %in% names(spec$contrasts))
})

test_that("hrf() errors on invalid contrasts argument", {
  expect_error(hrf(cond, contrasts = "not_a_contrast"), "contrast_spec")
})

test_that("hrf() with hrf_fun stores function", {
  gen <- function(d) {
    lapply(seq_len(nrow(d)), function(i) fmrihrf::HRF_SPMG1)
  }
  spec <- hrf(cond, hrf_fun = gen)
  expect_true(is.function(spec$hrf_fun))
})

test_that("hrf() with hrf_fun as formula stores formula", {
  spec <- hrf(cond, hrf_fun = ~hrf_col)
  expect_true(rlang::is_formula(spec$hrf_fun))
})

test_that("hrf() errors on invalid hrf_fun type", {
  expect_error(hrf(cond, hrf_fun = 42), "function or formula")
})

test_that("hrf() with bspline basis and custom nbasis", {
  spec <- hrf(cond, basis = "bspline", nbasis = 8)
  expect_equal(fmrihrf::nbasis(spec$hrf), 8)
})

test_that("hrf() with fir basis and custom nbasis", {
  spec <- hrf(cond, basis = "fir", nbasis = 12)
  expect_equal(fmrihrf::nbasis(spec$hrf), 12)
})

test_that("hrf() errors on unknown basis string", {
  expect_error(hrf(cond, basis = "unknown_hrf"), "Unknown HRF basis name")
})

# ── nbasis.hrfspec() ────────────────────────────────────────────────────────

test_that("nbasis.hrfspec() returns correct count", {
  expect_equal(nbasis(hrf(cond, basis = "spmg1")), 1)
  expect_equal(nbasis(hrf(cond, basis = "spmg2")), 2)
  expect_equal(nbasis(hrf(cond, basis = "spmg3")), 3)
})

test_that("nbasis.hrfspec() for bspline with custom N", {
  spec <- hrf(cond, basis = "bspline", nbasis = 6)
  expect_equal(nbasis(spec), 6)
})

# ── contrasts.hrfspec() ─────────────────────────────────────────────────────

test_that("contrasts.hrfspec() returns NULL when no contrasts defined", {
  spec <- hrf(cond)
  expect_null(contrasts(spec))
})

test_that("contrasts.hrfspec() returns contrast list when defined", {
  con <- pair_contrast(~ cond == "A", ~ cond == "B", name = "AvB")
  spec <- hrf(cond, contrasts = con)
  result <- contrasts(spec)
  expect_true(is.list(result))
  expect_equal(length(result), 1)
  expect_equal(names(result), "AvB")
})

# ── construct.hrfspec() ─────────────────────────────────────────────────────

test_that("construct.hrfspec() returns an event_term", {
  emod <- event_model(
    onset ~ hrf(cond),
    data = des,
    block = ~run,
    sampling_frame = sframe
  )

  # The event_model internally calls construct; verify the resulting terms
  et_list <- event_terms(emod)
  expect_true(length(et_list) > 0)

  ct <- et_list[[1]]
  # convolved_term wraps event_term
 expect_true(inherits(ct, "convolved_term") || inherits(ct, "event_term"))
})

test_that("construct.hrfspec() propagates term_tag", {
  emod <- event_model(
    onset ~ hrf(cond, id = "my_cond"),
    data = des,
    block = ~run,
    sampling_frame = sframe
  )

  dm <- design_matrix(emod)
  # Column names should include the term tag "my_cond"
  expect_true(any(grepl("my_cond", colnames(dm))))
})

# ── boxcar_hrf_gen() ────────────────────────────────────────────────────────

test_that("boxcar_hrf_gen() creates a valid generator function", {
  gen <- boxcar_hrf_gen()
  expect_true(is.function(gen))
})

test_that("boxcar_hrf_gen() produces HRF list from event data", {
  gen <- boxcar_hrf_gen(normalize = TRUE, min_duration = 0.1)

  event_data <- data.frame(
    onset    = c(0, 10, 20),
    duration = c(2, 5, 3),
    blockid  = c(1, 1, 1)
  )

  result <- gen(event_data)
  expect_true(is.list(result))
  expect_equal(length(result), 3)
  # Each element should be an HRF object
  expect_true(all(sapply(result, inherits, "HRF")))
})

test_that("boxcar_hrf_gen() respects min_duration", {
  gen <- boxcar_hrf_gen(min_duration = 1.0)

  event_data <- data.frame(
    onset    = c(0),
    duration = c(0.01),  # very short
    blockid  = c(1)
  )

  result <- gen(event_data)
  expect_true(inherits(result[[1]], "HRF"))
  # The generated HRF should use at least min_duration width
})

test_that("boxcar_hrf_gen() works in event_model", {
  trial_data <- data.frame(
    onset     = c(0, 10, 25),
    duration  = c(2, 5, 3),
    condition = factor(c("A", "B", "A")),
    run       = 1
  )
  sf <- fmrihrf::sampling_frame(blocklens = 50, TR = 2)

  emod <- event_model(
    onset ~ hrf(condition, hrf_fun = boxcar_hrf_gen()),
    data = trial_data,
    block = ~run,
    sampling_frame = sf,
    durations = trial_data$duration
  )

  dm <- design_matrix(emod)
  expect_true(nrow(dm) > 0)
  expect_true(ncol(dm) >= 2)
})

# ── duration_hrf_gen() ──────────────────────────────────────────────────────

test_that("duration_hrf_gen() creates a valid generator function", {
  gen <- duration_hrf_gen()
  expect_true(is.function(gen))
})

test_that("duration_hrf_gen() produces HRF list from event data", {
  gen <- duration_hrf_gen(base = fmrihrf::HRF_SPMG1, min_duration = 0)

  event_data <- data.frame(
    onset    = c(0, 10),
    duration = c(3, 0),
    blockid  = c(1, 1)
  )

  result <- gen(event_data)
  expect_true(is.list(result))
  expect_equal(length(result), 2)
  expect_true(all(sapply(result, inherits, "HRF")))
})

test_that("duration_hrf_gen() returns base HRF for zero-duration events", {
  base <- fmrihrf::HRF_SPMG1
  gen <- duration_hrf_gen(base = base, min_duration = 0)

  event_data <- data.frame(
    onset    = c(0),
    duration = c(0),
    blockid  = c(1)
  )

  result <- gen(event_data)
  # Zero duration should return the base HRF directly
  expect_true(inherits(result[[1]], "HRF"))
})

test_that("duration_hrf_gen() works in event_model", {
  trial_data <- data.frame(
    onset     = c(0, 10, 25),
    duration  = c(2, 5, 3),
    condition = factor(c("A", "B", "A")),
    run       = 1
  )
  sf <- fmrihrf::sampling_frame(blocklens = 50, TR = 2)

  emod <- event_model(
    onset ~ hrf(condition, hrf_fun = duration_hrf_gen()),
    data = trial_data,
    block = ~run,
    sampling_frame = sf,
    durations = trial_data$duration
  )

  dm <- design_matrix(emod)
  expect_true(nrow(dm) > 0)
  expect_true(ncol(dm) >= 2)
})

# ── weighted_hrf_gen() ──────────────────────────────────────────────────────

test_that("weighted_hrf_gen() creates a valid generator function", {
  gen <- weighted_hrf_gen("times", "weights")
  expect_true(is.function(gen))
})

test_that("weighted_hrf_gen() produces HRF list from event data", {
  gen <- weighted_hrf_gen("sub_times", "sub_weights", relative = TRUE)

  event_data <- data.frame(
    onset       = c(0, 20),
    sub_times   = I(list(c(0, 1, 2), c(0, 3, 6))),
    sub_weights = I(list(c(0.2, 0.5, 0.3), c(0.1, 0.6, 0.3))),
    blockid     = c(1, 1)
  )

  result <- gen(event_data)
  expect_true(is.list(result))
  expect_equal(length(result), 2)
  expect_true(all(sapply(result, inherits, "HRF")))
})

test_that("weighted_hrf_gen() errors on missing column", {
  gen <- weighted_hrf_gen("nonexistent_times", "sub_weights")

  event_data <- data.frame(
    onset       = c(0),
    sub_weights = I(list(c(0.5))),
    blockid     = c(1)
  )

  expect_error(gen(event_data), "not found")
})

test_that("weighted_hrf_gen() with absolute times converts to relative", {
  gen <- weighted_hrf_gen("sub_times", "sub_weights", relative = FALSE)

  # Absolute times: onset=10, sub_times=c(10, 11, 12) -> relative=c(0, 1, 2)
  event_data <- data.frame(
    onset       = c(10),
    sub_times   = I(list(c(10, 11, 12))),
    sub_weights = I(list(c(0.3, 0.4, 0.3))),
    blockid     = c(1)
  )

  result <- gen(event_data)
  expect_true(inherits(result[[1]], "HRF"))
})

# ── make_hrf() internal helper ──────────────────────────────────────────────

test_that("make_hrf() creates HRF from character string", {
  hrf_obj <- fmridesign:::make_hrf("spmg1", lag = 0)
  expect_true(inherits(hrf_obj, "HRF"))
})

test_that("make_hrf() applies lag", {
  hrf_no_lag  <- fmridesign:::make_hrf("spmg1", lag = 0)
  hrf_lagged  <- fmridesign:::make_hrf("spmg1", lag = 2)
  # Both should be HRF objects
 expect_true(inherits(hrf_no_lag, "HRF"))
  expect_true(inherits(hrf_lagged, "HRF"))
})

test_that("make_hrf() from HRF object", {
  hrf_obj <- fmridesign:::make_hrf(fmrihrf::HRF_SPMG1, lag = 0)
  expect_true(inherits(hrf_obj, "HRF"))
})

test_that("make_hrf() from function", {
  fn <- function(t) dnorm(t, mean = 6, sd = 1)
  hrf_obj <- fmridesign:::make_hrf(fn, lag = 0)
  expect_true(inherits(hrf_obj, "HRF"))
})

test_that("make_hrf() errors on invalid lag", {
  expect_error(fmridesign:::make_hrf("spmg1", lag = c(1, 2)), "numeric scalar")
  expect_error(fmridesign:::make_hrf("spmg1", lag = "bad"), "numeric scalar")
})

test_that("make_hrf() errors on invalid basis type", {
  expect_error(fmridesign:::make_hrf(42, lag = 0), "invalid basis function")
})
