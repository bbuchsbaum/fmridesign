# Tests for first-class design column metadata (R/design_metadata.R).

test_that("event_model attaches col_metadata aligned with design matrix", {
  des <- data.frame(
    onset = c(0, 10, 20, 30, 40, 50),
    run   = 1,
    cond  = factor(c("A", "B", "A", "B", "A", "B")),
    rt    = c(0.4, 0.6, 0.5, 0.55, 0.7, 0.45)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = 60, TR = 1)
  emod <- event_model(
    onset ~ hrf(cond) + hrf(Scale(rt)),
    data = des, block = ~run, sampling_frame = sframe
  )

  dm <- design_matrix(emod)
  meta <- design_meta(emod)

  expect_s3_class(meta, "tbl_df")
  expect_equal(nrow(meta), ncol(dm))
  expect_equal(meta$name, colnames(dm))
  expect_equal(meta$col, seq_len(ncol(dm)))

  # Categorical term: modulation should be amplitude.
  cond_rows <- meta[meta$term_tag == "cond", ]
  expect_true(all(cond_rows$modulation_type == "amplitude"))

  # Scale(rt) term: modulation should be parametric, modulation_id == "rt".
  rt_rows <- meta[meta$term_tag == "z_rt", ]
  expect_true(nrow(rt_rows) >= 1L)
  expect_true(all(rt_rows$modulation_type == "parametric"))
  expect_true(all(rt_rows$modulation_id == "rt"))
})

test_that("factor HRF metadata has one row per expanded column", {
  des <- data.frame(
    condition = factor(c("A", "B", "A", "B", "A", "B")),
    onsets = c(1, 15, 30, 45, 60, 75),
    run = c(1, 1, 1, 2, 2, 2)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)

  emod <- event_model(
    onsets ~ hrf(condition),
    data = des,
    block = ~run,
    sampling_frame = sframe
  )

  dm <- design_matrix(emod)
  meta <- design_meta(emod)

  expect_equal(ncol(dm), 2)
  expect_equal(nrow(meta), ncol(dm))
  expect_equal(meta$col, seq_len(ncol(dm)))
  expect_equal(meta$name, colnames(dm))
})

test_that("design_colmap reads from col_metadata when available", {
  des <- data.frame(
    onset = c(0, 10, 20, 30),
    run   = 1,
    cond  = factor(c("A", "B", "A", "B"))
  )
  sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
  emod <- event_model(onset ~ hrf(cond), data = des,
                      block = ~run, sampling_frame = sframe)

  cm_meta <- design_meta(emod)
  cm_view <- design_colmap(emod)

  # Both should describe the same columns.
  expect_equal(cm_view$name, colnames(design_matrix(emod)))
  expect_equal(cm_view$term_tag, cm_meta$term_tag)
  expect_equal(cm_view$modulation_type, cm_meta$modulation_type)

  # design_colmap adds a pretty_name column derived from metadata.
  expect_true("pretty_name" %in% names(cm_view))
  expect_equal(length(cm_view$pretty_name), nrow(cm_view))
})

test_that("multi-basis HRF yields basis_ix and basis_total", {
  des <- data.frame(
    onset = c(0, 10, 20, 30),
    run   = 1,
    cond  = factor(c("A", "B", "A", "B"))
  )
  sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
  emod <- event_model(onset ~ hrf(cond, basis = "spmg3"),
                      data = des, block = ~run, sampling_frame = sframe)
  meta <- design_meta(emod)

  expect_true(all(!is.na(meta$basis_ix)))
  expect_true(all(meta$basis_total == 3L))
  expect_true(all(meta$basis_label %in% c("canonical", "derivative", "dispersion")))
})

test_that("multi-basis factor HRF metadata matches expanded columns", {
  des <- data.frame(
    onset = c(0, 10, 20, 30),
    run   = 1,
    cond  = factor(c("A", "B", "A", "B"))
  )
  sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)

  emod <- event_model(onset ~ hrf(cond, basis = "spmg3"),
                      data = des, block = ~run, sampling_frame = sframe)
  dm <- design_matrix(emod)
  meta <- design_meta(emod)

  expect_equal(ncol(dm), 6)
  expect_equal(nrow(meta), ncol(dm))
  expect_equal(meta$col, seq_len(ncol(dm)))
  expect_equal(meta$basis_ix, rep(1:3, times = 2))
  expect_true(all(meta$basis_total == 3L))
})

# --- basis_name / basis_label for decorated HRFs ------------------------------

basis_colmap <- function(..., strip = FALSE) {
  des <- data.frame(onset = c(2, 10, 18, 26), run = 1,
                    cond = factor(c("A", "B", "A", "B")), RT = c(0.1, 0.5, 0.3, 0.9))
  sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
  emod <- event_model(onset ~ hrf(cond, ...), data = des, block = ~run,
                      sampling_frame = sframe)
  if (strip) attr(emod$design_matrix, "col_metadata") <- NULL
  design_colmap(emod)
}

test_that("lagged, blocked and normalised bases keep their basis name and labels", {
  spmg3_labels <- c("canonical", "derivative", "dispersion")

  cm <- basis_colmap(basis = "spmg3", lag = 2)
  expect_identical(unique(cm$basis_name), "spmg3")
  expect_identical(cm$basis_label, rep(spmg3_labels, 2))

  cm <- basis_colmap(basis = "spmg2", lag = 2)
  expect_identical(unique(cm$basis_name), "spmg2")
  expect_identical(cm$basis_label, rep(c("canonical", "derivative"), 2))

  cm <- basis_colmap(basis = "fir", nbasis = 4, lag = 2)
  expect_identical(unique(cm$basis_name), "fir")
  expect_identical(cm$basis_label, rep(sprintf("lag_%02d", 0:3), 2))

  cm <- basis_colmap(basis = fmrihrf::block_hrf(fmrihrf::HRF_SPMG3, width = 2))
  expect_identical(unique(cm$basis_name), "spmg3")
  expect_identical(cm$basis_label, rep(spmg3_labels, 2))

  cm <- basis_colmap(basis = fmrihrf::normalise_hrf(fmrihrf::HRF_SPMG2))
  expect_identical(unique(cm$basis_name), "spmg2")

  # The reconstruction path used without construction metadata agrees
  cm <- basis_colmap(basis = "spmg3", lag = 2, strip = TRUE)
  expect_identical(unique(cm$basis_name), "spmg3")
  expect_identical(cm$basis_label, rep(spmg3_labels, 2))
})

test_that("a lagged multi-basis feature term gets basis labels", {
  skip_if_not(
    exists("feature_regressor", asNamespace("fmrihrf"), inherits = FALSE),
    "fmrihrf::feature_regressor is required"
  )
  rms <- abs(sin(seq(0, 39.5, by = 0.5)))
  des <- data.frame(onset = c(2, 10), run = 1, cond = factor(c("A", "B")))
  emod <- event_model(
    onset ~ hrf(cond) + feature(rms, dt = 0.5, id = "rms", basis = "spmg3",
                                lag = 1, center = FALSE, scale = "none"),
    data = des, block = ~run, sampling_frame = fmrihrf::sampling_frame(blocklens = 40, TR = 1)
  )
  cm <- design_colmap(emod)
  feat <- cm[cm$modulation_type == "feature", ]
  expect_identical(feat$basis_name, rep("spmg3", 3))
  expect_identical(feat$basis_label, c("canonical", "derivative", "dispersion"))
})

test_that("spmg1 basis_name does not depend on how fmrihrf classes HRF_SPMG1", {
  # fmrihrf 0.4.0 classes HRF_SPMG1 c("HRF", "function"); later versions add
  # "SPMG1_HRF". Both must give the same basis_name.
  h <- fmrihrf::HRF_SPMG1
  plain <- h
  class(plain) <- setdiff(class(h), "SPMG1_HRF")
  subclassed <- h
  class(subclassed) <- union("SPMG1_HRF", class(h))
  expect_identical(fmridesign:::.hrf_basis_name(plain), "spmg1")
  expect_identical(fmridesign:::.hrf_basis_name(subclassed), "spmg1")
  expect_identical(unique(basis_colmap(basis = "spmg1")$basis_name), "spmg1")
  expect_identical(unique(basis_colmap(basis = "spmg1", strip = TRUE)$basis_name), "spmg1")
})

test_that(".hrf_basis_name strips decorator suffixes and keeps custom names", {
  hbn <- fmridesign:::.hrf_basis_name
  expect_identical(hbn(NULL), NA_character_)
  expect_identical(hbn(fmrihrf::HRF_FIR), "fir")
  expect_identical(hbn(fmrihrf::HRF_BSPLINE), "bspline")
  expect_identical(hbn(fmrihrf::HRF_GAMMA), "gamma")
  expect_identical(
    hbn(fmrihrf::lag_hrf(fmrihrf::block_hrf(fmrihrf::HRF_SPMG2, width = 2), 1)),
    "spmg2"
  )
  custom <- fmrihrf::as_hrf(function(t) dnorm(t, 5), name = "MyHRF")
  expect_identical(hbn(custom), "MyHRF")
  expect_identical(hbn(fmrihrf::lag_hrf(custom, 2)), "MyHRF")
})

test_that(".basis_label is vectorised", {
  bl <- fmridesign:::.basis_label
  expect_identical(bl("spmg3", 1:3), c("canonical", "derivative", "dispersion"))
  expect_identical(bl(c("fir", "tent", NA, "spmg2"), c(1L, 2L, 1L, NA)),
                   c("lag_00", "component_02", NA, NA))
  expect_identical(bl("spmg1", integer(0)), character(0))
})

test_that("plain-function bases are named after the basis argument", {
  # gen_hrf() used to name them after its own argument ("basis")
  my_ramp <- function(t) ifelse(t >= 0 & t < 10, t / 10, 0)
  sf <- sampling_frame(blocklens = 60, TR = 2)
  d <- data.frame(onset = c(5, 30, 70), cond = factor(c("a", "b", "a")), run = 1)
  basis_of <- function(form) {
    em <- event_model(form, data = d, block = ~run, sampling_frame = sf)
    unique(design_colmap(em)$basis_name)
  }
  expect_equal(basis_of(onset ~ hrf(cond, basis = my_ramp)), "my_ramp")
  expect_equal(basis_of(onset ~ hrf(cond, basis = my_ramp, lag = 2)), "my_ramp")
  expect_equal(basis_of(onset ~ hrf(cond, basis = function(t) dnorm(t, 5, 2))), "custom")
  expect_equal(basis_of(onset ~ trialwise(basis = my_ramp)), "my_ramp")
})
