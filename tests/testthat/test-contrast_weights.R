# Tests for contrast specification constructors, contrast_weights methods,
# print/plot helpers, and factory functions.

library(testthat)
library(fmrihrf)

# ---------------------------------------------------------------------------
# Shared fixture: simple 2-condition, 2-run event model
# ---------------------------------------------------------------------------
make_simple_emod <- function() {
  des <- data.frame(
    onset = c(0, 10, 20, 30, 5, 15, 25, 35),
    run   = c(1, 1, 1, 1, 2, 2, 2, 2),
    cond  = factor(c("A", "B", "A", "B", "A", "B", "A", "B"))
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 1)
  event_model(onset ~ hrf(cond), data = des, block = ~run,
              sampling_frame = sframe)
}

make_3level_emod <- function() {
  des <- data.frame(
    onset = c(0, 10, 20, 5, 15, 25),
    run   = c(1, 1, 1, 2, 2, 2),
    cond  = factor(c("A", "B", "C", "A", "B", "C"))
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(30, 30), TR = 1)
  event_model(onset ~ hrf(cond), data = des, block = ~run,
              sampling_frame = sframe)
}

# ===========================================================================
# Section 1: Specification constructors (no model needed)
# ===========================================================================

# --- unit_contrast ---
test_that("unit_contrast creates correct spec class", {
  con <- unit_contrast(~ cond == "A", name = "A_baseline")
  expect_s3_class(con, "unit_contrast_spec")
  expect_s3_class(con, "contrast_spec")
  expect_equal(con$name, "A_baseline")
  expect_null(con$B)
})

test_that("unit_contrast validates inputs", {
  expect_error(unit_contrast("not a formula", name = "bad"))
  expect_error(unit_contrast(~ A, name = 123))
  expect_error(unit_contrast(~ A, name = "ok", where = "not formula"))
})

test_that("unit_contrast with where clause", {
  con <- unit_contrast(~ cond == "A", name = "A_early", where = ~ run == 1)
  expect_s3_class(con, "unit_contrast_spec")
  expect_false(is.null(con$where))
})

# --- pair_contrast ---
test_that("pair_contrast creates correct spec class", {
  con <- pair_contrast(~ cond == "A", ~ cond == "B", name = "AvB")
  expect_s3_class(con, "pair_contrast_spec")
  expect_s3_class(con, "contrast_spec")
  expect_equal(con$name, "AvB")
  expect_false(is.null(con$A))
  expect_false(is.null(con$B))
})

test_that("pair_contrast validates inputs", {
  expect_error(pair_contrast("not formula", ~ B, name = "bad"))
  expect_error(pair_contrast(~ A, "not formula", name = "bad"))
  expect_error(pair_contrast(~ A, ~ B, name = 42))
})

test_that("pair_contrast stores basis and basis_weights", {
  con <- pair_contrast(~ A, ~ B, name = "test", basis = 1:3,
                       basis_weights = c(0.2, 0.5, 0.3))
  expect_equal(con$basis, 1:3)
  expect_equal(con$basis_weights, c(0.2, 0.5, 0.3))
})

test_that("pair_contrast rejects invalid basis", {
  expect_error(pair_contrast(~ A, ~ B, name = "x", basis = -1))
  expect_error(pair_contrast(~ A, ~ B, name = "x", basis = NA))
})

test_that("pair_contrast rejects negative basis_weights", {
  expect_error(pair_contrast(~ A, ~ B, name = "x", basis_weights = c(-1, 1)))
})

# --- oneway_contrast ---
test_that("oneway_contrast creates correct spec class", {
  con <- oneway_contrast(~ cond, name = "main_cond")
  expect_s3_class(con, "oneway_contrast_spec")
  expect_s3_class(con, "contrast_spec")
  expect_equal(con$name, "main_cond")
  expect_null(con$B)
})

test_that("oneway_contrast validates inputs", {
  expect_error(oneway_contrast("not formula", name = "bad"))
  expect_error(oneway_contrast(~ cond, name = 42))
})

test_that("oneway_contrast with where clause", {
  con <- oneway_contrast(~ cond, name = "main_cond", where = ~ run == 1)
  expect_false(is.null(con$where))
})

# --- interaction_contrast ---
test_that("interaction_contrast creates correct spec class", {
  con <- interaction_contrast(~ A * B, name = "AxB")
  expect_s3_class(con, "interaction_contrast_spec")
  expect_s3_class(con, "contrast_spec")
  expect_equal(con$name, "AxB")
})

test_that("interaction_contrast validates inputs", {
  expect_error(interaction_contrast("not formula", name = "bad"))
  expect_error(interaction_contrast(~ A * B, name = 123))
})

# --- column_contrast ---
test_that("column_contrast creates correct spec class", {
  con <- column_contrast(pattern_A = "^cond_A$", name = "col_A")
  expect_s3_class(con, "column_contrast_spec")
  expect_s3_class(con, "contrast_spec")
  expect_equal(con$pattern_A, "^cond_A$")
  expect_null(con$pattern_B)
})

test_that("column_contrast with A and B patterns", {
  con <- column_contrast(pattern_A = "^A$", pattern_B = "^B$", name = "AvB")
  expect_equal(con$pattern_A, "^A$")
  expect_equal(con$pattern_B, "^B$")
})

test_that("column_contrast validates inputs", {
  expect_error(column_contrast(pattern_A = 123, name = "bad"))
  expect_error(column_contrast(pattern_A = "ok", name = 123))
  expect_error(column_contrast(pattern_A = "ok", pattern_B = 42, name = "bad"))
})

# --- poly_contrast ---
test_that("poly_contrast creates correct spec class", {
  con <- poly_contrast(~ dose, name = "lin_dose", degree = 1)
  expect_s3_class(con, "poly_contrast_spec")
  expect_s3_class(con, "contrast_spec")
  expect_equal(con$degree, 1)
})

test_that("poly_contrast with value_map", {
  con <- poly_contrast(~ dose, name = "dose_lin", degree = 2,
                       value_map = list("low" = 0, "med" = 1, "high" = 3))
  expect_equal(con$value_map, list("low" = 0, "med" = 1, "high" = 3))
  expect_equal(con$degree, 2)
})

test_that("poly_contrast validates inputs", {
  expect_error(poly_contrast("not formula", name = "bad"))
  expect_error(poly_contrast(~ A, name = 42))
  expect_error(poly_contrast(~ A, name = "ok", degree = 0))
  expect_error(poly_contrast(~ A, name = "ok", degree = -1))
})

# --- contrast_set ---
test_that("contrast_set creates correct class", {
  c1 <- pair_contrast(~ A, ~ B, name = "AvB")
  c2 <- unit_contrast(~ A, name = "A")
  cs <- contrast_set(c1, c2)
  expect_s3_class(cs, "contrast_set")
  expect_length(cs, 2)
})

test_that("contrast_set rejects non-contrast_spec objects", {
  expect_error(contrast_set("not a contrast"))
  expect_error(contrast_set(list(a = 1)))
})

# --- one_against_all_contrast ---
test_that("one_against_all_contrast returns contrast_set", {
  cs <- one_against_all_contrast(c("A", "B", "C"), facname = "cond")
  expect_s3_class(cs, "contrast_set")
  expect_length(cs, 3)
  # Each element should be a pair_contrast_spec
  for (i in seq_along(cs)) {
    expect_s3_class(cs[[i]], "pair_contrast_spec")
  }
})

# --- pairwise_contrasts ---
test_that("pairwise_contrasts returns correct number of contrasts", {
  cs <- pairwise_contrasts(c("A", "B", "C"), facname = "cond")
  expect_s3_class(cs, "contrast_set")
  expect_length(cs, 3)  # C(3,2) = 3
})

test_that("pairwise_contrasts with 4 levels", {
  cs <- pairwise_contrasts(c("A", "B", "C", "D"), facname = "cond")
  expect_length(cs, 6)  # C(4,2) = 6
})

test_that("pairwise_contrasts with custom prefix", {
  cs <- pairwise_contrasts(c("A", "B"), facname = "cond", name_prefix = "pair")
  expect_true(grepl("^pair_", cs[[1]]$name))
})

test_that("pairwise_contrasts requires at least two levels", {
  expect_error(pairwise_contrasts(c("A"), facname = "cond"))
})

# --- sliding_window_contrasts ---
test_that("sliding_window_contrasts returns correct number of contrasts", {
  # For L=5, k=2: n_con = 5 - 2*2 + 1 = 2
  cs <- sliding_window_contrasts(as.character(1:5), facname = "x", window_size = 2)
  expect_s3_class(cs, "contrast_set")
  expect_length(cs, 2)
})

test_that("sliding_window_contrasts with k=1", {
  # For L=4, k=1: n_con = 4 - 2 + 1 = 3
  cs <- sliding_window_contrasts(LETTERS[1:4], facname = "fac", window_size = 1)
  expect_length(cs, 3)
  for (i in seq_along(cs)) {
    expect_s3_class(cs[[i]], "pair_contrast_spec")
  }
})

test_that("sliding_window_contrasts errors when window too large", {
  expect_error(sliding_window_contrasts(c("A", "B", "C"), facname = "f", window_size = 2))
})

test_that("sliding_window_contrasts errors with fewer than 2 levels", {
  expect_error(sliding_window_contrasts(c("A"), facname = "f", window_size = 1))
})

# ===========================================================================
# Section 2: contrast_weights methods (require event model)
# ===========================================================================

test_that("contrast_weights.unit_contrast_spec computes correct weights", {
  emod <- make_simple_emod()
  con <- unit_contrast(~ cond == "A", name = "A_base")
  term1 <- terms(emod)[[1]]
  cw <- contrast_weights(con, term1)

  expect_s3_class(cw, "contrast")
  expect_equal(cw$name, "A_base")
  # Should have positive weights for A and zero for B
  w <- cw$weights[, 1]
  expect_true(all(w >= 0))
  # Only A should have weight
  expect_true(sum(w) > 0)
})

test_that("contrast_weights.unit_contrast_spec - all conditions", {
  emod <- make_simple_emod()
  con <- unit_contrast(~ cond, name = "all_conds")
  term1 <- terms(emod)[[1]]
  cw <- contrast_weights(con, term1)

  w <- cw$weights[, 1]
  # All conditions selected: equal positive weights summing to 1
  expect_equal(sum(w), 1, tolerance = 1e-8)
})

test_that("contrast_weights.pair_contrast_spec computes sum-to-zero weights", {
  emod <- make_simple_emod()
  con <- pair_contrast(~ cond == "A", ~ cond == "B", name = "AvB")
  term1 <- terms(emod)[[1]]
  cw <- contrast_weights(con, term1)

  expect_s3_class(cw, "contrast")
  w <- cw$weights[, 1]
  # Sum should be zero for balanced contrast
  expect_equal(sum(w), 0, tolerance = 1e-8)
  # A should be positive, B negative
  expect_true(any(w > 0))
  expect_true(any(w < 0))
})

test_that("contrast_weights.oneway_contrast_spec returns Helmert-based F-contrast", {
  emod <- make_3level_emod()
  con <- oneway_contrast(~ cond, name = "main_cond")
  term1 <- terms(emod)[[1]]
  cw <- contrast_weights(con, term1)

  expect_true(inherits(cw, "contrast"))
  # For 3 levels: should produce a matrix with 2 columns (Helmert k-1)
  expect_equal(ncol(cw$weights), 2)
})

test_that("contrast_weights.interaction_contrast_spec with 2x2 design", {
  des <- data.frame(
    onset = c(0, 10, 20, 30, 5, 15, 25, 35),
    run   = c(1, 1, 1, 1, 2, 2, 2, 2),
    cat   = factor(c("face", "scene", "face", "scene",
                     "face", "scene", "face", "scene")),
    att   = factor(c("attend", "attend", "ignore", "ignore",
                     "attend", "attend", "ignore", "ignore"))
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 1)
  emod <- event_model(onset ~ hrf(cat, att), data = des, block = ~run,
                      sampling_frame = sframe)

  con <- interaction_contrast(~ cat * att, name = "cat_by_att")
  term1 <- terms(emod)[[1]]
  cw <- contrast_weights(con, term1)

  expect_true(inherits(cw, "contrast"))
  # 2x2 interaction: (2-1)*(2-1) = 1 column
  expect_equal(ncol(cw$weights), 1)
})

test_that("contrast_weights.column_contrast_spec matches A pattern", {
  emod <- make_simple_emod()
  term1 <- terms(emod)[[1]]
  cnames <- conditions(term1, expand_basis = TRUE)

  # Match the condition containing "A"
  con <- column_contrast(pattern_A = "A", name = "col_A")
  cw <- contrast_weights(con, term1)

  expect_s3_class(cw, "column_contrast")
  w <- cw$weights[, 1]
  # Positive weights for A-matching columns

  a_idx <- grep("A", cw$condnames)
  expect_true(length(a_idx) > 0)
  expect_true(all(w[a_idx] > 0))
})

test_that("contrast_weights.column_contrast_spec A vs B pattern", {
  emod <- make_simple_emod()
  term1 <- terms(emod)[[1]]

  con <- column_contrast(pattern_A = "A", pattern_B = "B", name = "col_AvB")
  cw <- contrast_weights(con, term1)

  w <- cw$weights[, 1]
  # Should sum to zero
  expect_equal(sum(w), 0, tolerance = 1e-8)
})

test_that("contrast_weights.poly_contrast_spec produces polynomial weights", {
  des <- data.frame(
    onset = seq(0, 50, by = 10),
    run   = rep(1, 6),
    dose  = factor(c("1", "2", "3", "1", "2", "3"))
  )
  sframe <- fmrihrf::sampling_frame(blocklens = 60, TR = 1)
  emod <- event_model(onset ~ hrf(dose), data = des, block = ~run,
                      sampling_frame = sframe)

  con <- poly_contrast(~ dose, name = "lin_dose", degree = 1,
                       value_map = list("1" = 1, "2" = 2, "3" = 3))
  term1 <- terms(emod)[[1]]
  cw <- contrast_weights(con, term1)

  expect_true(inherits(cw, "contrast"))
  expect_equal(ncol(cw$weights), 1)
  # Weights should be the linear component of poly(c(1,2,3))
  expected <- as.vector(poly(c(1, 2, 3), degree = 1))
  expect_equal(as.vector(cw$weights[, 1]), expected, tolerance = 1e-6)
})

test_that("contrast_weights.poly_contrast_spec with degree 2", {
  des <- data.frame(
    onset = seq(0, 30, by = 10),
    run   = rep(1, 4),
    level = factor(c("1", "2", "3", "4"))
  )
  sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
  emod <- event_model(onset ~ hrf(level), data = des, block = ~run,
                      sampling_frame = sframe)

  con <- poly_contrast(~ level, name = "quad_level", degree = 2,
                       value_map = list("1" = 1, "2" = 2, "3" = 3, "4" = 4))
  term1 <- terms(emod)[[1]]
  cw <- contrast_weights(con, term1)

  expect_equal(ncol(cw$weights), 2)
  # Should match poly(1:4, degree = 2) values
  expected <- poly(1:4, degree = 2)
  expect_equal(as.vector(cw$weights[, 1]), as.vector(expected[, 1]), tolerance = 1e-6)
  expect_equal(as.vector(cw$weights[, 2]), as.vector(expected[, 2]), tolerance = 1e-6)
})

# ===========================================================================
# Section 3: contrast_set used with contrast_weights
# ===========================================================================

test_that("contrast_set with mixed spec types is valid", {
  c1 <- pair_contrast(~ cond == "A", ~ cond == "B", name = "AvB")
  c2 <- unit_contrast(~ cond == "A", name = "A_base")
  c3 <- oneway_contrast(~ cond, name = "main")
  cs <- contrast_set(c1, c2, c3)
  expect_length(cs, 3)
  expect_s3_class(cs, "contrast_set")
})

# ===========================================================================
# Section 4: Print methods
# ===========================================================================

test_that("print.contrast_spec outputs name", {
  con <- pair_contrast(~ A, ~ B, name = "AvB")
  out <- capture.output(print(con))
  expect_true(any(grepl("AvB", out)))
})

test_that("print.contrast_spec shows where clause when present", {
  con <- pair_contrast(~ A, ~ B, name = "test", where = ~ run == 1)
  out <- capture.output(print(con))
  expect_true(any(grepl("where", out)))
})

test_that("print.contrast_set outputs overview", {
  c1 <- pair_contrast(~ A, ~ B, name = "c1")
  c2 <- unit_contrast(~ A, name = "c2")
  cs <- contrast_set(c1, c2)
  out <- capture.output(print(cs))
  expect_true(any(grepl("Contrast Set", out)))
  expect_true(any(grepl("c1", out)))
  expect_true(any(grepl("c2", out)))
})

test_that("print.contrast outputs weights and term info", {
  emod <- make_simple_emod()
  con <- pair_contrast(~ cond == "A", ~ cond == "B", name = "AvB")
  term1 <- terms(emod)[[1]]
  cw <- contrast_weights(con, term1)
  out <- capture.output(print(cw))
  expect_true(any(grepl("AvB", out)))
  expect_true(any(grepl("weights", out)))
})

test_that("print.poly_contrast_spec shows degree and formula", {
  con <- poly_contrast(~ dose, name = "lin", degree = 2,
                       value_map = list("lo" = 0, "hi" = 1))
  out <- capture.output(print(con))
  expect_true(any(grepl("degree", out)))
  expect_true(any(grepl("2", out)))
})

# ===========================================================================
# Section 5: contrast subtraction (contrast_diff_spec)
# ===========================================================================

test_that("subtracting two contrast_specs creates contrast_diff_spec", {
  c1 <- pair_contrast(~ A, ~ B, name = "c1")
  c2 <- pair_contrast(~ C, ~ D, name = "c2")
  diff <- c1 - c2
  expect_s3_class(diff, "contrast_diff_spec")
  expect_s3_class(diff, "contrast_spec")
  expect_true(grepl("c1", diff$name))
  expect_true(grepl("c2", diff$name))
})

# ===========================================================================
# Section 6: plot_contrasts (minimal smoke test)
# ===========================================================================

test_that("plot_contrasts.event_model returns a ggplot object", {
  des <- data.frame(
    onset = c(0, 10, 20, 30, 40, 50),
    run   = rep(1, 6),
    cond  = factor(c("A", "B", "C", "A", "B", "C"))
  )
  sframe <- fmrihrf::sampling_frame(blocklens = 60, TR = 1)
  cset <- contrast_set(
    pair_contrast(~ cond == "A", ~ cond == "B", name = "A_vs_B"),
    pair_contrast(~ cond == "B", ~ cond == "C", name = "B_vs_C")
  )
  emod <- event_model(onset ~ hrf(cond, contrasts = cset),
                      data = des, block = ~run, sampling_frame = sframe)
  p <- plot_contrasts(emod)
  expect_s3_class(p, "ggplot")
})

# ===========================================================================
# Section 7: sliding_window_contrasts with event model
# ===========================================================================

test_that("sliding_window_contrasts work with event model", {
  des <- data.frame(
    onset = seq(0, 40, by = 10),
    run   = rep(1, 5),
    intensity = factor(as.character(1:5))
  )
  sframe <- fmrihrf::sampling_frame(blocklens = 50, TR = 1)
  emod <- event_model(onset ~ hrf(intensity), data = des, block = ~run,
                      sampling_frame = sframe)
  cs <- sliding_window_contrasts(levels(des$intensity), facname = "intensity",
                                 window_size = 2)
  term1 <- terms(emod)[[1]]
  # Should be able to compute weights for each contrast
  for (i in seq_along(cs)) {
    cw <- contrast_weights(cs[[i]], term1)
    expect_true(inherits(cw, "contrast"))
    w <- cw$weights[, 1]
    expect_equal(sum(w), 0, tolerance = 1e-8)
  }
})
