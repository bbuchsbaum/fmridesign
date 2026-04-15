# Tests for event class constructors and methods defined in R/event-classes.R

library(testthat)

# ===========================================================================
# event() internal constructor (accessed via public wrappers)
# ===========================================================================

test_that("event() with factor data creates categorical event", {
  fac <- factor(c("A", "B", "A", "B"))
  onsets <- c(1, 10, 20, 30)
  blockids <- rep(1, 4)

  ev <- fmridesign:::event(fac, name = "cond", onsets = onsets,
                           blockids = blockids)
  expect_s3_class(ev, "event")
  expect_s3_class(ev, "event_seq")
  expect_false(ev$continuous)
  expect_equal(as.character(ev$varname), "cond")
  expect_equal(length(ev$onsets), 4)
})

test_that("event() with character data creates categorical event", {
  ch <- c("X", "Y", "X", "Y")
  onsets <- c(1, 10, 20, 30)
  blockids <- rep(1, 4)

  ev <- fmridesign:::event(ch, name = "cond", onsets = onsets,
                           blockids = blockids)
  expect_false(ev$continuous)
  expect_true(!is.null(ev$meta$levels))
})

test_that("event() with numeric data creates continuous event", {
  vec <- c(1.5, 2.3, 3.1, 4.7)
  onsets <- c(1, 10, 20, 30)
  blockids <- rep(1, 4)

  ev <- fmridesign:::event(vec, name = "modulator", onsets = onsets,
                           blockids = blockids)
  expect_true(ev$continuous)
  expect_true(is.matrix(ev$value))
  expect_equal(ncol(ev$value), 1)
})

test_that("event() with subset applies correctly", {
  fac <- factor(c("A", "B", "C", "D"))
  onsets <- c(1, 10, 20, 30)
  blockids <- rep(1, 4)
  subset <- c(TRUE, FALSE, TRUE, FALSE)

  ev <- fmridesign:::event(fac, name = "cond", onsets = onsets,
                           blockids = blockids, subset = subset)
  expect_equal(length(ev$onsets), 2)
  expect_equal(nrow(ev$value), 2)
})

test_that("event() subset length mismatch errors", {
  fac <- factor(c("A", "B"))
  onsets <- c(1, 10)
  blockids <- rep(1, 2)
  expect_error(
    fmridesign:::event(fac, name = "cond", onsets = onsets,
                       blockids = blockids, subset = c(TRUE)),
    "subset length"
  )
})

test_that("event() subset with NA errors", {
  fac <- factor(c("A", "B"))
  onsets <- c(1, 10)
  blockids <- rep(1, 2)
  expect_error(
    fmridesign:::event(fac, name = "cond", onsets = onsets,
                       blockids = blockids, subset = c(TRUE, NA)),
    "NA"
  )
})

# ===========================================================================
# event_factor()
# ===========================================================================

test_that("event_factor creates categorical event from factor", {
  fac <- factor(c("A", "B", "C", "A", "B", "C"))
  onsets <- seq(1, 60, length.out = 6)
  blockids <- rep(1, 6)

  ev <- event_factor(fac, "condition", onsets = onsets, blockids = blockids)
  expect_s3_class(ev, "event")
  expect_false(ev$continuous)
  expect_equal(as.character(ev$varname), "condition")
})

test_that("event_factor preserves factor levels", {
  fac <- factor(c("A", "B", "A"), levels = c("A", "B", "C"))
  onsets <- c(1, 10, 20)
  blockids <- rep(1, 3)

  ev <- event_factor(fac, "cond", onsets = onsets, blockids = blockids)
  # All original levels should be preserved, including unused "C"
  expect_true("C" %in% ev$meta$levels)
  expect_equal(length(ev$meta$levels), 3)
})

test_that("event_factor with character input", {
  ch <- c("X", "Y", "Z")
  onsets <- c(1, 10, 20)
  blockids <- rep(1, 3)

  ev <- event_factor(ch, "fac", onsets = onsets, blockids = blockids)
  expect_false(ev$continuous)
})

test_that("event_factor warns for non-factor/character input", {
  expect_warning(
    event_factor(c(1, 2, 3), "nums", onsets = c(1, 10, 20),
                 blockids = rep(1, 3)),
    "not a factor"
  )
})

test_that("event_factor with subset", {
  fac <- factor(c("A", "B", "C", "D"))
  onsets <- c(1, 10, 20, 30)
  blockids <- rep(1, 4)

  ev <- event_factor(fac, "cond", onsets = onsets, blockids = blockids,
                     subset = c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(length(ev$onsets), 2)
})

test_that("event_factor with durations", {
  fac <- factor(c("A", "B"))
  onsets <- c(1, 10)
  blockids <- rep(1, 2)

  ev <- event_factor(fac, "cond", onsets = onsets, blockids = blockids,
                     durations = c(2, 3))
  expect_equal(ev$durations, c(2, 3))
})

# ===========================================================================
# event_variable()
# ===========================================================================

test_that("event_variable creates continuous event", {
  vec <- c(1.5, 2.3, 3.1)
  onsets <- c(1, 10, 20)
  blockids <- rep(1, 3)

  ev <- event_variable(vec, "rt", onsets = onsets, blockids = blockids)
  expect_s3_class(ev, "event")
  expect_true(ev$continuous)
  expect_true(is.matrix(ev$value))
  expect_equal(ncol(ev$value), 1)
})

test_that("event_variable rejects factor input", {
  expect_error(
    event_variable(factor(c("A", "B")), "bad", onsets = c(1, 10),
                   blockids = rep(1, 2))
  )
})

test_that("event_variable rejects non-numeric input", {
  expect_error(
    event_variable("text", "bad", onsets = 1, blockids = 1)
  )
})

test_that("event_variable preserves values", {
  vec <- c(100, 200, 300)
  onsets <- c(1, 10, 20)
  blockids <- rep(1, 3)

  ev <- event_variable(vec, "val", onsets = onsets, blockids = blockids)
  expect_equal(as.vector(ev$value), vec)
})

test_that("event_variable with subset", {
  vec <- c(1, 2, 3, 4)
  onsets <- c(1, 10, 20, 30)
  blockids <- rep(1, 4)

  ev <- event_variable(vec, "val", onsets = onsets, blockids = blockids,
                       subset = c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(length(ev$onsets), 2)
  expect_equal(as.vector(ev$value), c(1, 3))
})

# ===========================================================================
# event_matrix()
# ===========================================================================

test_that("event_matrix creates continuous event from matrix", {
  mat <- matrix(rnorm(20), nrow = 10, ncol = 2,
                dimnames = list(NULL, c("V1", "V2")))
  onsets <- seq(1, 100, length.out = 10)
  blockids <- rep(1, 10)

  ev <- event_matrix(mat, "mset", onsets = onsets, blockids = blockids)
  expect_s3_class(ev, "event")
  expect_true(ev$continuous)
  expect_equal(ncol(ev$value), 2)
  expect_equal(nrow(ev$value), 10)
})

test_that("event_matrix rejects non-matrix input", {
  expect_error(
    event_matrix(c(1, 2, 3), "bad", onsets = c(1, 2, 3),
                 blockids = rep(1, 3))
  )
})

test_that("event_matrix rejects non-numeric matrix", {
  mat <- matrix(c("a", "b", "c", "d"), nrow = 2)
  expect_error(
    event_matrix(mat, "bad", onsets = c(1, 2), blockids = rep(1, 2))
  )
})

test_that("event_matrix checks nrow matches onsets", {
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  expect_error(
    event_matrix(mat, "bad", onsets = c(1, 2), blockids = rep(1, 2)),
    "mismatch"
  )
})

test_that("event_matrix assigns colnames when missing", {
  mat <- matrix(1:10, nrow = 5, ncol = 2)
  onsets <- seq(1, 50, length.out = 5)
  blockids <- rep(1, 5)

  ev <- event_matrix(mat, "m", onsets = onsets, blockids = blockids)
  expect_false(is.null(colnames(ev$value)))
})

test_that("event_matrix with subset", {
  mat <- matrix(1:8, nrow = 4, ncol = 2, dimnames = list(NULL, c("a", "b")))
  onsets <- c(1, 10, 20, 30)
  blockids <- rep(1, 4)

  ev <- event_matrix(mat, "m", onsets = onsets, blockids = blockids,
                     subset = c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(nrow(ev$value), 2)
})

# ===========================================================================
# event_basis()
# ===========================================================================

test_that("event_basis creates event from BSpline basis", {
  basis <- BSpline(1:10, degree = 3)
  onsets <- seq(0, 9)
  blockids <- rep(1, 10)

  ev <- event_basis(basis, onsets = onsets, blockids = blockids)
  expect_s3_class(ev, "event")
  expect_true(ev$continuous)
  expect_false(is.null(ev$meta$basis))
})

test_that("event_basis uses basis$name as default name", {
  basis <- BSpline(1:10, degree = 3)
  onsets <- seq(0, 9)
  blockids <- rep(1, 10)

  ev <- event_basis(basis, onsets = onsets, blockids = blockids)
  expect_true(grepl("bs_", ev$varname))
})

test_that("event_basis uses custom name when provided", {
  basis <- Poly(1:10, degree = 2)
  onsets <- seq(0, 9)
  blockids <- rep(1, 10)

  ev <- event_basis(basis, name = "my_poly", onsets = onsets,
                    blockids = blockids)
  expect_equal(as.character(ev$varname), "my_poly")
})

test_that("event_basis rejects non-ParametricBasis input", {
  expect_error(
    event_basis(list(a = 1), onsets = 1, blockids = 1),
    "ParametricBasis"
  )
})

test_that("event_basis with Poly basis", {
  basis <- Poly(1:8, degree = 2)
  onsets <- seq(0, 7)
  blockids <- rep(1, 8)

  ev <- event_basis(basis, onsets = onsets, blockids = blockids)
  expect_true(ev$continuous)
  expect_equal(nrow(ev$value), 8)
})

test_that("event_basis with Scale basis", {
  basis <- Scale(c(10, 20, 30, 40, 50))
  onsets <- c(0, 5, 10, 15, 20)
  blockids <- rep(1, 5)

  ev <- event_basis(basis, onsets = onsets, blockids = blockids)
  expect_true(ev$continuous)
  expect_equal(nrow(ev$value), 5)
})

test_that("event_basis with subset", {
  basis <- BSpline(1:10, degree = 3)
  onsets <- seq(0, 9)
  blockids <- rep(1, 10)
  subset <- c(rep(TRUE, 5), rep(FALSE, 5))

  ev <- event_basis(basis, onsets = onsets, blockids = blockids,
                    subset = subset)
  expect_equal(length(ev$onsets), 5)
  expect_equal(nrow(ev$value), 5)
})

# ===========================================================================
# levels.event
# ===========================================================================

test_that("levels.event returns factor levels for categorical event", {
  fac <- factor(c("A", "B", "C", "A"), levels = c("A", "B", "C"))
  onsets <- c(1, 10, 20, 30)
  blockids <- rep(1, 4)

  ev <- event_factor(fac, "cond", onsets = onsets, blockids = blockids)
  expect_equal(levels(ev), c("A", "B", "C"))
})

test_that("levels.event returns colnames for numeric variable", {
  vec <- c(1, 2, 3)
  onsets <- c(1, 10, 20)
  blockids <- rep(1, 3)

  ev <- event_variable(vec, "rt", onsets = onsets, blockids = blockids)
  lvls <- levels(ev)
  expect_true(is.character(lvls))
  expect_length(lvls, 1)
})

test_that("levels.event returns colnames for matrix event", {
  mat <- matrix(1:6, nrow = 3, ncol = 2, dimnames = list(NULL, c("V1", "V2")))
  onsets <- c(1, 10, 20)
  blockids <- rep(1, 3)

  ev <- event_matrix(mat, "m", onsets = onsets, blockids = blockids)
  lvls <- levels(ev)
  expect_length(lvls, 2)
})

test_that("levels.event returns basis levels for basis event", {
  basis <- Poly(1:5, degree = 2)
  onsets <- 0:4
  blockids <- rep(1, 5)

  ev <- event_basis(basis, onsets = onsets, blockids = blockids)
  lvls <- levels(ev)
  expect_true(is.character(lvls))
  expect_length(lvls, 2)
})

# ===========================================================================
# cells.event
# ===========================================================================

test_that("cells.event returns tibble with factor levels for categorical", {
  fac <- factor(c("A", "B", "A", "B"))
  onsets <- c(1, 10, 20, 30)
  blockids <- rep(1, 4)

  ev <- event_factor(fac, "cond", onsets = onsets, blockids = blockids)
  cc <- cells(ev)
  expect_s3_class(cc, "tbl_df")
  expect_equal(nrow(cc), 2)  # 2 unique levels
  expect_true("cond" %in% names(cc))
})

test_that("cells.event with drop.empty = FALSE keeps empty levels", {
  fac <- factor(c("A", "A"), levels = c("A", "B", "C"))
  onsets <- c(1, 10)
  blockids <- rep(1, 2)

  ev <- event_factor(fac, "cond", onsets = onsets, blockids = blockids)

  cc_drop <- cells(ev, drop.empty = TRUE)
  cc_keep <- cells(ev, drop.empty = FALSE)

  # drop.empty = TRUE should only keep A
  expect_equal(nrow(cc_drop), 1)
  # drop.empty = FALSE should keep all 3 levels
  expect_equal(nrow(cc_keep), 3)
})

test_that("cells.event for continuous returns variable name", {
  vec <- c(1, 2, 3)
  onsets <- c(1, 10, 20)
  blockids <- rep(1, 3)

  ev <- event_variable(vec, "modulator", onsets = onsets, blockids = blockids)
  cc <- cells(ev)
  expect_s3_class(cc, "tbl_df")
  expect_equal(nrow(cc), 1)
})

test_that("cells.event has count attribute", {
  fac <- factor(c("A", "B", "A", "A"))
  onsets <- c(1, 10, 20, 30)
  blockids <- rep(1, 4)

  ev <- event_factor(fac, "cond", onsets = onsets, blockids = blockids)
  cc <- cells(ev)
  cnt <- attr(cc, "count")
  expect_false(is.null(cnt))
  # A should have count 3, B should have count 1
  expect_equal(cnt[["A"]], 3)
  expect_equal(cnt[["B"]], 1)
})

# ===========================================================================
# elements.event
# ===========================================================================

test_that("elements.event returns value matrix for 'values' mode", {
  vec <- c(10, 20, 30)
  onsets <- c(1, 10, 20)
  blockids <- rep(1, 3)

  ev <- event_variable(vec, "val", onsets = onsets, blockids = blockids)
  elem <- elements(ev, what = "values")
  expect_true(is.matrix(elem))
  expect_equal(nrow(elem), 3)
})

test_that("elements.event returns factor labels for categorical 'labels'", {
  fac <- factor(c("A", "B", "A", "B"))
  onsets <- c(1, 10, 20, 30)
  blockids <- rep(1, 4)

  ev <- event_factor(fac, "cond", onsets = onsets, blockids = blockids)
  elem <- elements(ev, what = "labels")
  expect_true(is.factor(elem))
  expect_equal(length(elem), 4)
  expect_equal(as.character(elem), c("A", "B", "A", "B"))
})

test_that("elements.event labels for continuous repeats name", {
  vec <- c(1, 2, 3)
  onsets <- c(1, 10, 20)
  blockids <- rep(1, 3)

  ev <- event_variable(vec, "rt", onsets = onsets, blockids = blockids)
  elem <- elements(ev, what = "labels")
  expect_equal(length(elem), 3)
  # All should be the same label
  expect_equal(length(unique(elem)), 1)
})

# ===========================================================================
# is_continuous / is_categorical
# ===========================================================================

test_that("is_continuous returns TRUE for numeric event", {
  vec <- c(1, 2, 3)
  onsets <- c(1, 10, 20)
  blockids <- rep(1, 3)

  ev <- event_variable(vec, "val", onsets = onsets, blockids = blockids)
  expect_true(is_continuous(ev))
  expect_false(is_categorical(ev))
})

test_that("is_continuous returns FALSE for factor event", {
  fac <- factor(c("A", "B"))
  onsets <- c(1, 10)
  blockids <- rep(1, 2)

  ev <- event_factor(fac, "cond", onsets = onsets, blockids = blockids)
  expect_false(is_continuous(ev))
  expect_true(is_categorical(ev))
})

test_that("is_continuous returns TRUE for matrix event", {
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  onsets <- c(1, 10, 20)
  blockids <- rep(1, 3)

  ev <- event_matrix(mat, "m", onsets = onsets, blockids = blockids)
  expect_true(is_continuous(ev))
})

test_that("is_continuous returns TRUE for basis event", {
  basis <- Poly(1:5, degree = 2)
  onsets <- 0:4
  blockids <- rep(1, 5)

  ev <- event_basis(basis, onsets = onsets, blockids = blockids)
  expect_true(is_continuous(ev))
})

# ===========================================================================
# labels.event
# ===========================================================================

test_that("labels.event for categorical returns Variable[Level] format", {
  fac <- factor(c("A", "B", "A", "B"))
  onsets <- c(1, 10, 20, 30)
  blockids <- rep(1, 4)

  ev <- event_factor(fac, "Condition", onsets = onsets, blockids = blockids)
  lab <- labels(ev)
  expect_true(is.character(lab))
  # Should contain both levels
  expect_length(lab, 2)
})

test_that("labels.event for numeric variable returns variable name", {
  vec <- c(1, 2, 3)
  onsets <- c(1, 10, 20)
  blockids <- rep(1, 3)

  ev <- event_variable(vec, "Modulator", onsets = onsets, blockids = blockids)
  lab <- labels(ev)
  expect_true(is.character(lab))
})

# ===========================================================================
# print.event
# ===========================================================================

test_that("print.event for categorical runs without error", {
  fac <- factor(c("A", "B", "C"))
  onsets <- c(1, 10, 20)
  blockids <- rep(1, 3)

  ev <- event_factor(fac, "cond", onsets = onsets, blockids = blockids)
  expect_identical(print(ev), ev)
})

test_that("print.event for continuous runs without error", {
  vec <- c(1.5, 2.3, 3.1)
  onsets <- c(1, 10, 20)
  blockids <- rep(1, 3)

  ev <- event_variable(vec, "rt", onsets = onsets, blockids = blockids)
  expect_identical(print(ev), ev)
})

test_that("print.event handles larger event sets", {
  fac <- factor(c("A", "B", "A", "B", "A"))
  onsets <- c(1, 10, 20, 30, 40)
  blockids <- rep(1, 5)

  ev <- event_factor(fac, "cond", onsets = onsets, blockids = blockids)
  expect_identical(print(ev), ev)
})

test_that("print.event returns invisible self", {
  vec <- c(1, 2, 3)
  onsets <- c(1, 10, 20)
  blockids <- rep(1, 3)
  ev <- event_variable(vec, "val", onsets = onsets, blockids = blockids)

  result <- capture.output(ret <- print(ev))
  expect_identical(ret, ev)
})

# ===========================================================================
# columns.event (alias for levels.event)
# ===========================================================================

test_that("columns.event is alias for levels.event", {
  fac <- factor(c("A", "B", "C"))
  onsets <- c(1, 10, 20)
  blockids <- rep(1, 3)

  ev <- event_factor(fac, "cond", onsets = onsets, blockids = blockids)
  expect_equal(columns(ev), levels(ev))
})
