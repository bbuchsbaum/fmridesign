# Tests for basis classes defined in R/basis.R

library(testthat)

# ===========================================================================
# dctbasis
# ===========================================================================

test_that("dctbasis returns correct dimensions without constant", {
  mat <- fmridesign:::dctbasis(10, p = 10, const = FALSE)
  expect_true(is.matrix(mat))
  # p-1 columns (indices 2:p) when const = FALSE

  expect_equal(nrow(mat), 10)
  expect_equal(ncol(mat), 9)
})

test_that("dctbasis returns correct dimensions with constant", {
  mat <- fmridesign:::dctbasis(10, p = 10, const = TRUE)
  expect_equal(ncol(mat), 10)  # 1 constant + 9 cosine columns
  expect_equal(nrow(mat), 10)
})

test_that("dctbasis with small n", {
  mat <- fmridesign:::dctbasis(3, p = 3, const = FALSE)
  expect_equal(nrow(mat), 3)
  expect_equal(ncol(mat), 2)
})

# ===========================================================================
# Scale
# ===========================================================================

test_that("Scale creates z-score basis with correct class", {
  x <- c(1, 3, 5, 7, 9)
  sb <- Scale(x)
  expect_s3_class(sb, "Scale")
  expect_s3_class(sb, "ParametricBasis")
})

test_that("Scale computes correct mean and sd", {
  x <- c(10, 20, 30, 40, 50)
  sb <- Scale(x)
  expect_equal(sb$mean, mean(x))
  expect_equal(sb$sd, sd(x))
})

test_that("Scale produces z-scored values", {
  x <- c(10, 20, 30, 40, 50)
  sb <- Scale(x)
  expected_z <- (x - mean(x)) / sd(x)
  expect_equal(as.vector(sb$y), expected_z)
})

test_that("Scale result is a single-column matrix", {
  x <- c(1, 2, 3)
  sb <- Scale(x)
  expect_true(is.matrix(sb$y))
  expect_equal(ncol(sb$y), 1)
  expect_equal(nrow(sb$y), 3)
})

test_that("Scale handles NAs by replacing with 0", {
  x <- c(1, NA, 3, 5, NA)
  sb <- Scale(x)
  z_vals <- as.vector(sb$y)
  # NAs should become 0

  expect_false(anyNA(z_vals))
  expect_equal(z_vals[2], 0)
  expect_equal(z_vals[5], 0)
})

test_that("Scale handles zero variance with guard", {
  x <- c(5, 5, 5, 5)
  sb <- Scale(x)
  # sd should be 1e-6 (guard value)
  expect_equal(sb$sd, 1e-6)
})

test_that("predict.Scale applies stored mean/sd to new data", {
  x <- c(10, 20, 30, 40, 50)
  sb <- Scale(x)
  newdata <- c(15, 25, 35)
  pred <- predict(sb, newdata)
  expected <- (newdata - sb$mean) / sb$sd
  expect_equal(as.vector(pred), expected)
  expect_true(is.matrix(pred))
  expect_equal(ncol(pred), 1)
})

test_that("predict.Scale replaces NAs with 0", {
  x <- c(10, 20, 30)
  sb <- Scale(x)
  pred <- predict(sb, c(15, NA, 25))
  expect_equal(as.vector(pred)[2], 0)
})

test_that("sub_basis.Scale subsets correctly", {
  x <- 1:10
  sb <- Scale(x)
  ss <- sub_basis(sb, 1:5)
  expect_equal(length(ss$x), 5)
  expect_equal(nrow(ss$y), 5)
  # Mean and sd should be preserved from original
  expect_equal(ss$mean, sb$mean)
  expect_equal(ss$sd, sb$sd)
})

test_that("sub_basis.Scale with logical subsetting", {
  x <- 1:6
  sb <- Scale(x)
  idx <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  ss <- sub_basis(sb, idx)
  expect_equal(nrow(ss$y), 3)
})

test_that("levels.Scale returns argname", {
  x <- c(1, 2, 3)
  sb <- Scale(x)
  expect_equal(levels(sb), sb$argname)
})

test_that("columns.Scale returns continuous token", {
  x <- c(1, 2, 3)
  sb <- Scale(x)
  cols <- columns(sb)
  expect_true(is.character(cols))
  expect_length(cols, 1)
})

test_that("nbasis.Scale returns 1", {
  x <- c(1, 2, 3)
  sb <- Scale(x)
  expect_equal(nbasis(sb), 1L)
})

# ===========================================================================
# Standardized
# ===========================================================================

test_that("Standardized creates correct class", {
  x <- c(10, 20, 30, 40, 50)
  sb <- Standardized(x)
  expect_s3_class(sb, "Standardized")
  expect_s3_class(sb, "ParametricBasis")
})

test_that("Standardized computes correct mean and sd", {
  x <- c(10, 20, 30, 40, 50)
  sb <- Standardized(x)
  expect_equal(sb$mean, mean(x))
  expect_equal(sb$sd, sd(x))
})

test_that("Standardized produces standardized values", {
  x <- c(10, 20, 30, 40, 50)
  sb <- Standardized(x)
  expected <- (x - mean(x)) / sd(x)
  expect_equal(as.vector(sb$y), expected)
})

test_that("Standardized handles NAs", {
  x <- c(1, NA, 3)
  sb <- Standardized(x)
  expect_false(anyNA(sb$y))
  expect_equal(as.vector(sb$y)[2], 0)
})

test_that("Standardized handles zero variance", {
  x <- c(7, 7, 7)
  sb <- Standardized(x)
  expect_equal(sb$sd, 1e-6)
})

test_that("predict.Standardized works on new data", {
  x <- c(10, 20, 30, 40, 50)
  sb <- Standardized(x)
  newdata <- c(15, 25)
  pred <- predict(sb, newdata)
  expected <- (newdata - sb$mean) / sb$sd
  expect_equal(as.vector(pred), expected)
  expect_true(is.matrix(pred))
})

test_that("predict.Standardized replaces NAs with 0", {
  x <- c(10, 20, 30)
  sb <- Standardized(x)
  pred <- predict(sb, c(NA, 20))
  expect_equal(as.vector(pred)[1], 0)
})

test_that("sub_basis.Standardized subsets correctly", {
  x <- 1:10
  sb <- Standardized(x)
  ss <- sub_basis(sb, 3:7)
  expect_equal(nrow(ss$y), 5)
  expect_equal(ss$mean, sb$mean)
  expect_equal(ss$sd, sb$sd)
})

test_that("levels.Standardized returns argname", {
  x <- c(1, 2, 3)
  sb <- Standardized(x)
  expect_equal(levels(sb), sb$argname)
})

test_that("nbasis.Standardized returns 1", {
  x <- c(1, 2, 3)
  sb <- Standardized(x)
  expect_equal(nbasis(sb), 1L)
})

# ===========================================================================
# ScaleWithin
# ===========================================================================

test_that("ScaleWithin creates correct class", {
  x <- c(1, 2, 3, 10, 11, 12)
  g <- c("A", "A", "A", "B", "B", "B")
  sb <- ScaleWithin(x, g)
  expect_s3_class(sb, "ScaleWithin")
  expect_s3_class(sb, "ParametricBasis")
})

test_that("ScaleWithin stores per-group means and sds", {
  x <- c(1, 2, 3, 10, 11, 12)
  g <- c("A", "A", "A", "B", "B", "B")
  sb <- ScaleWithin(x, g)
  expect_equal(sb$means[["A"]], mean(c(1, 2, 3)))
  expect_equal(sb$means[["B"]], mean(c(10, 11, 12)))
  expect_equal(sb$sds[["A"]], sd(c(1, 2, 3)))
  expect_equal(sb$sds[["B"]], sd(c(10, 11, 12)))
})

test_that("ScaleWithin z-scores within groups", {
  x <- c(1, 2, 3, 10, 11, 12)
  g <- c("A", "A", "A", "B", "B", "B")
  sb <- ScaleWithin(x, g)
  z <- as.vector(sb$y)
  # Within group A: (1-2)/1, (2-2)/1, (3-2)/1 = -1, 0, 1
  expect_equal(z[1:3], (c(1, 2, 3) - 2) / sd(c(1, 2, 3)), tolerance = 1e-10)
})

test_that("ScaleWithin result is a matrix", {
  x <- c(1, 2, 3, 4, 5, 6)
  g <- c("X", "X", "X", "Y", "Y", "Y")
  sb <- ScaleWithin(x, g)
  expect_true(is.matrix(sb$y))
  expect_equal(ncol(sb$y), 1)
  expect_equal(nrow(sb$y), 6)
})

test_that("ScaleWithin errors on length mismatch", {
  expect_error(ScaleWithin(1:5, c("A", "B")))
})

test_that("predict.ScaleWithin uses stored stats", {
  x <- c(1, 2, 3, 10, 11, 12)
  g <- c("A", "A", "A", "B", "B", "B")
  sb <- ScaleWithin(x, g)
  pred <- predict(sb, c(2.5, 11.5), newgroup = c("A", "B"))
  expect_true(is.matrix(pred))
  expect_equal(nrow(pred), 2)
  # For group A: (2.5 - 2) / sd(1:3)
  expect_equal(as.vector(pred)[1], (2.5 - sb$means[["A"]]) / sb$sds[["A"]], tolerance = 1e-10)
})

test_that("sub_basis.ScaleWithin subsets correctly", {
  x <- c(1, 2, 3, 10, 11, 12)
  g <- c("A", "A", "A", "B", "B", "B")
  sb <- ScaleWithin(x, g)
  ss <- sub_basis(sb, 1:3)
  expect_equal(nrow(ss$y), 3)
  expect_equal(length(ss$x), 3)
  expect_equal(length(ss$group), 3)
})

test_that("nbasis.ScaleWithin returns 1", {
  x <- c(1, 2, 3, 4, 5, 6)
  g <- c("A", "A", "A", "B", "B", "B")
  sb <- ScaleWithin(x, g)
  expect_equal(nbasis(sb), 1L)
})

# ===========================================================================
# RobustScale
# ===========================================================================

test_that("RobustScale creates correct class", {
  x <- c(1, 2, 3, 4, 100)
  rb <- RobustScale(x)
  expect_s3_class(rb, "RobustScale")
  expect_s3_class(rb, "ParametricBasis")
})

test_that("RobustScale stores median and mad", {
  x <- c(1, 2, 3, 4, 5)
  rb <- RobustScale(x)
  expect_equal(rb$median, median(x))
  expect_equal(rb$mad, mad(x))
})

test_that("RobustScale computes (x - median) / mad", {
  x <- c(1, 2, 3, 4, 5)
  rb <- RobustScale(x)
  expected <- (x - median(x)) / mad(x)
  expect_equal(as.vector(rb$y), expected)
})

test_that("RobustScale handles NAs", {
  x <- c(1, NA, 3, 5, NA)
  rb <- RobustScale(x)
  z_vals <- as.vector(rb$y)
  expect_false(anyNA(z_vals))
  expect_equal(z_vals[2], 0)
  expect_equal(z_vals[5], 0)
})

test_that("RobustScale handles zero MAD", {
  x <- c(5, 5, 5, 5)
  rb <- RobustScale(x)
  expect_equal(rb$mad, 1e-6)
})

test_that("predict.RobustScale uses stored median/mad", {
  x <- c(1, 2, 3, 4, 5)
  rb <- RobustScale(x)
  newdata <- c(0, 3, 6)
  pred <- predict(rb, newdata)
  expected <- (newdata - rb$median) / rb$mad
  expect_equal(as.vector(pred), expected)
  expect_true(is.matrix(pred))
})

test_that("predict.RobustScale replaces NAs", {
  x <- c(1, 2, 3)
  rb <- RobustScale(x)
  pred <- predict(rb, c(NA, 2))
  expect_equal(as.vector(pred)[1], 0)
})

test_that("nbasis.RobustScale returns 1", {
  x <- c(1, 2, 3)
  rb <- RobustScale(x)
  expect_equal(nbasis(rb), 1L)
})

test_that("columns.RobustScale returns continuous token", {
  x <- c(1, 2, 3)
  rb <- RobustScale(x)
  cols <- columns(rb)
  expect_true(is.character(cols))
  expect_length(cols, 1)
})

# ===========================================================================
# Poly
# ===========================================================================

test_that("Poly creates correct class", {
  x <- 1:10
  pb <- Poly(x, degree = 3)
  expect_s3_class(pb, "Poly")
  expect_s3_class(pb, "ParametricBasis")
})

test_that("Poly produces correct dimensions", {
  x <- 1:10
  pb <- Poly(x, degree = 3)
  expect_equal(nrow(pb$y), 10)
  expect_equal(ncol(pb$y), 3)
})

test_that("Poly values match stats::poly", {
  x <- 1:10
  pb <- Poly(x, degree = 2)
  expected <- poly(x, degree = 2)
  expect_equal(unclass(pb$y), unclass(expected), tolerance = 1e-10)
})

test_that("predict.Poly works on new data", {
  x <- 1:10
  pb <- Poly(x, degree = 3)
  newdata <- c(5.5, 7.3, 11.2)
  pred <- predict(pb, newdata)
  expected <- predict(poly(x, degree = 3), newdata)
  expect_equal(pred, expected, tolerance = 1e-10)
})

test_that("levels.Poly returns zero-padded indices", {
  x <- 1:10
  pb <- Poly(x, degree = 3)
  lvls <- levels(pb)
  expect_length(lvls, 3)
  # Check that levels are character strings
  expect_true(is.character(lvls))
})

test_that("columns.Poly returns continuous tokens", {
  x <- 1:10
  pb <- Poly(x, degree = 2)
  cols <- columns(pb)
  expect_length(cols, 2)
  expect_true(is.character(cols))
})

test_that("nbasis.Poly returns degree", {
  x <- 1:10
  pb <- Poly(x, degree = 4)
  expect_equal(nbasis(pb), 4)
})

test_that("sub_basis.Poly subsets correctly", {
  x <- 1:10
  pb <- Poly(x, degree = 2)
  ss <- sub_basis(pb, 1:5)
  expect_equal(nrow(ss$y), 5)
  expect_equal(length(ss$x), 5)
})

# ===========================================================================
# BSpline
# ===========================================================================

test_that("BSpline creates correct class", {
  x <- seq(0, 1, length.out = 10)
  bs_obj <- BSpline(x, degree = 3)
  expect_s3_class(bs_obj, "BSpline")
  expect_s3_class(bs_obj, "ParametricBasis")
})

test_that("BSpline produces correct row count", {
  x <- seq(0, 1, length.out = 20)
  bs_obj <- BSpline(x, degree = 3)
  expect_equal(nrow(bs_obj$y), 20)
})

test_that("predict.BSpline works on new data", {
  x <- seq(0, 1, length.out = 20)
  bs_obj <- BSpline(x, degree = 3)
  newdata <- c(0.25, 0.5, 0.75)
  pred <- predict(bs_obj, newdata)
  expect_true(is.matrix(pred))
  expect_equal(nrow(pred), 3)
})

test_that("levels.BSpline returns correct length", {
  x <- seq(0, 1, length.out = 10)
  bs_obj <- BSpline(x, degree = 3)
  lvls <- levels(bs_obj)
  expect_equal(length(lvls), ncol(bs_obj$y))
})

test_that("columns.BSpline returns correct length", {
  x <- seq(0, 1, length.out = 10)
  bs_obj <- BSpline(x, degree = 3)
  cols <- columns(bs_obj)
  expect_length(cols, 3)
  expect_true(is.character(cols))
})

test_that("nbasis.BSpline returns degree", {
  x <- seq(0, 1, length.out = 10)
  bs_obj <- BSpline(x, degree = 4)
  expect_equal(nbasis(bs_obj), 4)
})

test_that("sub_basis.BSpline subsets correctly", {
  x <- seq(0, 1, length.out = 10)
  bs_obj <- BSpline(x, degree = 3)
  ss <- sub_basis(bs_obj, 1:5)
  expect_equal(nrow(ss$y), 5)
  expect_equal(length(ss$x), 5)
})

# ===========================================================================
# Ident
# ===========================================================================

test_that("Ident creates correct class", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(2, 4, 6, 8, 10)
  ib <- Ident(x, y)
  expect_s3_class(ib, "Ident")
  expect_s3_class(ib, "ParametricBasis")
})

test_that("Ident stores values as matrix", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)
  ib <- Ident(x, y)
  expect_true(is.matrix(ib$y))
  expect_equal(nrow(ib$y), 3)
  expect_equal(ncol(ib$y), 2)
})

test_that("predict.Ident requires matrix or data.frame input", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)
  ib <- Ident(x, y)
  expect_error(predict(ib, c(10, 20, 30)))
})

test_that("predict.Ident works with data.frame", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)
  ib <- Ident(x, y)
  newdata <- data.frame(x = c(10, 20), y = c(40, 50))
  pred <- predict(ib, newdata)
  expect_true(is.matrix(pred))
  expect_equal(nrow(pred), 2)
  expect_equal(ncol(pred), 2)
})

test_that("levels.Ident returns variable names", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)
  ib <- Ident(x, y)
  expect_equal(levels(ib), c("x", "y"))
})

test_that("columns.Ident returns continuous tokens", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)
  ib <- Ident(x, y)
  cols <- columns(ib)
  expect_length(cols, 2)
})

test_that("nbasis.Ident returns number of columns", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)
  ib <- Ident(x, y)
  expect_equal(nbasis(ib), 2)
})

test_that("sub_basis.Ident subsets correctly", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(2, 4, 6, 8, 10)
  ib <- Ident(x, y)
  ss <- sub_basis(ib, c(1, 3, 5))
  expect_equal(nrow(ss$y), 3)
})
