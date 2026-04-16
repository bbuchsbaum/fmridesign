# Standard test setup
des <- data.frame(
  onset = c(0, 10, 20, 30, 5, 15, 25, 35),
  run = c(1, 1, 1, 1, 2, 2, 2, 2),
  cond = factor(c("A", "B", "A", "B", "A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 1)
emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
bmod <- baseline_model(basis = "bs", degree = 3, sframe = sframe)

# --- .residualize_with_matrix (internal) ---

test_that(".residualize_with_matrix returns residuals orthogonal to design", {
  X <- cbind(1, 1:10)
  Y <- matrix(rnorm(20), ncol = 2)
  R <- fmridesign:::.residualize_with_matrix(X, Y)
  expect_equal(nrow(R), 10)
  expect_equal(ncol(R), 2)
  # Residuals must be orthogonal to the design columns
  cross <- crossprod(X, R)
  expect_true(all(abs(cross) < 1e-10))
})

test_that(".residualize_with_matrix handles vector Y", {
  X <- cbind(1, 1:5)
  Y <- rnorm(5)
  R <- fmridesign:::.residualize_with_matrix(X, Y)
  expect_equal(nrow(R), 5)
  expect_equal(ncol(R), 1)
})

test_that(".residualize_with_matrix handles data.frame Y", {
  X <- cbind(1, 1:5)
  Y <- data.frame(a = rnorm(5), b = rnorm(5))
  R <- fmridesign:::.residualize_with_matrix(X, Y)
  expect_equal(dim(R), c(5, 2))
})

test_that(".residualize_with_matrix errors on row mismatch", {
  X <- cbind(1, 1:5)
  Y <- matrix(rnorm(12), ncol = 2)
  expect_error(fmridesign:::.residualize_with_matrix(X, Y), "Row mismatch")
})

# --- residualize.matrix ---

test_that("residualize.matrix returns correct residuals", {
  X <- matrix(c(rep(1, 10), 1:10), ncol = 2)
  Y <- matrix(rnorm(20), ncol = 2)
  R <- residualize(X, Y)
  expect_equal(dim(R), c(10, 2))
  cross <- crossprod(X, R)
  expect_true(all(abs(cross) < 1e-10))
})

test_that("residualize.matrix with integer cols selects columns", {
  X <- matrix(c(rep(1, 10), 1:10, (1:10)^2), ncol = 3)
  colnames(X) <- c("intercept", "linear", "quadratic")
  Y <- matrix(rnorm(20), ncol = 2)
  # Only project out columns 1 and 2

R <- residualize(X, Y, cols = c(1, 2))
  expect_equal(dim(R), c(10, 2))
  # Residuals orthogonal to selected columns only
  cross <- crossprod(X[, 1:2], R)
  expect_true(all(abs(cross) < 1e-10))
})

test_that("residualize.matrix with character cols selects columns", {
  X <- matrix(c(rep(1, 10), 1:10, (1:10)^2), ncol = 3)
  colnames(X) <- c("intercept", "linear", "quadratic")
  Y <- matrix(rnorm(20), ncol = 2)
  R <- residualize(X, Y, cols = "linear")
  expect_equal(dim(R), c(10, 2))
  cross <- crossprod(X[, "linear", drop = FALSE], R)
  expect_true(all(abs(cross) < 1e-10))
})

# --- residualize.event_model ---

test_that("residualize.event_model returns correct dimensions", {
  dm <- design_matrix(emod)
  n <- nrow(dm)
  Y <- matrix(rnorm(n * 3), ncol = 3)
  R <- residualize(emod, Y)
  expect_equal(dim(R), c(n, 3))
})

test_that("residualize.event_model residuals are orthogonal to design", {
  dm <- design_matrix(emod)
  X <- as.matrix(dm)
  n <- nrow(X)
  Y <- matrix(rnorm(n * 2), ncol = 2)
  R <- residualize(emod, Y)
  cross <- crossprod(X, R)
  expect_true(all(abs(cross) < 1e-10))
})

test_that("residualize.event_model with cols parameter works", {
  dm <- design_matrix(emod)
  cn <- colnames(dm)
  n <- nrow(dm)
  Y <- matrix(rnorm(n * 2), ncol = 2)
  # Use first column name
  R <- residualize(emod, Y, cols = cn[1])
  expect_equal(dim(R), c(n, 2))
})

# --- residualize.baseline_model ---

test_that("residualize.baseline_model returns correct dimensions", {
  dm <- design_matrix(bmod)
  n <- nrow(dm)
  Y <- matrix(rnorm(n * 2), ncol = 2)
  R <- residualize(bmod, Y)
  expect_equal(dim(R), c(n, 2))
})

test_that("residualize.baseline_model residuals are orthogonal to design", {
  dm <- design_matrix(bmod)
  X <- as.matrix(dm)
  n <- nrow(X)
  Y <- matrix(rnorm(n * 2), ncol = 2)
  R <- residualize(bmod, Y)
  cross <- crossprod(X, R)
  expect_true(all(abs(cross) < 1e-10))
})
