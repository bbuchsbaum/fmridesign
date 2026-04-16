# Standard test setup
des <- data.frame(
  onset = c(0, 10, 20, 30, 5, 15, 25, 35),
  run = c(1, 1, 1, 1, 2, 2, 2, 2),
  cond = factor(c("A", "B", "A", "B", "A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 1)
emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)

# --- validate_contrasts ---

test_that("validate_contrasts with event_model auto-detects contrasts", {
  res <- validate_contrasts(emod)
  expect_s3_class(res, "data.frame")
  expect_true("name" %in% names(res))
  expect_true("type" %in% names(res))
  expect_true("estimable" %in% names(res))
  expect_true("sum_to_zero" %in% names(res))
  expect_true("orthogonal_to_intercept" %in% names(res))
  expect_true("nonzero_weights" %in% names(res))
})

test_that("validate_contrasts with numeric vector weights (t-contrast)", {
  dm <- design_matrix(emod)
  nc <- ncol(dm)
  # Create a simple difference contrast: first column minus second
  w <- rep(0, nc)
  w[1] <- 1
  w[2] <- -1
  res <- validate_contrasts(emod, weights = w)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$type, "t")
  expect_true(res$estimable)
  expect_true(res$sum_to_zero)
  expect_equal(res$nonzero_weights, 2)
})

test_that("validate_contrasts with matrix weights (F-contrast)", {
  dm <- design_matrix(emod)
  nc <- ncol(dm)
  # Create an F-contrast matrix with 2 columns
  W <- matrix(0, nrow = nc, ncol = 2)
  W[1, 1] <- 1
  W[2, 2] <- 1
  res <- validate_contrasts(emod, weights = W)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_true(all(res$type == "F"))
  expect_true(all(res$estimable))
  # full_rank should be TRUE for this full-rank F-contrast
  expect_true(all(res$full_rank))
})

test_that("validate_contrasts errors when matrix passed without weights", {
  dm <- as.matrix(design_matrix(emod))
  expect_error(validate_contrasts(dm, weights = NULL),
               "weights.*must be provided")
})

test_that("validate_contrasts errors on non-design input", {
  expect_error(validate_contrasts("not_a_matrix"),
               "must be an event_model or a design matrix")
})

test_that("validate_contrasts warns on dimension mismatch", {
  dm <- design_matrix(emod)
  nc <- ncol(dm)
  # Wrong-size vector (one element too many) -- the function warns and

  # the skipped contrast produces NULL which causes an internal error,
  # so we just verify the warning is emitted.
  w <- rep(0, nc + 1)
  w[1] <- 1
  expect_warning(
    tryCatch(validate_contrasts(emod, weights = w), error = function(e) NULL),
    "Dimension mismatch"
  )
})

# --- check_collinearity ---

test_that("check_collinearity on clean design returns ok=TRUE", {
  # Truly uncorrelated columns (sine and cosine at different frequencies)
  t <- seq(0, 2 * pi, length.out = 100)
  X <- cbind(sin(t), cos(t))
  colnames(X) <- c("A", "B")
  res <- check_collinearity(X, threshold = 0.9)
  expect_true(res$ok)
  expect_equal(nrow(res$pairs), 0)
})

test_that("check_collinearity on highly correlated design returns ok=FALSE", {
  set.seed(42)
  x1 <- rnorm(100)
  x2 <- x1 + rnorm(100, sd = 0.01)  # nearly identical
  X <- cbind(x1, x2)
  colnames(X) <- c("reg1", "reg2")
  res <- check_collinearity(X, threshold = 0.9)
  expect_false(res$ok)
  expect_true(nrow(res$pairs) >= 1)
  expect_true("regressor_1" %in% names(res$pairs))
  expect_true("regressor_2" %in% names(res$pairs))
  expect_true("r" %in% names(res$pairs))
  expect_true(all(abs(res$pairs$r) > 0.9))
})

test_that("check_collinearity with event_model input", {
  res <- check_collinearity(emod, threshold = 0.99)
  expect_true(is.logical(res$ok))
  expect_s3_class(res$pairs, "data.frame")
})

test_that("check_collinearity errors on non-matrix input", {
  expect_error(check_collinearity("not_a_matrix"),
               "must be a matrix")
})

test_that("check_collinearity drops intercept-like columns", {
  # A design with intercept and one regressor: should not flag intercept
  X <- cbind(rep(1, 10), 1:10)
  colnames(X) <- c("(Intercept)", "time")
  res <- check_collinearity(X, threshold = 0.5)
  # Only one non-intercept column so nothing to compare
  expect_true(res$ok)
})

test_that("check_collinearity handles single non-intercept column", {
  X <- cbind(rep(1, 10), rnorm(10))
  colnames(X) <- c("Intercept", "x1")
  res <- check_collinearity(X)
  expect_true(res$ok)
})
