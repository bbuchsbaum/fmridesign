test_that("six cubic spline columns differ from legacy degree-six drift", {
  sf <- fmrihrf::sampling_frame(blocklens = 146, TR = 2)
  mat <- function(...) as.matrix(design_matrix(baseline_model(sframe = sf, ...)))
  legacy <- mat(basis = "bs", degree = 6)
  cubic <- mat(basis = "bs", degree = 3, df = 6)
  direct <- splines::bs(seq_len(146), degree = 3, df = 6)
  projector <- function(x) tcrossprod(qr.Q(qr(x)))
  expect_equal(dim(cubic), c(146L, 7L))
  expect_equal(qr(cubic)$rank, 7L)
  expect_equal(as.numeric(cubic[, 1:6]), as.numeric(direct), tolerance = 1e-12)
  expect_equal(attr(direct, "degree"), 3L)
  expect_equal(as.numeric(attr(direct, "knots")), c(37.25, 73.5, 109.75))
  expect_equal(projector(legacy), projector(cbind(1, poly(seq_len(146), 6))),
               tolerance = 1e-12)
  expect_gt(max(abs(projector(cubic) - projector(legacy))), 1e-3)
})

test_that("spline controls construct isolated bases for unequal runs", {
  lens <- c(53, 79)
  sf <- fmrihrf::sampling_frame(blocklens = lens, TR = 2)
  for (basis in c("bs", "ns")) {
    for (control in list(list(df = 6), list(knots = c(12, 25, 40)),
                         list(knots = numeric(0)))) {
      args <- c(list(basis = basis, degree = 3), control)
      model <- do.call(baseline_model, c(args, list(sframe = sf)))
      spec <- do.call(baseline, args)
      drift <- as.matrix(design_matrix(construct(spec, sf)))
      expect_equal(drift, as.matrix(design_matrix(model$terms$drift)))
      n <- ncol(drift) / 2
      rows <- list(seq_len(lens[1]), lens[1] + seq_len(lens[2]))
      for (i in 1:2) {
        cols <- (i - 1) * n + seq_len(n)
        oracle_args <- c(list(x = seq_len(lens[i]), intercept = FALSE), control)
        if (basis == "bs") oracle_args$degree <- 3
        oracle <- do.call(get(basis, asNamespace("splines")), oracle_args)
        expect_equal(as.numeric(drift[rows[[i]], cols]), as.numeric(oracle),
                     tolerance = 1e-12)
        expect_true(all(drift[-rows[[i]], cols] == 0))
      }
      full <- as.matrix(design_matrix(model))
      expect_equal(qr(full)$rank, ncol(full))
      for (intercept in c("global", "none")) {
        other <- do.call(baseline_model, c(args, list(sframe = sf, intercept = intercept)))
        expect_equal(ncol(design_matrix(other)), 2 * n + as.integer(intercept == "global"))
      }
    }
  }
})

test_that("natural splines accept explicit df independently of legacy degree", {
  sf <- fmrihrf::sampling_frame(blocklens = 30, TR = 1)
  model <- baseline_model(basis = "ns", df = 2, sframe = sf)
  expect_equal(ncol(design_matrix(model)), 3L)
  legacy <- baseline_model(basis = "ns", degree = 4, sframe = sf)
  explicit <- baseline_model(basis = "ns", df = 4, sframe = sf)
  expect_equal(design_matrix(legacy), design_matrix(explicit))
})

test_that("invalid spline controls fail clearly", {
  sf <- fmrihrf::sampling_frame(blocklens = c(50, 30), TR = 1)
  expect_error(baseline(basis = "poly", df = 6), "only supported")
  expect_error(baseline(basis = "constant", knots = 3), "only supported")
  expect_error(baseline(basis = "bs", degree = 3, df = 6, knots = 10), "only one")
  for (df in list(NA_real_, Inf, 0, 2.5, c(3, 4), "6", numeric(0))) {
    expect_error(baseline(basis = "bs", degree = 3, df = df), "positive integer")
  }
  expect_error(baseline(basis = "bs", degree = 3, df = 2), "at least")
  expect_error(baseline(basis = "bs", knots = NA_real_), "finite numeric")
  for (knots in list(1, 30, 35)) {
    expect_error(baseline_model(basis = "bs", degree = 3, knots = knots,
                                sframe = sf), "inside every run")
  }
})
