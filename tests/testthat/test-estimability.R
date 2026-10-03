test_that("no-rest reproduction exposes weak absolute betas despite pairwise pass", {
  tasks <- paste0("task", seq_len(12))
  on <- seq(0, by = 35, length.out = 12)
  ev <- rbind(data.frame(onset = on + 5, duration = 30, condition = tasks),
              data.frame(onset = on, duration = 5, condition = "cue"))
  ev <- ev[order(ev$onset), ]
  ev$condition <- factor(ev$condition)
  ev$run <- 1
  sf <- fmrihrf::sampling_frame(blocklens = 420, TR = 1)
  model <- event_model(onset ~ hrf(condition, basis = "spmg1"), data = ev,
                       block = ~run, sampling_frame = sf, durations = ev$duration)
  E <- as.matrix(design_matrix(model))
  X <- cbind(E, intercept = 1)
  task_idx <- grep("task", colnames(E))
  W <- diag(ncol(E))[, task_idx, drop = FALSE]
  W[task_idx, ] <- W[task_idx, ] - 1 / length(task_idx)
  report <- check_estimability(model, contrasts = list(centred = W))
  expect_true(check_collinearity(X)$ok)
  expect_s3_class(report, "estimability_check")
  expect_true(report$full_rank)
  expect_false(report$ok)
  expect_equal(report$rank, 14L)
  expect_equal(report$condition_number, 19.79912, tolerance = 1e-4)
  expect_equal(report$baseline_pattern, "baseline_minus_regressors")
  weak <- report$weakest_direction
  expect_equal(abs(weak$loading[weak$baseline]), 0.7127586, tolerance = 1e-5)
  expect_true(all(sign(weak$loading[!weak$baseline]) == -sign(weak$loading[weak$baseline])))
  metrics <- report$contrasts
  absolute <- metrics$variance_factor[metrics$quantity == "absolute"][task_idx]
  centred <- metrics$variance_factor[metrics$quantity == "contrast"]
  expect_true(all(metrics$estimable))
  expect_equal(median(absolute / centred), 8.795058, tolerance = 1e-5)
  # Independent direct inverse oracle, rather than repeating the SVD algorithm.
  covariance <- solve(crossprod(X))
  expect_equal(absolute, unname(diag(covariance)[task_idx]), tolerance = 1e-10)
  Wfull <- rbind(W, 0)
  expect_equal(centred, unname(diag(t(Wfull) %*% covariance %*% Wfull)), tolerance = 1e-10)
  expect_equal(report$coverage$fraction, 1)
  expect_true(report$coverage$high_coverage)
  expect_match(paste(report$messages, collapse = " "), "centred contrasts")
  without_baseline <- check_estimability(model, baseline = FALSE)
  expect_lt(without_baseline$condition_number, 2)
  expect_null(without_baseline$baseline_pattern)
  expect_equal(check_estimability(X)$condition_number, report$condition_number)
})

test_that("exact aliases distinguish identifiable contrasts from absolute coefficients", {
  A <- model.matrix(~ 0 + factor(rep(seq_len(12), each = 10)))
  colnames(A) <- paste0("task", seq_len(12))
  X <- cbind(A, intercept = 1)
  cvec <- c(1, -1, rep(0, 11))
  report <- check_estimability(X, contrasts = list(difference = cvec))
  expect_true(check_collinearity(X)$ok)
  expect_equal(report$rank, 12L)
  expect_identical(report$condition_number, Inf)
  expect_false(report$contrasts$estimable[2])
  expect_true(all(is.na(report$contrasts$variance_factor[-1L])))
  expect_true(report$contrasts$estimable[1])
  # Difference of two independent group means, each based on ten observations.
  expect_equal(report$contrasts$variance_factor[1], 2 / 10, tolerance = 1e-12)
  expect_equal(report$baseline_pattern, "baseline_minus_regressors")
})

test_that("wide, zero-column and all-zero designs retain their null space", {
  X <- rbind(c(1, 0, 1, 0), c(0, 1, 0, 0))
  colnames(X) <- c("a", "b", "duplicate_a", "zero")
  report <- check_estimability(X, contrasts = list(sum = c(1, 0, 1, 0)))
  expect_equal(report$rank, 2L)
  expect_equal(length(report$singular_values), 4L)
  expect_equal(report$contrasts$estimable, c(TRUE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(report$contrasts$variance_factor[c(1, 3)], c(1, 1), tolerance = 1e-12)
  expect_true(all(is.na(report$contrasts$variance_factor[c(2, 4, 5)])))
  zero <- check_estimability(matrix(0, 3, 2), contrasts = c(0, 0))
  expect_equal(zero$rank, 0L)
  expect_identical(zero$condition_number, Inf)
  expect_equal(zero$contrasts$estimable, c(TRUE, FALSE, FALSE))
  expect_equal(zero$contrasts$variance_factor[1], 0)
  singleton <- check_estimability(matrix(2, 1, 1))
  expect_equal(singleton$condition_number, 1)
  expect_equal(singleton$contrasts$variance_factor, 1 / 4)
})

test_that("column units and permutations preserve inference with transformed contrasts", {
  X <- cbind(a = c(1, 2, 4, 0, 3), b = c(-2, 1, 0, 2, 1), intercept = 1)
  weights <- c(a = 2, b = -1, intercept = 0)
  original <- check_estimability(X, weights, absolute = FALSE)
  scales <- c(1e-6, -1000, 7)
  scaled <- sweep(X, 2L, scales, "*")
  perm <- c(3, 1, 2)
  # Named weights deliberately stay in the original order.
  changed <- check_estimability(scaled[, perm], weights * scales, absolute = FALSE)
  expect_equal(changed$condition_number, original$condition_number, tolerance = 1e-12)
  expect_equal(changed$contrasts$variance_factor, original$contrasts$variance_factor, tolerance = 1e-12)
  # This identity also applies when a generalized inverse is needed.
  Xa <- cbind(X, redundant = X[, "a"] + X[, "b"])
  c_alias <- c(weights, redundant = sum(weights[1:2]))
  alias_report <- check_estimability(Xa, c_alias, absolute = FALSE)
  alias_scaled <- check_estimability(sweep(Xa, 2L, c(scales, 100), "*"),
                                    c_alias * c(scales, 100), absolute = FALSE)
  expect_true(alias_report$contrasts$estimable)
  expect_equal(alias_scaled$rank, alias_report$rank)
  expect_equal(alias_scaled$contrasts$variance_factor, alias_report$contrasts$variance_factor, tolerance = 1e-10)
})

test_that("numerical rank cutoff controls near aliases without discarding variance", {
  X <- cbind(a = c(1, 0, 0), b = c(1, 1e-6, 0))
  full <- check_estimability(X, tol = 1e-8)
  truncated <- check_estimability(X, tol = 1e-5)
  expect_true(full$full_rank)
  expect_true(all(full$contrasts$estimable))
  expect_gt(min(full$contrasts$variance_factor), 1e11)
  expect_equal(truncated$rank, 1L)
  expect_true(all(!truncated$contrasts$estimable))
  expect_true(all(is.na(truncated$contrasts$variance_factor)))
})

test_that("baseline handling keeps full designs and uses per-run intercepts", {
  sf <- fmrihrf::sampling_frame(c(20, 30), TR = 2)
  ev <- data.frame(onset = c(0, 10, 2, 20), run = c(1, 1, 2, 2),
                   cond = factor(c("A", "B", "A", "B")))
  model <- event_model(onset ~ hrf(cond), ev, block = ~run, sampling_frame = sf)
  E <- as.matrix(design_matrix(model))
  automatic <- check_estimability(model)
  expect_equal(automatic$n_columns, ncol(E) + 2L)
  B <- cbind(run1 = rep(c(1, 0), c(20, 30)), run2 = rep(c(0, 1), c(20, 30)))
  manual <- check_estimability(E, baseline = B)
  expect_equal(automatic$condition_number, manual$condition_number)
  expect_equal(automatic$contrasts$variance_factor, manual$contrasts$variance_factor)
  bmod <- baseline_model(basis = "poly", degree = 1, sframe = sf)
  with_drift <- check_estimability(model, baseline = bmod)
  full <- cbind(E, as.matrix(design_matrix(bmod)))
  expect_equal(with_drift$condition_number, check_estimability(full)$condition_number)
  expect_equal(with_drift$n_columns, ncol(full))
  expect_equal(with_drift$baseline$source, "baseline_model")
  expect_equal(check_estimability(E, baseline = TRUE)$n_columns, ncol(E) + 1)
  expect_equal(check_estimability(cbind(E, constant = 2), baseline = TRUE)$n_columns, ncol(E) + 1)
  # Explicitly supplied duplicate baseline columns must remain visible as aliases.
  duplicate <- check_estimability(cbind(E, constant = 1), baseline = matrix(1, nrow(E), 1))
  expect_false(duplicate$full_rank)
  expect_error(check_estimability(model, baseline = baseline_model(
    sframe = fmrihrf::sampling_frame(50, TR = 2))), "same sampling frame")
  # Reuse run intercepts already present as covariates, including scaled ones.
  regs <- as.data.frame(sweep(B, 2L, c(2, 3), "*"))
  existing <- event_model(~ covariate(run1, run2, data = regs), sampling_frame = sf)
  reused <- check_estimability(existing)
  expect_equal(reused$n_columns, 2L)
  expect_equal(reused$rank, 2L)
  expect_equal(reused$baseline$columns, c("cov_run1", "cov_run2"))
  expect_null(reused$coverage)
})

test_that("declared t and multicolumn contrasts are evaluated in the full design", {
  sf <- fmrihrf::sampling_frame(60, TR = 1)
  ev <- data.frame(onset = seq(0, 50, by = 10), run = 1,
                   cond = factor(rep(c("A", "B", "C"), 2)))
  cset <- contrast_set(diff = pair_contrast(~ cond == "A", ~ cond == "B", name = "diff"),
                        main = oneway_contrast(~ cond, name = "main"))
  model <- event_model(onset ~ hrf(cond, contrasts = cset), ev,
                       block = ~run, sampling_frame = sf)
  report <- check_estimability(model, absolute = FALSE)
  expect_equal(nrow(report$contrasts), 3L)
  expect_true(all(report$contrasts$estimable))
  declared <- contrast_weights(model)
  W <- do.call(cbind, lapply(declared, function(z) z$offset_weights))
  X <- cbind(as.matrix(design_matrix(model)), 1)
  W <- rbind(W, 0)
  expected <- diag(t(W) %*% solve(crossprod(X), W))
  expect_equal(report$contrasts$variance_factor, unname(expected), tolerance = 1e-10)
})

test_that("coverage clips and unions event intervals per run without double counting", {
  sf <- fmrihrf::sampling_frame(c(10, 15), TR = 2)
  ev <- data.frame(onset = c(0, 4, 18, 0), duration = c(6, 8, 8, 30),
                   run = c(1, 1, 1, 2), cond = factor(c("A", "B", "A", "B")))
  model <- suppressWarnings(event_model(onset ~ hrf(cond) + hrf(cond, id = "copy"),
    ev, block = ~run, sampling_frame = sf, durations = ev$duration))
  report <- check_estimability(model)
  expect_equal(report$coverage$covered, c(14, 30))
  expect_equal(report$coverage$fraction, c(0.7, 1))
  expect_equal(report$coverage$high_coverage, c(FALSE, TRUE))
  subsetted <- suppressWarnings(event_model(onset ~ hrf(cond, subset = run == 1),
    ev, block = ~run, sampling_frame = sf, durations = ev$duration))
  expect_equal(check_estimability(subsetted)$coverage$covered, c(14, 0))
  overridden <- suppressWarnings(event_model(onset ~ hrf(cond, durations = 2),
    ev, block = ~run, sampling_frame = sf, durations = ev$duration))
  expect_equal(check_estimability(overridden)$coverage$covered, c(6, 2))
  impulses <- suppressWarnings(event_model(onset ~ hrf(cond), ev, block = ~run,
                                           sampling_frame = sf, durations = 0))
  expect_equal(check_estimability(impulses)$coverage$fraction, c(0, 0))
  expect_null(check_estimability(as.matrix(design_matrix(model)))$coverage)
})

test_that("named contrast alignment is strict even when dimensions already match", {
  X <- cbind(a = c(1, 0, 0), b = c(0, 2, 0))
  report <- check_estimability(X, c(b = 1, a = 2), absolute = FALSE)
  expect_equal(report$contrasts$variance_factor, 4 + 1 / 4)
  subset <- check_estimability(X, c(b = 1), absolute = FALSE)
  expect_equal(subset$contrasts$variance_factor, 1 / 4)
  expect_error(check_estimability(X, c(typo = 1, a = 2)), "Unknown contrast")
  expect_error(check_estimability(X, c(a = 1, a = 2)), "unique")
  expect_error(check_estimability(X, c(1, 2, 3)), "span the input or full design")
  expect_error(check_estimability(X, c(NA, 1)), "finite")
  expect_error(check_estimability(X, list(c(1, 0), NULL)), "numeric design matrix")
  empty <- check_estimability(X, absolute = FALSE)
  expect_equal(nrow(empty$contrasts), 0L)
  expect_named(empty$contrasts, c("name", "quantity", "estimable", "variance_factor", "null_fraction"))
})

test_that("invalid inputs fail explicitly and sparse designs give the same result", {
  X <- cbind(a = c(1, 0, 2), b = c(0, 1, 0))
  dense <- check_estimability(X)
  expect_equal(check_estimability(Matrix::Matrix(X, sparse = TRUE)), dense)
  expect_error(check_estimability("x"), "numeric design matrix")
  expect_error(check_estimability(matrix(NA_real_, 2, 2)), "finite")
  expect_error(check_estimability(matrix(0, 0, 2)), "nonempty")
  expect_error(check_estimability(X, baseline = matrix(1, 4, 1)), "same number of rows")
  expect_error(check_estimability(X, baseline = cbind(a = 1:3)), "unique")
  expect_error(check_estimability(X, baseline = NA), "TRUE or FALSE")
  expect_error(check_estimability(X, tol = 0), "tol")
  expect_error(check_estimability(X, condition_threshold = NA), "condition_threshold")
  expect_error(check_estimability(X, coverage_threshold = 1.1), "coverage_threshold")
  expect_error(check_estimability(X, absolute = NA), "absolute")
  expect_output(print(dense), "Design estimability: rank 2/2")
})
