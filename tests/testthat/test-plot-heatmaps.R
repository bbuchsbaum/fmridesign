# Diagnostics behind design_map(), correlation_map() and plot_contrasts().

layer_labels <- function(p) {
  b <- ggplot2::ggplot_build(p)
  unlist(lapply(b$data, function(d) if ("label" %in% names(d)) as.character(d$label)))
}

test_that(".fd_hm_contrast_se matches a hand computation", {
  set.seed(1)
  n <- 40
  run <- rep(1:2, each = n / 2)
  DM <- cbind(a = rnorm(n), b = rnorm(n), c = rnorm(n))
  W <- cbind(c(1, -1, 0), c(0.5, 0.5, -1))

  X <- cbind(DM, as.numeric(run == 1), as.numeric(run == 2))
  XtXi <- solve(crossprod(X))
  hand <- apply(W, 2, function(w) {
    ca <- c(w, 0, 0)
    sqrt(drop(t(ca) %*% XtXi %*% ca))
  })

  expect_equal(fmridesign:::.fd_hm_contrast_se(DM, run, W), hand, tolerance = 1e-10)
})

test_that(".fd_hm_contrast_se flags non-estimable and zero contrasts", {
  set.seed(2)
  n <- 30
  run <- rep(1, n)
  a <- rnorm(n)
  DM <- cbind(a = a, b = rnorm(n), a2 = a) # a and a2 are aliased
  W <- cbind(c(1, 0, 0), c(1, 0, 1), c(0, 0, 0))
  se <- fmridesign:::.fd_hm_contrast_se(DM, run, W)
  expect_true(is.na(se[1])) # a alone is not estimable
  expect_true(is.finite(se[2]) && se[2] > 0) # a + a2 is estimable
  expect_equal(se[3], 0)
})

test_that(".fd_hm_vif matches 1 / (1 - R^2) and marks aliased columns", {
  set.seed(3)
  n <- 60
  run <- rep(1:2, each = 30)
  a <- rnorm(n)
  b <- 0.6 * a + rnorm(n)
  c <- rnorm(n)
  vif <- fmridesign:::.fd_hm_vif(cbind(a, b, c), run)

  za <- a - ave(a, run)
  zb <- b - ave(b, run)
  zc <- c - ave(c, run)
  r2 <- summary(lm(za ~ zb + zc))$r.squared
  expect_equal(vif[1], 1 / (1 - r2), tolerance = 1e-8)

  vif_alias <- fmridesign:::.fd_hm_vif(cbind(a, b, a + b, c), run)
  expect_true(all(is.infinite(vif_alias[1:3])))
  expect_true(is.finite(vif_alias[4]))

  # Constant within every run: undefined, not an error
  vif_const <- fmridesign:::.fd_hm_vif(cbind(a, k = as.numeric(run)), run)
  expect_true(is.na(vif_const[2]))
})

test_that("correlation_map labels aliased columns instead of erroring", {
  des <- data.frame(onset = c(5, 30, 60, 90, 120, 150), run = 1,
                    cond = factor(rep(c("A", "B"), 3)), z = 1)
  sframe <- fmrihrf::sampling_frame(blocklens = 100, TR = 2)
  # z is constant, so its regressor equals the sum of the two conditions.
  emod <- suppressWarnings(event_model(onset ~ hrf(cond) + hrf(z), data = des,
                                       block = ~run, sampling_frame = sframe))
  p <- correlation_map(emod)
  expect_s3_class(p, "ggplot")
  labs <- layer_labels(p)
  expect_true(any(labs == "aliased"))
  expect_match(p$labels$subtitle, "aliased")
})

test_that("correlation_map limits = 'data' uses the observed range", {
  des <- data.frame(onset = c(0, 10, 20, 30, 5, 15, 25, 35), run = rep(1:2, each = 4),
                    cond = factor(c("A", "B", "A", "B", "B", "A", "B", "A")))
  sframe <- fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 1)
  emod <- event_model(onset ~ hrf(cond), data = des, block = ~run,
                      sampling_frame = sframe)
  p <- correlation_map(emod, limits = "data")
  sc <- p$scales$get_scales("fill")
  r <- abs(stats::cor(as.matrix(design_matrix(emod)))[2, 1])
  expect_equal(sc$get_limits(), c(-r, r), tolerance = 1e-8)
  expect_s3_class(correlation_map(emod, absolute_limits = FALSE), "ggplot")
})

test_that("design_map flags a near-flat column", {
  sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)
  set.seed(4)
  nuis <- lapply(1:2, function(r) data.frame(m1 = rnorm(50), m2 = rnorm(50) * 1e-4))
  bmod <- baseline_model(basis = "poly", degree = 2, sframe = sframe,
                         nuisance_list = nuis)
  p <- design_map(bmod)
  expect_match(p$labels$subtitle, "near-flat")
  expect_match(p$labels$subtitle, "m2")
})

test_that("plot_contrasts reports the SE multiplier for each row", {
  des <- data.frame(onset = c(0, 10, 20, 30, 40, 50), run = 1,
                    cond = factor(c("A", "B", "C", "A", "B", "C")))
  sframe <- fmrihrf::sampling_frame(blocklens = 60, TR = 1)
  cset <- contrast_set(pair_contrast(~ cond == "A", ~ cond == "B", name = "A_vs_B"))
  emod <- event_model(onset ~ hrf(cond, contrasts = cset), data = des, block = ~run,
                      sampling_frame = sframe)
  p <- plot_contrasts(emod)
  expect_s3_class(p, "ggplot")

  X <- cbind(as.matrix(design_matrix(emod)), 1)
  w <- c(contrast_weights(emod)[[1]]$offset_weights[, 1], 0)
  hand <- sqrt(drop(t(w) %*% solve(crossprod(X)) %*% w))
  grobs_text <- unlist(lapply(p$layers, function(l) {
    g <- l$geom_params$grob
    if (!is.null(g) && inherits(g, "text")) g$label
  }))
  expect_true(fmridesign:::.fd_num(hand, 2) %in% grobs_text)
})
