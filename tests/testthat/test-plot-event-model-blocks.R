# Tests for plot.event_model() block disambiguation (GitHub issue #6).
# Multi-run designs must not connect a regressor across block boundaries on a
# shared block-relative time axis (which produced spurious oscillations).

make_pm_model <- function() {
  set.seed(1)
  mk <- function(run) data.frame(
    onset = seq(20, 220, by = 40), run = run,
    cond  = factor("a"), mod = scale(rnorm(6))[, 1])
  ev <- rbind(mk(1), mk(2))
  sf <- fmrihrf::sampling_frame(blocklens = c(140, 140), TR = 1.77)
  event_model(onset ~ hrf(cond, id = "main") + hrf(mod, id = "mod"),
              data = ev, block = ~run, sampling_frame = sf, durations = 20)
}

test_that("plot data carries per-timepoint block and per-block line groups", {
  emod <- make_pm_model()
  d <- plot(emod)$data
  expect_true(".block" %in% colnames(d))
  expect_true(".group" %in% colnames(d))
  # one line group per (regressor x block)
  n_reg <- length(unique(as.character(d$Regressor)))
  expect_equal(length(unique(d$.group)), n_reg * 2L)
})

test_that("within-block plotted series matches the design matrix (no spurious oscillation)", {
  emod <- make_pm_model()
  d  <- plot(emod)$data
  dm <- as.matrix(design_matrix(emod))

  modreg <- grep("mod", unique(as.character(d$Regressor)), value = TRUE)[1]
  modcol <- grep("mod", colnames(dm), value = TRUE)[1]

  b1 <- levels(d$.block)[1]
  sub <- d[as.character(d$Regressor) == modreg & d$.block == b1, ]
  sub <- sub[order(sub$Time), ]

  # The plotted line within a block must not wiggle more than the underlying
  # design-matrix column. Before the fix the plot zig-zagged between runs and
  # this exceeded the DM value.
  expect_equal(max(abs(diff(sub$Response))),
               max(abs(diff(dm[1:140, modcol]))),
               tolerance = 1e-8)
})

test_that("global time gives each block a distinct, non-overlapping x-range", {
  emod <- make_pm_model()
  d <- plot(emod)$data  # block_x = "global" by default
  r1 <- range(d$Time[d$.block == levels(d$.block)[1]])
  r2 <- range(d$Time[d$.block == levels(d$.block)[2]])
  expect_lte(r1[2], r2[1])  # block 1 ends before block 2 begins
})

test_that("run-relative time restarts each block", {
  emod <- make_pm_model()
  d <- plot(emod, block_x = "run")$data
  # both blocks should start near the same (small) within-run time
  s1 <- min(d$Time[d$.block == levels(d$.block)[1]])
  s2 <- min(d$Time[d$.block == levels(d$.block)[2]])
  expect_equal(s1, s2, tolerance = 1e-8)
})

test_that("faceting and time-axis options return ggplot objects", {
  emod <- make_pm_model()
  expect_s3_class(plot(emod, facet_by_block = TRUE), "ggplot")
  expect_s3_class(plot(emod, block_x = "run", facet_by_block = TRUE), "ggplot")
  expect_s3_class(plot(emod, facet_threshold = 1, facet_by_block = TRUE), "ggplot")
})

test_that("single-block models still plot with one block group", {
  ev <- data.frame(onset = c(5, 15, 25), run = 1, cond = factor("a"))
  sf <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
  em <- event_model(onset ~ hrf(cond), data = ev, block = ~run, sampling_frame = sf)
  d <- plot(em)$data
  expect_equal(length(unique(d$.block)), 1L)
  expect_s3_class(plot(em), "ggplot")
})
