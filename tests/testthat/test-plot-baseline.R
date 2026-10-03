set.seed(11)
sf_pb <- fmrihrf::sampling_frame(blocklens = c(30, 30, 30), TR = 2)
nuis_pb <- lapply(1:3, function(r) {
  m <- matrix(rnorm(60), 30, 2)
  colnames(m) <- c("tx", "ty")
  as.data.frame(m)
})
bm_pb <- baseline_model(basis = "bs", degree = 4, sframe = sf_pb,
                        nuisance_list = nuis_pb)

line_data <- function(p) {
  b <- ggplot2::ggplot_build(p)
  idx <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))
  b$data[[idx[1]]]
}

test_that("plot.baseline_model shows every non-constant term by default", {
  p <- plot(bm_pb)
  expect_s3_class(p, "ggplot")
  expect_setequal(levels(p$data$facet), c("Drift", "Nuisance"))
  expect_setequal(unique(p$data$label[p$data$term == "nuisance"]), c("tx", "ty"))
  expect_match(p$labels$caption, "run intercepts \\(3 columns\\)")
  expect_match(p$labels$subtitle, "B-spline, 4 per run")
  expect_match(p$labels$subtitle, "2 nuisance per run")
  expect_no_warning(ggplot2::ggplot_build(p))
})

test_that("plot.baseline_model never connects lines across runs", {
  p <- plot(bm_pb)
  d <- p$data
  runs_per_group <- tapply(d$run, d$group, function(r) length(unique(r)))
  expect_true(all(runs_per_group == 1L))
  # Block-diagonal columns appear only in their own run: 4 drift + 2 nuisance
  # series per run, each 30 scans long.
  expect_equal(nrow(d), 3 * (4 + 2) * 30)
})

test_that("plot.baseline_model keeps a basis colour across runs", {
  p <- plot(bm_pb)
  d <- p$data[p$data$term == "drift", ]
  cols <- tapply(d$colour, d$label, function(v) length(unique(v)))
  expect_true(all(cols == 1L))
  expect_equal(length(unique(d$colour)), 4L)
})

test_that("plot.baseline_model term_name keeps exact and partial matching", {
  p <- plot(bm_pb, term_name = "nuisance")
  expect_equal(unique(p$data$term), "nuisance")
  expect_message(p2 <- plot(bm_pb, term_name = "nuis"), "unique partial match")
  expect_equal(unique(p2$data$term), "nuisance")
  # Constant terms can be requested explicitly.
  p3 <- plot(bm_pb, term_name = "block")
  expect_equal(unique(p3$data$term), "block")
  expect_error(plot(bm_pb, term_name = "zzz"), "not found")
  expect_error(plot(bm_pb, term_name = "i"), "matches multiple terms")
})

test_that("plot.baseline_model handles constant-only and single-run models", {
  bm_c <- baseline_model(basis = "constant", sframe = sf_pb)
  expect_message(p <- plot(bm_c), "constant within runs")
  expect_s3_class(p, "ggplot")
  sf1 <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
  p1 <- plot(baseline_model(basis = "poly", degree = 2, sframe = sf1))
  expect_no_warning(ggplot2::ggplot_build(p1))
})

test_that("plot.baseline_model supports run-relative layout", {
  p <- plot(bm_pb, block_x = "run")
  expect_s3_class(p$facet, "FacetGrid")
  expect_lte(max(p$data$time), 60)
  expect_no_warning(ggplot2::ggplot_build(p))
})

test_that("plot.sampling_frame draws one bar per run (not a blank panel)", {
  sf_var <- fmrihrf::sampling_frame(blocklens = c(160, 140, 175), TR = 2)
  p <- plot(sf_var)
  b <- ggplot2::ggplot_build(p)
  rect_ix <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomRect"), logical(1)))
  bars <- b$data[[rect_ix]]
  expect_equal(nrow(bars), 3L)
  expect_equal(sort(bars$xmax - bars$xmin), sort(c(320, 280, 350)))
  expect_match(p$labels$subtitle, "3 runs")
  expect_match(p$labels$subtitle, "475 scans")
  expect_match(p$labels$subtitle, "total 15:50")
})

test_that("plot.sampling_frame grid draws one cell per scan", {
  sf_small <- fmrihrf::sampling_frame(blocklens = c(12, 20), TR = 2)
  p <- plot(sf_small, style = "grid")
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$data[[1]]), 32L)
})

test_that("plot.sampling_frame reports per-run TR", {
  sf_tr <- fmrihrf::sampling_frame(blocklens = c(10, 20), TR = c(2, 1.5))
  p <- plot(sf_tr)
  expect_match(p$labels$subtitle, "varies by run")
  expect_true(any(grepl("TR 1.5 s", p$data$lab)))
})

test_that("plot.baseline_model splits motion parameters into lanes", {
  nuis <- lapply(1:3, function(r) {
    m <- matrix(rnorm(30 * 4), 30, 4)
    colnames(m) <- c("tx", "ty", "rx", "ry")
    m
  })
  bm <- baseline_model(basis = "poly", degree = 2, sframe = sf_pb,
                       nuisance_list = nuis)
  p <- plot(bm)
  expect_equal(levels(p$data$facet), c("Drift", "Translation", "Rotation"))
  expect_setequal(unique(p$data$label[p$data$facet == "Rotation"]), c("rx", "ry"))
  # x/y hues match across the two lanes.
  col_of <- function(l) unique(p$data$colour[p$data$label == l])
  expect_equal(col_of("tx"), col_of("rx"))
  expect_equal(.fd_bl_motion_kind(c("trans_x", "rot_z", "pitch", "x", "csf", "tz_derivative1")),
               c("translation", "rotation", "rotation", "translation", NA, "translation"))
})

test_that("plot.baseline_model drift_scale = 'unit' rescales and says so", {
  p <- plot(bm_pb, drift_scale = "unit")
  d <- p$data[p$data$term == "drift", ]
  peaks <- tapply(abs(d$value), d$group, max)
  expect_true(all(abs(peaks - 1) < 1e-12))
  expect_match(p$labels$caption, "divided by its peak")
  expect_match(p$labels$caption, "DCT high-pass with cutoff")
})

test_that("plot.baseline_model falls back to a legend for many series", {
  nuis <- lapply(1:3, function(r) {
    m <- matrix(rnorm(30 * 9), 30, 9)
    colnames(m) <- paste0("comp", 1:9)
    m
  })
  bm <- baseline_model(basis = "poly", degree = 2, sframe = sf_pb,
                       nuisance_list = nuis)
  p <- plot(bm)
  expect_equal(p$theme$legend.position, "bottom")
  expect_no_warning(ggplot2::ggplot_build(p))
})

test_that("plot.sampling_frame flags runs of unusual length", {
  sf_var <- fmrihrf::sampling_frame(blocklens = c(160, 140, 160), TR = 2)
  p <- plot(sf_var)
  expect_equal(p$data$face, c("plain", "bold", "plain"))
  expect_match(p$labels$caption, "most common length \\(160 scans\\)")
})

test_that("plot.baseline_model drift colours are distinct and motion lanes drop the y title", {
  nuis <- lapply(1:3, function(r) {
    m <- matrix(rnorm(30 * 4), 30, 4)
    colnames(m) <- c("tx", "ty", "rx", "ry")
    m
  })
  bm <- baseline_model(basis = "bs", degree = 5, sframe = sf_pb,
                       nuisance_list = nuis)
  p <- plot(bm)
  d <- p$data[p$data$term == "drift", ]
  expect_equal(as.vector(tapply(d$colour, d$label, `[`, 1)), .fd_cat[1:5])
  expect_null(p$labels$y)
  expect_match(p$labels$caption, "units supplied")
  expect_match(p$labels$caption, "no interior knots")
  # Without motion lanes the generic y title stays.
  expect_equal(plot(bm_pb)$labels$y, "Regressor value")
})

sf_ev <- fmrihrf::sampling_frame(blocklens = c(50, 50, 50), TR = 2)
ev_df <- data.frame(onset = c(10, 40, 70, 5, 30, 150, 20),
                    run = c(1, 1, 1, 2, 2, 2, 4),
                    cond = factor(c("a", "b", "a", "b", "a", "b", "a")))

test_that("plot.sampling_frame overlays event onsets as a coverage check", {
  p <- plot(sf_ev, events = ev_df)
  b <- ggplot2::ggplot_build(p)
  seg_ix <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomSegment"), logical(1)))
  ticks <- b$data[[seg_ix[1]]]
  # 5 onsets fall inside runs 1-2; 150 s is past the end of run 2 (100 s)
  # and run 4 does not exist.
  expect_equal(nrow(ticks), 5L)
  expect_equal(sort(ticks$x), sort(c(10, 40, 70, 105, 130)))
  expect_match(p$labels$caption, "5 event onsets")
  expect_match(p$labels$caption, "2 onsets outside")
  expect_match(p$labels$caption, "No events in run 3")

  em <- event_model(onset ~ hrf(cond), data = ev_df[1:5, ], block = ~run,
                    sampling_frame = fmrihrf::sampling_frame(c(50, 50), TR = 2))
  p2 <- plot(fmrihrf::sampling_frame(c(50, 50), TR = 2), style = "lane", events = em)
  expect_match(p2$labels$caption, "5 event onsets")
  p3 <- plot(sf_ev, style = "grid", events = ev_df)
  expect_no_warning(ggplot2::ggplot_build(p3))
  expect_error(plot(sf_ev, events = data.frame(t = 1)), "onset")
})

test_that("plot.sampling_frame lane style draws adjacent segments", {
  sf_var <- fmrihrf::sampling_frame(blocklens = c(160, 140, 175), TR = 2)
  p <- plot(sf_var, style = "lane")
  b <- ggplot2::ggplot_build(p)
  rect_ix <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomRect"), logical(1)))
  bars <- b$data[[rect_ix]]
  expect_equal(nrow(bars), 3L)
  expect_equal(length(unique(bars$ymin)), 1L)
  expect_equal(bars$xmin[-1], bars$xmax[-3])
  sf_many <- fmrihrf::sampling_frame(blocklens = c(100, 150, 40, 120, 90, 60, 30), TR = 2)
  expect_no_warning(ggplot2::ggplot_build(plot(sf_many, style = "lane")))
})

test_that("plot.sampling_frame places all timeline labels the same way", {
  sf_var <- fmrihrf::sampling_frame(blocklens = c(160, 20, 175), TR = 2)
  p <- plot(sf_var)
  # A short run forces every label outside, at the right end of its bar.
  expect_equal(p$data$lx - p$data$end, rep(p$data$lx[1] - p$data$end[1], 3))
  sf_tr <- fmrihrf::sampling_frame(blocklens = c(10, 20, 20), TR = c(2, 1.5, 2))
  expect_true(all(grepl("TR ", plot(sf_tr)$data$lab)))
})
