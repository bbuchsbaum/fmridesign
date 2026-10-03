library(testthat)

make_sf <- function(n = 60, TR = 2) fmrihrf::sampling_frame(blocklens = n, TR = TR)

layer_classes <- function(p) vapply(p$layers, function(l) class(l$geom)[1], character(1))

test_that(".fd_colinfo labels factorial, basis and trialwise columns readably", {
  des <- data.frame(onset = c(5, 25, 45, 65), run = 1,
                    task = factor(c("face", "scene", "face", "scene")),
                    load = factor(c("low", "low", "high", "high")))
  em <- event_model(onset ~ hrf(task, load), data = des, block = ~run,
                    sampling_frame = make_sf())
  ci <- .fd_colinfo(em)
  expect_setequal(ci$label, c("face × low", "scene × low",
                              "face × high", "scene × high"))

  em3 <- event_model(onset ~ hrf(task, basis = "spmg3"), data = des, block = ~run,
                     sampling_frame = make_sf())
  ci3 <- .fd_colinfo(em3)
  # Labels follow the data layout (condition-major), not the column names.
  expect_equal(ci3$label, rep(c("face", "scene"), each = 3))
  expect_equal(ci3$basis_ix, rep(1:3, 2))

  emt <- event_model(onset ~ trialwise(), data = des, block = ~run,
                     sampling_frame = make_sf())
  expect_true(all(grepl("^trial ", .fd_colinfo(emt)$label)))
})

test_that("spmg3 basis labels match the columns' actual time courses", {
  des <- data.frame(onset = c(5, 60), run = 1, task = factor(c("face", "scene")))
  em3 <- event_model(onset ~ hrf(task, basis = "spmg3"), data = des, block = ~run,
                     sampling_frame = fmrihrf::sampling_frame(100, 1))
  dm <- as.matrix(design_matrix(em3))
  ci <- .fd_colinfo(em3)
  canon <- which(ci$basis_ix == 1)
  # Each canonical column peaks shortly after its own condition's onset.
  peak_t <- apply(dm[, canon, drop = FALSE], 2, which.max)
  expect_true(peak_t[ci$label[canon] == "face"] < 40)
  expect_true(peak_t[ci$label[canon] == "scene"] > 60)
})

test_that("stacked style gives basis sets one facet row per basis function", {
  des <- data.frame(onset = c(5, 25, 45), run = 1, task = factor(c("a", "b", "a")))
  em3 <- event_model(onset ~ hrf(task, basis = "spmg3"), data = des, block = ~run,
                     sampling_frame = make_sf())
  p <- plot(em3)
  b <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(b$layout$layout$ROW)), 6L)
})

test_that("auto style switches to heatmap for many rows and long basis sets", {
  d <- data.frame(onset = seq(5, 95, by = 6), run = 1)
  emt <- event_model(onset ~ trialwise(), data = d, block = ~run,
                     sampling_frame = make_sf(60, 2))
  expect_true("GeomTile" %in% layer_classes(plot(emt)))

  d2 <- data.frame(onset = c(5, 30, 60), run = 1, s = factor(c("a", "b", "a")))
  emf <- event_model(onset ~ hrf(s, basis = "fir", nbasis = 6), data = d2, block = ~run,
                     sampling_frame = make_sf(60, 2))
  expect_true("GeomTile" %in% layer_classes(plot(emf)))
  expect_false("GeomTile" %in% layer_classes(plot(emf, style = "stacked")))
})

test_that("event onsets are drawn at their global times, and durations as bars", {
  sf <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)
  des <- data.frame(onset = c(10, 40, 10, 40), run = c(1, 1, 2, 2),
                    cond = factor(c("a", "b", "a", "b")))
  em <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sf,
                    durations = rep(12, 4))
  p <- plot(em)
  rug <- p$layers[[which(layer_classes(p) == "GeomRug")]]
  expect_setequal(rug$data$onset, c(10, 40, 110, 140))
  expect_true("GeomSegment" %in% layer_classes(p))

  p0 <- plot(event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sf))
  expect_false("GeomSegment" %in% layer_classes(p0))
  expect_false("GeomRug" %in% layer_classes(plot(em, show_events = FALSE)))
})

test_that("factorial terms get hue families that follow the first factor", {
  des <- data.frame(onset = c(5, 25, 45, 65), run = 1,
                    task = factor(c("face", "scene", "face", "scene")),
                    load = factor(c("low", "low", "high", "high")))
  em <- event_model(onset ~ hrf(task, load), data = des, block = ~run,
                    sampling_frame = make_sf())
  ci <- .fd_colinfo(em)
  cols <- .fd_structured_colours(ci, .fd_ev_row_key(ci))
  expect_length(unique(cols), 4L)
  hue <- function(x) grDevices::rgb2hsv(grDevices::col2rgb(x))["h", ]
  face <- cols[grepl("^face", names(cols))]
  scene <- cols[grepl("^scene", names(cols))]
  expect_lt(abs(diff(hue(face))), 0.05)
  expect_gt(abs(hue(face[1]) - hue(scene[1])), 0.2)
})

test_that("title and subtitle can be overridden or removed", {
  des <- data.frame(onset = c(5, 25), run = 1, cond = factor(c("a", "b")))
  em <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = make_sf())
  p <- plot(em, title = "Mine", subtitle = NA)
  expect_equal(p$labels$title, "Mine")
  expect_null(p$labels$subtitle)
  expect_match(plot(em)$labels$subtitle, "2 regressors")
})

test_that("plotting helpers produce readable axis values", {
  expect_equal(.fd_mmss(c(0, 59, 60, 125)), c("0:00", "0:59", "1:00", "2:05"))
  expect_equal(.fd_num(1.62), "1.6")
  expect_equal(.fd_num(-0.4), "−0.4")
  # Breaks recover the data extremes from the expanded limits.
  lims <- c(-0.71, 0.54) + c(-0.22, 0.08) * 1.25
  expect_equal(.fd_peak_breaks(lims), c(-0.71, 0, 0.54))
  lims2 <- c(0, 1.93) + c(-0.22, 0.08) * 1.93
  expect_equal(.fd_peak_breaks(lims2), c(0, 1.9))
  expect_error(fmridesign_palette("categorical", 9), "distinct hues")
  expect_length(fmridesign_palette("diverging", 11), 11L)
})

test_that("y_scale = 'term' gives rows of one term identical y ranges", {
  des <- data.frame(onset = c(5, 25, 45, 65, 85), run = 1,
                    cond = factor(c("a", "b", "a", "a", "b")), m = c(0.1, -0.2, 0.3, 0, 0.5))
  em <- event_model(onset ~ hrf(cond) + hrf(m), data = des, block = ~run,
                    sampling_frame = fmrihrf::sampling_frame(60, 2))
  yr <- function(p) {
    b <- ggplot2::ggplot_build(p)
    vapply(b$layout$panel_params, function(pp) diff(pp$y.range), numeric(1))
  }
  r_term <- yr(plot(em))
  expect_equal(r_term[1], r_term[2])           # cond a and b share a scale
  expect_false(isTRUE(all.equal(r_term[1], r_term[3])))  # modulator keeps its own
  r_row <- yr(plot(em, y_scale = "row"))
  expect_length(unique(round(r_row, 8)), 3L)
  r_all <- yr(plot(em, y_scale = "shared"))
  expect_length(unique(round(r_all, 8)), 1L)
})

test_that("time_range zooms the x axis and is validated", {
  des <- data.frame(onset = c(5, 25), run = 1, cond = factor(c("a", "b")))
  em <- event_model(onset ~ hrf(cond), data = des, block = ~run,
                    sampling_frame = fmrihrf::sampling_frame(60, 2))
  b <- ggplot2::ggplot_build(plot(em, time_range = c(0, 30)))
  expect_equal(b$layout$panel_params[[1]]$x.range, c(0, 30))
  expect_error(plot(em, time_range = c(30, 0)), "increasing")
})

test_that("trialwise heatmap reports each trial's max correlation with others", {
  d <- data.frame(onset = c(5, 9, 40, 80), run = 1)
  emt <- event_model(onset ~ trialwise(), data = d, block = ~run,
                     sampling_frame = fmrihrf::sampling_frame(60, 2))
  p <- plot(emt, style = "heatmap")
  txt <- p$layers[[which(layer_classes(p) == "GeomText")[1]]]$data
  R <- stats::cor(as.matrix(design_matrix(emt)))
  diag(R) <- NA
  expect_equal(unname(sort(txt$r)), unname(sort(apply(abs(R), 2, max, na.rm = TRUE))))
  # The two close trials overlap far more than the isolated ones.
  r_by <- stats::setNames(txt$r, as.character(txt$Regressor))
  cn <- colnames(design_matrix(emt))
  expect_gt(min(r_by[cn[1:2]]), 3 * max(r_by[cn[3:4]]))
})
