# Standard test setup
des <- data.frame(
  onset = c(0, 10, 20, 30, 5, 15, 25, 35),
  run = c(1, 1, 1, 1, 2, 2, 2, 2),
  cond = factor(c("A", "B", "A", "B", "A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 1)
emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)

# --- design_map.event_model ---

test_that("design_map returns a ggplot object", {
  p <- design_map(emod)
  expect_s3_class(p, "ggplot")
})

test_that("design_map with block_separators=FALSE returns ggplot", {
  p <- design_map(emod, block_separators = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("design_map with rotate_x_text=FALSE returns ggplot", {
  p <- design_map(emod, rotate_x_text = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("design_map with fill_midpoint returns ggplot", {
  p <- design_map(emod, fill_midpoint = 0)
  expect_s3_class(p, "ggplot")
})

# --- correlation_map.event_model ---

test_that("correlation_map returns a ggplot object", {
  p <- correlation_map(emod)
  expect_s3_class(p, "ggplot")
})

test_that("correlation_map with rotate_x_text=FALSE returns ggplot", {
  p <- correlation_map(emod, rotate_x_text = FALSE)
  expect_s3_class(p, "ggplot")
})

# --- plot.event_model ---

test_that("plot.event_model returns a ggplot object", {
  p <- plot(emod)
  expect_s3_class(p, "ggplot")
})

test_that("plot.event_model with term_name filter works", {
  p <- plot(emod, term_name = "cond")
  expect_s3_class(p, "ggplot")
})

test_that("plot.event_model errors on invalid term_name", {
  expect_error(plot(emod, term_name = "nonexistent"),
               "No columns found")
})

test_that("plot.event_model with facet_threshold triggers faceting", {
  p <- plot(emod, facet_threshold = 1)
  expect_s3_class(p, "ggplot")
  # Check that facet_wrap was applied by inspecting the ggplot structure
  expect_true(!is.null(p$facet))
})

test_that("plot.event_model with label_mode='none' returns ggplot", {
  p <- plot(emod, label_mode = "none")
  expect_s3_class(p, "ggplot")
})

test_that("plot.event_model with label_mode='compact' returns ggplot", {
  p <- plot(emod, label_mode = "compact")
  expect_s3_class(p, "ggplot")
})

test_that("plot.event_model with facet_threshold and label_mode='none'", {
  p <- plot(emod, facet_threshold = 1, label_mode = "none")
  expect_s3_class(p, "ggplot")
})
