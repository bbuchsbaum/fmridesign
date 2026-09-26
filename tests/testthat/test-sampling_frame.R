# Standard test setup
sframe <- fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 1)

# --- print.sampling_frame ---

test_that("printing a sampling_frame dispatches to fmrihrf's method", {
  # fmrihrf owns the sampling_frame class and its print method; fmridesign
  # must not register a competing one (it would overwrite fmrihrf's on load).
  expect_false("print.sampling_frame" %in%
                 getNamespaceInfo("fmridesign", "S3methods")[, 3])
  expect_identical(
    environment(getS3method("print", "sampling_frame")),
    asNamespace("fmrihrf")
  )
  result <- withVisible(print(sframe))
  expect_false(result$visible)
  expect_identical(result$value, sframe)
})

# --- plot.sampling_frame ---

test_that("plot.sampling_frame timeline style returns ggplot", {
  p <- plot(sframe, style = "timeline")
  expect_s3_class(p, "ggplot")
})

test_that("plot.sampling_frame grid style returns ggplot", {
  p <- plot(sframe, style = "grid")
  expect_s3_class(p, "ggplot")
})

test_that("plot.sampling_frame with show_ticks=TRUE returns ggplot", {
  p <- plot(sframe, style = "timeline", show_ticks = TRUE, tick_every = 10)
  expect_s3_class(p, "ggplot")
})

test_that("plot.sampling_frame with single block works", {
  sf1 <- fmrihrf::sampling_frame(blocklens = 60, TR = 2)
  p <- plot(sf1, style = "timeline")
  expect_s3_class(p, "ggplot")
})

test_that("plot.sampling_frame grid style with multiple blocks works", {
  sf3 <- fmrihrf::sampling_frame(blocklens = c(20, 30, 25), TR = 1)
  p <- plot(sf3, style = "grid")
  expect_s3_class(p, "ggplot")
})
