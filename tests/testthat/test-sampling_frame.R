# Standard test setup
sframe <- fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 1)

# --- print.sampling_frame ---

test_that("print.sampling_frame returns invisible(x)", {
  result <- withVisible(print(sframe))
  expect_false(result$visible)
  expect_identical(result$value, sframe)
})

test_that("print.sampling_frame output includes block info", {
  out <- capture.output(print(sframe))
  combined <- paste(out, collapse = "\n")
  expect_true(grepl("Sampling frame", combined))
  expect_true(grepl("Blocks.*2", combined))
  expect_true(grepl("Scans.*80", combined))
})

test_that("print.sampling_frame output includes TR info", {
  out <- capture.output(print(sframe))
  combined <- paste(out, collapse = "\n")
  expect_true(grepl("TR", combined))
})

test_that("print.sampling_frame works with single block", {
  sf1 <- fmrihrf::sampling_frame(blocklens = 60, TR = 2)
  out <- capture.output(print(sf1))
  combined <- paste(out, collapse = "\n")
  expect_true(grepl("Blocks.*1", combined))
  expect_true(grepl("Scans.*60", combined))
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
