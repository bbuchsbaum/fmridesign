test_that("hrf subset drops unused factor levels when one event remains", {
  events <- data.frame(
    onset = c(0, 10, 20),
    condition = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
    keep = c(FALSE, TRUE, FALSE),
    run = 1L
  )
  sf <- fmrihrf::sampling_frame(blocklens = 30, TR = 2)

  model <- event_model(
    onset ~ hrf(condition, subset = keep),
    data = events,
    block = ~run,
    sampling_frame = sf
  )
  design <- as.matrix(design_matrix(model))

  expect_identical(colnames(design), "condition_condition.B")
  expect_gt(sum(abs(design[, 1])), 0)
})

test_that("zero-event hrf subset keeps its declared zero columns", {
  events <- data.frame(
    onset = c(0, 10),
    condition = factor(c("A", "B")),
    keep = FALSE,
    run = 1L
  )
  sf <- fmrihrf::sampling_frame(blocklens = 30, TR = 2)

  expect_warning(
    model <- event_model(
      onset ~ hrf(condition, subset = keep),
      data = events,
      block = ~run,
      sampling_frame = sf
    ),
    class = "fmridesign_zero_events"
  )
  design <- as.matrix(design_matrix(model))

  expect_identical(
    colnames(design),
    c("condition_condition.A", "condition_condition.B")
  )
  expect_true(all(design == 0))
})
