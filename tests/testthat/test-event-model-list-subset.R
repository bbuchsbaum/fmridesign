test_that("list interface evaluates subset expressions with base operators", {
  event_data <- data.frame(
    onset = c(1, 10, 20, 30),
    condition = factor(c("A", "B", "A", "B")),
    duration = 1,
    run = rep(1, 4)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 2)

  model <- event_model(
    list(stim = hrf(condition, id = "stim", subset = condition == "A")),
    data = event_data,
    block = ~run,
    sampling_frame = sframe,
    durations = event_data$duration
  )

  expect_s3_class(model, "event_model")
  expect_equal(nrow(design_matrix(model)), 40)
})

test_that("list interface preserves subset helper environments", {
  event_data <- data.frame(
    onset = c(1, 10, 20, 30),
    condition = factor(c("a", "b", "a", "b")),
    duration = 1,
    run = rep(1, 4)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 2)
  keep_a <- function(x) x == "a"

  formula_model <- event_model(
    onset ~ hrf(condition, subset = keep_a(condition)),
    data = event_data,
    block = ~run,
    sampling_frame = sframe,
    durations = event_data$duration
  )
  list_model <- event_model(
    list(hrf(condition, subset = keep_a(condition))),
    data = event_data,
    block = ~run,
    sampling_frame = sframe,
    durations = event_data$duration
  )

  expect_equal(as.matrix(design_matrix(list_model)),
               as.matrix(design_matrix(formula_model)),
               ignore_attr = TRUE)
})
