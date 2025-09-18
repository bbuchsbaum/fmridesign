test_that("event_model can be created", {
  # Create simple event data
  event_data <- data.frame(
    condition = factor(c("A", "B", "A", "B")),
    onsets = c(1, 10, 20, 80),
    run = c(1, 1, 1, 1)
  )
  
  # Create sampling frame
  sframe <- sampling_frame(blocklens = 100, TR = 2)
  
  # Create event model
  ev_model <- event_model(
    onsets ~ hrf(condition),
    data = event_data,
    block = ~run,
    sampling_frame = sframe
  )
  
  expect_s3_class(ev_model, "event_model")
  expect_true(!is.null(ev_model$design_matrix))
})