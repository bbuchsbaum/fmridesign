test_that("basic event model creation works", {
  # Create test data
  event_data <- data.frame(
    condition = factor(c("A", "B", "A", "B")),
    onsets = c(1, 10, 20, 30),
    run = c(1, 1, 1, 1)
  )
  
  # Create sampling frame
  sframe <- sampling_frame(blocklens = 40, TR = 2)
  
  # Create event model
  ev_model <- event_model(
    onsets ~ hrf(condition),
    data = event_data,
    block = ~run,
    sampling_frame = sframe
  )
  
  expect_s3_class(ev_model, "event_model")
  expect_true(!is.null(ev_model$design_matrix))
  expect_equal(nrow(ev_model$design_matrix), 40)
})

test_that("baseline model creation works", {
  sframe <- sampling_frame(blocklens = c(50, 50), TR = 2)
  
  # Test different basis types
  bm_bs <- baseline_model(basis = "bs", degree = 3, sframe = sframe)
  expect_s3_class(bm_bs, "baseline_model")
  
  bm_poly <- baseline_model(basis = "poly", degree = 2, sframe = sframe)
  expect_s3_class(bm_poly, "baseline_model")
  
  bm_const <- baseline_model(basis = "constant", sframe = sframe)
  expect_s3_class(bm_const, "baseline_model")
})

test_that("event_factor and event_variable work", {
  onsets <- c(1, 10, 20, 30)
  blockids <- c(1, 1, 1, 1)
  
  # Test event_factor
  ef <- event_factor(
    factor(c("A", "B", "A", "B")),
    onsets = onsets,
    blockids = blockids,
    name = "condition"
  )
  expect_s3_class(ef, "event")
  expect_true(is_categorical(ef))
  
  # Test event_variable
  ev <- event_variable(
    c(0.5, 0.7, 0.6, 0.8),
    onsets = onsets,
    blockids = blockids,
    name = "rt"
  )
  expect_s3_class(ev, "event")
  expect_true(is_continuous(ev))
})

test_that("design matrix extraction works", {
  event_data <- data.frame(
    condition = factor(c("A", "B", "A", "B")),
    onsets = c(1, 10, 20, 30),
    run = c(1, 1, 1, 1)
  )
  
  sframe <- sampling_frame(blocklens = 40, TR = 2)
  
  ev_model <- event_model(
    onsets ~ hrf(condition),
    data = event_data,
    block = ~run,
    sampling_frame = sframe
  )
  
  dm <- design_matrix(ev_model)
  expect_s3_class(dm, "tbl_df")
  expect_equal(nrow(dm), 40)
  expect_true(ncol(dm) >= 2)  # At least 2 columns for 2-level factor
})

test_that("contrast_weights is properly exported", {
  expect_true(exists("contrast_weights"))
  expect_true(is.function(contrast_weights))
})