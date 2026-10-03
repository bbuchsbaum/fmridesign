test_that("helper contrast sets combine with individual specifications", {
  lv <- c("Degraded", "Foreign", "Intact")
  p <- pair_contrast(~ condition == "Intact", ~ condition == "Degraded",
                     name = "intelligibility")
  o <- one_against_all_contrast(lv, facname = "condition")
  combined <- contrast_set(p, o)
  spliced <- do.call(contrast_set, c(list(p), unclass(o)))
  expect_identical(combined, spliced)
  expect_length(combined, 4L)
  expect_identical(combined[[1L]], p)

  # Exercise the catalogue through the public model-to-weight path.
  ev <- data.frame(onset = seq(0, 50, by = 10), condition = factor(rep(lv, 2)), run = 1)
  sf <- fmrihrf::sampling_frame(60, TR = 1)
  model <- event_model(onset ~ hrf(condition, contrasts = combined), ev,
                       block = ~run, sampling_frame = sf)
  weights <- contrast_weights(model)
  expect_length(weights, 4L)
  expected <- list(c(-1, 0, 1), c(1, -0.5, -0.5), c(-0.5, 1, -0.5), c(-0.5, -0.5, 1))
  for (i in seq_along(expected)) {
    expect_equal(as.numeric(weights[[i]]$offset_weights), expected[[i]])
  }
  expect_true(all(check_estimability(model, absolute = FALSE)$contrasts$estimable))
})

test_that("recursive flattening preserves leaf names, duplicates and empty sets", {
  a <- contrast(~ A - B, name = "A_B")
  b <- contrast(~ B - C, name = "B_C")
  nested <- structure(list(first = a, group = contrast_set(second = b)),
                      class = c("contrast_set", "list"))
  expect_identical(contrast_set(outer = nested, contrast_set()), contrast_set(first = a, second = b))
  expect_identical(contrast_set(), contrast_set(contrast_set(), contrast_set()))
  expect_identical(contrast_set(first = a, second = b),
                   structure(list(first = a, second = b), class = c("contrast_set", "list")))
  expect_identical(contrast_set(a, contrast_set(a)), contrast_set(a, a))
  expect_identical(contrast_set(contrast_set(same = a), same = b), contrast_set(same = a, same = b))
  pairs <- pairwise_contrasts(c("A", "B", "C"), "condition")
  expect_identical(contrast_set(contrast_set(pairs)), pairs)
})

test_that("invalid leaves report their argument path and how to splice plain lists", {
  a <- contrast(~ A - B, name = "A_B")
  expect_error(contrast_set(a, 42), "argument 2 must be a contrast_spec or contrast_set", fixed = TRUE)
  expect_error(contrast_set(a, NULL), "argument 2 must be", fixed = TRUE)
  expect_error(contrast_set(a, list(a)), "do.call(contrast_set, specs)", fixed = TRUE)
  malformed <- structure(list(a, list(a)), class = c("contrast_set", "list"))
  expect_error(contrast_set(a, malformed), "argument 2[[2]] must be", fixed = TRUE)
})
