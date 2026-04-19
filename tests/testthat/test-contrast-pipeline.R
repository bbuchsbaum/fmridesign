# Tests for the unified contrast pipeline (R/contrast_pipeline.R) and the
# refactored basis-expansion path used by built-in spec methods.

test_that("pair_contrast with multi-basis HRF expands across components", {
  des <- data.frame(
    onset = c(0, 10, 20, 30, 40, 50),
    run   = 1,
    cond  = factor(c("A", "B", "A", "B", "A", "B"))
  )
  sframe <- fmrihrf::sampling_frame(blocklens = 60, TR = 1)
  cset <- contrast_set(
    AvB = pair_contrast(~ cond == "A", ~ cond == "B", name = "AvB")
  )
  emod <- event_model(onset ~ hrf(cond, basis = "spmg3", contrasts = cset),
                      data = des, block = ~run, sampling_frame = sframe)

  cw <- contrast_weights(emod)
  expect_true(length(cw) >= 1L)
  # Find the AvB contrast (key has form "<term>#<contrast>")
  avb <- cw[[grep("AvB$", names(cw))[1]]]
  expect_false(is.null(avb))
  # offset_weights has rows = all design columns, cols = 1
  expect_equal(nrow(avb$offset_weights), ncol(design_matrix(emod)))
  # Each basis-component pair (A_b01 vs B_b01, A_b02 vs B_b02, ...) sums to 0
  expect_lt(abs(sum(avb$offset_weights[, 1])), 1e-8)
})

test_that("oneway_contrast with multi-basis HRF produces F-contrast columns", {
  des <- data.frame(
    onset = c(0, 10, 20, 30, 40, 50),
    run   = 1,
    cond  = factor(c("A", "B", "C", "A", "B", "C"))
  )
  sframe <- fmrihrf::sampling_frame(blocklens = 60, TR = 1)
  cset <- contrast_set(
    main = oneway_contrast(~ cond, name = "main")
  )
  emod <- event_model(onset ~ hrf(cond, basis = "spmg2", contrasts = cset),
                      data = des, block = ~run, sampling_frame = sframe)

  cw <- contrast_weights(emod)
  main <- cw[[grep("main$", names(cw))[1]]]
  expect_false(is.null(main))
  expect_equal(nrow(main$offset_weights), ncol(design_matrix(emod)))
  # Helmert basis for 3 levels => 2 contrast columns
  expect_equal(ncol(main$offset_weights), 2L)
})

test_that("contrast_mask is a generic that errors for unregistered specs", {
  expect_error(
    contrast_mask(structure(list(name = "x"), class = c("foo_spec", "contrast_spec")),
                  term = NULL),
    "no applicable method"
  )
})

test_that("contrast_from_mask packages a mask as a contrast object", {
  des <- data.frame(
    onset = c(0, 10, 20, 30),
    run   = 1,
    cond  = factor(c("A", "B", "A", "B"))
  )
  sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
  emod <- event_model(onset ~ hrf(cond),
                      data = des, block = ~run, sampling_frame = sframe)
  term <- terms(emod)[[1]]

  # Build a base mask manually (rows = base conditions).
  base_names <- conditions(term, drop.empty = FALSE, expand_basis = FALSE)
  w <- matrix(c(1, -1), ncol = 1,
              dimnames = list(base_names, "AvB"))
  spec <- structure(list(name = "AvB"),
                    class = c("custom_spec", "contrast_spec", "list"))
  out <- contrast_from_mask(list(weights = w, condnames = base_names),
                            spec = spec, term = term)
  expect_s3_class(out, "contrast")
  expect_equal(nrow(out$weights), length(base_names))  # nbasis = 1, no expansion
  expect_lt(abs(sum(out$weights)), 1e-10)
})
