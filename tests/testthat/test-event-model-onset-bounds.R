# Tests for the onset/sampling-frame bounds backstop (GitHub issue #5).
# event_model() should warn (or error, when strict = TRUE) when event onsets
# fall outside the sampling frame.

test_that("event_model warns when an onset is at/after the run end", {
  ev <- data.frame(onset = c(20, 100, 460), run = 1, cond = factor("a"))
  sf <- fmrihrf::sampling_frame(blocklens = 257, TR = 1.77)  # run spans [0, 454.89)
  expect_warning(
    event_model(onset ~ hrf(cond), data = ev, block = ~run,
                sampling_frame = sf, durations = 0),
    "outside the sampling frame"
  )
})

test_that("the warning names the offending block, bound, and onset", {
  ev <- data.frame(onset = c(20, 460), run = 1, cond = factor("a"))
  sf <- fmrihrf::sampling_frame(blocklens = 257, TR = 1.77)
  w <- tryCatch(
    event_model(onset ~ hrf(cond), data = ev, block = ~run,
                sampling_frame = sf, durations = 0),
    warning = conditionMessage)
  expect_match(w, "Block 1")
  expect_match(w, "454.9")
  expect_match(w, "460")
})

test_that("strict = TRUE turns the out-of-frame warning into an error", {
  ev <- data.frame(onset = c(20, 460), run = 1, cond = factor("a"))
  sf <- fmrihrf::sampling_frame(blocklens = 257, TR = 1.77)
  expect_error(
    event_model(onset ~ hrf(cond), data = ev, block = ~run,
                sampling_frame = sf, durations = 0, strict = TRUE),
    "outside the sampling frame"
  )
})

test_that("event_model warns when an event extends past the run end", {
  ev <- data.frame(onset = c(10, 440), run = 1, cond = factor("a"))
  sf <- fmrihrf::sampling_frame(blocklens = 257, TR = 1.77)  # bound 454.89
  expect_warning(
    event_model(onset ~ hrf(cond), data = ev, block = ~run,
                sampling_frame = sf, durations = 30),  # 440 + 30 > 454.89
    "extend past run end"
  )
})

test_that("negative onsets are flagged before the downstream error", {
  # A negative onset is also rejected downstream by fmrihrf::regressor(); our
  # backstop should still warn first with an actionable message. `try()` swallows
  # the later hard error so we can assert on the warning.
  ev <- data.frame(onset = c(-5, 20), run = 1, cond = factor("a"))
  sf <- fmrihrf::sampling_frame(blocklens = 100, TR = 2)
  expect_warning(
    try(event_model(onset ~ hrf(cond), data = ev, block = ~run,
                    sampling_frame = sf, durations = 0), silent = TRUE),
    "outside the sampling frame"
  )
})

test_that("no warning is raised when all onsets are within the frame", {
  ev <- data.frame(onset = c(20, 100, 300), run = 1, cond = factor("a"))
  sf <- fmrihrf::sampling_frame(blocklens = 257, TR = 1.77)
  expect_no_warning(
    event_model(onset ~ hrf(cond), data = ev, block = ~run,
                sampling_frame = sf, durations = 0)
  )
})

test_that("bounds are computed per block, honouring per-block TR", {
  # block 1: bound = 100 * 2 = 200; block 2: bound = 100 * 1.77 = 177
  ev <- data.frame(onset = c(5, 5, 600), run = c(1, 2, 2), cond = factor("a"))
  sf <- fmrihrf::sampling_frame(blocklens = c(100, 100), TR = c(2, 1.77))
  w <- tryCatch(
    event_model(onset ~ hrf(cond), data = ev, block = ~run,
                sampling_frame = sf, durations = 0),
    warning = conditionMessage)
  expect_match(w, "Block 2")
  expect_match(w, "177")
  # block 1 onset (5 s) is comfortably in-bounds and must not be reported
  expect_false(grepl("Block 1", w))
})

test_that("the list interface is also checked", {
  ev <- data.frame(onset = c(10, 500), condition = factor(c("A", "B")), run = 1)
  sf <- fmrihrf::sampling_frame(blocklens = 100, TR = 2)  # bound 200
  expect_warning(
    event_model(list(stim = hrf(condition)), data = ev, block = ~run,
                sampling_frame = sf, durations = 0),
    "outside the sampling frame"
  )
})
