# fmridesign re-exports fmrihrf's accessor generics rather than defining its
# own, so methods from either package are reachable through either namespace.

test_that("accessor generics are fmrihrf's own functions", {
  expect_identical(fmridesign::onsets, fmrihrf::onsets)
  expect_identical(fmridesign::durations, fmrihrf::durations)
  expect_identical(fmridesign::blockids, fmrihrf::blockids)
  expect_identical(fmridesign::nbasis, fmrihrf::nbasis)
})

test_that("fmrihrf:: generics dispatch to fmridesign methods", {
  term <- event_term(
    list(condition = factor(c("A", "B", "A"))),
    onsets = c(0, 10, 20), blockids = c(1, 1, 2), durations = c(2, 3, 4)
  )
  expect_equal(fmrihrf::onsets(term), c(0, 10, 20))
  expect_equal(fmrihrf::durations(term), c(2, 3, 4))
  expect_equal(fmrihrf::blockids(term), c(1, 1, 2))

  spec <- hrf(condition, basis = "spmg3")
  expect_equal(fmrihrf::nbasis(spec), 3L)

  sf <- fmrihrf::sampling_frame(blocklens = c(20, 20), TR = 2)
  des <- data.frame(onset = c(0, 10, 20), run = c(1, 1, 2),
                    condition = factor(c("A", "B", "A")))
  em <- event_model(onset ~ hrf(condition), data = des, block = ~run,
                    sampling_frame = sf)
  expect_equal(fmrihrf::blockids(em), c(1, 1, 2))
})

test_that("fmridesign:: generics dispatch to fmrihrf methods", {
  reg <- fmrihrf::regressor(c(5, 15), fmrihrf::HRF_SPMG1, duration = c(1, 2))
  expect_equal(fmridesign::onsets(reg), c(5, 15))
  expect_equal(fmridesign::durations(reg), c(1, 2))
  expect_equal(fmridesign::nbasis(fmrihrf::HRF_SPMG3), 3L)
  expect_equal(fmridesign::nbasis(reg), 1L)

  sf <- fmrihrf::sampling_frame(blocklens = c(3, 2), TR = 2)
  expect_equal(fmridesign::blockids(sf), c(1L, 1L, 1L, 2L, 2L))
})
