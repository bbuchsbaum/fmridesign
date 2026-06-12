test_that("event_model warns for degenerate parametric modulators", {
  sframe <- fmrihrf::sampling_frame(blocklens = 100, TR = 2)
  ev <- data.frame(
    onset = seq(0, 38, length.out = 20),
    block = 1,
    cond = factor(rep("A", 20)),
    modv = seq_len(20),
    modc = rep(2, 20),
    modz = rep(0, 20),
    modna = NA_real_
  )

  expect_no_warning(
    event_model(
      onset ~ hrf(cond) + hrf(modv),
      data = ev,
      block = ~block,
      sampling_frame = sframe
    )
  )

  expect_warning(
    event_model(
      onset ~ hrf(cond) + hrf(modc),
      data = ev,
      block = ~block,
      sampling_frame = sframe
    ),
    "zero variance"
  )

  expect_warning(
    event_model(
      onset ~ hrf(cond) + hrf(modz),
      data = ev,
      block = ~block,
      sampling_frame = sframe
    ),
    "all zero"
  )

  expect_warning(
    event_model(
      onset ~ hrf(cond) + hrf(modna),
      data = ev,
      block = ~block,
      sampling_frame = sframe
    ),
    "NA values detected"
  )
})
