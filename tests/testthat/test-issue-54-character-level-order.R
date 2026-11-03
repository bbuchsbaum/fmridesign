test_that("character conditions get sorted levels, matching factor()", {
  sf <- fmrihrf::sampling_frame(blocklens = 50, TR = 2)
  ev <- data.frame(onset = c(0, 20, 40, 60),
                   trial_type = c("house", "face", "face", "house"),
                   run = 1, stringsAsFactors = FALSE)
  cn_chr <- colnames(design_matrix(event_model(onset ~ hrf(trial_type), data = ev,
                                               block = ~run, sampling_frame = sf)))
  ev2 <- ev
  ev2$trial_type <- factor(ev2$trial_type)
  cn_fac <- colnames(design_matrix(event_model(onset ~ hrf(trial_type), data = ev2,
                                               block = ~run, sampling_frame = sf)))
  expect_identical(cn_chr, cn_fac)
  expect_match(cn_chr[1], "face")

  # Order depends only on the level set, not on first appearance
  ev3 <- ev
  ev3$trial_type <- rev(ev3$trial_type)
  ev3$trial_type[1:2] <- c("face", "house")
  cn_other <- colnames(design_matrix(event_model(onset ~ hrf(trial_type), data = ev3,
                                                 block = ~run, sampling_frame = sf)))
  expect_identical(cn_chr, cn_other)
})

test_that("factor input keeps its given level order", {
  f <- factor(c("b", "a", "c"), levels = c("c", "b", "a"))
  ef <- event_factor(f, "x", onsets = c(1, 2, 3), blockids = rep(1, 3))
  expect_identical(levels(ef), c("c", "b", "a"))
  ec <- event_factor(c("b", "a", "c"), "x", onsets = c(1, 2, 3), blockids = rep(1, 3))
  expect_identical(levels(ec), c("a", "b", "c"))
})
