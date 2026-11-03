# A host application sets palettes and type for every figure through options
# shared with the other neuroimaging packages; unset, nothing changes.

test_that("palettes follow the shared neuro.* options", {
  withr::local_options(neuro.palette.categorical = NULL, neuro.palette.diverging = NULL)
  expect_equal(fmridesign_palette("categorical", 2), c("#0072B2", "#D55E00"))
  withr::local_options(neuro.palette.categorical = c("#8a6d1f", "#2f7d6d", "#6b5b95"),
                       neuro.palette.diverging = c("#2a5bd7", "#f6f5f1", "#d33f29"))
  expect_equal(fmridesign_palette("categorical", 2), c("#8a6d1f", "#2f7d6d"))
  expect_error(fmridesign_palette("categorical", 4), "3 distinct hues")
  expect_equal(fmridesign_palette("diverging")[c(1, 3)], c("#2a5bd7", "#d33f29"))
})

test_that("the theme takes its size and family from the shared options", {
  withr::local_options(neuro.base_size = 13, neuro.base_family = "serif")
  th <- theme_fmridesign()
  expect_equal(th$text$size, 13)
  expect_equal(th$text$family, "serif")
  withr::local_options(fmridesign.base_size = 9)
  expect_equal(theme_fmridesign()$text$size, 9)   # the package's own option wins
})
