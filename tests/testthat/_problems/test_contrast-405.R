# Extracted from test_contrast.R:405

# prequel ----------------------------------------------------------------------
withr::local_options(list(mc.cores = 2))
library(testthat)
library(assertthat)
library(fmrihrf)
facedes <- NULL
try({
  suppressPackageStartupMessages(library(fmrireg))
  facedes <- try(read.table(system.file("extdata", "face_design.txt", package = "fmrireg"), header=TRUE), silent = TRUE)
  if (inherits(facedes, "try-error")) facedes <- NULL
}, silent = TRUE)
if (is.null(facedes)) {
  set.seed(1)
  n <- 10
  facedes <- data.frame(
    onset = seq(0, by = 4, length.out = n),
    rep_num = sample(c(-1, 1, 2, 3, 4), n, replace = TRUE),
    run = rep(1, n)
  )
}

# test -------------------------------------------------------------------------
set.seed(5)
des <- data.frame(onset = seq(2, by = 6, length.out = 12), run = 1,
                    cond = factor(rep(c("A", "B", "C"), 4)), RT = rnorm(12))
sframe <- fmrihrf::sampling_frame(blocklens = 100, TR = 2)
cset <- contrast_set(
    pair_contrast(~ cond == "A", ~ cond == "B", name = "ab"),
    unit_contrast(~ cond == "C", name = "c"),
    oneway_contrast(~ cond, name = "main"),
    poly_contrast(~ cond, name = "lin", degree = 1,
                  value_map = list(A = 1, B = 2, C = 3)),
    contrast(~ A - C, name = "ac")
  )
ref <- event_model(onset ~ hrf(cond, contrasts = cset), data = des,
                     block = ~run, sampling_frame = sframe)
pm <- event_model(onset ~ hrf(cond, RT, contrasts = cset), data = des,
                    block = ~run, sampling_frame = sframe)
expect_silent(cw_pm <- contrast_weights(pm))
cw_ref <- contrast_weights(ref)
expect_equal(unname(cw_pm[[grep("#ab$", names(cw_pm))]]$offset_weights[, 1]), c(1, -1, 0))
expect_equal(unname(cw_pm[[grep("#ac$", names(cw_pm))]]$offset_weights[, 1]), c(1, 0, -1))
for (i in seq_along(cw_ref)) {
    expect_equal(unname(cw_pm[[i]]$offset_weights), unname(cw_ref[[i]]$offset_weights),
                 info = names(cw_ref)[i])
  }
expect_equal(rownames(cw_pm[[1]]$weights), conditions(terms(pm)[[1]]))
pm2 <- event_model(onset ~ hrf(cond, RT, basis = "spmg2", contrasts = cset),
                     data = des, block = ~run, sampling_frame = sframe)
ab2 <- contrast_weights(pm2)[[1]]
expect_equal(unname(ab2$offset_weights[, 1]), c(1, 1, -1, -1, 0, 0))
