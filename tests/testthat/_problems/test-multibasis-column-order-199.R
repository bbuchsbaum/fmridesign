# Extracted from test-multibasis-column-order.R:199

# prequel ----------------------------------------------------------------------
.mb_truth <- function(ev, cond_key, cond_tags, hrf, sf, prefix, precision = 0.3) {
  nb <- fmrihrf::nbasis(hrf)
  bids <- fmrihrf::blockids(sf)
  n <- length(bids)
  pad <- max(2L, nchar(as.character(nb)))
  out <- list()
  for (k in seq_along(cond_tags)) {
    acc <- matrix(0, n, nb)
    for (b in unique(bids)) {
      rows <- which(bids == b)
      sel <- cond_key == names(cond_tags)[k] & ev$run == b
      if (!any(sel)) next
      local_t <- fmrihrf::samples(sf, blockids = b, global = FALSE)
      reg <- fmrihrf::regressor(ev$onset[sel], hrf)
      y <- fmrihrf::evaluate(reg, local_t, precision = precision)
      acc[rows, ] <- matrix(y, ncol = nb)
    }
    for (j in seq_len(nb)) {
      nm <- sprintf(paste0("%s%s_b%0", pad, "d"), prefix, cond_tags[[k]], j)
      out[[nm]] <- acc[, j]
    }
  }
  do.call(cbind, out)
}
.expect_columns_match_truth <- function(X, truth, tol = 1e-8) {
  X <- as.matrix(X)
  expect_setequal(colnames(X), colnames(truth))
  for (nm in colnames(truth)) {
    diff <- max(abs(unname(X[, nm]) - unname(truth[, nm])))
    expect_true(diff < tol * max(1, max(abs(truth[, nm]))),
                info = sprintf("column '%s' does not match its (condition, basis) truth (max diff %.3g)",
                               nm, diff))
  }
}

# test -------------------------------------------------------------------------
ev <- data.frame(onset = c(5, 30, 55, 80), task = factor(c("face", "scene", "face", "scene")),
                   run = 1)
sf <- fmrihrf::sampling_frame(110, TR = 1)
em <- event_model(
    onset ~ hrf(task, basis = "spmg3",
                contrasts = contrast_set(
                  pair_contrast(~ task == "face", ~ task == "scene", name = "fs_can", basis = 1),
                  pair_contrast(~ task == "face", ~ task == "scene", name = "fs_all")
                )),
    data = ev, block = ~run, sampling_frame = sf
  )
X <- as.matrix(design_matrix(em))
truth <- .mb_truth(ev, as.character(ev$task),
                     c(face = "task.face", scene = "task.scene"),
                     fmrihrf::HRF_SPMG3, sf, prefix = "task_")
cw <- contrast_weights(em)
w <- cw[["task#fs_can"]]$offset_weights[, 1]
expect_equal(unname(drop(X %*% w)),
               unname(truth[, "task_task.face_b01"] - truth[, "task_task.scene_b01"]),
               tolerance = 1e-8)
expect_equal(unname(w[w != 0]), c(1, -1))
w_all <- cw[["task#fs_all"]]$offset_weights[, 1]
expect_equal(unname(drop(X %*% w_all)),
               unname(rowSums(truth[, grep("face", colnames(truth))]) -
                        rowSums(truth[, grep("scene", colnames(truth))])),
               tolerance = 1e-8)
