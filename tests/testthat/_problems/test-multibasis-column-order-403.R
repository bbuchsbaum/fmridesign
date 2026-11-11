# Extracted from test-multibasis-column-order.R:403

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
.mb_fixture <- function(form_rhs, basis) {
  ev <- data.frame(
    onset = seq(5, 225, by = 20),
    cat = factor(rep(c("face", "obj"), 6)),
    load = factor(rep(c("hi", "hi", "lo", "lo"), 3)),
    lev = factor(rep(c("a", "b", "c"), each = 4)),
    run = 1
  )
  sf <- fmrihrf::sampling_frame(260, TR = 1)
  list(ev = ev, sf = sf)
}
.expected_signal <- function(truth, prefix, bw, nb, bases = seq_len(nb)) {
  out <- 0
  for (cond in names(bw)) {
    for (j in bases) {
      nm <- sprintf("%s%s_b%02d", prefix, cond, j)
      out <- out + bw[[cond]] * truth[, nm]
    }
  }
  out
}

# test -------------------------------------------------------------------------
fx <- .mb_fixture()
ev <- fx$ev
sf <- fx$sf
em <- event_model(
    onset ~ hrf(cat, load, basis = "spmg2", contrasts = contrast_set(
      unit_contrast(~ cat == "face", name = "unit"),
      contrast(~ cat.face_load.hi - cat.obj_load.hi, name = "form")
    )),
    data = ev, block = ~run, sampling_frame = sf
  )
X <- as.matrix(design_matrix(em))
key <- paste(ev$cat, ev$load, sep = ":")
tags <- c("face:hi" = "cat.face_load.hi", "obj:hi" = "cat.obj_load.hi",
            "face:lo" = "cat.face_load.lo", "obj:lo" = "cat.obj_load.lo")
truth <- .mb_truth(ev, key, tags, fmrihrf::HRF_SPMG2, sf, prefix = "cat_load_")
expect_no_warning(cw <- contrast_weights(em))
expect_equal(unname(drop(X %*% cw[["cat_load#unit"]]$offset_weights)),
               unname(.expected_signal(truth, "cat_load_",
                                       c(cat.face_load.hi = 0.5, cat.face_load.lo = 0.5), 2)),
               tolerance = 1e-8)
expect_equal(unname(drop(X %*% cw[["cat_load#form"]]$offset_weights)),
               unname(.expected_signal(truth, "cat_load_",
                                       c(cat.face_load.hi = 1, cat.obj_load.hi = -1), 2)),
               tolerance = 1e-8)
