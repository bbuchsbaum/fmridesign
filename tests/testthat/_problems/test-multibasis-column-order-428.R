# Extracted from test-multibasis-column-order.R:428

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
cset <- do.call(contrast_set, c(
    list(poly_contrast(~ lev, name = "lin", degree = 1, value_map = list(a = 1, b = 2, c = 3))),
    unclass(one_against_all_contrast(c("a", "b", "c"), "lev")),
    unclass(pairwise_contrasts(c("a", "b", "c"), "lev")),
    unclass(sliding_window_contrasts(c("a", "b", "c"), "lev", window_size = 1)),
    list(column_contrast("^lev\\.a_b01$", "^lev\\.b_b01$", name = "colab"))
  ))
em <- event_model(onset ~ hrf(lev, basis = "spmg3", contrasts = cset),
                    data = ev, block = ~run, sampling_frame = sf)
X <- as.matrix(design_matrix(em))
truth <- .mb_truth(ev, as.character(ev$lev),
                     c(a = "lev.a", b = "lev.b", c = "lev.c"),
                     fmrihrf::HRF_SPMG3, sf, prefix = "lev_")
expect_no_warning(cw <- contrast_weights(em))
sig <- function(name) unname(drop(X %*% cw[[paste0("lev#", name)]]$offset_weights))
exp <- function(bw, bases = 1:3) unname(.expected_signal(truth, "lev_", bw, 3, bases))
lin <- stats::contr.poly(3)[, 1]
lin_sig <- sig("lin")
expect_equal(lin_sig, exp(c(lev.a = lin[1], lev.b = lin[2], lev.c = lin[3])), tolerance = 1e-8)
