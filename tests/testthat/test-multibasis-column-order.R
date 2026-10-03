# Ground-truth tests for multi-basis column naming (GitHub issues #23, #25).
#
# Every named design-matrix column of a multi-basis hrf() term must contain the
# regressor for the (condition, basis) pair its name claims. Truth is built
# independently with fmrihrf::regressor() + fmrihrf::evaluate(), one onset set
# per condition and run, without going through fmridesign's convolution or
# naming code.

# Independent truth: named matrix with columns "<prefix><cond>_b##" in the
# documented condition-major order.
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

test_that("SPMG3 with 2 conditions: column names match column contents (#23)", {
  ev <- data.frame(onset = c(5, 30), task = factor(c("face", "scene")), run = 1)
  sf <- fmrihrf::sampling_frame(60, TR = 1)
  em <- event_model(onset ~ hrf(task, basis = "spmg3"), data = ev,
                    block = ~run, sampling_frame = sf)
  X <- as.matrix(design_matrix(em))
  expect_equal(colnames(X), c(
    "task_task.face_b01", "task_task.face_b02", "task_task.face_b03",
    "task_task.scene_b01", "task_task.scene_b02", "task_task.scene_b03"
  ))
  truth <- .mb_truth(ev, as.character(ev$task),
                     c(face = "task.face", scene = "task.scene"),
                     fmrihrf::HRF_SPMG3, sf, prefix = "task_")
  .expect_columns_match_truth(X, truth)

  # The issue's concrete symptom: scene_b01 is scene's canonical response
  t <- fmrihrf::samples(sf, global = TRUE)
  expect_true(t[which.max(X[, "task_task.scene_b01"])] > 30)
  expect_true(t[which.max(X[, "task_task.face_b01"])] < 30)
})

test_that("SPMG2 with 3 conditions: column names match column contents", {
  ev <- data.frame(onset = c(10, 60, 110, 140), cond = factor(c("A", "B", "C", "A")), run = 1)
  sf <- fmrihrf::sampling_frame(90, TR = 2)
  em <- event_model(onset ~ hrf(cond, basis = "spmg2"), data = ev,
                    block = ~run, sampling_frame = sf)
  truth <- .mb_truth(ev, as.character(ev$cond),
                     c(A = "cond.A", B = "cond.B", C = "cond.C"),
                     fmrihrf::HRF_SPMG2, sf, prefix = "cond_")
  .expect_columns_match_truth(design_matrix(em), truth)
  expect_equal(colnames(design_matrix(em)), colnames(truth))
})

test_that("custom 2-basis HRF from issue #23 is labelled correctly", {
  ev <- data.frame(onset = c(10, 60, 110), cond = factor(c("A", "B", "C")), run = 1)
  sf <- fmrihrf::sampling_frame(80, TR = 2)
  H <- fmrihrf::as_hrf(function(t) cbind((t >= 0 & t < 4) * 1, (t >= 0 & t < 4) * -1),
                       name = "pm", nbasis = 2L, span = 6)
  em <- event_model(onset ~ hrf(cond, basis = H), data = ev, block = ~run,
                    sampling_frame = sf, precision = 0.1)
  truth <- .mb_truth(ev, as.character(ev$cond),
                     c(A = "cond.A", B = "cond.B", C = "cond.C"),
                     H, sf, prefix = "cond_", precision = 0.1)
  .expect_columns_match_truth(design_matrix(em), truth)
})

test_that("FIR basis: column names match column contents", {
  ev <- data.frame(onset = c(4, 40, 70), cond = factor(c("A", "B", "A")), run = 1)
  sf <- fmrihrf::sampling_frame(100, TR = 1)
  em <- event_model(onset ~ hrf(cond, basis = "fir", nbasis = 6), data = ev,
                    block = ~run, sampling_frame = sf)
  hrf <- attr(terms(em)[[1]], "hrfspec")$hrf
  expect_equal(fmrihrf::nbasis(hrf), 6L)
  truth <- .mb_truth(ev, as.character(ev$cond),
                     c(A = "cond.A", B = "cond.B"),
                     hrf, sf, prefix = "cond_")
  .expect_columns_match_truth(design_matrix(em), truth)
})

test_that("multi-run SPMG3: column names match column contents", {
  ev <- data.frame(
    onset = c(5, 25, 45, 8, 30, 50),
    task = factor(c("face", "scene", "house", "scene", "house", "face")),
    run = c(1, 1, 1, 2, 2, 2)
  )
  sf <- fmrihrf::sampling_frame(c(70, 70), TR = 1)
  em <- event_model(onset ~ hrf(task, basis = "spmg3"), data = ev,
                    block = ~run, sampling_frame = sf)
  truth <- .mb_truth(ev, as.character(ev$task),
                     c(face = "task.face", house = "task.house", scene = "task.scene"),
                     fmrihrf::HRF_SPMG3, sf, prefix = "task_")
  .expect_columns_match_truth(design_matrix(em), truth)
})

test_that("factorial multi-basis term: column names match column contents", {
  ev <- data.frame(
    onset = c(5, 25, 45, 65, 85, 105),
    task = factor(c("face", "scene", "face", "scene", "face", "scene")),
    load = factor(c("low", "low", "high", "high", "low", "high"), levels = c("high", "low")),
    run = 1
  )
  sf <- fmrihrf::sampling_frame(140, TR = 1)
  em <- event_model(onset ~ hrf(task, load, basis = "spmg2"), data = ev,
                    block = ~run, sampling_frame = sf)
  key <- paste(ev$task, ev$load, sep = ":")
  tags <- c("face:high" = "task.face_load.high", "scene:high" = "task.scene_load.high",
            "face:low" = "task.face_load.low", "scene:low" = "task.scene_load.low")
  truth <- .mb_truth(ev, key, tags, fmrihrf::HRF_SPMG2, sf, prefix = "task_load_")
  .expect_columns_match_truth(design_matrix(em), truth)
})

test_that("conditions(expand_basis = TRUE), col_metadata and colmap agree with columns", {
  ev <- data.frame(onset = c(10, 60, 110), cond = factor(c("A", "B", "C")), run = 1)
  sf <- fmrihrf::sampling_frame(80, TR = 2)
  em <- event_model(onset ~ hrf(cond, basis = "spmg3"), data = ev, block = ~run,
                    sampling_frame = sf)
  dm <- design_matrix(em)
  term <- terms(em)[[1]]
  expect_equal(paste0("cond_", conditions(term, expand_basis = TRUE)), colnames(dm))
  expect_equal(paste0("cond_", conditions(em, expand_basis = TRUE)), colnames(dm))

  cm <- design_colmap(em)
  expect_equal(cm$condition, rep(c("cond.A", "cond.B", "cond.C"), each = 3))
  expect_equal(cm$basis_ix, rep(1:3, times = 3))
  expect_equal(as.integer(sub(".*_b", "", cm$name)), cm$basis_ix)
  expect_equal(sub("_b[0-9]+$", "", sub("^cond_", "", cm$name)), cm$condition)
  expect_equal(cm$basis_label[1:3], c("canonical", "derivative", "dispersion"))
})

test_that("condition_basis_list returns each condition's own basis columns", {
  term <- event_term(list(cond = factor(c("A", "B", "A"))),
                     onsets = c(2, 30, 55), blockids = c(1, 1, 1))
  sf <- fmrihrf::sampling_frame(blocklens = 80, TR = 1)
  cbl <- condition_basis_list(term, fmrihrf::HRF_SPMG2, sf)
  t <- fmrihrf::samples(sf, global = TRUE)
  for (lev in c("A", "B")) {
    ons <- c(2, 30, 55)[c("A", "B", "A") == lev]
    truth <- fmrihrf::evaluate(fmrihrf::regressor(ons, fmrihrf::HRF_SPMG2), t, precision = 0.3)
    got <- cbl[[paste0("cond.", lev)]]
    expect_equal(unname(as.matrix(got)), unname(truth), tolerance = 1e-8)
  }
})

test_that("name-based contrasts on multi-basis terms target the right data", {
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
  # X %*% w must equal face canonical minus scene canonical, from the truth
  expect_equal(unname(drop(X %*% w)),
               unname(truth[, "task_task.face_b01"] - truth[, "task_task.scene_b01"]),
               tolerance = 1e-8)
  expect_equal(unname(w[w != 0]), c(1, -1))

  w_all <- cw[["task#fs_all"]]$offset_weights[, 1]
  expect_equal(unname(drop(X %*% w_all)),
               unname(rowSums(truth[, grep("face", colnames(truth))]) -
                        rowSums(truth[, grep("scene", colnames(truth))])),
               tolerance = 1e-8)
})

test_that("Fcontrasts(<event_model>) is non-zero for multi-basis terms (#25)", {
  ev <- data.frame(onset = seq(10, 130, by = 30), cond = factor(c("A", "B", "C", "A", "B")),
                   run = 1)
  sf <- fmrihrf::sampling_frame(80, TR = 2)
  for (b in c("spmg1", "spmg2", "spmg3")) {
    em <- event_model(onset ~ hrf(cond, basis = b), data = ev, block = ~run,
                      sampling_frame = sf)
    nb <- fmrihrf::nbasis(attr(terms(em)[[1]], "hrfspec")$hrf)
    expect_no_warning(Fc <- Fcontrasts(em))
    W <- Fc[["cond#cond"]]
    expect_equal(dim(W), c(3L * nb, 2L * nb), info = b)
    expect_equal(rownames(W), colnames(design_matrix(em)), info = b)
    # full column rank (tests the condition effect in every basis)
    expect_equal(qr(W)$rank, 2L * nb, info = b)
    # each column restricted to a single basis and sums to zero
    for (k in seq_len(ncol(W))) {
      nz <- rownames(W)[W[, k] != 0]
      if (nb > 1) expect_length(unique(sub(".*_b", "", nz)), 1)
      expect_equal(sum(W[, k]), 0)
    }
    # Per-basis block equals the term-level condition contrast
    Fl <- Fcontrasts(terms(em)[[1]])[["cond"]]
    for (j in seq_len(nb)) {
      rows <- if (nb > 1) grep(sprintf("_b%02d$", j), rownames(W)) else seq_len(nrow(W))
      cols <- if (nb > 1) grep(sprintf("_b%02d$", j), colnames(W)) else seq_len(ncol(W))
      expect_equal(unname(W[rows, cols]), unname(Fl), info = paste(b, j))
    }
  }
})

test_that("Fcontrasts(<event_model>) for factorial multi-basis term is non-zero", {
  ev <- data.frame(
    onset = c(5, 25, 45, 65, 85, 105),
    task = factor(c("face", "scene", "face", "scene", "face", "scene")),
    load = factor(c("low", "low", "high", "high", "low", "high")),
    run = 1
  )
  sf <- fmrihrf::sampling_frame(140, TR = 1)
  em <- event_model(onset ~ hrf(task, load, basis = "spmg2"), data = ev,
                    block = ~run, sampling_frame = sf)
  expect_no_warning(Fc <- Fcontrasts(em))
  expect_setequal(names(Fc), c("task_load#task", "task_load#load", "task_load#task:load"))
  for (nm in names(Fc)) {
    expect_equal(qr(Fc[[nm]])$rank, 2L, info = nm)
  }
})

test_that("factorial Fcontrasts label main effects by the right factor", {
  ev <- data.frame(
    onset = c(5, 25, 45, 65, 85, 105),
    task = factor(c("face", "scene", "face", "scene", "face", "scene")),
    load = factor(c("low", "low", "high", "high", "low", "high")),
    run = 1
  )
  sf <- fmrihrf::sampling_frame(140, TR = 1)
  for (b in c("spmg1", "spmg2")) {
    em <- event_model(onset ~ hrf(task, load, basis = b), data = ev,
                      block = ~run, sampling_frame = sf)
    Fc <- Fcontrasts(em)
    rn <- rownames(Fc[[1]])
    for (k in seq_len(ncol(Fc[["task_load#task"]]))) {
      w <- Fc[["task_load#task"]][, k]
      # main effect of task: equal weight on both load levels of a task level
      expect_equal(unname(w[grep("task.face_load.high", rn, fixed = TRUE)]),
                   unname(w[grep("task.face_load.low", rn, fixed = TRUE)]), info = b)
      expect_equal(unname(w[grep("task.scene_load.high", rn, fixed = TRUE)]),
                   unname(w[grep("task.scene_load.low", rn, fixed = TRUE)]), info = b)
    }
    for (k in seq_len(ncol(Fc[["task_load#load"]]))) {
      w <- Fc[["task_load#load"]][, k]
      expect_equal(unname(w[grep("task.face_load.high", rn, fixed = TRUE)]),
                   unname(w[grep("task.scene_load.high", rn, fixed = TRUE)]), info = b)
    }
  }
  # Term-level matrices agree with conditions() order as well
  Ft <- Fcontrasts(terms(em)[[1]])
  expect_equal(unname(Ft$task[, 1]), c(1, -1, 1, -1))
  expect_equal(unname(Ft$load[, 1]), c(1, 1, -1, -1))
  expect_equal(rownames(Ft$task), conditions(terms(em)[[1]], drop.empty = FALSE))
})

test_that("interaction_contrast weights map to the term's columns", {
  ev <- data.frame(
    onset = c(5, 25, 45, 65, 85, 105, 125, 145),
    task = factor(rep(c("face", "scene"), 4)),
    load = factor(rep(c("low", "low", "high", "high"), 2)),
    run = 1
  )
  sf <- fmrihrf::sampling_frame(180, TR = 1)
  for (b in c("spmg1", "spmg2")) {
    em <- event_model(
      onset ~ hrf(task, load, basis = b,
                  contrasts = contrast_set(interaction_contrast(~ task * load, name = "tl"))),
      data = ev, block = ~run, sampling_frame = sf
    )
    expect_no_warning(cw <- contrast_weights(em))
    w <- cw[["task_load#tl"]]$offset_weights
    nb <- if (b == "spmg1") 1L else 2L
    expect_equal(sum(w != 0), 4L * nb, info = b)
    expect_equal(sum(w), 0, info = b)
    cn <- rownames(w)
    sgn <- function(tag) sign(w[grep(tag, cn, fixed = TRUE)[1], 1])
    # interaction: (face-scene) differs in sign between loads
    expect_equal(sgn("task.face_load.high"), sgn("task.scene_load.low"), info = b)
    expect_equal(sgn("task.face_load.high"), -sgn("task.face_load.low"), info = b)
    expect_equal(sgn("task.face_load.high"), -sgn("task.scene_load.high"), info = b)
  }
})

# --- Contrast constructors on multi-basis terms: ground truth ---------------
# For each contrast, X %*% w must equal the same linear combination of the
# independently built (condition, basis) truth columns, applied once per basis.

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

# Expected weights on truth columns from base-condition weights `bw`
# (named by base condition tag), replicated over basis indices `bases`.
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

test_that("unit_contrast and formula contrast target the right basis columns", {
  fx <- .mb_fixture()
  ev <- fx$ev; sf <- fx$sf
  for (b in c("spmg1", "spmg3")) {
    hrf_obj <- if (b == "spmg1") fmrihrf::HRF_SPMG1 else fmrihrf::HRF_SPMG3
    nb <- fmrihrf::nbasis(hrf_obj)
    em <- event_model(
      onset ~ hrf(cat, basis = b, contrasts = contrast_set(
        unit_contrast(~ cat == "face", name = "unit"),
        contrast(~ face - obj, name = "form"),
        contrast(~ face - obj, name = "fo") - unit_contrast(~ cat == "face", name = "u2")
      )),
      data = ev, block = ~run, sampling_frame = sf
    )
    X <- as.matrix(design_matrix(em))
    truth <- .mb_truth(ev, as.character(ev$cat), c(face = "cat.face", obj = "cat.obj"),
                       hrf_obj, sf, prefix = "cat_")
    if (nb == 1) colnames(truth) <- sub("_b01$", "", colnames(truth))
    pre <- "cat_"
    sig <- function(bw) {
      out <- 0
      for (cond in names(bw)) {
        nms <- if (nb == 1) paste0(pre, cond) else sprintf("%s%s_b%02d", pre, cond, seq_len(nb))
        for (nm in nms) out <- out + bw[[cond]] * truth[, nm]
      }
      out
    }
    expect_no_warning(cw <- contrast_weights(em))
    expect_equal(unname(drop(X %*% cw[["cat#unit"]]$offset_weights)),
                 unname(sig(c(cat.face = 1))), tolerance = 1e-8, info = b)
    expect_equal(unname(drop(X %*% cw[["cat#form"]]$offset_weights)),
                 unname(sig(c(cat.face = 1, cat.obj = -1))), tolerance = 1e-8, info = b)
    expect_equal(nrow(cw[["cat#unit"]]$weights), 2L * nb)
    dname <- grep("fo:u2", names(cw), fixed = TRUE, value = TRUE)
    expect_length(dname, 1)
    expect_equal(unname(drop(X %*% cw[[dname]]$offset_weights)),
                 unname(sig(c(cat.obj = -1))), tolerance = 1e-8, info = b)
  }
})

test_that("unit_contrast and formula contrast on a factorial multi-basis term", {
  fx <- .mb_fixture()
  ev <- fx$ev; sf <- fx$sf
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
})

test_that("remaining contrast constructors target the right basis columns", {
  fx <- .mb_fixture()
  ev <- fx$ev; sf <- fx$sf
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
  expect_equal(sig("con_a_vs_other"), exp(c(lev.a = 1, lev.b = -0.5, lev.c = -0.5)),
               tolerance = 1e-8)
  expect_equal(sig("con_b_c"), exp(c(lev.b = 1, lev.c = -1)), tolerance = 1e-8)
  win <- grep("^lev#win_", names(cw), value = TRUE)
  expect_true(length(win) > 0)
  expect_equal(sig("colab"), exp(c(lev.a = 1, lev.b = -1), bases = 1), tolerance = 1e-8)
})

test_that("Fcontrasts(<event_model>) skips terms with no categorical variable", {
  set.seed(11)
  ev <- data.frame(onset = seq(5, 155, by = 20), cat = factor(rep(c("face", "obj"), 4)),
                   rt = rnorm(8), run = 1)
  sf <- fmrihrf::sampling_frame(200, TR = 1)
  em <- event_model(onset ~ hrf(cat, basis = "spmg2") + hrf(rt), data = ev,
                    block = ~run, sampling_frame = sf)
  expect_no_error(Fc <- Fcontrasts(em))
  expect_named(Fc, "cat#cat")
  expect_equal(qr(Fc[["cat#cat"]])$rank, 2L)
  expect_true(all(Fc[["cat#cat"]][grep("^rt", rownames(Fc[["cat#cat"]])), ] == 0))

  em_rt <- event_model(onset ~ hrf(rt), data = ev, block = ~run, sampling_frame = sf)
  expect_identical(Fcontrasts(em_rt), list())
  # Direct call on a continuous-only term still signals an error
  expect_error(Fcontrasts(terms(em_rt)[[1]]), "No categorical")
})

test_that("unit_contrast honours a logical selector in A", {
  ev <- data.frame(onset = c(5, 25, 45, 65), cond = factor(c("A", "B", "A", "B")), run = 1)
  sf <- fmrihrf::sampling_frame(90, TR = 1)
  term <- terms(event_model(onset ~ hrf(cond), data = ev, block = ~run,
                            sampling_frame = sf))[[1]]
  w <- contrast_weights(unit_contrast(~ cond == "A", name = "A"), term)$weights[, 1]
  expect_equal(unname(w), c(1, 0))
  w_all <- contrast_weights(unit_contrast(~ cond, name = "all"), term)$weights[, 1]
  expect_equal(unname(w_all), c(0.5, 0.5))
})
