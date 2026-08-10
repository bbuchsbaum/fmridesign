# Regression tests for the shared-HRF C++ evaluation hot path in
# .convolve_event_term_matrix() / .eval_design_cols_shared_hrf() and the
# deferred-tibble event_model builder. These pin bit-identical output vs the
# legacy fmrihrf::regressor + evaluate() path, plus colnames / metadata /
# col_indices / term_spans and the public convolve() / event_model() surfaces.

testthat::local_edition(3)

# ---------------------------------------------------------------------------
# Reference implementations
# ---------------------------------------------------------------------------

.legacy_convolve_matrix <- function(term, hrf, sampling_frame, precision = 0.3,
                                    summate = TRUE) {
  dmat <- as.matrix(design_matrix(term, drop.empty = TRUE))
  globons <- fmrihrf::global_onsets(sampling_frame, term$onsets, term$blockids)
  nb <- fmrihrf::nbasis(hrf)
  sample_times <- fmrihrf::samples(sampling_frame, global = TRUE)
  sample_blockids <- fmrihrf::blockids(sampling_frame)
  out <- matrix(0, nrow = length(sample_times), ncol = ncol(dmat) * nb)

  for (bid in unique(sample_blockids)) {
    rows <- which(sample_blockids == bid)
    idx <- which(term$blockids == bid)
    if (!length(idx) || ncol(dmat) == 0L) {
      next
    }
    dblock <- dmat[idx, , drop = FALSE]
    for (j in seq_len(ncol(dblock))) {
      amp <- dblock[, j]
      nz <- which(amp != 0)
      if (!length(nz)) {
        next
      }
      reg <- fmrihrf::regressor(
        onsets = globons[idx][nz],
        hrf = hrf,
        amplitude = amp[nz],
        duration = term$durations[idx][nz],
        summate = summate
      )
      res <- fmrihrf::evaluate(reg, sample_times[rows], precision = precision)
      target <- ((j - 1L) * nb + 1L):((j - 1L) * nb + nb)
      out[rows, target] <- res
    }
  }
  out
}

.numeric_matrix <- function(x) {
  # Compare values only: strip dimnames and any attached attributes
  # (e.g. col_metadata on the shared-path matrix).
  m <- matrix(as.matrix(x), nrow = nrow(x), ncol = ncol(x))
  storage.mode(m) <- "double"
  m
}

.expect_shared_matches_legacy <- function(term, hrf, sf, precision = 0.3,
                                          summate = TRUE, normalize = FALSE) {
  new <- fmridesign:::.convolve_event_term_matrix(
    term, hrf = hrf, sampling_frame = sf,
    precision = precision, summate = summate, normalize = normalize
  )
  old <- .legacy_convolve_matrix(term, hrf, sf, precision = precision,
                                 summate = summate)
  if (isTRUE(normalize) && ncol(old) > 0L) {
    for (j in seq_len(ncol(old))) {
      peak <- max(abs(old[, j]))
      if (peak > 0) old[, j] <- old[, j] / peak
    }
  }

  expect_identical(.numeric_matrix(new), .numeric_matrix(old))
  expect_equal(nrow(new), nrow(old))
  expect_equal(ncol(new), ncol(old))
  expect_false(is.null(colnames(new)))
  expect_false(is.null(attr(new, "col_metadata")))
  expect_s3_class(attr(new, "col_metadata"), "tbl_df")
  expect_equal(nrow(attr(new, "col_metadata")), ncol(new))
  expect_identical(attr(new, "col_metadata")$name, colnames(new))
}

# ---------------------------------------------------------------------------
# Core bit-identical comparisons
# ---------------------------------------------------------------------------

test_that("shared-HRF path matches legacy evaluate for categorical SPMG1", {
  set.seed(11)
  des <- data.frame(
    onset = c(sort(runif(10, 0, 100)), sort(runif(10, 0, 100))),
    run = rep(1:2, each = 10),
    cond = factor(sample(c("A", "B", "C"), 20, TRUE))
  )
  sf <- fmrihrf::sampling_frame(blocklens = c(120, 120), TR = 1)
  m <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sf)
  term <- terms(m)[[1]]
  .expect_shared_matches_legacy(term, attr(term, "hrfspec")$hrf, sf)
})

test_that("shared-HRF path matches legacy for SPMG3 and FIR", {
  set.seed(22)
  des <- data.frame(
    onset = sort(runif(16, 0, 100)),
    run = rep(1:2, each = 8),
    cond = factor(rep(c("A", "B"), 8))
  )
  sf <- fmrihrf::sampling_frame(blocklens = c(120, 120), TR = 1)

  m3 <- event_model(onset ~ hrf(cond, basis = "spmg3"),
                    data = des, block = ~run, sampling_frame = sf)
  term3 <- terms(m3)[[1]]
  .expect_shared_matches_legacy(term3, attr(term3, "hrfspec")$hrf, sf)

  mf <- event_model(onset ~ hrf(cond, basis = "fir", nbasis = 8),
                    data = des, block = ~run, sampling_frame = sf)
  termf <- terms(mf)[[1]]
  .expect_shared_matches_legacy(termf, attr(termf, "hrfspec")$hrf, sf)
})

test_that("shared-HRF path matches legacy for parametric modulators and durations", {
  set.seed(33)
  des <- data.frame(
    onset = sort(runif(12, 0, 80)),
    run = 1L,
    cond = factor(rep(c("A", "B"), 6)),
    rt = rnorm(12, 0.8, 0.2),
    duration = runif(12, 0, 2)
  )
  sf <- fmrihrf::sampling_frame(blocklens = 100, TR = 1)
  m <- event_model(
    onset ~ hrf(cond, durations = duration) + hrf(rt, durations = duration),
    data = des, block = ~run, sampling_frame = sf
  )
  for (term in terms(m)) {
    .expect_shared_matches_legacy(term, attr(term, "hrfspec")$hrf, sf)
  }
})

test_that("shared-HRF path matches legacy for trialwise / LSS", {
  set.seed(44)
  des <- data.frame(
    onset = unlist(lapply(1:3, function(r) sort(runif(8, 0, 80)))),
    run = rep(1:3, each = 8)
  )
  sf <- fmrihrf::sampling_frame(blocklens = rep(100, 3), TR = 1)
  m <- event_model(onset ~ trialwise(), data = des, block = ~run, sampling_frame = sf)
  term <- terms(m)[[1]]
  .expect_shared_matches_legacy(term, attr(term, "hrfspec")$hrf, sf)
})

test_that("shared-HRF path matches legacy with negative amplitudes", {
  # Negative modulators must not be treated as empty by the col_has / amp != 0 tests.
  cond <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
  mod <- c(-2, 1, -0.5, 3)
  ons <- c(5, 25, 5, 25)
  bids <- c(1, 1, 2, 2)
  sf <- fmrihrf::sampling_frame(blocklens = c(60, 60), TR = 1)
  term <- event_term(list(cond = cond, mod = mod), onsets = ons, blockids = bids)
  attr(term, "term_tag") <- "cond_mod"
  attr(term, "hrfspec") <- list(hrf = fmrihrf::HRF_SPMG1, summate = TRUE, normalize = FALSE)
  .expect_shared_matches_legacy(term, fmrihrf::HRF_SPMG1, sf)
})

test_that("shared-HRF path matches legacy for dense all-live blocks", {
  # Every condition present in every run: exercises the all(col_has) branch
  # (no sparse skip) while still using shared-HRF evaluation.
  set.seed(55)
  des <- data.frame(
    onset = c(sort(runif(12, 0, 100)), sort(runif(12, 0, 100))),
    run = rep(1:2, each = 12),
    cond = factor(rep(c("A", "B", "C"), length.out = 24))
  )
  sf <- fmrihrf::sampling_frame(blocklens = c(120, 120), TR = 1)
  m <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sf)
  term <- terms(m)[[1]]
  dmat <- as.matrix(design_matrix(term))
  # Sanity: every column has events in both blocks.
  for (bid in 1:2) {
    expect_true(all(colSums(abs(dmat[term$blockids == bid, , drop = FALSE])) > 0))
  }
  .expect_shared_matches_legacy(term, attr(term, "hrfspec")$hrf, sf)
})

test_that("shared-HRF path matches legacy under normalize=TRUE", {
  set.seed(66)
  des <- data.frame(
    onset = sort(runif(10, 0, 80)),
    run = 1L,
    cond = factor(rep(c("A", "B"), 5)),
    duration = runif(10, 0.5, 3)
  )
  sf <- fmrihrf::sampling_frame(blocklens = 100, TR = 1)
  m <- event_model(
    onset ~ hrf(cond, durations = duration, normalize = TRUE),
    data = des, block = ~run, sampling_frame = sf
  )
  term <- terms(m)[[1]]
  .expect_shared_matches_legacy(
    term, attr(term, "hrfspec")$hrf, sf, normalize = TRUE
  )
})

test_that("shared-HRF path matches legacy at non-default precision", {
  set.seed(77)
  des <- data.frame(
    onset = sort(runif(8, 0, 80)),
    run = 1L,
    cond = factor(rep(c("A", "B"), 4))
  )
  sf <- fmrihrf::sampling_frame(blocklens = 100, TR = 1)
  m <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sf)
  term <- terms(m)[[1]]
  for (prec in c(0.1, 0.5, 1.0)) {
    .expect_shared_matches_legacy(
      term, attr(term, "hrfspec")$hrf, sf, precision = prec
    )
  }
})

# ---------------------------------------------------------------------------
# Public API / deferred tibble / event_model bookkeeping
# ---------------------------------------------------------------------------

test_that("public convolve() matches matrix path and preserves metadata", {
  set.seed(88)
  des <- data.frame(
    onset = sort(runif(10, 0, 80)),
    run = 1L,
    cond = factor(rep(c("A", "B"), 5))
  )
  sf <- fmrihrf::sampling_frame(blocklens = 100, TR = 1)
  m <- event_model(onset ~ hrf(cond, basis = "spmg2"),
                   data = des, block = ~run, sampling_frame = sf)
  term <- terms(m)[[1]]
  hrf <- attr(term, "hrfspec")$hrf

  mat <- fmridesign:::.convolve_event_term_matrix(term, hrf, sf, precision = 0.3)
  tib <- convolve(term, hrf, sf, precision = 0.3)

  expect_s3_class(tib, "tbl_df")
  expect_identical(.numeric_matrix(tib), .numeric_matrix(mat))
  expect_identical(colnames(tib), colnames(mat))
  expect_identical(attr(tib, "col_metadata"), attr(mat, "col_metadata"))
})

test_that("event_model end-to-end locks values, names, indices, and metadata", {
  set.seed(99)
  des <- data.frame(
    onset = c(sort(runif(10, 0, 100)), sort(runif(10, 0, 100))),
    run = rep(1:2, each = 10),
    cond = factor(rep(c("A", "B"), 10)),
    task = factor(rep(c("X", "Y"), each = 5, times = 2)),
    rt = rnorm(20, 0.8, 0.15)
  )
  sf <- fmrihrf::sampling_frame(blocklens = c(120, 120), TR = 1)
  m <- event_model(
    onset ~ hrf(cond) + hrf(cond, task) + hrf(rt, basis = "spmg3"),
    data = des, block = ~run, sampling_frame = sf
  )
  dm <- design_matrix(m)

  # Rebuild term-by-term via the legacy evaluate path and cbind.
  term_mats <- lapply(terms(m), function(term) {
    hrf <- attr(term, "hrfspec")$hrf
    legacy <- .legacy_convolve_matrix(term, hrf, sf, precision = 0.3)
    shared <- fmridesign:::.convolve_event_term_matrix(
      term, hrf, sf, precision = 0.3
    )
    expect_identical(.numeric_matrix(shared), .numeric_matrix(legacy))
    shared
  })
  ref <- do.call(cbind, term_mats)
  colnames(ref) <- make.names(unlist(lapply(term_mats, colnames)), unique = TRUE)

  expect_identical(.numeric_matrix(dm), .numeric_matrix(ref))
  expect_identical(colnames(dm), colnames(ref))

  col_indices <- attr(dm, "col_indices")
  term_spans <- attr(dm, "term_spans")
  meta <- attr(dm, "col_metadata")

  expect_type(col_indices, "list")
  expect_equal(names(col_indices), names(terms(m)))
  expect_equal(sum(lengths(col_indices)), ncol(dm))
  expect_equal(unname(as.integer(term_spans[length(term_spans)])), ncol(dm))
  expect_s3_class(meta, "tbl_df")
  expect_equal(nrow(meta), ncol(dm))
  expect_identical(meta$name, colnames(dm))
  expect_true(all(meta$term_index %in% seq_along(terms(m))))
})

test_that("trialwise add_sum mean column matches legacy average", {
  set.seed(101)
  des <- data.frame(
    onset = unlist(lapply(1:2, function(r) sort(runif(6, 0, 80)))),
    run = rep(1:2, each = 6)
  )
  sf <- fmrihrf::sampling_frame(blocklens = c(100, 100), TR = 1)
  m <- event_model(
    onset ~ trialwise(add_sum = TRUE),
    data = des, block = ~run, sampling_frame = sf
  )
  term <- terms(m)[[1]]
  hrf <- attr(term, "hrfspec")$hrf

  shared <- fmridesign:::.convolve_event_term_matrix(term, hrf, sf, precision = 0.3)
  legacy <- .legacy_convolve_matrix(term, hrf, sf, precision = 0.3)
  # add_sum appends the row-mean of the trial columns.
  expect_equal(ncol(shared), ncol(legacy) + 1L)
  expect_identical(.numeric_matrix(shared)[, seq_len(ncol(legacy))], legacy)
  expect_equal(
    as.numeric(shared[, ncol(shared)]),
    rowMeans(legacy),
    tolerance = 0
  )
  expect_true(grepl("_mean$", colnames(shared)[ncol(shared)]))
})

# ---------------------------------------------------------------------------
# Helpers and edge cases
# ---------------------------------------------------------------------------

test_that("hrf_fine_matrix matches fmrihrf .memo_hrf for common bases", {
  memo <- utils::getFromNamespace(".memo_hrf", "fmrihrf")
  precision <- 0.3
  for (hrf in list(fmrihrf::HRF_SPMG1, fmrihrf::HRF_SPMG2, fmrihrf::HRF_SPMG3)) {
    span <- attr(hrf, "span")
    ours <- fmridesign:::.hrf_fine_matrix(hrf, span, precision)
    theirs <- memo(hrf, span, precision)
    expect_identical(ours, theirs)
  }
})

test_that(".eval_design_cols_shared_hrf filters out-of-window onsets", {
  # Onsets before grid[1] - span or after grid[end] must be dropped, matching
  # fmrihrf:::prep_reg_inputs. Place one in-window and one far out-of-window.
  hrf <- fmrihrf::HRF_SPMG1
  span <- attr(hrf, "span")
  precision <- 0.3
  grid <- seq(0, 50, by = 1)
  hrf_matrix <- fmridesign:::.hrf_fine_matrix(hrf, span, precision)

  # Column 1: onset inside window. Column 2: onset far after grid end.
  dmat <- matrix(c(1, 0,
                   0, 1), nrow = 2, byrow = TRUE)
  globons <- c(10, 1000)
  durations <- c(0, 0)

  got <- fmridesign:::.eval_design_cols_shared_hrf(
    dmat = dmat, globons = globons, durations = durations, grid = grid,
    hrf_matrix = hrf_matrix, hrf_span = span, precision = precision,
    nb = 1L, col_idx = c(1L, 2L)
  )

  ref_in <- as.numeric(fmrihrf::evaluate(
    fmrihrf::regressor(10, hrf = hrf), grid, precision = precision
  ))
  expect_identical(got[, 1], ref_in)
  expect_true(all(got[, 2] == 0))
})

test_that(".eval_design_cols_shared_hrf leaves empty columns as zeros", {
  hrf <- fmrihrf::HRF_SPMG1
  span <- attr(hrf, "span")
  precision <- 0.3
  grid <- seq(0, 40, by = 1)
  hrf_matrix <- fmridesign:::.hrf_fine_matrix(hrf, span, precision)
  dmat <- matrix(c(1, 0, 0,
                   0, 0, 2), nrow = 2, byrow = TRUE)
  got <- fmridesign:::.eval_design_cols_shared_hrf(
    dmat = dmat, globons = c(5, 15), durations = c(0, 0), grid = grid,
    hrf_matrix = hrf_matrix, hrf_span = span, precision = precision,
    nb = 1L, col_idx = c(1L, 2L, 3L)
  )
  expect_equal(dim(got), c(length(grid), 3L))
  expect_true(all(got[, 2] == 0))
  expect_gt(sum(abs(got[, 1])), 0)
  expect_gt(sum(abs(got[, 3])), 0)
})

test_that("multi-basis scatter places basis columns in the correct slots", {
  # nb=3: live column 2 must land in output columns 4:6, not 1:3.
  hrf <- fmrihrf::HRF_SPMG3
  span <- attr(hrf, "span")
  precision <- 0.3
  grid <- seq(0, 60, by = 1)
  hrf_matrix <- fmridesign:::.hrf_fine_matrix(hrf, span, precision)
  nb <- fmrihrf::nbasis(hrf)
  dmat <- matrix(c(0, 1, 0), nrow = 1)
  live <- fmridesign:::.eval_design_cols_shared_hrf(
    dmat = dmat, globons = 10, durations = 0, grid = grid,
    hrf_matrix = hrf_matrix, hrf_span = span, precision = precision,
    nb = nb, col_idx = 2L
  )
  expect_equal(ncol(live), nb)

  # Scatter the way convolve does for nb > 1.
  out <- matrix(0, nrow = length(grid), ncol = 3L * nb)
  keep <- 2L
  target <- as.vector(vapply(
    keep,
    function(j) ((j - 1L) * nb + 1L):((j - 1L) * nb + nb),
    integer(nb)
  ))
  out[, target] <- live

  expect_true(all(out[, 1:3] == 0))
  expect_true(all(out[, 7:9] == 0))
  expect_identical(out[, 4:6], live)

  ref <- fmrihrf::evaluate(
    fmrihrf::regressor(10, hrf = hrf), grid, precision = precision
  )
  expect_identical(out[, 4:6], ref)
})

test_that("HRF does not bleed across run boundaries under shared-HRF path", {
  # Event at the very end of run 1 must not produce nonzero signal in run 2.
  des <- data.frame(
    onset = c(90, 10),
    run = c(1L, 2L),
    cond = factor(c("A", "B"), levels = c("A", "B"))
  )
  sf <- fmrihrf::sampling_frame(blocklens = c(100, 100), TR = 1)
  m <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sf)
  dm <- as.matrix(design_matrix(m))
  tp_block <- fmrihrf::blockids(sf)
  a_col <- grep("A", colnames(dm))
  b_col <- grep("B", colnames(dm))
  expect_true(all(dm[tp_block == 2, a_col] == 0))
  expect_true(all(dm[tp_block == 1, b_col] == 0))
  expect_gt(sum(abs(dm[tp_block == 1, a_col])), 0)
  expect_gt(sum(abs(dm[tp_block == 2, b_col])), 0)
})

test_that("empty block still yields exact zero region under shared-HRF path", {
  cond <- factor(c("A", "B", "A", "B"), levels = c("A", "B"))
  ons <- c(5, 20, 5, 20)
  bids <- c(1, 1, 2, 2) # no events in block 3
  sf <- fmrihrf::sampling_frame(blocklens = c(60, 60, 60), TR = 1)
  term <- event_term(list(cond = cond), onsets = ons, blockids = bids)
  attr(term, "term_tag") <- "cond"
  mat <- fmridesign:::.convolve_event_term_matrix(term, fmrihrf::HRF_SPMG1, sf)
  tp_block <- fmrihrf::blockids(sf)
  expect_equal(nrow(mat), 180L)
  expect_true(all(mat[tp_block == 3, ] == 0))
  .expect_shared_matches_legacy(term, fmrihrf::HRF_SPMG1, sf)
})

test_that("NaN modulator fallback still returns finite design of correct shape", {
  cond <- factor(c("A", "A", "B", "A", "B", "B"), levels = c("A", "B"))
  mod <- c(1, NaN, 3, 1, 1, 1)
  ons <- c(5, 20, 35, 5, 20, 35)
  bids <- c(1, 1, 1, 2, 2, 2)
  sf <- fmrihrf::sampling_frame(blocklens = c(60, 60), TR = 1)
  # event_term warns about NA in the continuous modulator; that warning is the
  # signal that we are on the misaligned / legacy-fallback path.
  expect_warning(
    term <- event_term(list(cond = cond, mod = mod), onsets = ons, blockids = bids),
    "NA values detected"
  )
  attr(term, "term_tag") <- "cond_mod"

  # nrow(design_matrix) != n events -> shared path must NOT be taken, but
  # convolution must still succeed with the legacy NA-filter path.
  dmat <- design_matrix(term)
  expect_false(nrow(dmat) == length(term$blockids))

  expect_no_error(conv <- convolve(term, fmrihrf::HRF_SPMG1, sf))
  expect_s3_class(conv, "tbl_df")
  expect_equal(nrow(conv), 120L)
  expect_true(ncol(conv) >= 1L)
  expect_true(all(is.finite(as.matrix(conv)) | is.na(as.matrix(conv))))
})

test_that("hrf_fun per-onset path still matches evaluate sum (legacy path)", {
  # Shared-HRF is disabled when hrf_list is present; lock the fallback.
  des <- data.frame(
    onset = c(5, 25, 45),
    condition = factor(c("A", "B", "A")),
    run = 1L
  )
  sf <- fmrihrf::sampling_frame(blocklens = 80, TR = 1)
  gen <- function(d) {
    lapply(seq_len(nrow(d)), function(i) {
      if (d$condition[i] == "A") fmrihrf::HRF_SPMG1 else fmrihrf::HRF_GAMMA
    })
  }
  m <- event_model(
    onset ~ hrf(condition, hrf_fun = gen),
    data = des, block = ~run, sampling_frame = sf
  )
  dm <- as.matrix(design_matrix(m))
  expect_equal(nrow(dm), 80L)
  expect_true(ncol(dm) >= 2L)
  expect_true(all(is.finite(dm)))

  # Condition A uses SPMG1 on onsets 5 and 45; column must match that sum.
  a_cols <- grep("A", colnames(dm), value = TRUE)
  expect_true(length(a_cols) >= 1L)
  ref_a <- fmrihrf::evaluate(
    fmrihrf::regressor(c(5, 45), hrf = fmrihrf::HRF_SPMG1),
    fmrihrf::samples(sf, global = TRUE), precision = 0.3
  )
  # With drop.empty / naming, the A column is the SPMG1 superposition.
  expect_equal(as.numeric(dm[, a_cols[1]]), as.numeric(ref_a), tolerance = 1e-10)
})
