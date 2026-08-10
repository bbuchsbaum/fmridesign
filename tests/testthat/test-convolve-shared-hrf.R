# Correctness tests for the shared-HRF C++ evaluation hot path in
# .convolve_event_term_matrix() / .eval_design_cols_shared_hrf().
# These pin bit-identical output vs the legacy fmrihrf::regressor +
# evaluate() path across categorical, multi-basis, modulated, FIR,
# trialwise, multi-run, and duration > 0 cases.

testthat::local_edition(3)

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

.expect_shared_matches_legacy <- function(term, hrf, sf, precision = 0.3,
                                          summate = TRUE) {
  new <- fmridesign:::.convolve_event_term_matrix(
    term, hrf = hrf, sampling_frame = sf,
    precision = precision, summate = summate
  )
  old <- .legacy_convolve_matrix(term, hrf, sf, precision = precision,
                                 summate = summate)
  expect_equal(
    unname(matrix(as.numeric(new), nrow = nrow(new), ncol = ncol(new))),
    old,
    tolerance = 0
  )
}

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

test_that("hrf_fine_matrix matches fmrihrf .memo_hrf", {
  hrf <- fmrihrf::HRF_SPMG3
  span <- attr(hrf, "span")
  precision <- 0.3
  ours <- fmridesign:::.hrf_fine_matrix(hrf, span, precision)
  theirs <- utils::getFromNamespace(".memo_hrf", "fmrihrf")(hrf, span, precision)
  expect_identical(ours, theirs)
})
