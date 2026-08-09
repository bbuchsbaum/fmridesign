# Correctness tests for the per-block all-zero-column skip fast path in
# convolve.event_term() (R/event_vector.R). These assert implementation-
# independent invariants so they pin the numeric result regardless of whether
# the fast path or the fallback path is taken.

testthat::local_edition(3)

test_that("trialwise design is block-diagonal and obeys superposition", {
  set.seed(101)
  n_runs <- 4; epr <- 12; blocklen <- 120
  des <- data.frame(
    onset = as.numeric(unlist(lapply(seq_len(n_runs),
              function(r) sort(runif(epr, 0, blocklen - 20))))),
    run = rep(seq_len(n_runs), each = epr)
  )
  sf <- fmrihrf::sampling_frame(blocklens = rep(blocklen, n_runs), TR = 1)
  m <- event_model(onset ~ trialwise(), data = des, block = ~run, sampling_frame = sf)

  dm <- as.matrix(design_matrix(m))
  tp_block <- fmrihrf::blockids(sf)  # block id per timepoint (row)

  # Block-diagonal: each trial column is nonzero only within a single block.
  for (j in seq_len(ncol(dm))) {
    nz_blocks <- unique(tp_block[abs(dm[, j]) > 0])
    expect_length(nz_blocks, 1L)
  }

  # Superposition: with single-onset unit-amplitude trials and summate = TRUE,
  # the row-sum over all trial columns equals one regressor built from ALL
  # onsets. This is independent of the skip optimization.
  globons <- fmrihrf::global_onsets(sf, des$onset, des$run)
  ref <- fmrihrf::evaluate(
    fmrihrf::regressor(onsets = globons, hrf = fmrihrf::HRF_SPMG1, amplitude = 1),
    fmrihrf::samples(sf, global = TRUE), precision = 0.3)
  expect_equal(rowSums(dm), as.numeric(ref), tolerance = 1e-10)
})

test_that("a factor level present in only some runs yields exact zero blocks", {
  set.seed(202)
  # Levels A/B occur only in run 1, C/D only in run 2.
  des <- data.frame(
    onset = c(sort(runif(8, 0, 100)), sort(runif(8, 0, 100))),
    run = rep(1:2, each = 8),
    cond = factor(c(sample(c("A", "B"), 8, TRUE), sample(c("C", "D"), 8, TRUE)),
                  levels = c("A", "B", "C", "D"))
  )
  sf <- fmrihrf::sampling_frame(blocklens = c(120, 120), TR = 1)
  m <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sf)

  dm <- design_matrix(m)
  tp_block <- fmrihrf::blockids(sf)
  cn <- colnames(dm)
  dmm <- as.matrix(dm)

  # A/B columns must be exactly zero over run-2 rows; C/D exactly zero over run 1.
  ab <- grep("cond\\.(A|B)", cn)
  cd <- grep("cond\\.(C|D)", cn)
  expect_true(all(dmm[tp_block == 2, ab] == 0))
  expect_true(all(dmm[tp_block == 1, cd] == 0))
  # And the active blocks are not all-zero (the regressors are really there).
  expect_gt(sum(abs(dmm[tp_block == 1, ab])), 0)
  expect_gt(sum(abs(dmm[tp_block == 2, cd])), 0)
})

test_that("multi-basis sparse design stays block-diagonal by column position", {
  # spmg3 (nb = 3): cond A only in run 1, cond B only in run 2. Each convolved
  # column must be nonzero within exactly one run (the nb-wide scatter must not
  # leak a condition's basis columns into the other block). This asserts the
  # positional invariant the skip fast path must preserve; it does not rely on
  # column *names* (see note in the PR about a separate multi-basis naming issue).
  set.seed(303)
  des <- data.frame(
    onset = c(sort(runif(6, 0, 100)), sort(runif(6, 0, 100))),
    run = rep(1:2, each = 6),
    cond = factor(c(rep("A", 6), rep("B", 6)), levels = c("A", "B"))
  )
  sf <- fmrihrf::sampling_frame(blocklens = c(120, 120), TR = 1)
  m <- event_model(onset ~ hrf(cond, basis = "spmg3"),
                   data = des, block = ~run, sampling_frame = sf)
  dm <- as.matrix(design_matrix(m))
  tp_block <- fmrihrf::blockids(sf)

  expect_equal(ncol(dm), 6L)  # 2 conditions x 3 basis
  # Every column is nonzero in exactly one run (strict block-diagonal structure).
  for (j in seq_len(ncol(dm))) {
    nz_runs <- unique(tp_block[abs(dm[, j]) > 1e-12])
    expect_length(nz_runs, 1L)
  }
  # Exactly 3 columns live in run 1 (cond A's 3 bases) and 3 in run 2 (cond B's).
  run_of_col <- vapply(seq_len(ncol(dm)),
                       function(j) unique(tp_block[abs(dm[, j]) > 1e-12]), integer(1))
  expect_equal(sort(as.integer(table(run_of_col))), c(3L, 3L))
})

test_that("zero-amplitude block is exactly zero even with in-block cancellation", {
  # Direct convolve: factor x modulator; block 1 has +2/-2 (cancels in sum but
  # is individually nonzero), block 3 modulator is all zero.
  cond <- factor(c("A", "A", "B",  "A", "B", "B",  "A", "B"), levels = c("A", "B"))
  mod  <- c(2, -2, 5,   1, 1, 1,   0, 0)
  ons  <- c(5, 20, 35,  5, 20, 35, 5, 20)
  bids <- c(1, 1, 1,    2, 2, 2,   3, 3)
  sf   <- fmrihrf::sampling_frame(blocklens = c(60, 60, 60), TR = 1)
  term <- event_term(list(cond = cond, mod = mod), onsets = ons, blockids = bids)

  conv <- as.matrix(convolve(term, fmrihrf::HRF_SPMG1, sf))
  tp_block <- fmrihrf::blockids(sf)

  # Block 3 has all-zero modulator -> every column exactly zero over block-3 rows.
  expect_true(all(conv[tp_block == 3, ] == 0))
  # The +2/-2 column (A:mod) is genuinely nonzero in block 1 (cancellation in the
  # amplitude sum must NOT cause the column to be dropped/zeroed).
  a_col <- grep("A", colnames(convolve(term, fmrihrf::HRF_SPMG1, sf)))[1]
  expect_gt(sum(abs(conv[tp_block == 1, a_col])), 0)
})

test_that("NaN in a modulator falls back and still convolves without error", {
  cond <- factor(c("A", "A", "B",  "A", "B", "B"), levels = c("A", "B"))
  mod  <- c(1, NaN, 3,  1, 1, 1)     # NaN forces the fallback path
  ons  <- c(5, 20, 35, 5, 20, 35)
  bids <- c(1, 1, 1,   2, 2, 2)
  sf   <- fmrihrf::sampling_frame(blocklens = c(60, 60), TR = 1)
  term <- event_term(list(cond = cond, mod = mod), onsets = ons, blockids = bids)
  expect_no_error(conv <- convolve(term, fmrihrf::HRF_SPMG1, sf))
  expect_equal(nrow(convolve(term, fmrihrf::HRF_SPMG1, sf)), 120L)
})

test_that("empty block (a run with no events) yields a correct zero region", {
  cond <- factor(c("A", "B", "A", "B"), levels = c("A", "B"))
  ons  <- c(5, 20, 5, 20)
  bids <- c(1, 1, 2, 2)                 # no events in block 3
  sf   <- fmrihrf::sampling_frame(blocklens = c(60, 60, 60), TR = 1)
  term <- event_term(list(cond = cond), onsets = ons, blockids = bids)
  conv <- as.matrix(convolve(term, fmrihrf::HRF_SPMG1, sf))
  tp_block <- fmrihrf::blockids(sf)
  expect_equal(nrow(conv), 180L)
  expect_true(all(conv[tp_block == 3, ] == 0))
})
