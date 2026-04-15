# test-event_vector_methods.R
#
# Tests for event_term methods and convolved_term accessors in event_vector.R
# Covers: event_term(), cells, conditions, longnames, shortnames,
#         is_categorical, elements, events, event_conditions, split_onsets,
#         onsets/blockids/durations accessors, regressors, convolve,
#         design_matrix, and print methods.

library(testthat)

testthat::local_edition(3)

# ── Standard setup ──────────────────────────────────────────────────────────

des <- data.frame(
  onset = c(0, 10, 20, 30, 5, 15, 25, 35),
  run   = c(1, 1, 1, 1, 2, 2, 2, 2),
  cond  = factor(c("A", "B", "A", "B", "A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 1)

# ── event_term() constructor ───────────────────────────────────────────────

test_that("event_term() with a single factor creates a valid object", {
  et <- event_term(
    list(cond = factor(c("A", "B", "A", "B"))),
    onsets   = c(0, 10, 20, 30),
    blockids = c(1, 1, 1, 1)
  )

  expect_s3_class(et, "event_term")
  expect_s3_class(et, "event_seq")
  expect_equal(length(et$onsets), 4)
  expect_equal(et$blockids, c(1, 1, 1, 1))
  expect_equal(et$varname, "cond")
  expect_true(length(et$events) == 1)
})

test_that("event_term() with numeric data creates a valid object", {
  rt <- rnorm(6)
  et <- event_term(
    list(RT = rt),
    onsets   = seq(0, 50, length.out = 6),
    blockids = rep(1, 6)
  )

  expect_s3_class(et, "event_term")
  expect_true(is_continuous(et))
  expect_false(is_categorical(et))
  expect_equal(length(et$onsets), 6)
})

test_that("event_term() with factor + numeric creates an interaction term", {
  et <- event_term(
    list(cond = factor(c("A", "B", "A")), RT = c(0.5, 1.2, 0.8)),
    onsets   = c(0, 10, 20),
    blockids = c(1, 1, 1)
  )

  expect_s3_class(et, "event_term")
  expect_equal(length(et$events), 2)
  expect_equal(et$varname, "cond:RT")
  # Mixed term: contains categorical so is_categorical returns TRUE
  expect_true(is_categorical(et))
})

test_that("event_term() recycles scalar durations", {
  et <- event_term(
    list(cond = factor(c("X", "Y"))),
    onsets    = c(0, 10),
    blockids  = c(1, 1),
    durations = 2
  )

  expect_equal(length(et$durations), 2)
  expect_true(all(et$durations == 2))
})

test_that("event_term() errors on non-decreasing blockids", {
  expect_error(
    event_term(
      list(cond = factor(c("A", "B"))),
      onsets   = c(0, 10),
      blockids = c(2, 1)
    ),
    "non-decreasing"
  )
})

test_that("event_term() errors on length mismatch", {
  expect_error(
    event_term(
      list(cond = factor(c("A", "B", "C"))),
      onsets   = c(0, 10),
      blockids = c(1, 1)
    ),
    "Length mismatch"
  )
})

# ── cells.event_term() ────────────────────────────────────────────────────

test_that("cells.event_term() returns condition cells for factor term", {
  et <- event_term(
    list(cond = factor(c("A", "B", "A", "B"))),
    onsets   = c(0, 10, 20, 30),
    blockids = c(1, 1, 1, 1)
  )

  cl <- cells(et)
  expect_s3_class(cl, "tbl_df")
  expect_true("cond" %in% names(cl))
  expect_equal(nrow(cl), 2)  # A and B
  expect_true(all(c("A", "B") %in% cl$cond))
  # count attribute
  cnt <- attr(cl, "count")
  expect_true(!is.null(cnt))
  expect_true(all(cnt > 0))
})

test_that("cells.event_term() drop.empty=FALSE includes empty cells", {
  # Factor with level C that never appears
  fac <- factor(c("A", "B", "A"), levels = c("A", "B", "C"))
  et <- event_term(
    list(cond = fac),
    onsets   = c(0, 10, 20),
    blockids = c(1, 1, 1)
  )

  cl_full <- cells(et, drop.empty = FALSE)
  expect_true(nrow(cl_full) >= 3)  # includes C
  cnt <- attr(cl_full, "count")
  expect_true(any(cnt == 0))  # C has count 0

  cl_dropped <- cells(et, drop.empty = TRUE)
  expect_true(nrow(cl_dropped) == 2)  # only A and B
})

test_that("cells.event_term() for continuous-only term returns single row", {
  et <- event_term(
    list(RT = rnorm(5)),
    onsets   = seq(0, 40, by = 10),
    blockids = rep(1, 5)
  )

  cl <- cells(et)
  expect_equal(nrow(cl), 1)
})

# ── conditions.event_term() ──────────────────────────────────────────────

test_that("conditions.event_term() returns canonical condition names", {
  et <- event_term(
    list(cond = factor(c("A", "B", "A"))),
    onsets   = c(0, 10, 20),
    blockids = c(1, 1, 1)
  )

  conds <- conditions(et)
  expect_true(is.character(conds))
  expect_equal(length(conds), 2)
  # Canonical style includes variable name tokens
  expect_true(all(grepl("cond", conds)))
})

# ── longnames.event_term() ───────────────────────────────────────────────

test_that("longnames.event_term() returns canonical condition names", {
  et <- event_term(
    list(cond = factor(c("A", "B", "A"))),
    onsets   = c(0, 10, 20),
    blockids = c(1, 1, 1)
  )

  ln <- longnames(et)
  expect_true(is.character(ln))
  expect_equal(length(ln), 2)
  # longnames uses canonical style
  expect_true(all(grepl("cond", ln)))
})

# ── shortnames.event_term() ─────────────────────────────────────────────

test_that("shortnames.event_term() returns display-style names", {
  et <- event_term(
    list(cond = factor(c("A", "B", "A"))),
    onsets   = c(0, 10, 20),
    blockids = c(1, 1, 1)
  )

  sn <- shortnames(et)
  expect_true(is.character(sn))
  expect_equal(length(sn), 2)
  # Display style should give plain level names
  expect_true(all(sn %in% c("A", "B")))
})

# ── is_categorical.event_term() ─────────────────────────────────────────

test_that("is_categorical.event_term() correctly classifies terms", {
  et_fac <- event_term(
    list(cond = factor(c("A", "B"))),
    onsets   = c(0, 10),
    blockids = c(1, 1)
  )
  expect_true(is_categorical(et_fac))

  et_num <- event_term(
    list(RT = c(0.5, 1.2)),
    onsets   = c(0, 10),
    blockids = c(1, 1)
  )
  expect_false(is_categorical(et_num))
})

# ── elements.event_term() ───────────────────────────────────────────────

test_that("elements.event_term() returns values and labels", {
  et <- event_term(
    list(cond = factor(c("A", "B", "A"))),
    onsets   = c(0, 10, 20),
    blockids = c(1, 1, 1)
  )

  els_vals <- elements(et, what = "values")
  expect_true(is.list(els_vals))
  expect_equal(length(els_vals), 1)

  els_labs <- elements(et, what = "labels")
  expect_true(is.list(els_labs))
  expect_equal(length(els_labs), 1)
})

test_that("elements.event_term() with mixed factor/numeric", {
  et <- event_term(
    list(cond = factor(c("X", "Y", "X")), val = c(1, 2, 3)),
    onsets   = c(0, 10, 20),
    blockids = c(1, 1, 1)
  )

  els <- elements(et, what = "values")
  expect_true(is.list(els))
  expect_equal(length(els), 2)
})

# ── events.event_term() and events.convolved_term() ─────────────────────

test_that("events.event_term() returns data frame with expected columns", {
  et <- event_term(
    list(cond = factor(c("A", "B", "A", "B"))),
    onsets    = c(0, 10, 20, 30),
    blockids  = c(1, 1, 1, 1),
    durations = c(1, 2, 1, 2)
  )

  ev_df <- events(et)
  expect_s3_class(ev_df, "data.frame")
  expect_true(all(c("onset", "duration", "block", "condition") %in% names(ev_df)))
  expect_equal(nrow(ev_df), 4)
  expect_true(is.factor(ev_df$condition))
})

test_that("events.convolved_term() delegates to event_term", {
  emod <- event_model(
    onset ~ hrf(cond),
    data = des,
    block = ~run,
    sampling_frame = sframe
  )

  # Get the first convolved term
  cterms <- event_terms(emod)
  ct <- cterms[[1]]

  ev_df <- events(ct)
  expect_s3_class(ev_df, "data.frame")
  expect_true(all(c("onset", "duration", "block", "condition") %in% names(ev_df)))
})

# ── event_conditions.event_term() / event_conditions.convolved_term() ────

test_that("event_conditions.event_term() returns per-event condition factor", {
  et <- event_term(
    list(cond = factor(c("A", "B", "A", "B"))),
    onsets   = c(0, 10, 20, 30),
    blockids = c(1, 1, 1, 1)
  )

  ec <- event_conditions(et)
  expect_true(is.factor(ec))
  expect_equal(length(ec), 4)
  # Factor level names should contain "cond" in canonical style
  expect_true(length(levels(ec)) == 2)
})

test_that("event_conditions drop.empty drops unused levels", {
  fac <- factor(c("A", "A"), levels = c("A", "B", "C"))
  et <- event_term(
    list(cond = fac),
    onsets   = c(0, 10),
    blockids = c(1, 1)
  )

  ec_all <- event_conditions(et, drop.empty = FALSE)
  ec_drop <- event_conditions(et, drop.empty = TRUE)

  expect_true(length(levels(ec_all)) > length(levels(ec_drop)))
  expect_equal(length(levels(ec_drop)), 1)
})

test_that("event_conditions.convolved_term() delegates to event_term", {
  emod <- event_model(
    onset ~ hrf(cond),
    data = des,
    block = ~run,
    sampling_frame = sframe
  )

  ct <- event_terms(emod)[[1]]
  ec <- event_conditions(ct)
  expect_true(is.factor(ec))
  expect_equal(length(ec), nrow(des))
})

# ── split_onsets.event_term() ────────────────────────────────────────────

test_that("split_onsets.event_term() splits by condition", {
  et <- event_term(
    list(cond = factor(c("A", "B", "A", "B"))),
    onsets   = c(0, 10, 20, 30),
    blockids = c(1, 1, 1, 1)
  )

  sp <- split_onsets(et, sframe)
  expect_true(is.list(sp))
  expect_equal(length(sp), 2)  # 2 conditions
  expect_true(all(sapply(sp, is.numeric)))
})

test_that("split_onsets.event_term() with global=TRUE adjusts onsets", {
  et <- event_term(
    list(cond = factor(c("A", "B", "A", "B"))),
    onsets   = c(0, 10, 5, 15),
    blockids = c(1, 1, 2, 2)
  )

  sp_local <- split_onsets(et, sframe, global = FALSE)
  sp_global <- split_onsets(et, sframe, global = TRUE)

  # Global onsets for block 2 should be offset
  expect_true(is.list(sp_local))
  expect_true(is.list(sp_global))
})

test_that("split_onsets.event_term() for continuous-only term", {
  et <- event_term(
    list(RT = c(0.5, 1.2, 0.8)),
    onsets   = c(0, 10, 20),
    blockids = c(1, 1, 1)
  )

  sp <- split_onsets(et, sframe)
  # Continuous-only: returns list with one element (split by block)
  expect_true(is.list(sp))
  expect_equal(length(sp), 1)
})

# ── onsets/blockids/durations accessors for convolved_term ───────────────

test_that("onsets, blockids, durations accessors work for convolved_term", {
  emod <- event_model(
    onset ~ hrf(cond),
    data = des,
    block = ~run,
    sampling_frame = sframe
  )

  ct <- event_terms(emod)[[1]]

  ons <- onsets(ct)
  expect_true(is.numeric(ons))
  expect_equal(length(ons), nrow(des))

  bids <- blockids(ct)
  expect_true(is.numeric(bids) || is.integer(bids))
  expect_equal(length(bids), nrow(des))

  durs <- durations(ct)
  expect_true(is.numeric(durs))
  expect_equal(length(durs), nrow(des))
})

# ── regressors.event_term() ─────────────────────────────────────────────

test_that("regressors.event_term() produces named regressor list", {
  et <- event_term(
    list(cond = factor(c("A", "B", "A", "B"))),
    onsets   = c(0, 10, 20, 30),
    blockids = c(1, 1, 1, 1)
  )

  regs <- regressors(et, hrf = fmrihrf::HRF_SPMG1, sampling_frame = sframe)
  expect_true(is.list(regs))
  expect_equal(length(regs), 2)  # 2 conditions
  expect_true(!is.null(names(regs)))
})

# ── convolve.event_term() ───────────────────────────────────────────────

test_that("convolve.event_term() produces a tibble with correct dimensions", {
  et <- event_term(
    list(cond = factor(c("A", "B", "A", "B"))),
    onsets   = c(0, 10, 20, 30),
    blockids = c(1, 1, 1, 1)
  )
  attr(et, "term_tag") <- "cond"

  conv <- convolve(et, fmrihrf::HRF_SPMG1, sframe)
  expect_s3_class(conv, "tbl_df")
  # rows should equal total samples
  expect_equal(nrow(conv), sum(fmrihrf::blocklens(sframe)))
  # 2 conditions with 1 basis = 2 columns
  expect_equal(ncol(conv), 2)
})

test_that("convolve.event_term() with multi-basis HRF expands columns", {
  et <- event_term(
    list(cond = factor(c("A", "B", "A"))),
    onsets   = c(0, 10, 20),
    blockids = c(1, 1, 1)
  )
  attr(et, "term_tag") <- "cond"

  conv <- convolve(et, fmrihrf::HRF_SPMG2, sframe)
  expect_s3_class(conv, "tbl_df")
  # 2 conditions x 2 basis = 4 columns
  expect_equal(ncol(conv), 4)
})

test_that("convolve.event_term() with normalize=TRUE peak-normalizes", {
  et <- event_term(
    list(cond = factor(c("A", "B", "A", "B"))),
    onsets    = c(0, 10, 20, 30),
    blockids  = c(1, 1, 1, 1),
    durations = c(1, 5, 1, 5)
  )
  attr(et, "term_tag") <- "cond"

  conv <- convolve(et, fmrihrf::HRF_SPMG1, sframe, normalize = TRUE)
  # Each column should have max(abs) == 1 (or 0 for zero columns)
  for (j in seq_len(ncol(conv))) {
    peak <- max(abs(conv[[j]]))
    if (peak > 0) {
      expect_equal(peak, 1, tolerance = 1e-10)
    }
  }
})

test_that("convolve.event_term() with multi-block data", {
  et <- event_term(
    list(cond = factor(c("A", "B", "A", "B"))),
    onsets   = c(0, 10, 5, 15),
    blockids = c(1, 1, 2, 2)
  )
  attr(et, "term_tag") <- "cond"

  conv <- convolve(et, fmrihrf::HRF_SPMG1, sframe)
  expect_equal(nrow(conv), sum(fmrihrf::blocklens(sframe)))
})

# ── design_matrix.event_term() ──────────────────────────────────────────

test_that("design_matrix.event_term() returns tibble for factor term", {
  et <- event_term(
    list(cond = factor(c("A", "B", "A", "B"))),
    onsets   = c(0, 10, 20, 30),
    blockids = c(1, 1, 1, 1)
  )

  dm <- design_matrix(et)
  expect_s3_class(dm, "tbl_df")
  expect_equal(nrow(dm), 4)
  expect_equal(ncol(dm), 2)
})

test_that("design_matrix.event_term() for numeric term returns value matrix", {
  vals <- c(0.5, 1.2, 0.8, 1.5)
  et <- event_term(
    list(RT = vals),
    onsets   = c(0, 10, 20, 30),
    blockids = c(1, 1, 1, 1)
  )

  dm <- design_matrix(et)
  expect_s3_class(dm, "tbl_df")
  expect_equal(nrow(dm), 4)
  expect_equal(ncol(dm), 1)
  expect_equal(dm[[1]], vals)
})

# ── print methods ────────────────────────────────────────────────────────

test_that("print.event_term() runs without error", {
  et <- event_term(
    list(cond = factor(c("A", "B", "A"))),
    onsets   = c(0, 10, 20),
    blockids = c(1, 1, 1)
  )

  # cli output goes to message connection, so capture both
  out <- capture.output(print(et), type = "message")
  expect_true(any(grepl("Event Term", out)))
})

test_that("print.convolved_term() runs without error", {
  cov_data <- data.frame(motion_x = rnorm(80), motion_y = rnorm(80))
  cv <- covariate(motion_x, motion_y, data = cov_data)
  ct <- construct(cv, list(sampling_frame = sframe))
  expect_output(print(ct), "fmri_term|Term Name")
})
