# Tests for event_model methods that have low coverage
# Covers: design_matrix.event_model, event_terms.event_model, term_matrices.event_model,
#   conditions.event_model, condition_map.event_model, split_by_block.event_model,
#   blocklens.event_model, blockids.event_model, contrast_weights.convolved_term,
#   Fcontrasts.convolved_term, contrasts.event_model, contrast_weights.event_model,
#   Fcontrasts.event_model, print.event_model

# ---------- shared setup --------------------------------------------------

des <- data.frame(
  onset = c(0, 10, 20, 30, 5, 15, 25, 35),
  run   = c(1, 1, 1, 1, 2, 2, 2, 2),
  cond  = factor(c("A", "B", "A", "B", "A", "B", "A", "B"))
)

sframe <- fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 1)

emod <- event_model(onset ~ hrf(cond), data = des, block = ~run,
                    sampling_frame = sframe)

# =========================================================================
# Section 1: design_matrix.event_model
# =========================================================================

test_that("design_matrix.event_model returns tibble with correct dims", {
  dm <- design_matrix(emod)

  expect_s3_class(dm, "tbl_df")
  # Total scans = 40 + 40 = 80

  expect_equal(nrow(dm), 80)
  # 2 conditions (A, B) with canonical HRF => 2 columns
  expect_equal(ncol(dm), 2)
})

test_that("design_matrix.event_model with blockid subsets rows", {
  dm1 <- design_matrix(emod, blockid = 1)
  dm2 <- design_matrix(emod, blockid = 2)

  expect_equal(nrow(dm1), 40)
  expect_equal(nrow(dm2), 40)
  # Same number of columns as full matrix
  expect_equal(ncol(dm1), ncol(design_matrix(emod)))
  expect_equal(ncol(dm2), ncol(design_matrix(emod)))
})

test_that("design_matrix.event_model with invalid blockid returns empty tibble", {
  expect_warning(dm_bad <- design_matrix(emod, blockid = 99))
  expect_equal(nrow(dm_bad), 0)
})

# =========================================================================
# Section 2: event_terms.event_model
# =========================================================================

test_that("event_terms returns list of terms", {
  et <- event_terms(emod)

  expect_type(et, "list")
  expect_true(length(et) >= 1)
  expect_s3_class(et[[1]], "event_term")
})

test_that("event_terms and terms return the same thing", {
  expect_identical(event_terms(emod), terms(emod))
})

# =========================================================================
# Section 3: term_matrices.event_model
# =========================================================================

test_that("term_matrices returns list of matrices matching term count", {
  tmats <- term_matrices(emod)

  expect_type(tmats, "list")
  expect_equal(length(tmats), length(terms(emod)))
  expect_equal(names(tmats), names(terms(emod)))

  # Each matrix should have rows matching total scans
  for (mat in tmats) {
    expect_equal(nrow(mat), 80)
  }

  # Total columns across all term matrices should match design matrix
  total_cols <- sum(sapply(tmats, ncol))
  expect_equal(total_cols, ncol(design_matrix(emod)))
})

# =========================================================================
# Section 4: conditions.event_model
# =========================================================================

test_that("conditions returns condition names", {
  conds <- conditions(emod)

  expect_type(conds, "character")
  expect_true(length(conds) >= 2)
  # Should contain references to A and B
  expect_true(any(grepl("A", conds)))
  expect_true(any(grepl("B", conds)))
})

# =========================================================================
# Section 5: condition_map.event_model
# =========================================================================

test_that("condition_map returns tibble with expected columns", {
  cmap <- condition_map(emod)

  expect_s3_class(cmap, "tbl_df")
  expect_true("term" %in% names(cmap))
  expect_true("display" %in% names(cmap))
  expect_true("canonical" %in% names(cmap))
  expect_true("column_name" %in% names(cmap))
  expect_true(nrow(cmap) >= 2)
})

# =========================================================================
# Section 6: split_by_block.event_model
# =========================================================================

test_that("split_by_block returns list per block", {
  blocks <- split_by_block(emod)

  expect_type(blocks, "list")
  expect_equal(length(blocks), 2)
  # Each element should be a design matrix subset
  expect_equal(nrow(blocks[[1]]), 40)
  expect_equal(nrow(blocks[[2]]), 40)
  expect_equal(ncol(blocks[[1]]), ncol(design_matrix(emod)))
})

# =========================================================================
# Section 7: blocklens.event_model and blockids.event_model
# =========================================================================

test_that("blocklens returns block lengths", {
  bl <- blocklens(emod)

  expect_equal(bl, c(40, 40))
})

test_that("blockids returns per-event block IDs", {
  bids <- blockids(emod)

  expect_true(is.numeric(bids) || is.integer(bids))
  expect_equal(length(bids), nrow(des))
  expect_true(all(bids %in% c(1, 2)))
})

# =========================================================================
# Section 8: term_names.event_model
# =========================================================================

test_that("term_names returns character vector", {
  tn <- term_names(emod)

  expect_type(tn, "character")
  expect_true(length(tn) >= 1)
})

# =========================================================================
# Section 9: print.event_model
# =========================================================================

test_that("print.event_model produces output without error", {
  # print.event_model uses cli, which writes to stderr/connection, not stdout
  # Just verify it runs without error and returns invisibly
  out <- capture.output(result <- print(emod), type = "message")
  expect_identical(result, emod)
})

test_that("print.event_model returns invisible self", {
  result <- invisible(NULL)
  expect_no_error(result <- print(emod))
  expect_identical(result, emod)
})

# =========================================================================
# Section 10: contrasts.event_model (model without contrasts)
# =========================================================================

test_that("contrasts on model with no explicit contrasts returns empty list", {
  con <- contrasts(emod)
  expect_type(con, "list")
  expect_equal(length(con), 0)
})

# =========================================================================
# Section 11: Contrast functionality with explicit contrasts
# =========================================================================

test_that("event_model with pair_contrast works", {
  con <- pair_contrast(~ cond == "A", ~ cond == "B", name = "AvB")
  cset <- contrast_set(AvB = con)
  emod_con <- event_model(onset ~ hrf(cond, contrasts = cset),
                          data = des, block = ~run,
                          sampling_frame = sframe)

  expect_s3_class(emod_con, "event_model")
})

test_that("contrasts.event_model returns non-empty list with contrasts", {
  con <- pair_contrast(~ cond == "A", ~ cond == "B", name = "AvB")
  cset <- contrast_set(AvB = con)
  emod_con <- event_model(onset ~ hrf(cond, contrasts = cset),
                          data = des, block = ~run,
                          sampling_frame = sframe)

  clist <- contrasts(emod_con)
  expect_type(clist, "list")
  expect_true(length(clist) > 0)
})

test_that("contrast_weights.event_model returns contrast weights", {
  con <- pair_contrast(~ cond == "A", ~ cond == "B", name = "AvB")
  cset <- contrast_set(AvB = con)
  emod_con <- event_model(onset ~ hrf(cond, contrasts = cset),
                          data = des, block = ~run,
                          sampling_frame = sframe)

  cw <- contrast_weights(emod_con)
  expect_type(cw, "list")
  expect_true(length(cw) > 0)

  # Each contrast weight entry should have an offset_weights matrix
  for (entry in cw) {
    expect_true(!is.null(entry$offset_weights))
    # offset_weights should have as many rows as design matrix columns
    expect_equal(nrow(entry$offset_weights), ncol(design_matrix(emod_con)))
  }
})

test_that("contrast_weights.event_model with column_contrast works", {
  ccon <- column_contrast(pattern_A = "cond.A", pattern_B = "cond.B", name = "col_AvB")
  cset <- contrast_set(col_AvB = ccon)
  emod_con <- event_model(onset ~ hrf(cond, contrasts = cset),
                          data = des, block = ~run,
                          sampling_frame = sframe)

  cw <- contrast_weights(emod_con)
  expect_type(cw, "list")
  expect_true(length(cw) > 0)
})

# =========================================================================
# Section 12: Fcontrasts.event_model
# =========================================================================

test_that("Fcontrasts.event_model returns a list", {
  fcon <- Fcontrasts(emod)
  expect_type(fcon, "list")
  # With 2 levels, should generate an F-contrast
  expect_true(length(fcon) > 0)
})

test_that("Fcontrasts.event_model results have correct structure", {
  fcon <- Fcontrasts(emod)
  if (length(fcon) > 0) {
    for (fc in fcon) {
      expect_true(is.matrix(fc))
      # Should have rows matching total design matrix columns
      expect_equal(nrow(fc), ncol(design_matrix(emod)))
    }
  }
})

# =========================================================================
# Section 13: Multi-term event model
# =========================================================================

test_that("event_model with multiple hrf terms works", {
  des2 <- data.frame(
    onset = c(0, 10, 20, 30, 5, 15, 25, 35),
    run   = c(1, 1, 1, 1, 2, 2, 2, 2),
    cond  = factor(c("A", "B", "A", "B", "A", "B", "A", "B")),
    mod   = rnorm(8)
  )

  emod2 <- event_model(onset ~ hrf(cond) + hrf(mod),
                       data = des2, block = ~run,
                       sampling_frame = sframe)

  expect_equal(length(terms(emod2)), 2)

  dm <- design_matrix(emod2)
  expect_equal(nrow(dm), 80)
  # 2 conditions from cond + 1 continuous from mod = 3 columns
  expect_equal(ncol(dm), 3)

  # term_matrices should have 2 entries
  tmats <- term_matrices(emod2)
  expect_equal(length(tmats), 2)

  # conditions should include all from both terms
  conds <- conditions(emod2)
  expect_true(length(conds) >= 3)
})

# =========================================================================
# Section 14: split_by_block with single block
# =========================================================================

test_that("split_by_block with single block returns list of length 1", {
  des1 <- data.frame(
    onset = c(0, 10, 20, 30),
    run   = 1,
    cond  = factor(c("A", "B", "A", "B"))
  )
  sframe1 <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
  emod1 <- event_model(onset ~ hrf(cond), data = des1, block = ~run,
                       sampling_frame = sframe1)

  blocks <- split_by_block(emod1)
  expect_equal(length(blocks), 1)
  expect_equal(nrow(blocks[[1]]), 40)
})

# =========================================================================
# Section 15: design_matrix attributes
# =========================================================================

test_that("design_matrix has col_indices attribute", {
  dm <- design_matrix(emod)
  ci <- attr(dm, "col_indices")

  expect_type(ci, "list")
  expect_equal(length(ci), length(terms(emod)))
  # Indices should cover all columns
  all_idx <- sort(unlist(ci, use.names = FALSE))
  expect_equal(all_idx, seq_len(ncol(dm)))
})

test_that("design_matrix has term_spans attribute", {
  dm <- design_matrix(emod)
  ts <- attr(dm, "term_spans")

  expect_true(!is.null(ts))
  expect_true(is.numeric(ts))
  expect_equal(length(ts), length(terms(emod)))
})

# =========================================================================
# Section 16: condition_map with expand_basis
# =========================================================================

test_that("condition_map with expand_basis=FALSE works", {
  cmap <- condition_map(emod, expand_basis = FALSE)
  expect_s3_class(cmap, "tbl_df")
  expect_true(nrow(cmap) >= 2)
})

# =========================================================================
# Section 17: Visualization methods (design_map, correlation_map)
# =========================================================================

test_that("design_map.event_model returns ggplot", {
  p <- design_map(emod)
  expect_s3_class(p, "ggplot")
})

test_that("correlation_map.event_model returns ggplot", {
  p <- correlation_map(emod)
  expect_s3_class(p, "ggplot")
})

test_that("plot.event_model returns ggplot", {
  p <- plot(emod)
  expect_s3_class(p, "ggplot")
})

# =========================================================================
# Section 18: Edge cases
# =========================================================================

test_that("event_model with formula works with different HRF basis", {
  emod_spmg3 <- event_model(onset ~ hrf(cond, basis = "spmg3"),
                             data = des, block = ~run,
                             sampling_frame = sframe)

  dm <- design_matrix(emod_spmg3)
  expect_equal(nrow(dm), 80)
  # spmg3 has 3 basis functions * 2 conditions = 6 columns
  expect_equal(ncol(dm), 6)

  tmats <- term_matrices(emod_spmg3)
  expect_equal(ncol(tmats[[1]]), 6)
})
