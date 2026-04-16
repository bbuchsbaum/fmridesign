# Tests for baseline_model methods that have low coverage
# Covers: baseline_model() constructor variants, baseline_terms(), term_matrices(),
#   term_names(), cells(), print(), design_matrix(), conditions(), nuisance(),
#   block(), construct.baselinespec, construct.nuisancespec, construct.blockspec,
#   correlation_map.baseline_model, design_map.baseline_model

# ---------- shared setup --------------------------------------------------

make_sframe_1 <- function() fmrihrf::sampling_frame(blocklens = 50, TR = 1)
make_sframe_2 <- function() fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 1)

# =========================================================================
# Section 1: baseline_model() constructor with different drift bases
# =========================================================================

test_that("baseline_model with basis='poly' creates valid object", {
  sframe <- make_sframe_2()
  bmod <- baseline_model(basis = "poly", degree = 3, sframe = sframe)

  expect_s3_class(bmod, "baseline_model")
  expect_true("drift" %in% names(terms(bmod)))
  expect_true("block" %in% names(terms(bmod)))

  dm <- design_matrix(bmod)
  # poly degree 3 => 3 cols per block * 2 blocks = 6, + runwise intercept = 2 cols per block = 2

  expect_equal(ncol(dm), 3 * 2 + 2)
  expect_equal(nrow(dm), 80)
})

test_that("baseline_model with basis='bs' creates valid object", {
  sframe <- make_sframe_2()
  bmod <- baseline_model(basis = "bs", degree = 5, sframe = sframe)

  expect_s3_class(bmod, "baseline_model")
  dm <- design_matrix(bmod)
  # bs degree 5 => 5 cols per block * 2 + runwise intercept 2 = 12
  expect_equal(ncol(dm), 5 * 2 + 2)
  expect_equal(nrow(dm), 80)
})

test_that("baseline_model with basis='ns' creates valid object", {
  sframe <- make_sframe_2()
  bmod <- baseline_model(basis = "ns", degree = 3, sframe = sframe)

  expect_s3_class(bmod, "baseline_model")
  dm <- design_matrix(bmod)
  # ns df=3 => 3 cols per block * 2 + runwise intercept 2 = 8
  expect_equal(ncol(dm), 3 * 2 + 2)
  expect_equal(nrow(dm), 80)
})

test_that("baseline_model with basis='bs' rejects degree < 3", {
  sframe <- make_sframe_1()
  expect_error(baseline_model(basis = "bs", degree = 2, sframe = sframe))
})

test_that("baseline_model with basis='ns' rejects degree < 3", {
  sframe <- make_sframe_1()
  expect_error(baseline_model(basis = "ns", degree = 2, sframe = sframe))
})

test_that("baseline_model with intercept='none' omits block term", {
  sframe <- make_sframe_2()
  bmod <- baseline_model(basis = "poly", degree = 2, sframe = sframe, intercept = "none")

  expect_false("block" %in% names(terms(bmod)))
  expect_true("drift" %in% names(terms(bmod)))
  # poly degree 2 => 2 cols per block * 2 blocks = 4, no intercept

  expect_equal(ncol(design_matrix(bmod)), 4)
})

test_that("baseline_model with intercept='global' creates global intercept", {
  sframe <- make_sframe_2()
  bmod <- baseline_model(basis = "poly", degree = 2, sframe = sframe, intercept = "global")

  expect_true("block" %in% names(terms(bmod)))
  block_dm <- design_matrix(terms(bmod)$block)
  # Global intercept should be a single column of ones

  expect_equal(ncol(block_dm), 1)
  expect_true(all(as.matrix(block_dm) == 1))
})

# =========================================================================
# Section 2: baseline_terms() method
# =========================================================================

test_that("baseline_terms returns the terms list", {
  sframe <- make_sframe_2()
  bmod <- baseline_model(basis = "poly", degree = 2, sframe = sframe)

  bt <- baseline_terms(bmod)
  expect_type(bt, "list")
  expect_true(all(names(bt) %in% c("drift", "block", "nuisance")))
  # Should be same as terms()
  expect_identical(bt, terms(bmod))
})

# =========================================================================
# Section 3: term_matrices() method
# =========================================================================

test_that("term_matrices returns a list of matrices for baseline_model", {
  sframe <- make_sframe_2()
  bmod <- baseline_model(basis = "poly", degree = 2, sframe = sframe)

  tmats <- term_matrices(bmod)
  expect_type(tmats, "list")
  expect_true(length(tmats) >= 1)
  # Each element should be a matrix-like object (tibble from design_matrix)
  for (nm in names(tmats)) {
    expect_true(ncol(tmats[[nm]]) > 0)
    expect_equal(nrow(tmats[[nm]]), 80)
  }
})

# =========================================================================
# Section 4: term_names() method
# =========================================================================

test_that("term_names returns character vector of term varnames", {
  sframe <- make_sframe_2()
  bmod <- baseline_model(basis = "bs", degree = 3, sframe = sframe)

  tn <- term_names(bmod)
  expect_type(tn, "character")
  expect_true(length(tn) >= 1)
  # Should include the drift term name
  expect_true(any(grepl("baseline", tn)))
})

# =========================================================================
# Section 5: cells() method
# =========================================================================

test_that("cells returns a tibble with expected columns", {
  sframe <- make_sframe_2()
  bmod <- baseline_model(basis = "poly", degree = 2, sframe = sframe)

  cl <- cells(bmod)
  expect_s3_class(cl, "tbl_df")
  expect_true("term" %in% names(cl))
  expect_true("level" %in% names(cl))
  expect_true("basis" %in% names(cl))
  expect_true("index" %in% names(cl))
  expect_true(nrow(cl) > 0)
})

# =========================================================================
# Section 6: print() method
# =========================================================================

test_that("print.baseline_model produces output without error", {
  sframe <- make_sframe_2()
  bmod <- baseline_model(basis = "poly", degree = 2, sframe = sframe)

  expect_output(print(bmod), "Baseline Model")
})

test_that("print.baseline_model works with nuisance terms", {
  sframe <- make_sframe_2()
  nlist <- list(
    matrix(rnorm(40 * 2), 40, 2),
    matrix(rnorm(40 * 2), 40, 2)
  )
  bmod <- baseline_model(basis = "poly", degree = 2, sframe = sframe, nuisance_list = nlist)

  expect_output(print(bmod), "Baseline Model")
  expect_output(print(bmod), "Nuisance")
})

# =========================================================================
# Section 7: design_matrix() for baseline_model
# =========================================================================

test_that("design_matrix.baseline_model returns tibble with correct dimensions", {
  sframe <- make_sframe_2()
  bmod <- baseline_model(basis = "poly", degree = 2, sframe = sframe)

  dm <- design_matrix(bmod)
  expect_s3_class(dm, "tbl_df")
  expect_equal(nrow(dm), 80)
  expect_true(ncol(dm) > 0)
})

test_that("design_matrix.baseline_model with blockid subsets correctly", {
  sframe <- make_sframe_2()
  bmod <- baseline_model(basis = "constant", sframe = sframe)

  dm_b1 <- design_matrix(terms(bmod)$drift, blockid = 1)
  dm_b2 <- design_matrix(terms(bmod)$drift, blockid = 2)

  expect_equal(nrow(dm_b1), 40)
  expect_equal(nrow(dm_b2), 40)
})

# =========================================================================
# Section 8: conditions() for baseline_term
# =========================================================================

test_that("conditions.baseline_term returns column names", {
  sframe <- make_sframe_1()
  bmod <- baseline_model(basis = "poly", degree = 2, sframe = sframe)

  drift_term <- terms(bmod)$drift
  conds <- conditions(drift_term)
  expect_type(conds, "character")
  expect_true(length(conds) > 0)
  # Should match the column names of the drift design matrix
  expect_equal(conds, colnames(design_matrix(drift_term)))
})

# =========================================================================
# Section 9: nuisance() function
# =========================================================================

test_that("nuisance() creates a nuisancespec object", {
  mat <- matrix(rnorm(10), nrow = 5)
  ns <- nuisance(mat)

  expect_s3_class(ns, "nuisancespec")
  expect_true(!is.null(ns$name))
})

# =========================================================================
# Section 10: block() function
# =========================================================================

test_that("block() creates a blockspec object", {
  bs <- block(run)

  expect_s3_class(bs, "blockspec")
  expect_true(!is.null(bs$name))
})

# =========================================================================
# Section 11: construct.baselinespec
# =========================================================================

test_that("construct.baselinespec with poly basis produces correct matrix", {
  sframe <- make_sframe_1()
  spec <- baseline(degree = 2, basis = "poly")
  result <- construct(spec, sframe)

  expect_s3_class(result, "baseline_term")
  dm <- design_matrix(result)
  # Poly degree 2 => 2 columns for 1 block
  expect_equal(ncol(dm), 2)
  expect_equal(nrow(dm), 50)
})

test_that("construct.baselinespec with constant and global intercept returns single column", {
  sframe <- make_sframe_2()
  spec <- baseline(degree = 1, basis = "constant", intercept = "global")
  result <- construct(spec, sframe)

  expect_s3_class(result, "baseline_term")
  dm <- design_matrix(result)
  expect_equal(ncol(dm), 1)
  expect_equal(nrow(dm), 80)
  expect_true(all(as.matrix(dm) == 1))
})

test_that("construct.baselinespec with bs basis produces expected dimensions", {
  sframe <- make_sframe_1()
  spec <- baseline(degree = 4, basis = "bs")
  result <- construct(spec, sframe)

  dm <- design_matrix(result)
  expect_equal(ncol(dm), 4)
  expect_equal(nrow(dm), 50)
})

test_that("construct.baselinespec with ns basis produces expected dimensions", {
  sframe <- make_sframe_1()
  spec <- baseline(degree = 3, basis = "ns")
  result <- construct(spec, sframe)

  dm <- design_matrix(result)
  expect_equal(ncol(dm), 3)
  expect_equal(nrow(dm), 50)
})

# =========================================================================
# Section 12: construct.blockspec
# =========================================================================

test_that("construct.blockspec creates a block term", {
  sframe <- make_sframe_2()
  bspec <- block(run)

  # construct.blockspec needs model_spec with sampling_frame
  result <- construct(bspec, list(sampling_frame = sframe))

  expect_s3_class(result, "baseline_term")
  dm <- design_matrix(result)
  # construct.blockspec calls construct_block_term without intercept arg,

  # which defaults to "global" => single column of 1s
  expect_equal(ncol(dm), 1)
  expect_equal(nrow(dm), 80)
})

# =========================================================================
# Section 13: correlation_map.baseline_model returns ggplot
# =========================================================================

test_that("correlation_map.baseline_model returns a ggplot object", {
  sframe <- make_sframe_2()
  bmod <- baseline_model(basis = "poly", degree = 2, sframe = sframe)

  p <- correlation_map(bmod)
  expect_s3_class(p, "ggplot")
})

test_that("correlation_map.baseline_model with half_matrix works", {
  sframe <- make_sframe_2()
  bmod <- baseline_model(basis = "poly", degree = 2, sframe = sframe)

  p <- correlation_map(bmod, half_matrix = TRUE)
  expect_s3_class(p, "ggplot")
})

# =========================================================================
# Section 14: design_map.baseline_model returns ggplot
# =========================================================================

test_that("design_map.baseline_model returns a ggplot object", {
  sframe <- make_sframe_2()
  bmod <- baseline_model(basis = "poly", degree = 2, sframe = sframe)

  p <- design_map(bmod)
  expect_s3_class(p, "ggplot")
})

test_that("design_map.baseline_model with fill_midpoint works", {
  sframe <- make_sframe_2()
  bmod <- baseline_model(basis = "poly", degree = 2, sframe = sframe)

  p <- design_map(bmod, fill_midpoint = 0)
  expect_s3_class(p, "ggplot")
})

test_that("design_map.baseline_model with no block separators works", {
  sframe <- make_sframe_1()
  bmod <- baseline_model(basis = "poly", degree = 2, sframe = sframe)

  p <- design_map(bmod, block_separators = FALSE)
  expect_s3_class(p, "ggplot")
})

# =========================================================================
# Section 15: baseline() helper function
# =========================================================================

test_that("baseline() creates baselinespec with correct fields", {
  spec <- baseline(degree = 3, basis = "poly")

  expect_s3_class(spec, "baselinespec")
  expect_s3_class(spec, "nuisancespec")
  expect_equal(spec$degree, 3)
  expect_equal(spec$basis, "poly")
  expect_true(is.function(spec$fun))
  expect_equal(spec$intercept, "runwise")
  expect_true(grepl("poly", spec$name))
})

test_that("baseline() with constant basis sets degree to 1", {
  spec <- baseline(degree = 5, basis = "constant")

  expect_equal(spec$degree, 1)
})

test_that("baseline() respects custom name", {
  spec <- baseline(degree = 2, basis = "poly", name = "my_drift")

  expect_equal(spec$name, "my_drift")
})

# =========================================================================
# Section 16: design_matrix.baseline_term with blockid and allrows
# =========================================================================

test_that("design_matrix.baseline_term with blockid returns subset of rows", {
  sframe <- make_sframe_2()
  spec <- baseline(degree = 2, basis = "poly")
  term <- construct(spec, sframe)

  dm_block1 <- design_matrix(term, blockid = 1)
  expect_equal(nrow(dm_block1), 40)

  dm_block2 <- design_matrix(term, blockid = 2)
  expect_equal(nrow(dm_block2), 40)
})

test_that("design_matrix.baseline_term with blockid and allrows=TRUE returns all rows", {
  sframe <- make_sframe_2()
  spec <- baseline(degree = 2, basis = "poly")
  term <- construct(spec, sframe)

  dm_all <- design_matrix(term, blockid = 1, allrows = TRUE)
  expect_equal(nrow(dm_all), 80)
  # But only the columns for block 1
  expect_equal(ncol(dm_all), 2)
})

# =========================================================================
# Section 17: make_nuisance_term (internal)
# =========================================================================

test_that("nuisance_list with mismatched block count errors", {
  sframe <- make_sframe_2()
  # Only 1 nuisance matrix but 2 blocks
  nlist <- list(matrix(rnorm(40 * 2), 40, 2))
  expect_error(
    baseline_model(basis = "poly", degree = 2, sframe = sframe, nuisance_list = nlist)
  )
})

test_that("nuisance_list with mismatched row count errors", {
  sframe <- make_sframe_2()
  # Both matrices have wrong number of rows
  nlist <- list(
    matrix(rnorm(30 * 2), 30, 2),
    matrix(rnorm(40 * 2), 40, 2)
  )
  expect_error(
    baseline_model(basis = "poly", degree = 2, sframe = sframe, nuisance_list = nlist)
  )
})
