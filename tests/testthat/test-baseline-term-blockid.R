# design_matrix(<baseline term>, blockid = b) must return the rows of blocks
# `b` and exactly the columns that are non-structurally-zero in those blocks,
# each column once, in the term's column order.

.blockid_fixture <- function() {
  sf <- fmrihrf::sampling_frame(c(20, 25, 30), TR = 2)
  set.seed(1)
  nuis <- lapply(fmrihrf::blocklens(sf), function(n) {
    m <- matrix(rnorm(n * 2), n, 2)
    colnames(m) <- c("tx", "ty")
    m
  })
  list(
    sf = sf,
    global = baseline_model(basis = "poly", degree = 2, sframe = sf,
                            intercept = "global", nuisance_list = nuis),
    runwise = baseline_model(basis = "poly", degree = 2, sframe = sf,
                             intercept = "runwise"),
    const_global = baseline_model(basis = "constant", sframe = sf,
                                  intercept = "global")
  )
}

.expected_block_cols <- function(full, rows) {
  m <- as.matrix(full)
  nz <- colSums(m[rows, , drop = FALSE] != 0) > 0
  which(nz)
}

.check_term_blockid <- function(term, blocks, rowind) {
  full <- design_matrix(term)
  for (b in blocks) {
    rows <- unlist(rowind[b])
    exp_cols <- .expected_block_cols(full, rows)
    sub <- design_matrix(term, blockid = b)
    info <- paste0("term=", term$varname, " blockid=", paste(b, collapse = ","))
    expect_equal(dim(sub), c(length(rows), length(exp_cols)), info = info)
    expect_false(anyDuplicated(names(sub)) > 0, info = info)
    expect_identical(names(sub), names(full)[exp_cols], info = info)
    expect_equal(unname(as.matrix(sub)),
                 unname(as.matrix(full)[rows, exp_cols, drop = FALSE]),
                 info = info)

    sub_all <- design_matrix(term, blockid = b, allrows = TRUE)
    expect_equal(dim(sub_all), c(nrow(full), length(exp_cols)), info = info)
    expect_identical(names(sub_all), names(full)[exp_cols], info = info)
  }
}

test_that("global block intercept returns a single column for any blockid", {
  fx <- .blockid_fixture()
  tm <- terms(fx$global)
  rowind <- tm$block$rowind
  for (b in list(1L, 2L, 3L, c(1L, 3L), 2:3, 1:3)) {
    sub <- design_matrix(tm$block, blockid = b)
    expect_equal(ncol(sub), 1L)
    expect_identical(names(sub), "constant_global")
    expect_equal(nrow(sub), length(unlist(rowind[b])))
  }
  .check_term_blockid(tm$block, list(1L, 2L, c(1L, 3L), 1:3), rowind)
})

test_that("drift, runwise and nuisance terms subset by block correctly", {
  fx <- .blockid_fixture()
  blocks <- list(1L, 3L, c(1L, 3L), 2:3, 1:3)
  for (term in c(terms(fx$global), terms(fx$runwise))) {
    .check_term_blockid(term, blocks, term$rowind)
  }
})

test_that("constant-basis global drift term keeps its column for every block", {
  fx <- .blockid_fixture()
  for (term in terms(fx$const_global)) {
    .check_term_blockid(term, list(1L, 2L, 3L, c(2L, 3L), 1:3), term$rowind)
  }
})

test_that("baseline_model design_matrix with blockid has unique columns", {
  fx <- .blockid_fixture()
  for (bm in list(fx$global, fx$runwise, fx$const_global)) {
    full <- design_matrix(bm)
    expect_identical(design_matrix(bm, blockid = 1:3), full)
    rows <- which(fmrihrf::blockids(fx$sf) %in% c(1, 3))
    exp_cols <- .expected_block_cols(full, rows)
    sub <- expect_silent(design_matrix(bm, blockid = c(1L, 3L)))
    expect_false(anyDuplicated(names(sub)) > 0)
    expect_identical(names(sub), names(full)[exp_cols])
    expect_equal(unname(as.matrix(sub)),
                 unname(as.matrix(full)[rows, exp_cols, drop = FALSE]))
  }
})

test_that("terms without block structure refuse blockid subsetting", {
  mt <- fmridesign:::matrix_term("m", matrix(1, 4, 2))
  expect_equal(dim(design_matrix(mt)), c(4L, 2L))
  expect_error(design_matrix(mt, blockid = 1), "no block structure")
})
