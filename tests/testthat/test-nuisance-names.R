# Nuisance regressor column names (issue #28) and design_colmap() metadata
# for baseline models.

.nuis_cols <- function(bm) {
  cn <- colnames(design_matrix(bm))
  cn[startsWith(cn, "nuis_")]
}

test_that("baseline_model keeps user nuisance column names (issue #28)", {
  set.seed(1)
  sf <- fmrihrf::sampling_frame(104, TR = 2.2)
  N <- cbind(trans_x = rnorm(104), rot_z = rnorm(104), WM = rnorm(104),
             spike_7 = as.numeric(seq_len(104) == 8))
  bm <- baseline_model(basis = "constant", sframe = sf, nuisance_list = list(N))

  expect_equal(
    colnames(design_matrix(bm)),
    c("base_constant1_block_1", "nuis_trans_x_block_1", "nuis_rot_z_block_1",
      "nuis_WM_block_1", "nuis_spike_7_block_1")
  )
  expect_equal(bm$terms$nuisance$source_colnames, colnames(N))
})

test_that("nuisance names are unique across runs and carry the run", {
  set.seed(2)
  sf <- fmrihrf::sampling_frame(c(50, 50), 2)
  nl <- list(data.frame(tx = rnorm(50), ty = rnorm(50)),
             data.frame(tx = rnorm(50), ty = rnorm(50)))
  bm <- baseline_model(basis = "poly", degree = 1, sframe = sf,
                       nuisance_list = nl, nuisance_check = "none")
  nm <- .nuis_cols(bm)
  expect_equal(nm, c("nuis_tx_block_1", "nuis_ty_block_1",
                     "nuis_tx_block_2", "nuis_ty_block_2"))
  expect_false(anyDuplicated(colnames(design_matrix(bm))) > 0)
  expect_equal(bm$terms$nuisance$source_colnames, c("tx", "ty", "tx", "ty"))
})

test_that("unnamed nuisance matrices fall back to column indices", {
  set.seed(3)
  sf <- fmrihrf::sampling_frame(c(20, 20), 2)
  nl <- list(matrix(rnorm(40), 20, 2), matrix(rnorm(60), 20, 3))
  bm <- baseline_model(basis = "constant", sframe = sf, nuisance_list = nl,
                       nuisance_check = "none")
  expect_equal(.nuis_cols(bm),
               c("nuis_1_block_1", "nuis_2_block_1",
                 "nuis_1_block_2", "nuis_2_block_2", "nuis_3_block_2"))

  # Partially named: only the unnamed column falls back to its index
  m <- matrix(rnorm(40), 20, 2, dimnames = list(NULL, c("fd", "")))
  bm2 <- baseline_model(basis = "constant", sframe = fmrihrf::sampling_frame(20, 2),
                        nuisance_list = list(m), nuisance_check = "none")
  expect_equal(.nuis_cols(bm2), c("nuis_fd_block_1", "nuis_2_block_1"))
})

test_that("duplicate and non-syntactic nuisance names are sanitised and unique", {
  set.seed(4)
  sf <- fmrihrf::sampling_frame(30, 2)
  m <- matrix(rnorm(30 * 6), 30, 6)
  colnames(m) <- c("tx", "tx", "trans x", "trans.x", "1st", "a-b")
  bm <- baseline_model(basis = "constant", sframe = sf, nuisance_list = list(m),
                       nuisance_check = "none")
  nm <- .nuis_cols(bm)
  expect_length(nm, 6)
  expect_false(anyDuplicated(nm) > 0)
  expect_true(all(make.names(nm) == nm))
  expect_true(all(grepl("^nuis_.+_block_1$", nm)))
  expect_equal(nm[1], "nuis_tx_block_1")
  expect_equal(nm[3], "nuis_trans_x_block_1")
  expect_equal(nm[5], "nuis_1st_block_1")
  expect_equal(nm[6], "nuis_a_b_block_1")
  # the originals are preserved verbatim for display
  expect_equal(bm$terms$nuisance$source_colnames[3:6],
               c("trans x", "trans.x", "1st", "a-b"))
})

test_that("nuisance_check = 'drop' keeps the surviving user names", {
  sf <- fmrihrf::sampling_frame(c(6, 6), 1)
  nl <- list(
    data.frame(dvars = seq_len(6), std_dvars = 10 * seq_len(6), zero_col = 0),
    data.frame(motion_x = c(-2, -1, 0, 1, 2, 3), motion_y = c(1, -1, 1, -1, 1, -1))
  )
  expect_warning(
    bm <- baseline_model(basis = "constant", sframe = sf, nuisance_list = nl,
                         nuisance_check = "drop"),
    "zero_col"
  )
  expect_equal(.nuis_cols(bm),
               c("nuis_dvars_block_1", "nuis_motion_x_block_2", "nuis_motion_y_block_2"))

  # Dropping from an unnamed matrix keeps the original column index
  m <- cbind(rnorm(6), 0, rnorm(6))
  expect_warning(
    bm2 <- baseline_model(basis = "constant", sframe = fmrihrf::sampling_frame(6, 1),
                          nuisance_list = list(m), nuisance_check = "drop")
  )
  expect_equal(.nuis_cols(bm2), c("nuis_1_block_1", "nuis_3_block_1"))
})

test_that("clean_nuisance output round-trips through baseline_model", {
  sf <- fmrihrf::sampling_frame(6, 1)
  m <- cbind(rnorm(6), 0, rnorm(6))
  cleaned <- clean_nuisance(list(m), sf, basis = "constant")
  bm <- baseline_model(basis = "constant", sframe = sf,
                       nuisance_list = cleaned$nuisance_list, nuisance_check = "none")
  expect_equal(.nuis_cols(bm), c("nuis_1_block_1", "nuis_3_block_1"))
})

test_that("design_colmap reports role and run for nuisance, drift and intercept columns", {
  set.seed(5)
  sf <- fmrihrf::sampling_frame(c(40, 40, 40), 2)
  nl <- replicate(3, as.data.frame(matrix(rnorm(40 * 6), 40, 6,
    dimnames = list(NULL, c("tx", "ty", "tz", "rx", "ry", "rz")))),
    simplify = FALSE)
  bm <- baseline_model(basis = "poly", degree = 2, sframe = sf,
                       nuisance_list = nl, nuisance_check = "none")
  cm <- design_colmap(bm)
  expect_equal(nrow(cm), ncol(design_matrix(bm)))
  expect_equal(cm$name, colnames(design_matrix(bm)))

  drift <- cm[cm$term_tag == "drift", ]
  expect_true(all(drift$role == "drift"))
  expect_equal(drift$run, rep(1:3, each = 2))
  expect_equal(drift$basis_ix, rep(1:2, 3))

  icpt <- cm[cm$term_tag == "block", ]
  expect_true(all(icpt$role == "intercept"))
  expect_equal(icpt$run, 1:3)

  nuis <- cm[cm$term_tag == "nuisance", ]
  expect_equal(nrow(nuis), 18L)
  expect_true(all(nuis$role == "nuisance"))
  expect_true(all(nuis$basis_name == "nuisance"))
  expect_equal(nuis$run, rep(1:3, each = 6))
  expect_equal(nuis$basis_ix, rep(1:6, 3))
  expect_equal(nuis$basis_label, rep(c("tx", "ty", "tz", "rx", "ry", "rz"), 3))
  expect_true(all(nuis$is_block_diagonal))
})

test_that("design_colmap nuisance runs follow blocks even with ragged columns", {
  set.seed(6)
  sf <- fmrihrf::sampling_frame(c(20, 20), 2)
  nl <- list(matrix(rnorm(60), 20, 3), matrix(rnorm(20), 20, 1))
  bm <- baseline_model(basis = "constant", sframe = sf, nuisance_list = nl,
                       nuisance_check = "none")
  cm <- design_colmap(bm)
  nuis <- cm[cm$role == "nuisance", ]
  expect_equal(nuis$run, c(1L, 1L, 1L, 2L))
  expect_equal(nuis$basis_ix, c(1L, 2L, 3L, 1L))
  expect_true(all(cm$role[cm$term_tag == "drift"] == "drift"))
})

test_that("nuisance names are stable for more than nine runs", {
  sf <- fmrihrf::sampling_frame(rep(5, 10), 1)
  set.seed(7)
  nl <- lapply(1:10, function(i) data.frame(fd = rnorm(5)))
  bm <- baseline_model(basis = "constant", sframe = sf, nuisance_list = nl,
                       nuisance_check = "none")
  expect_equal(.nuis_cols(bm), paste0("nuis_fd_block_", 1:10))
  cm <- design_colmap(bm)
  expect_equal(cm$run[cm$role == "nuisance"], 1:10)
})

test_that("print methods still work with named nuisance columns", {
  sf <- fmrihrf::sampling_frame(c(10, 10), 1)
  set.seed(8)
  nl <- list(data.frame(tx = rnorm(10)), data.frame(tx = rnorm(10)))
  bm <- baseline_model(basis = "poly", degree = 1, sframe = sf, nuisance_list = nl)
  expect_output(print(bm), "Nuisance columns: 2")
  report <- check_nuisance(nl, sf, basis = "poly", degree = 1)
  expect_output(print(report), "passed validation")
})
