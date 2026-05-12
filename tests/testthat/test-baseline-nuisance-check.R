test_that("check_nuisance reports zero-variance, duplicate, and aliased columns", {
  sframe <- fmrihrf::sampling_frame(blocklens = c(6, 6), TR = 1)
  nlist <- list(
    data.frame(
      dvars = seq_len(6),
      std_dvars = 10 * seq_len(6),
      zero_col = 0
    ),
    data.frame(
      motion_x = c(-2, -1, 0, 1, 2, 3),
      motion_y = c(1, -1, 1, -1, 1, -1)
    )
  )

  report <- check_nuisance(nlist, sframe, basis = "constant")

  expect_s3_class(report, "nuisance_check")
  expect_false(report$ok)
  expect_true("zero_variance" %in% report$problems$issue)
  expect_true("duplicate" %in% report$problems$issue)
  expect_true("rank_deficient_with_baseline" %in% report$problems$issue)
  expect_equal(report$by_block[[1]]$zero_variance, "zero_col")
  expect_equal(report$by_block[[1]]$aliased_columns, "std_dvars")
})

test_that("baseline_model warns about nuisance rank problems by default", {
  sframe <- fmrihrf::sampling_frame(blocklens = 6, TR = 1)
  nlist <- list(
    data.frame(
      dvars = seq_len(6),
      std_dvars = 10 * seq_len(6),
      zero_col = 0
    )
  )

  expect_warning(
    bmod <- baseline_model(basis = "constant", sframe = sframe, nuisance_list = nlist),
    "Zero-variance columns: zero_col"
  )
  expect_s3_class(bmod$nuisance_check, "nuisance_check")
  expect_false(bmod$nuisance_check$ok)
})

test_that("baseline_model can error on nuisance rank problems", {
  sframe <- fmrihrf::sampling_frame(blocklens = 6, TR = 1)
  nlist <- list(
    data.frame(
      dvars = seq_len(6),
      std_dvars = 10 * seq_len(6)
    )
  )

  expect_error(
    baseline_model(basis = "constant", sframe = sframe,
                   nuisance_list = nlist, nuisance_check = "error"),
    "Duplicate or near-duplicate columns"
  )
})

test_that("baseline_model can drop nuisance columns that do not increase rank", {
  sframe <- fmrihrf::sampling_frame(blocklens = c(6, 6), TR = 1)
  nlist <- list(
    data.frame(
      dvars = seq_len(6),
      std_dvars = 10 * seq_len(6),
      zero_col = 0
    ),
    data.frame(
      motion_x = c(-2, -1, 0, 1, 2, 3),
      motion_y = c(1, -1, 1, -1, 1, -1)
    )
  )

  expect_warning(
    bmod <- baseline_model(basis = "constant", sframe = sframe,
                           nuisance_list = nlist, nuisance_check = "drop"),
    "Dropped non-finite, zero-variance, and rank-aliased nuisance columns"
  )

  nuisance_dm <- design_matrix(terms(bmod)$nuisance)
  expect_equal(ncol(nuisance_dm), 3)

  dm <- as.matrix(design_matrix(bmod))
  expect_equal(qr(dm)$rank, ncol(dm))
})

test_that("clean_nuisance returns cleaned matrices and an audit report", {
  sframe <- fmrihrf::sampling_frame(blocklens = 6, TR = 1)
  nlist <- list(
    data.frame(
      dvars = seq_len(6),
      std_dvars = 10 * seq_len(6),
      zero_col = 0
    )
  )

  cleaned <- clean_nuisance(nlist, sframe, basis = "constant")

  expect_s3_class(cleaned, "cleaned_nuisance")
  expect_s3_class(cleaned$report, "nuisance_check")
  expect_equal(colnames(cleaned$nuisance_list[[1]]), "dvars")
})
