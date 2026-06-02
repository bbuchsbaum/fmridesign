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

test_that("na_action='zero' retains a leading-NA confound that 'drop' would remove", {
  sframe <- fmrihrf::sampling_frame(blocklens = 6, TR = 1)
  nlist <- list(data.frame(
    dvars  = c(NA, 2, 3, 4, 5, 6),     # fMRIPrep-style leading NA
    motion = c(-2, -1, 0, 1, 2, 3)
  ))

  # default na_action = "drop": dvars is non-finite and dropped wholesale
  bmod_drop <- suppressWarnings(
    baseline_model(basis = "constant", sframe = sframe,
                   nuisance_list = nlist, nuisance_check = "drop")
  )
  expect_equal(ncol(design_matrix(terms(bmod_drop)$nuisance)), 1)

  # na_action = "zero": the leading NA is repaired so the column is retained
  bmod_zero <- baseline_model(basis = "constant", sframe = sframe,
                              nuisance_list = nlist, nuisance_check = "drop",
                              na_action = "zero")
  nz <- as.matrix(design_matrix(terms(bmod_zero)$nuisance))
  expect_equal(ncol(nz), 2)
  expect_equal(unname(nz[1, 1]), 0)
  expect_false(anyNA(as.matrix(design_matrix(bmod_zero))))
})

test_that("na_action='median' imputes the column median for NA", {
  sframe <- fmrihrf::sampling_frame(blocklens = 6, TR = 1)
  nlist <- list(data.frame(
    dvars  = c(NA, 2, 3, 4, 5, 6),
    motion = c(-2, -1, 0, 1, 2, 3)
  ))

  bmod <- baseline_model(basis = "constant", sframe = sframe,
                         nuisance_list = nlist, nuisance_check = "drop",
                         na_action = "median")
  nz <- as.matrix(design_matrix(terms(bmod)$nuisance))
  expect_equal(ncol(nz), 2)
  expect_equal(unname(nz[1, 1]), stats::median(c(2, 3, 4, 5, 6)))  # == 4
})

test_that("na_action does not repair Inf/NaN; such columns are still dropped", {
  sframe <- fmrihrf::sampling_frame(blocklens = 6, TR = 1)
  nlist <- list(data.frame(
    bad    = c(Inf, 2, 3, 4, 5, 6),     # mid-series corruption, not a missing value
    motion = c(-2, -1, 0, 1, 2, 3)
  ))

  bmod <- suppressWarnings(
    baseline_model(basis = "constant", sframe = sframe,
                   nuisance_list = nlist, nuisance_check = "drop",
                   na_action = "zero")
  )
  dm <- as.matrix(design_matrix(bmod))
  expect_equal(ncol(design_matrix(terms(bmod)$nuisance)), 1)  # 'bad' dropped, motion kept
  expect_false(anyNA(dm))
  expect_false(any(is.infinite(dm)))
})

test_that("an all-NA column is dropped under na_action='zero'", {
  sframe <- fmrihrf::sampling_frame(blocklens = 6, TR = 1)
  nlist <- list(data.frame(
    empty  = rep(NA_real_, 6),
    motion = c(-2, -1, 0, 1, 2, 3)
  ))

  bmod <- suppressWarnings(
    baseline_model(basis = "constant", sframe = sframe,
                   nuisance_list = nlist, nuisance_check = "drop",
                   na_action = "zero")
  )
  # all-NA -> all-zero -> zero-variance -> dropped, leaving only motion
  expect_equal(ncol(design_matrix(terms(bmod)$nuisance)), 1)
})

test_that("na_action repairs NA even when nuisance_check='none'", {
  sframe <- fmrihrf::sampling_frame(blocklens = 6, TR = 1)
  nlist <- list(data.frame(dvars = c(NA, 2, 3, 4, 5, 6)))

  # checks skipped + default na_action='drop': NA leaks into the design matrix
  bmod_leak <- baseline_model(basis = "constant", sframe = sframe,
                              nuisance_list = nlist, nuisance_check = "none")
  expect_true(anyNA(as.matrix(design_matrix(bmod_leak))))

  # na_action='zero' repairs it even with checks off
  bmod_ok <- baseline_model(basis = "constant", sframe = sframe,
                            nuisance_list = nlist, nuisance_check = "none",
                            na_action = "zero")
  expect_false(anyNA(as.matrix(design_matrix(bmod_ok))))
})

test_that("check_nuisance and clean_nuisance honor na_action", {
  sframe <- fmrihrf::sampling_frame(blocklens = 6, TR = 1)
  nlist <- list(data.frame(
    dvars  = c(NA, 2, 3, 4, 5, 6),
    motion = c(-2, -1, 0, 1, 2, 3)
  ))

  # default 'drop': dvars flagged non-finite
  rep_drop <- check_nuisance(nlist, sframe, basis = "constant")
  expect_true("non_finite" %in% rep_drop$problems$issue)
  expect_equal(rep_drop$by_block[[1]]$non_finite, "dvars")

  # 'zero': dvars repaired -> no non_finite problem, column retained
  rep_zero <- check_nuisance(nlist, sframe, basis = "constant", na_action = "zero")
  expect_false("non_finite" %in% rep_zero$problems$issue)

  cleaned <- clean_nuisance(nlist, sframe, basis = "constant", na_action = "zero")
  expect_equal(ncol(cleaned$nuisance_list[[1]]), 2)
  expect_equal(unname(cleaned$nuisance_list[[1]][1, "dvars"]), 0)
})
