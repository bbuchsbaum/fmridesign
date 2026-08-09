# Correctness tests for the fast-path metadata constructors introduced to avoid
# tibble::tibble()/dplyr::bind_rows() overhead in the event-model build path.
# See R/design_metadata.R (.new_meta_tibble, .make_col_metadata,
# .empty_col_metadata, .combine_col_metadata) and the convolve_design column
# extraction fix in R/event_vector.R.

testthat::local_edition(3)

# Reference implementations mirroring the previous tibble::tibble()/bind_rows()
# based code, used to assert the fast path is output-equivalent.
ref_make_col_metadata <- function(name,
                                   condition       = name,
                                   term_tag        = NA_character_,
                                   basis_name      = NA_character_,
                                   basis_ix        = NA_integer_,
                                   basis_total     = NA_integer_,
                                   basis_label     = NA_character_,
                                   role            = "task",
                                   model_source    = "event",
                                   modulation_type = "amplitude",
                                   modulation_id   = NA_character_,
                                   is_block_diagonal = FALSE) {
  n <- length(name)
  tibble::tibble(
    col              = seq_len(n),
    name             = name,
    term_tag         = term_tag,
    term_index       = NA_integer_,
    condition        = condition,
    run              = NA_integer_,
    role             = role,
    model_source     = model_source,
    basis_name       = basis_name,
    basis_ix         = basis_ix,
    basis_total      = basis_total,
    basis_label      = basis_label,
    is_block_diagonal = is_block_diagonal,
    modulation_type  = modulation_type,
    modulation_id    = modulation_id
  )
}

test_that(".empty_col_metadata is identical to the tibble() reference", {
  ref <- ref_make_col_metadata(character(0))
  cur <- fmridesign:::.empty_col_metadata()
  expect_s3_class(cur, "tbl_df")
  expect_identical(cur, ref)
})

test_that(".make_col_metadata equals tibble() reference for clean inputs", {
  # single categorical (nb == 1: scalar basis_ix/basis_label)
  a_cur <- fmridesign:::.make_col_metadata(
    name = c("cond_cond.A", "cond_cond.B"),
    condition = c("cond.A", "cond.B"),
    term_tag = "cond", basis_name = "HRF"
  )
  a_ref <- ref_make_col_metadata(
    name = c("cond_cond.A", "cond_cond.B"),
    condition = c("cond.A", "cond.B"),
    term_tag = "cond", basis_name = "HRF"
  )
  expect_identical(a_cur, a_ref)

  # multi-basis (nb == 3: vector basis_ix/basis_label)
  b_cur <- fmridesign:::.make_col_metadata(
    name = paste0("rt_rt_b0", 1:3),
    condition = rep("rt", 3),
    term_tag = "rt", basis_name = "SPMG3",
    basis_ix = 1:3, basis_total = 3L,
    basis_label = c("canonical", "derivative", "dispersion"),
    modulation_type = "parametric", modulation_id = "rt"
  )
  b_ref <- ref_make_col_metadata(
    name = paste0("rt_rt_b0", 1:3),
    condition = rep("rt", 3),
    term_tag = "rt", basis_name = "SPMG3",
    basis_ix = 1:3, basis_total = 3L,
    basis_label = c("canonical", "derivative", "dispersion"),
    modulation_type = "parametric", modulation_id = "rt"
  )
  expect_identical(b_cur, b_ref)
})

test_that(".combine_col_metadata equals dplyr::bind_rows for clean parts", {
  p1 <- fmridesign:::.make_col_metadata(
    name = c("cond_cond.A", "cond_cond.B"),
    condition = c("cond.A", "cond.B"), term_tag = "cond", basis_name = "HRF"
  )
  p2 <- fmridesign:::.make_col_metadata(
    name = paste0("rt_rt_b0", 1:3), condition = rep("rt", 3),
    term_tag = "rt", basis_name = "SPMG3", basis_ix = 1:3, basis_total = 3L,
    basis_label = c("canonical", "derivative", "dispersion"),
    modulation_type = "parametric", modulation_id = "rt"
  )
  parts <- list(cond = p1, rt = p2)

  cur <- fmridesign:::.combine_col_metadata(parts, names(parts), term_indices = c(1L, 2L))

  # Reference: replicate the stamping bind_rows did.
  ref_parts <- Map(function(md, i, nm) {
    md$term_index <- as.integer(i)
    md$term_tag <- nm
    md
  }, parts, c(1L, 2L), names(parts))
  cursor <- 0L
  for (i in seq_along(ref_parts)) {
    ref_parts[[i]]$col <- cursor + seq_len(nrow(ref_parts[[i]]))
    cursor <- cursor + nrow(ref_parts[[i]])
  }
  ref <- dplyr::bind_rows(ref_parts)

  expect_identical(cur, ref)
})

test_that(".new_meta_tibble recycles length-1 and rejects genuine mismatches", {
  ok <- fmridesign:::.new_meta_tibble(list(a = 1:3, b = "x", c = NA_integer_), n = 3L)
  expect_equal(nrow(ok), 3L)
  expect_identical(ok$b, rep("x", 3))          # recycled
  expect_true(all(vapply(ok, length, 1L) == 3L))  # no ragged columns

  # A length that is neither 1 nor n must error (guards against silent corruption
  # that raw new_tibble() would allow).
  expect_error(
    fmridesign:::.new_meta_tibble(list(a = 1:3, b = 1:2), n = 3L),
    "incompatible"
  )
})

test_that("metadata columns are ragged-free and row-subsettable across term types", {
  set.seed(11)
  sf <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 1)
  des <- data.frame(
    onset = rep(seq(1, 45, length.out = 10), 2),
    run = rep(1:2, each = 10),
    condition = factor(rep(c("A", "B"), length.out = 20)),
    task = factor(rep(c("go", "nogo"), length.out = 20)),
    rt = rnorm(20, 1, 0.2)
  )

  models <- list(
    cat   = event_model(onset ~ hrf(condition), data = des, block = ~run, sampling_frame = sf),
    inter = event_model(onset ~ hrf(condition, task), data = des, block = ~run, sampling_frame = sf),
    spmg3 = event_model(onset ~ hrf(rt, basis = "spmg3"), data = des, block = ~run, sampling_frame = sf),
    multi = event_model(onset ~ hrf(condition) + hrf(rt, basis = "spmg3"),
                        data = des, block = ~run, sampling_frame = sf),
    ident = event_model(onset ~ hrf(Ident(rt)), data = des, block = ~run, sampling_frame = sf)
  )

  for (nm in names(models)) {
    meta <- design_meta(models[[nm]])
    dm <- design_matrix(models[[nm]])
    # No ragged columns (the failure mode of an unvalidated new_tibble()).
    expect_true(all(vapply(meta, length, integer(1)) == nrow(meta)),
                info = paste("ragged metadata for model", nm))
    expect_equal(nrow(meta), ncol(dm), info = nm)
    expect_equal(meta$name, colnames(dm), info = nm)
    # Row subsetting must not error (would throw on a corrupt/ragged tibble).
    tag1 <- meta$term_tag[1]
    sub <- meta[!is.na(meta$term_tag) & meta$term_tag == tag1, ]
    expect_true(nrow(sub) >= 1L, info = nm)
  }
})

test_that("convolve_design extracts full columns for matrix and data.frame inputs", {
  hrf <- fmrihrf::HRF_SPMG1
  # Condition A: onsets 0 and 20 (row 1 has amplitude 1, so old and new agree on
  # tibble); Condition B: onset only at row 2 (amplitude 0 in row 1 -> the old
  # data.frame `[, i][[1]]` bug would drop it entirely).
  globons <- c(0, 10, 20)
  durations <- rep(0, 3)

  df  <- data.frame(A = c(1, 0, 1), B = c(0, 1, 0))
  tib <- tibble::tibble(A = c(1, 0, 1), B = c(0, 1, 0))

  regs_df  <- convolve_design(hrf, df,  globons, durations)
  regs_tib <- convolve_design(hrf, tib, globons, durations)

  # Regressor A must reference both of its onsets, B its single onset.
  expect_equal(fmrihrf::onsets(regs_df[[1]]), c(0, 20))
  expect_equal(fmrihrf::onsets(regs_df[[2]]), 10)
  # data.frame and tibble inputs now produce identical regressor onsets.
  expect_equal(fmrihrf::onsets(regs_df[[1]]), fmrihrf::onsets(regs_tib[[1]]))
  expect_equal(fmrihrf::onsets(regs_df[[2]]), fmrihrf::onsets(regs_tib[[2]]))
})
