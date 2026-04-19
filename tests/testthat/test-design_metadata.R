# Tests for first-class design column metadata (R/design_metadata.R).

test_that("event_model attaches col_metadata aligned with design matrix", {
  des <- data.frame(
    onset = c(0, 10, 20, 30, 40, 50),
    run   = 1,
    cond  = factor(c("A", "B", "A", "B", "A", "B")),
    rt    = c(0.4, 0.6, 0.5, 0.55, 0.7, 0.45)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = 60, TR = 1)
  emod <- event_model(
    onset ~ hrf(cond) + hrf(Scale(rt)),
    data = des, block = ~run, sampling_frame = sframe
  )

  dm <- design_matrix(emod)
  meta <- design_meta(emod)

  expect_s3_class(meta, "tbl_df")
  expect_equal(nrow(meta), ncol(dm))
  expect_equal(meta$name, colnames(dm))
  expect_equal(meta$col, seq_len(ncol(dm)))

  # Categorical term: modulation should be amplitude.
  cond_rows <- meta[meta$term_tag == "cond", ]
  expect_true(all(cond_rows$modulation_type == "amplitude"))

  # Scale(rt) term: modulation should be parametric, modulation_id == "rt".
  rt_rows <- meta[meta$term_tag == "z_rt", ]
  expect_true(nrow(rt_rows) >= 1L)
  expect_true(all(rt_rows$modulation_type == "parametric"))
  expect_true(all(rt_rows$modulation_id == "rt"))
})

test_that("design_colmap reads from col_metadata when available", {
  des <- data.frame(
    onset = c(0, 10, 20, 30),
    run   = 1,
    cond  = factor(c("A", "B", "A", "B"))
  )
  sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
  emod <- event_model(onset ~ hrf(cond), data = des,
                      block = ~run, sampling_frame = sframe)

  cm_meta <- design_meta(emod)
  cm_view <- design_colmap(emod)

  # Both should describe the same columns.
  expect_equal(cm_view$name, colnames(design_matrix(emod)))
  expect_equal(cm_view$term_tag, cm_meta$term_tag)
  expect_equal(cm_view$modulation_type, cm_meta$modulation_type)

  # design_colmap adds a pretty_name column derived from metadata.
  expect_true("pretty_name" %in% names(cm_view))
  expect_equal(length(cm_view$pretty_name), nrow(cm_view))
})

test_that("multi-basis HRF yields basis_ix and basis_total", {
  des <- data.frame(
    onset = c(0, 10, 20, 30),
    run   = 1,
    cond  = factor(c("A", "B", "A", "B"))
  )
  sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
  emod <- event_model(onset ~ hrf(cond, basis = "spmg3"),
                      data = des, block = ~run, sampling_frame = sframe)
  meta <- design_meta(emod)

  expect_true(all(!is.na(meta$basis_ix)))
  expect_true(all(meta$basis_total == 3L))
  expect_true(all(meta$basis_label %in% c("canonical", "derivative", "dispersion")))
})
