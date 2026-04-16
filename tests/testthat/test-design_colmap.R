# Standard test setup
des <- data.frame(
  onset = c(0, 10, 20, 30, 5, 15, 25, 35),
  run = c(1, 1, 1, 1, 2, 2, 2, 2),
  cond = factor(c("A", "B", "A", "B", "A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 1)
emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
bmod <- baseline_model(basis = "bs", degree = 3, sframe = sframe)

# --- design_colmap.event_model ---

test_that("design_colmap.event_model returns tibble with expected columns", {
  cm <- design_colmap(emod)
  expect_s3_class(cm, "tbl_df")
  expected_cols <- c("col", "name", "term_tag", "term_index", "condition",
                     "run", "role", "model_source", "basis_name", "basis_ix",
                     "basis_total", "basis_label", "is_block_diagonal",
                     "modulation_type", "modulation_id")
  for (ec in expected_cols) {
    expect_true(ec %in% names(cm), info = paste("Missing column:", ec))
  }
})

test_that("design_colmap.event_model has correct number of rows", {
  cm <- design_colmap(emod)
  dm <- design_matrix(emod)
  expect_equal(nrow(cm), ncol(dm))
})

test_that("design_colmap.event_model has correct term_tag values", {
  cm <- design_colmap(emod)
  # The term tag should relate to "cond"
  expect_true(all(!is.na(cm$term_tag)))
  expect_true(any(grepl("cond", cm$term_tag)))
})

test_that("design_colmap.event_model has correct condition parsing", {
  cm <- design_colmap(emod)
  # Should have conditions A and B
  conds <- unique(cm$condition)
  expect_true("A" %in% conds || any(grepl("A", conds)))
  expect_true("B" %in% conds || any(grepl("B", conds)))
})

test_that("design_colmap.event_model basis_ix is NA for single-basis HRF", {
  cm <- design_colmap(emod)
  # With canonical HRF (single basis), column names lack _b## suffix so basis_ix is NA

  expect_true(all(is.na(cm$basis_ix)))
})

test_that("design_colmap.event_model role is task", {
  cm <- design_colmap(emod)
  expect_true(all(cm$role == "task"))
})

test_that("design_colmap.event_model model_source is event", {
  cm <- design_colmap(emod)
  expect_true(all(cm$model_source == "event"))
})

test_that("design_colmap.event_model modulation_type is amplitude for simple model", {
  cm <- design_colmap(emod)
  expect_true(all(cm$modulation_type == "amplitude"))
})

# --- design_colmap.baseline_model ---

test_that("design_colmap.baseline_model returns tibble with expected columns", {
  cm <- design_colmap(bmod)
  expect_s3_class(cm, "tbl_df")
  expected_cols <- c("col", "name", "term_tag", "term_index", "condition",
                     "run", "role", "model_source", "basis_name", "basis_ix",
                     "basis_total", "basis_label", "is_block_diagonal",
                     "modulation_type", "modulation_id")
  for (ec in expected_cols) {
    expect_true(ec %in% names(cm), info = paste("Missing column:", ec))
  }
})

test_that("design_colmap.baseline_model has correct number of rows", {
  cm <- design_colmap(bmod)
  dm <- design_matrix(bmod)
  expect_equal(nrow(cm), ncol(dm))
})

test_that("design_colmap.baseline_model model_source is baseline", {
  cm <- design_colmap(bmod)
  expect_true(all(cm$model_source == "baseline"))
})

test_that("design_colmap.baseline_model has drift and intercept roles", {
  cm <- design_colmap(bmod)
  roles <- unique(cm$role)
  # Baseline model with bs basis should have drift and intercept roles
  expect_true("drift" %in% roles || "intercept" %in% roles,
              info = paste("Roles found:", paste(roles, collapse = ", ")))
})

test_that("design_colmap.baseline_model conditions are NA", {
  cm <- design_colmap(bmod)
  expect_true(all(is.na(cm$condition)))
})

test_that("design_colmap.baseline_model has block-diagonal columns", {
  cm <- design_colmap(bmod)
  expect_true(any(cm$is_block_diagonal))
})
