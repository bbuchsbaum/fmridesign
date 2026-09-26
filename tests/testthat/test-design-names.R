# Name and accessor methods that fmridesign owns for its own classes
# (event_model, event_term, convolved_term, feature_term, bare events,
# baseline_model). Downstream packages reach these by ordinary dispatch on
# the exported generics.

.names_fixture <- function() {
  sf <- fmrihrf::sampling_frame(blocklens = c(30, 30), TR = 2)
  des <- data.frame(
    onset = c(0, 10, 20, 0, 10, 20),
    run = rep(1:2, each = 3),
    condition = factor(rep(c("A", "B", "A"), 2)),
    attn = factor(c("x", "y", "y", "y", "x", "x")),
    rt = c(0.5, 1.1, 0.9, 1.3, 0.7, 1.0)
  )
  em <- event_model(
    onset ~ hrf(condition) + hrf(condition, attn, basis = "spmg2") + hrf(rt),
    data = des, block = ~run, sampling_frame = sf
  )
  list(sf = sf, des = des, em = em)
}

test_that("longnames/shortnames on event_model concatenate the term names", {
  fx <- .names_fixture()
  em <- fx$em
  expect_equal(
    longnames(em),
    c("condition.A", "condition.B",
      "condition.A_attn.x", "condition.B_attn.x",
      "condition.A_attn.y", "condition.B_attn.y", "rt")
  )
  expect_equal(shortnames(em), c("A", "B", "A:x", "B:x", "A:y", "B:y", "rt"))
  expect_equal(
    longnames(em),
    unlist(lapply(terms(em), longnames), use.names = FALSE)
  )
  expect_equal(
    shortnames(em),
    unlist(lapply(terms(em), shortnames), use.names = FALSE)
  )
  expect_equal(longnames(em), conditions(em, style = "canonical"))
  expect_equal(shortnames(em), conditions(em, style = "display"))
})

test_that("design-matrix columns are term tag + '_' + basis-expanded longnames", {
  fx <- .names_fixture()
  em <- fx$em
  cols <- columns(em)
  expect_equal(cols, colnames(design_matrix(em)))
  # the documented example: condition_condition.A vs condition.A
  expect_equal(cols[1:2], c("condition_condition.A", "condition_condition.B"))
  expect_equal(longnames(terms(em)[[1]]), c("condition.A", "condition.B"))

  col_idx <- attr(design_matrix(em), "col_indices")
  for (tag in names(terms(em))) {
    expect_equal(
      cols[col_idx[[tag]]],
      paste0(tag, "_", longnames(terms(em)[[tag]], expand_basis = TRUE)),
      info = tag
    )
  }
  expect_equal(
    longnames(em, expand_basis = TRUE),
    conditions(em, expand_basis = TRUE)
  )
})

test_that("columns.event_model and cells.event_model", {
  fx <- .names_fixture()
  em <- fx$em
  em_nodm <- em
  em_nodm["design_matrix"] <- list(NULL)
  expect_identical(columns(em_nodm), character(0))

  cl <- cells(em)
  expect_s3_class(cl, "tbl_df")
  expected <- dplyr::bind_rows(lapply(terms(em), function(t) tibble::as_tibble(cells(t))))
  expect_equal(cl, expected)
  expect_equal(nrow(cl), 2 + 4 + 1)
  expect_setequal(names(cl), c("condition", "attn", "rt"))

  empty <- em
  empty$terms <- list()
  expect_equal(cells(empty), tibble::tibble())
})

test_that("bare events are named like the single-variable event_term", {
  fx <- .names_fixture()
  des <- fx$des
  ev <- event_factor(des$condition, "condition", onsets = des$onset,
                     blockids = des$run)
  expect_s3_class(ev, "event_seq")
  expect_false(inherits(ev, "event_term"))
  expect_equal(longnames(ev), c("condition.A", "condition.B"))
  expect_equal(shortnames(ev), c("A", "B"))

  term <- event_term(list(condition = des$condition), onsets = des$onset,
                     blockids = des$run)
  expect_equal(longnames(ev), longnames(term))
  expect_equal(shortnames(ev), shortnames(term))

  evv <- event_variable(des$rt, "rt", onsets = des$onset, blockids = des$run)
  expect_equal(as.vector(longnames(evv)), "rt")
  expect_equal(as.vector(shortnames(evv)), "rt")
})

test_that("convolved_term methods delegate to the event term and hrfspec", {
  fx <- .names_fixture()
  em <- fx$em
  term <- terms(em)[["condition_attn"]]
  dm <- design_matrix(em)[, attr(design_matrix(em), "col_indices")$condition_attn]
  ct <- structure(
    list(varname = term$varname, evterm = term, design_matrix = dm,
         hrfspec = list(hrf = fmrihrf::HRF_SPMG2), sampling_frame = fx$sf),
    class = c("convolved_term", "fmri_term", "list")
  )
  expect_equal(longnames(ct), longnames(term))
  expect_equal(shortnames(ct), shortnames(term))
  expect_equal(conditions(ct), conditions(term))
  expect_equal(conditions(ct, expand_basis = TRUE),
               conditions(term, expand_basis = TRUE))
  expect_equal(event_table(ct), event_table(term))
  expect_equal(nbasis(ct), 2L)

  expect_identical(design_matrix(ct), dm)
  run2 <- design_matrix(ct, blockid = 2)
  expect_equal(nrow(run2), 30L)
  expect_equal(as.matrix(run2), as.matrix(dm[31:60, , drop = FALSE]),
               ignore_attr = TRUE)
  # a single-column term stays a matrix when subset by block
  ct1 <- ct
  ct1$design_matrix <- dm[, 1, drop = FALSE]
  expect_equal(dim(design_matrix(ct1, blockid = 1)), c(30L, 1L))

  # hrfspec stored as an attribute (extension convolved classes)
  ct_attr <- ct
  ct_attr$hrfspec <- NULL
  attr(ct_attr, "hrfspec") <- list(hrf = fmrihrf::HRF_SPMG3)
  expect_equal(nbasis(ct_attr), 3L)
  ct_none <- ct
  ct_none$hrfspec <- NULL
  expect_equal(nbasis(ct_none), 1L)
})

test_that("feature_term long and short names are its condition tags", {
  dt <- 0.2
  mat <- cbind(alpha = abs(sin(seq(0, 12, by = dt))),
               beta = abs(cos(seq(0, 12, by = dt))))
  sframe <- fmrihrf::sampling_frame(blocklens = 10, TR = 2)
  emod <- event_model(
    ~ feature(mat, dt = dt, id = "env", center = FALSE, scale = "none"),
    sampling_frame = sframe,
    precision = dt
  )
  ft <- terms(emod)[[1]]
  expect_s3_class(ft, "feature_term")
  expect_equal(longnames(ft), c("alpha", "beta"))
  expect_equal(shortnames(ft), c("alpha", "beta"))
  expect_equal(longnames(emod), c("alpha", "beta"))
  expect_equal(columns(emod), paste0("env_", longnames(ft)))
})

test_that("conditions.baseline_model lists every baseline column", {
  sf <- fmrihrf::sampling_frame(blocklens = c(20, 20), TR = 2)
  bm <- baseline_model(basis = "poly", degree = 2, sframe = sf)
  expect_equal(conditions(bm), colnames(design_matrix(bm)))
})

test_that("generics used by downstream packages are exported", {
  exports <- getNamespaceExports("fmridesign")
  needed <- c(
    "longnames", "shortnames", "construct", "correlation_map", "Fcontrasts",
    "columns", "cells", "conditions", "condition_map", "event_table",
    "design_matrix", "design_map", "contrast_weights", "event_terms",
    "baseline_terms", "term_matrices", "term_indices", "elements",
    "is_categorical", "is_continuous", "convolve", "nbasis", "blockids",
    "onsets", "durations"
  )
  expect_true(all(needed %in% exports),
              info = paste(setdiff(needed, exports), collapse = ", "))
  for (g in needed) {
    expect_true(is.function(getExportedValue("fmridesign", g)), info = g)
  }
})

test_that("methods for fmridesign classes are registered on the exported generics", {
  registered <- list(
    longnames = c("event_term", "event_model", "convolved_term", "event_seq",
                  "feature_term", "covariate_term", "covariate_convolved_term"),
    shortnames = c("event_term", "event_model", "convolved_term", "event_seq",
                   "feature_term", "covariate_term", "covariate_convolved_term"),
    Fcontrasts = c("event_model", "event_term", "convolved_term", "feature_term"),
    correlation_map = c("event_model", "baseline_model"),
    construct = c("hrfspec", "baselinespec", "covariatespec", "featurespec"),
    columns = "event_model",
    cells = c("event_model", "event_term", "convolved_term", "feature_term",
              "baseline_model"),
    conditions = c("event_model", "event_term", "convolved_term",
                   "feature_term", "baseline_model"),
    event_table = c("event_term", "convolved_term", "feature_term"),
    design_matrix = c("event_model", "event_term", "convolved_term",
                      "baseline_model"),
    nbasis = c("convolved_term", "feature_term", "hrfspec"),
    blockids = c("event_model", "event_term", "convolved_term")
  )
  for (g in names(registered)) {
    gen <- getExportedValue("fmridesign", g)
    for (cls in registered[[g]]) {
      m <- utils::getS3method(g, cls, optional = TRUE,
                              envir = environment(gen))
      expect_true(is.function(m), info = paste0(g, ".", cls))
    }
  }
})

test_that("fmridesign:: generics dispatch without namespace access", {
  fx <- .names_fixture()
  em <- fx$em
  expect_equal(fmridesign::longnames(em), longnames(em))
  expect_type(fmridesign::Fcontrasts(em), "list")
  expect_s3_class(fmridesign::correlation_map(em), "ggplot")
  expect_equal(fmridesign::columns(em), colnames(design_matrix(em)))
})
