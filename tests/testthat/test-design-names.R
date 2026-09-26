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

# ---------------------------------------------------------------------------
# Empty interaction cells
# ---------------------------------------------------------------------------

.empty_cell_fixture <- function() {
  sf <- fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 2)
  des <- data.frame(
    onset = rep(c(0, 10, 20, 30, 40, 50), 2),
    run = rep(1:2, each = 6),
    a = factor(rep(c("p", "q", "r", "p", "q", "r"), 2)),
    b = factor(rep(c("x", "x", "y", "y", "x", "x"), 2)),
    rt = c(0.3, 0.9, 1.4, 0.2, 0.8, 1.1, 0.5, 0.7, 1.2, 0.4, 1.0, 0.6)
  )
  # q never occurs with y: the middle cell q:y is empty; r:y is not
  des$b[des$a == "q"] <- "x"
  em <- event_model(onset ~ hrf(a, b) + hrf(a, b, basis = "spmg2", id = "ab2"),
                    data = des, block = ~run, sampling_frame = sf)
  list(sf = sf, des = des, em = em)
}

test_that("drop.empty = TRUE leaves out empty cells so names match columns", {
  fx <- .empty_cell_fixture()
  em <- fx$em
  t1 <- terms(em)[[1]]
  full <- c("a.p_b.x", "a.q_b.x", "a.r_b.x", "a.p_b.y", "a.q_b.y", "a.r_b.y")

  expect_equal(conditions(t1), full)                    # conditions(): full grid
  expect_equal(longnames(t1, drop.empty = FALSE), full)
  expect_equal(longnames(t1), full[-5])                 # q:y dropped
  expect_equal(shortnames(t1), c("p:x", "q:x", "r:x", "p:y", "r:y"))
  expect_equal(shortnames(t1, drop.empty = FALSE),
               c("p:x", "q:x", "r:x", "p:y", "q:y", "r:y"))

  cols <- columns(em)
  ci <- attr(design_matrix(em), "col_indices")
  expect_equal(cols[ci[[1]]], paste0("a_b_", longnames(t1)))
  t2 <- terms(em)[[2]]
  expect_equal(cols[ci[[2]]], paste0("ab2_", longnames(t2, expand_basis = TRUE)))
  expect_length(longnames(em, expand_basis = TRUE), ncol(design_matrix(em)))
  expect_equal(longnames(em), c(full[-5], full[-5]))
  expect_equal(longnames(em, drop.empty = FALSE), c(full, full))
  expect_equal(length(shortnames(em)), 10L)
})

test_that("condition_map lines each condition up with its column", {
  fx <- .empty_cell_fixture()
  em <- fx$em
  cm <- condition_map(em)
  expect_equal(nrow(cm), 10L)
  expect_false(anyNA(cm$column_name[cm$term == "a_b"]))
  expect_equal(cm$column_name[cm$term == "a_b"],
               paste0("a_b_", cm$canonical[cm$term == "a_b"]))
  expect_true(all(cm$column_name[cm$term == "a_b"] %in% columns(em)))
  # multi-basis term without expand_basis: no single column per condition
  expect_true(all(is.na(cm$column_name[cm$term == "ab2"])))

  cm_full <- condition_map(em, drop.empty = FALSE)
  ab <- cm_full[cm_full$term == "a_b", ]
  expect_equal(nrow(ab), 6L)
  expect_true(is.na(ab$column_name[ab$canonical == "a.q_b.y"]))
  expect_equal(sum(!is.na(ab$column_name)), 5L)

  cm_b <- condition_map(em, expand_basis = TRUE)
  ab2 <- cm_b[cm_b$term == "ab2", ]
  expect_equal(ab2$column_name, paste0("ab2_", ab2$canonical))
  expect_equal(ab2$column_name, columns(em)[attr(design_matrix(em), "col_indices")$ab2])
})

test_that("condition_map matches columns whose tag differs from the term name", {
  sf <- fmrihrf::sampling_frame(blocklens = 40, TR = 2)
  des <- data.frame(onset = c(0, 10, 20, 30), run = 1,
                    a = factor(c("p", "q", "p", "q")))
  expect_warning(
    em <- event_model(onset ~ hrf(a) + hrf(a, basis = "spmg1"),
                      data = des, block = ~run, sampling_frame = sf),
    class = "fmridesign_name_clash"
  )
  expect_true(any(startsWith(columns(em), "a.1_")))   # tag "a#1", columns "a.1_"
  cm <- condition_map(em)
  expect_false(anyNA(cm$column_name))
  expect_equal(sort(cm$column_name), sort(columns(em)))
})

# ---------------------------------------------------------------------------
# blockids(<event_model>)
# ---------------------------------------------------------------------------

test_that("blockids(<event_model>) is per event and notes it once per session", {
  fx <- .names_fixture()
  em <- fx$em
  state <- fmridesign:::.fd_notice_state
  old <- state$blockids_event_model
  on.exit(state$blockids_event_model <- old, add = TRUE)

  state$blockids_event_model <- NULL
  expect_message(b <- blockids(em), class = "fmridesign_blockids_per_event")
  expect_message(blockids(em), NA)
  expect_message(fmrihrf::blockids(em), NA)
  expect_equal(b, fx$des$run)
  expect_length(b, nrow(fx$des))
  expect_equal(fmrihrf::blockids(em$sampling_frame), rep(1:2, each = 30))
  expect_equal(blocklens(em), c(30, 30))
})

# ---------------------------------------------------------------------------
# correlation_map(<baseline_model>): within-run correlations
# ---------------------------------------------------------------------------

.baseline_corr_fixture <- function() {
  set.seed(42)
  bl <- c(80, 80)
  sf <- fmrihrf::sampling_frame(blocklens = bl, TR = 2)
  nuis <- list(matrix(rnorm(bl[1] * 2), bl[1], 2),
               matrix(rnorm(bl[2] * 2), bl[2], 2))
  baseline_model(basis = "poly", degree = 3, sframe = sf, nuisance_list = nuis)
}

.cells_by_name <- function(p, bm, within_run) {
  ci <- fmridesign:::.fd_hm_columns(bm)
  if (within_run) ci <- ci[ci$term != "block", , drop = FALSE]
  cn <- colnames(design_matrix(bm))[ci$col]
  d <- p$data
  a <- cn[d$y]; b <- cn[d$x]
  k <- ifelse(a < b, paste(a, b), paste(b, a))
  stats::setNames(d$r, k)
}

test_that("within_run correlations match the per-run computation", {
  bm <- .baseline_corr_fixture()
  DM <- as.matrix(design_matrix(bm))
  run <- rep(1:2, each = 80)

  # expected: drop intercepts, centre within run, correlate same-run pairs on
  # their run's rows; different-run pairs are not shown
  keep <- !grepl("^constant", colnames(DM))
  X <- DM[, keep]
  col_run <- apply(X != 0, 2, function(nz) {
    r <- unique(run[nz]); if (length(r) == 1L) r else NA
  })
  expected <- c()
  cn <- colnames(X)
  for (i in seq_along(cn)) for (j in seq_along(cn)) if (i < j) {
    ri <- col_run[i]; rj <- col_run[j]
    if (!is.na(ri) && !is.na(rj) && ri != rj) next
    rows <- if (!is.na(ri)) run == ri else if (!is.na(rj)) run == rj else TRUE
    xi <- X[, i] - ave(X[, i], run); xj <- X[, j] - ave(X[, j], run)
    k <- if (cn[i] < cn[j]) paste(cn[i], cn[j]) else paste(cn[j], cn[i])
    expected[k] <- stats::cor(xi[rows], xj[rows])
  }

  p <- correlation_map(bm)            # within_run = TRUE by default
  got <- .cells_by_name(p, bm, TRUE)
  expect_equal(length(got), 20L)      # 10 columns; 2 x choose(5, 2) same-run pairs
  expect_setequal(names(got), names(expected))
  expect_equal(got[names(expected)], expected, tolerance = 1e-12)

  p_full <- correlation_map(bm, half_matrix = FALSE)
  expect_equal(nrow(p_full$data), 40L)

  raw <- correlation_map(bm, within_run = FALSE)
  expect_equal(nrow(raw$data), choose(12, 2))
  got_raw <- .cells_by_name(raw, bm, FALSE)
  R <- stats::cor(DM)
  a <- sub(" .*", "", names(got_raw)); b <- sub(".* ", "", names(got_raw))
  expect_equal(unname(got_raw), R[cbind(a, b)], tolerance = 1e-12)
})

test_that("correlation_map rejects arguments it would otherwise drop", {
  bm <- .baseline_corr_fixture()
  expect_error(correlation_map(bm, bogus = 1), "unknown argument `bogus`")
  expect_error(fmridesign:::.fd_hm_check_tile_args(3), "must be named")
  expect_s3_class(correlation_map(bm, alpha = 0.5), "ggplot")

  lab <- correlation_map(bm, label_values = FALSE)
  expect_false(any(vapply(lab$layers, function(l) inherits(l$geom, "GeomText") &&
                            identical(l$data, ggplot2::waiver()), logical(1))))
  expect_error(correlation_map(bm, label_values = TRUE, annotate = TRUE),
               "not both")

  fx <- .names_fixture()
  expect_error(correlation_map(fx$em, within_rn = TRUE), "unknown argument")
  expect_s3_class(correlation_map(fx$em, within_run = TRUE), "ggplot")
})
