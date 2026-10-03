# column_contrast() pattern matching against design-matrix column names
# (GitHub issue #24). Patterns are matched first against the full design-matrix
# column names (`term_tag_condition_tag[_b##]`) and, if nothing matches there,
# against the term-level condition names (`condition_tag[_b##]`).

.cc_events <- function() {
  data.frame(
    onset = seq(10, 150, by = 20),
    cond = factor(rep(c("A", "B"), 4)),
    task = factor(rep(c("face", "face", "house", "house"), 2)),
    run = 1
  )
}

.cc_sf <- function() fmrihrf::sampling_frame(100, TR = 2)

# Weights of one contrast mapped onto the full design matrix, as a named vector.
.cc_full_weights <- function(emod, key) {
  cw <- contrast_weights(emod)
  expect_true(key %in% names(cw))
  w <- cw[[key]]$offset_weights[, 1]
  expect_identical(names(w), colnames(design_matrix(emod)))
  w
}

.cc_expected <- function(emod, pos, neg = character(0)) {
  cn <- colnames(design_matrix(emod))
  stopifnot(all(pos %in% cn), all(neg %in% cn))
  w <- stats::setNames(numeric(length(cn)), cn)
  w[pos] <- 1 / length(pos)
  if (length(neg)) w[neg] <- -1 / length(neg)
  w
}

test_that("documented design-matrix names select columns (single basis, issue #24 reprex)", {
  ev <- .cc_events()
  cset <- contrast_set(
    column_contrast("^cond_cond\\.A$", "^cond_cond\\.B$", name = "AvB")
  )
  em <- event_model(onset ~ hrf(cond, contrasts = cset),
                    data = ev, block = ~run, sampling_frame = .cc_sf())
  expect_identical(colnames(design_matrix(em)), c("cond_cond.A", "cond_cond.B"))

  expect_no_warning(w <- .cc_full_weights(em, "cond#AvB"))
  expect_equal(w, .cc_expected(em, "cond_cond.A", "cond_cond.B"))

  # Direct term-level call gives the same weights (row names are term-level).
  term <- terms(em)[[1]]
  expect_no_warning(cw <- contrast_weights(
    column_contrast("^cond_cond\\.A$", "^cond_cond\\.B$", name = "AvB"), term))
  expect_equal(unname(cw$weights[, 1]), c(1, -1))
  expect_identical(rownames(cw$weights), c("cond.A", "cond.B"))
})

test_that("legacy term-level patterns still work and select the same columns", {
  ev <- .cc_events()
  em <- event_model(onset ~ hrf(cond), data = ev, block = ~run,
                    sampling_frame = .cc_sf())
  term <- terms(em)[[1]]

  full <- contrast_weights(
    column_contrast("^cond_cond\\.A$", "^cond_cond\\.B$", name = "x"), term)
  legacy <- expect_no_warning(contrast_weights(
    column_contrast("^cond\\.A$", "^cond\\.B$", name = "x"), term))
  expect_identical(full$weights, legacy$weights)

  # Unanchored legacy pattern from the package examples.
  legacy2 <- contrast_weights(
    column_contrast("cond.A", "cond.B", name = "x"), term)
  expect_identical(full$weights, legacy2$weights)
})

test_that("multi-basis: documented names select only _b01 or only _b02 columns", {
  ev <- .cc_events()
  cset <- contrast_set(
    column_contrast("^mb_cond\\.A_b01$", "^mb_cond\\.B_b01$", name = "AvB_b1"),
    column_contrast("_b02$", name = "all_b2"),
    column_contrast("^mb_cond\\.A_b02$", "^mb_cond\\.B_b02$", name = "AvB_b2"),
    column_contrast("^cond\\.A_b01$", "^cond\\.B_b01$", name = "legacy_b1")
  )
  em <- event_model(onset ~ hrf(cond, basis = "spmg3", id = "mb", contrasts = cset),
                    data = ev, block = ~run, sampling_frame = .cc_sf())
  expect_identical(
    colnames(design_matrix(em)),
    c("mb_cond.A_b01", "mb_cond.A_b02", "mb_cond.A_b03",
      "mb_cond.B_b01", "mb_cond.B_b02", "mb_cond.B_b03")
  )

  expect_no_warning(cw <- contrast_weights(em))
  w1 <- cw[["mb#AvB_b1"]]$offset_weights[, 1]
  expect_equal(w1, .cc_expected(em, "mb_cond.A_b01", "mb_cond.B_b01"))
  w2 <- cw[["mb#AvB_b2"]]$offset_weights[, 1]
  expect_equal(w2, .cc_expected(em, "mb_cond.A_b02", "mb_cond.B_b02"))
  wall <- cw[["mb#all_b2"]]$offset_weights[, 1]
  expect_equal(wall, .cc_expected(em, c("mb_cond.A_b02", "mb_cond.B_b02")))
  wleg <- cw[["mb#legacy_b1"]]$offset_weights[, 1]
  expect_identical(wleg, w1)
})

test_that("factorial terms: documented names select interaction cells", {
  ev <- .cc_events()
  cset <- contrast_set(
    column_contrast("^task_cond_task\\.face_cond\\.A$",
                    "^task_cond_task\\.house_cond\\.A$", name = "FvH_A"),
    column_contrast("_cond\\.A$", "_cond\\.B$", name = "AvB"),
    column_contrast("^task\\.face_cond\\.A$", "^task\\.house_cond\\.A$",
                    name = "legacy_FvH_A")
  )
  em <- event_model(onset ~ hrf(task, cond, contrasts = cset),
                    data = ev, block = ~run, sampling_frame = .cc_sf())
  expect_no_warning(cw <- contrast_weights(em))

  w <- cw[["task_cond#FvH_A"]]$offset_weights[, 1]
  expect_equal(w, .cc_expected(em, "task_cond_task.face_cond.A",
                               "task_cond_task.house_cond.A"))
  expect_identical(cw[["task_cond#legacy_FvH_A"]]$offset_weights[, 1], w)

  w2 <- cw[["task_cond#AvB"]]$offset_weights[, 1]
  expect_equal(w2, .cc_expected(
    em,
    c("task_cond_task.face_cond.A", "task_cond_task.house_cond.A"),
    c("task_cond_task.face_cond.B", "task_cond_task.house_cond.B")
  ))
})

test_that("multi-term models: each term's tag is used for its own columns", {
  ev <- .cc_events()
  cs1 <- contrast_set(column_contrast("^cond_cond\\.A$", "^cond_cond\\.B$",
                                      name = "AvB"))
  cs2 <- contrast_set(column_contrast("^stim_task\\.face$", "^stim_task\\.house$",
                                      name = "FvH"))
  em <- event_model(
    onset ~ hrf(cond, contrasts = cs1) + hrf(task, id = "stim", contrasts = cs2),
    data = ev, block = ~run, sampling_frame = .cc_sf()
  )
  expect_identical(colnames(design_matrix(em)),
                   c("cond_cond.A", "cond_cond.B", "stim_task.face", "stim_task.house"))
  expect_no_warning(cw <- contrast_weights(em))
  expect_equal(cw[["cond#AvB"]]$offset_weights[, 1],
               .cc_expected(em, "cond_cond.A", "cond_cond.B"))
  expect_equal(cw[["stim#FvH"]]$offset_weights[, 1],
               .cc_expected(em, "stim_task.face", "stim_task.house"))

  # A pattern written with the other term's tag does not match this term.
  term2 <- terms(em)[[2]]
  expect_warning(
    expect_error(contrast_weights(
      column_contrast("^cond_task\\.face$", name = "wrong_tag"), term2)),
    "stim_task.face"
  )
})

test_that("regex anchors are respected in both namespaces", {
  ev <- .cc_events()
  em <- event_model(onset ~ hrf(cond, basis = "spmg3"), data = ev,
                    block = ~run, sampling_frame = .cc_sf())
  term <- terms(em)[[1]]
  rn <- rownames(contrast_weights(column_contrast("_b01$", name = "x"), term)$weights)

  # "^cond_cond" anchors on the full name: all columns
  w <- contrast_weights(column_contrast("^cond_cond", name = "x"), term)$weights[, 1]
  expect_true(all(w > 0))
  # "A_b01$" is end-anchored: only the A/b01 column in either namespace
  w <- contrast_weights(column_contrast("A_b01$", name = "x"), term)$weights[, 1]
  expect_equal(unname(w), c(1, 0, 0, 0, 0, 0))
  # "^cond\\.A" matches no full name (full names start with "cond_"), so the
  # term-level fallback applies and selects cond.A's three basis columns.
  w <- contrast_weights(column_contrast("^cond\\.A", name = "x"), term)$weights[, 1]
  expect_equal(unname(w), c(1, 1, 1, 0, 0, 0) / 3)
  expect_identical(rn, conditions(term, expand_basis = TRUE))
})

test_that("no-match warning lists the available column names", {
  ev <- .cc_events()
  em <- event_model(onset ~ hrf(cond), data = ev, block = ~run,
                    sampling_frame = .cc_sf())
  term <- terms(em)[[1]]
  msgs <- character()
  expect_error(
    withCallingHandlers(
      contrast_weights(column_contrast("^nope$", name = "none"), term),
      warning = function(w) {
        msgs <<- c(msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    "No conditions were selected"
  )
  expect_length(msgs, 1)
  expect_match(msgs, "cond_cond.A", fixed = TRUE)
  expect_match(msgs, "cond_cond.B", fixed = TRUE)
  expect_match(msgs, "term-level condition names", fixed = TRUE)
  expect_match(msgs, "cond.A, cond.B", fixed = TRUE)

  # Long candidate lists are truncated.
  ev2 <- data.frame(onset = seq(10, 190, by = 10),
                    lev = factor(sprintf("L%02d", 1:19)), run = 1)
  em2 <- event_model(onset ~ hrf(lev), data = ev2, block = ~run,
                     sampling_frame = .cc_sf())
  msgs <- character()
  expect_error(withCallingHandlers(
    contrast_weights(column_contrast("^nope$", name = "none"), terms(em2)[[1]]),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  ))
  expect_match(msgs, "lev_lev.L01", fixed = TRUE)
  expect_false(grepl("lev_lev.L19", msgs, fixed = TRUE))
  expect_match(msgs, "and 9 more", fixed = TRUE)
})

test_that("a pattern that selects different columns in the two namespaces is an error", {
  ev <- .cc_events()
  em <- event_model(onset ~ hrf(task, id = "hf"), data = ev, block = ~run,
                    sampling_frame = .cc_sf())
  term <- terms(em)[[1]]
  # "h" matches both full names (hf_task.face, hf_task.house) but only the
  # term-level name task.house: ambiguous, so refuse rather than guess.
  expect_error(
    contrast_weights(column_contrast("h", name = "amb"), term),
    "ambiguous"
  )
  # Patterns that agree across namespaces are fine.
  w <- contrast_weights(column_contrast("house", name = "ok"), term)$weights[, 1]
  expect_equal(unname(w), c(0, 1))
})

test_that("column_contrast on an untagged event_term matches unprefixed names", {
  term <- event_term(list(cond = factor(c("A", "B", "A"))),
                     onsets = c(0, 10, 20), blockids = c(1, 1, 1))
  w <- contrast_weights(column_contrast("^cond\\.A$", "^cond\\.B$", name = "x"),
                        term)$weights[, 1]
  expect_equal(unname(w), c(1, -1))
})
