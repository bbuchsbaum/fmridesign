test_that("subsetted terms return consistent local and offset contrast weights", {
  set.seed(17)
  trial_onsets <- c(5, 25, 45, 65)
  events <- data.frame(
    run = 1L,
    onset = as.vector(rbind(
      trial_onsets,
      trial_onsets + 4,
      trial_onsets + 10
    )),
    duration = rep(c(4, 6, 2), times = length(trial_onsets)),
    phase = factor(
      rep(c("a", "b", "c"), times = length(trial_onsets)),
      levels = c("a", "b", "c")
    ),
    modulator = rep(
      scale(rnorm(length(trial_onsets)), scale = FALSE)[, 1],
      each = 3
    )
  )
  sframe <- sampling_frame(blocklens = 100, TR = 1)
  pm_contrasts <- contrast_set(
    column_contrast("^phase\\.a_", name = "pm_a"),
    column_contrast("^phase\\.b_", name = "pm_b")
  )
  model <- event_model(
    onset ~ hrf(phase, basis = "spmg1") +
      hrf(
        phase,
        modulator,
        basis = "spmg1",
        prefix = "pm",
        subset = phase != "c",
        contrasts = pm_contrasts
      ),
    data = events,
    block = ~run,
    sampling_frame = sframe,
    durations = events$duration
  )

  warnings <- character()
  weights <- withCallingHandlers(
    contrast_weights(model),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 2L)
  expect_true(all(grepl("phase.c_modulator", warnings, fixed = TRUE)))

  for (name in c("pm#pm_a", "pm#pm_b")) {
    entry <- weights[[name]]
    term_indices <- attr(entry, "term_indices")
    reconciled_local <- entry$offset_weights[
      term_indices,
      ,
      drop = FALSE
    ]

    expect_equal(nrow(entry$weights), length(term_indices))
    expect_equal(unname(entry$weights), unname(reconciled_local))
    expect_false(any(grepl("phase.c_modulator", rownames(entry$weights), fixed = TRUE)))
  }
})
