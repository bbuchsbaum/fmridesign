skip_if_not(
  exists("feature_regressor", asNamespace("fmrihrf"), inherits = FALSE),
  "fmrihrf::feature_regressor is required"
)
has_durations <- "durations" %in% names(formals(fmrihrf::feature_regressor))

local_edition(3)

# A mixed model: one categorical hrf() term and a 3-column, 2-basis feature
# term whose columns fall into two sub-bands. The series covers the run.
.banded_model <- function(...) {
  dt <- 0.5
  t <- seq(0, 39.5, by = dt)
  phon <- cbind(stop = abs(sin(t)), nasal = abs(cos(t)),
                voiced = abs(sin(2 * t)))
  des <- data.frame(onset = c(2, 10, 20, 30), run = 1,
                    cond = factor(c("A", "B", "A", "B")))
  sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
  event_model(
    onset ~ hrf(cond) +
      feature(phon, dt = dt, id = "phon", basis = "spmg2",
              band = "phonological",
              subband = c("manner", "manner", "voicing"), ...),
    data = des, block = ~run, sampling_frame = sframe
  )
}

phon_cols <- c("phon_stop_b01", "phon_stop_b02", "phon_nasal_b01",
               "phon_nasal_b02", "phon_voiced_b01", "phon_voiced_b02")


# --- durations ---------------------------------------------------------------

test_that("durations pass through to an event-timed feature_regressor", {
  skip_if_not(has_durations, "fmrihrf::feature_regressor() lacks `durations`")
  onsets <- c(1, 4.5, 4.5, 30)
  durs <- c(0.4, 2, 0, 1)
  surprisal <- c(2, 7, 4, 3)
  sframe <- fmrihrf::sampling_frame(blocklens = 60, TR = 1)

  expect_no_warning(
    emod <- event_model(
      ~ feature(surprisal, times = onsets, durations = durs, id = "surp",
                center = FALSE, scale = "none"),
      sampling_frame = sframe, precision = 0.1
    )
  )
  expect_equal(colnames(design_matrix(emod)), "surp_surp")

  grid <- fmrihrf::samples(sframe, global = FALSE)
  ev <- fmrihrf::regressor(onsets, fmrihrf::HRF_SPMG1, duration = durs,
                           amplitude = surprisal)
  expect_equal(as.numeric(design_matrix(emod)[[1]]),
               as.numeric(fmrihrf::evaluate(ev, grid, precision = 0.1)),
               tolerance = 1e-10)

  term <- terms(emod)[[1]]
  expect_equal(term$durations, list(durs))
  expect_message(print(term), "explicit durations")
})

test_that("an event-timed feature is not held across a pause", {
  skip_if_not(has_durations, "fmrihrf::feature_regressor() lacks `durations`")
  onsets <- c(0, 40)
  sframe <- fmrihrf::sampling_frame(blocklens = 60, TR = 1)
  build <- function(...) {
    suppressWarnings(event_model(
      ~ feature(c(1, 1), times = onsets, id = "w", center = FALSE,
                scale = "none", ...),
      sampling_frame = sframe, precision = 0.1
    ))
  }
  grid <- fmrihrf::samples(sframe, global = FALSE)
  pause <- grid > 30 & grid < 40
  events <- design_matrix(build(durations = 0.5))[[1]]
  held <- design_matrix(build())[[1]]
  expect_equal(events[pause], rep(0, sum(pause)))
  expect_true(all(abs(held[pause]) > 0.1))
})

test_that("durations accept a scalar and a per-run list", {
  skip_if_not(has_durations, "fmrihrf::feature_regressor() lacks `durations`")
  sframe2 <- fmrihrf::sampling_frame(blocklens = c(30, 30), TR = 1)
  x <- list(c(1, 2, 3), c(4, 5))
  times <- list(c(1, 5, 9), c(2, 6))
  emod <- event_model(
    ~ feature(x, times = times, durations = list(c(0.5, 1, 0), 2), id = "w",
              center = FALSE, scale = "none"),
    sampling_frame = sframe2
  )
  expect_equal(terms(emod)[[1]]$durations, list(c(0.5, 1, 0), c(2, 2)))

  emod_s <- event_model(
    ~ feature(x, times = times, durations = 0, id = "w",
              center = FALSE, scale = "none"),
    sampling_frame = sframe2
  )
  expect_equal(terms(emod_s)[[1]]$durations, list(c(0, 0, 0), c(0, 0)))

  # run 2 rows depend only on run 2 events
  grid <- fmrihrf::samples(sframe2, global = FALSE)[31:60]
  ev <- fmrihrf::regressor(c(2, 6), fmrihrf::HRF_SPMG1, duration = 2,
                           amplitude = c(4, 5))
  expect_equal(as.numeric(design_matrix(emod)[[1]][31:60]),
               as.numeric(fmrihrf::evaluate(ev, grid, precision = 0.3)),
               tolerance = 1e-10)
})

test_that("durations validation", {
  skip_if_not(has_durations, "fmrihrf::feature_regressor() lacks `durations`")
  sframe <- fmrihrf::sampling_frame(blocklens = 30, TR = 1)
  sframe2 <- fmrihrf::sampling_frame(blocklens = c(30, 30), TR = 1)
  x <- c(1, 2, 3)
  expect_error(feature(x, dt = 1, durations = 0.5), "only be used with `times`")
  expect_error(
    event_model(~ feature(x, times = c(1, 2, 3), durations = c(1, 2), id = "w"),
                sampling_frame = sframe),
    "single number or have 3 entries"
  )
  expect_error(
    event_model(~ feature(x, times = c(1, 2, 3), durations = -1, id = "w"),
                sampling_frame = sframe),
    "finite and non-negative"
  )
  expect_error(
    event_model(~ feature(x, times = c(1, 2, 3), durations = list(1, 1),
                          id = "w"),
                sampling_frame = sframe),
    "list of length 2"
  )
  expect_error(
    event_model(~ feature(list(x, x[1:2]), times = list(1:3, 1:2),
                          durations = c(1, 2, 3), id = "w"),
                sampling_frame = sframe2),
    "list of duration vectors"
  )
  # ties are simultaneous events with durations, an error without
  expect_error(
    event_model(~ feature(x, times = c(1, 1, 3), id = "w"),
                sampling_frame = sframe),
    "strictly increasing"
  )
  expect_error(
    event_model(~ feature(x, times = c(3, 1, 2), durations = 0, id = "w"),
                sampling_frame = sframe),
    "non-decreasing"
  )
})


# --- bands in the column metadata --------------------------------------------

test_that("existing column names are unchanged by bands and contrasts", {
  emod <- .banded_model()
  expect_equal(colnames(design_matrix(emod)),
               c("cond_cond.A", "cond_cond.B", phon_cols))
})

test_that("design_colmap exposes band and subband for all term types", {
  cm <- design_colmap(.banded_model())
  expect_true(all(c("band", "subband") %in% names(cm)))
  expect_equal(cm$band, c("cond", "cond", rep("phonological", 6)))
  expect_equal(cm$subband,
               c(NA, NA, rep(c("manner", "manner", "voicing"), each = 2)))

  # default band is the term tag; no sub-band
  dt <- 0.5
  rms <- abs(sin(seq(0, 19.5, by = dt)))
  sframe <- fmrihrf::sampling_frame(blocklens = 20, TR = 1)
  cm0 <- design_colmap(event_model(
    ~ feature(rms, dt = dt, id = "rms"), sampling_frame = sframe
  ))
  expect_equal(cm0$band, "rms")
  expect_equal(cm0$subband, NA_character_)

  # a single string labels every column
  cm1 <- design_colmap(event_model(
    ~ feature(cbind(rms, rev(rms)), dt = dt, id = "env", subband = "energy"),
    sampling_frame = sframe
  ))
  expect_equal(cm1$band, c("env", "env"))
  expect_equal(cm1$subband, c("energy", "energy"))

  bm <- design_colmap(baseline_model(basis = "poly", degree = 2,
                                     sframe = sframe))
  expect_equal(bm$band, bm$term_tag)
  expect_true(all(is.na(bm$subband)))
})

test_that("the legacy colmap fallback also carries bands", {
  emod <- .banded_model()
  fast <- design_colmap(emod)
  attr(emod$design_matrix, "col_metadata") <- NULL
  slow <- design_colmap(emod)
  expect_equal(slow$band, fast$band)
  expect_equal(slow$subband, fast$subband)
})

test_that("band and subband validation", {
  x <- cbind(a = 1:4, b = 4:1)
  sframe <- fmrihrf::sampling_frame(blocklens = 4, TR = 1)
  expect_error(feature(x, dt = 1, band = c("a", "b")), "band")
  expect_error(feature(x, dt = 1, band = ""), "band")
  expect_error(feature(x, dt = 1, subband = 1:2), "subband")
  expect_error(feature(x, dt = 1, subband = c("a", NA)), "subband")
  expect_error(
    event_model(~ feature(x, dt = 1, id = "x", subband = c("p", "q", "r")),
                sampling_frame = sframe),
    "`subband` has 3 entries but the feature has 2 column"
  )
})


# --- design_bands ------------------------------------------------------------

test_that("design_bands returns the banded design for a mixed model", {
  emod <- .banded_model()
  b <- design_bands(emod)
  expect_named(b, c("X", "band", "subband", "families", "colmap"))
  expect_true(is.matrix(b$X) && is.numeric(b$X))
  expect_equal(colnames(b$X), colnames(design_matrix(emod)))
  expect_equal(unname(b$X), unname(as.matrix(design_matrix(emod))))
  expect_equal(b$band, c("cond", "cond", rep("phonological", 6)))
  expect_equal(b$subband,
               c("cond", "cond", rep(c("manner", "manner", "voicing"), each = 2)))
  expect_equal(b$families, list(phonological = c("manner", "voicing")))
  expect_equal(b$colmap, design_colmap(emod))
})

test_that("design_bands restricts to chosen bands or terms", {
  emod <- .banded_model()
  b <- design_bands(emod, bands = "phonological")
  expect_equal(colnames(b$X), phon_cols)
  expect_equal(b$band, rep("phonological", 6))
  expect_equal(b$colmap$col, 3:8)
  expect_equal(nrow(b$colmap), ncol(b$X))

  b2 <- design_bands(emod, terms = "cond")
  expect_equal(colnames(b2$X), c("cond_cond.A", "cond_cond.B"))
  expect_equal(b2$subband, c("cond", "cond"))
  expect_equal(b2$families, structure(list(), names = character(0)))

  expect_equal(ncol(design_bands(emod, terms = "cond",
                                 bands = "phonological")$X), 0L)
  expect_error(design_bands(emod, bands = "nope"), "Unknown bands: nope")
  expect_error(design_bands(emod, terms = "nope"), "Unknown terms: nope")
})


# --- F-contrasts -------------------------------------------------------------

test_that("a feature term has an omnibus F and one F per sub-band", {
  emod <- .banded_model()
  term <- terms(emod)[["phon"]]
  expect_equal(fmridesign:::.term_nbasis(term), 2L)

  fc_term <- Fcontrasts(term)
  expect_named(fc_term, c("phon", "manner", "voicing"))
  expect_equal(dim(fc_term$phon), c(3L, 3L))
  expect_equal(rownames(fc_term$manner), c("stop", "nasal", "voiced"))
  expect_equal(unname(fc_term$manner), rbind(c(1, 0), c(0, 1), c(0, 0)))
  expect_equal(unname(fc_term$voicing), rbind(0, 0, 1))

  fc <- Fcontrasts(emod)
  expect_named(fc, c("cond#cond", "phon#phon", "phon#manner", "phon#voicing"))
  dm_cols <- colnames(design_matrix(emod))

  tested <- function(M) rownames(M)[rowSums(abs(M)) > 0]
  for (nm in names(fc)[-1]) {
    expect_equal(rownames(fc[[nm]]), dm_cols)
    expect_equal(attr(fc[[nm]], "term_indices"), 3:8)
  }
  # one contrast column per tested design column, each picking exactly one
  expect_equal(dim(fc[["phon#phon"]]), c(8L, 6L))
  expect_equal(tested(fc[["phon#phon"]]), phon_cols)
  expect_equal(unname(fc[["phon#phon"]][3:8, ]), diag(6))

  expect_equal(dim(fc[["phon#manner"]]), c(8L, 4L))
  expect_equal(tested(fc[["phon#manner"]]), phon_cols[1:4])
  expect_equal(unname(fc[["phon#manner"]][3:6, ]), diag(4))

  expect_equal(dim(fc[["phon#voicing"]]), c(8L, 2L))
  expect_equal(tested(fc[["phon#voicing"]]), phon_cols[5:6])
  expect_equal(unname(fc[["phon#voicing"]][7:8, ]), diag(2))
})

test_that("a feature term without sub-bands has only the omnibus F", {
  dt <- 0.5
  rms <- abs(sin(seq(0, 19.5, by = dt)))
  sframe <- fmrihrf::sampling_frame(blocklens = 20, TR = 1)
  emod <- event_model(~ feature(rms, dt = dt, id = "rms"),
                      sampling_frame = sframe)
  fc <- Fcontrasts(emod)
  expect_named(fc, "rms#rms")
  expect_equal(dim(fc[[1]]), c(1L, 1L))
  expect_equal(rownames(fc[[1]]), "rms_rms")
  expect_equal(unname(fc[[1]]), matrix(1), ignore_attr = TRUE)
})


# --- user contrasts ----------------------------------------------------------

test_that("a column_contrast on a feature term resolves", {
  cset <- contrast_set(
    column_contrast("^phon_stop_b01$", "^phon_nasal_b01$",
                    name = "stop_vs_nasal"),
    column_contrast("_b01$", name = "canonical_mean")
  )
  emod <- .banded_model(contrasts = cset)
  expect_named(contrasts(terms(emod)[["phon"]]),
               c("stop_vs_nasal", "canonical_mean"))

  cw <- contrast_weights(emod)
  expect_named(cw, c("phon#stop_vs_nasal", "phon#canonical_mean"))

  w <- cw[["phon#stop_vs_nasal"]]$offset_weights
  expect_equal(rownames(w), colnames(design_matrix(emod)))
  expect_equal(as.numeric(w), c(0, 0, 1, 0, -1, 0, 0, 0))

  w2 <- cw[["phon#canonical_mean"]]$offset_weights
  expect_equal(as.numeric(w2), c(0, 0, 1, 0, 1, 0, 1, 0) / 3)

  # a single contrast_spec is accepted, as in hrf()
  spec <- feature(1:4, dt = 1, id = "x",
                  contrasts = column_contrast("x", name = "x_on"))
  expect_named(spec$contrasts, "x_on")
  expect_error(feature(1:4, dt = 1, contrasts = "nope"), "contrast_spec")
})


# --- feature names that collide with naming conventions ----------------------

test_that("a feature named like a basis suffix still gets a full F-contrast", {
  sframe <- fmrihrf::sampling_frame(blocklens = 60, TR = 1)
  set.seed(11)
  x <- rnorm(60)
  emod <- event_model(
    ~ feature(cbind(energy_b01 = x), dt = 1, id = "env", basis = "spmg2"),
    sampling_frame = sframe
  )
  expect_equal(colnames(design_matrix(emod)),
               c("env_energy_b01_b01", "env_energy_b01_b02"))
  expect_no_warning(Fc <- Fcontrasts(emod))
  omnibus <- unclass(as.matrix(Fc[["env#env"]]))
  expect_equal(dim(omnibus), c(2L, 2L))
  expect_equal(unname(omnibus[, , drop = FALSE]), diag(2), ignore_attr = TRUE)
  expect_equal(rownames(omnibus), colnames(design_matrix(emod)))
})

test_that("feature contrast weights are placed by term-level name only", {
  sframe <- fmrihrf::sampling_frame(blocklens = 60, TR = 1)
  set.seed(12)
  x <- rnorm(60)
  # the term-level name of the second column ("env_a") equals the full
  # design-matrix name of the first
  emod <- event_model(
    ~ feature(cbind(a = x, env_a = rev(x)), dt = 1, id = "env",
              subband = c("one", "two"),
              contrasts = column_contrast("^a$", "^env_env_a$", name = "diff")),
    sampling_frame = sframe
  )
  expect_equal(colnames(design_matrix(emod)), c("env_a", "env_env_a"))
  expect_no_warning(cw <- contrast_weights(emod))
  expect_equal(cw[["env#diff"]]$offset_weights[, "diff"],
               c(env_a = 1, env_env_a = -1))

  expect_no_warning(Fc <- Fcontrasts(emod))
  expect_equal(as.numeric(as.matrix(Fc[["env#one"]])), c(1, 0))
  expect_equal(as.numeric(as.matrix(Fc[["env#two"]])), c(0, 1))
  expect_equal(unname(unclass(as.matrix(Fc[["env#env"]]))[, , drop = FALSE]),
               diag(2), ignore_attr = TRUE)
})
