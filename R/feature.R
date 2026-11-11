#' @keywords internal
#' @noRd
is_timing_spec <- function(spec) {
  inherits(spec, "hrfspec") &&
    !inherits(spec, "featurespec") &&
    !inherits(spec, "covariatespec")
}


#' Continuous Feature Term
#'
#' Specify a continuously sampled feature (for example RMS energy) to be
#' convolved with an HRF via [fmrihrf::feature_regressor()]. Unlike [hrf()],
#' this is a time series, not a list of trials.
#'
#' `x` may be a numeric vector or a samples-by-features matrix (one run), or a
#' list of those with one element per `sampling_frame` block. Supply exactly
#' one of `dt` or `times`. Feature-only models are allowed:
#' `event_model(~ feature(x, dt = 0.1, id = "rms"), sampling_frame = sframe)`.
#'
#' A series given with `dt` or `times` alone is a zero-order hold: each value
#' lasts until the next sample. If such a series stops more than one TR
#' before the run does, the scans after its end get no contribution from it (as if the feature
#' were 0 there, after centering), and a warning says so.
#'
#' @section Event-timed features:
#' For a sparse feature tied to events (word surprisal, per-phone features),
#' supply `times` (onsets) and `durations`. Row `i` of `x` is then an event
#' starting at `times[i]` and lasting `durations[i]` seconds (`0` is an
#' impulse) with amplitude `x[i, ]`, and no value is held across the gap to
#' the next event. `times` need only be non-decreasing, and events may
#' overlap (their responses add). `center` and `scale` act across the events
#' of a run, the parametric-modulator convention. See
#' [fmrihrf::feature_regressor()].
#'
#' @section Bands, sub-bands and contrasts:
#' `band` and `subband` label the columns of the term for banded (grouped)
#' models; they are recorded in the `band` and `subband` columns of
#' [design_colmap()] and collected by [design_bands()]. Several terms may
#' share a band.
#'
#' `Fcontrasts()` gives a feature term an omnibus F-contrast over all of its
#' columns, and one F-contrast per declared sub-band. Each tests its feature
#' columns jointly in every basis function. `contrasts` attaches contrast
#' specifications to the term, as in [hrf()]. Feature terms have no
#' categorical cells, so use [column_contrast()], whose patterns select
#' design-matrix columns by name (for example
#' `column_contrast("^env_alpha$", "^env_beta$", name = "a_vs_b")`).
#'
#' @param x Numeric vector, matrix (samples x features), or a list of those
#'   (one per run).
#' @param dt Positive sampling interval in seconds. Mutually exclusive with
#'   `times`.
#' @param times Sample times in seconds, or a list of per-run time vectors.
#'   Mutually exclusive with `dt`.
#' @param durations Optional event durations in seconds, only valid with
#'   `times`: a single number, a vector with one entry per sample, or a list
#'   of those with one element per run (parallel to `times`). `NULL` (default)
#'   keeps the zero-order hold. Event durations are supported by fmrihrf
#'   (>= 0.5.0), the minimum required version.
#' @param start Start time in seconds used only when `dt` is supplied.
#'   Defaults to 0.
#' @param center,scale,mask,span Passed to [fmrihrf::feature_regressor()].
#'   Centering and scaling are applied per run.
#' @param basis,nbasis,lag HRF specification, as in [hrf()].
#' @param precision Optional evaluation precision in seconds. When `NULL`
#'   (default), the event-model precision is used, then tightened to
#'   `min(precision, dt)`.
#' @param id,name,prefix Term identifier used for column names. If omitted,
#'   the name of `x` is used.
#' @param band Optional single string naming the feature band this term
#'   belongs to. Defaults to the term's tag (its `id`).
#' @param subband Optional sub-band labels: `NULL` (none), a single string
#'   (every feature column), or a character vector with one entry per feature
#'   column of `x`.
#' @param contrasts Optional contrast specification(s) for the term: a
#'   `contrast_spec`, a [contrast_set()], or a list of `contrast_spec`
#'   objects, as in [hrf()].
#'
#' @return A `featurespec` object for use on the RHS of an [event_model()]
#'   formula or in the list interface.
#'
#' @examples
#' dt <- 0.5
#' rms <- abs(sin(seq(0, 29.5, by = dt)))
#' sframe <- sampling_frame(blocklens = 15, TR = 2)
#' emod <- event_model(
#'   ~ feature(rms, dt = dt, id = "rms", center = FALSE, scale = "none"),
#'   sampling_frame = sframe
#' )
#' ncol(design_matrix(emod))
#'
#' # Two-column feature with sub-bands and a column contrast
#' env <- cbind(low = rms, high = rev(rms))
#' emod2 <- event_model(
#'   ~ feature(env, dt = dt, id = "env", band = "acoustic",
#'             subband = c("low", "high"),
#'             contrasts = column_contrast("^env_low$", "^env_high$",
#'                                         name = "low_vs_high")),
#'   sampling_frame = sframe
#' )
#' names(Fcontrasts(emod2))
#' names(contrast_weights(emod2))
#' design_bands(emod2)$families
#'
#' @seealso [fmrihrf::feature_regressor()], [hrf()], [event_model()],
#'   [design_bands()]
#' @export
feature <- function(x,
                    dt = NULL,
                    times = NULL,
                    start = 0,
                    center = TRUE,
                    scale = c("none", "sd"),
                    mask = NULL,
                    basis = "spmg1",
                    nbasis = 1,
                    lag = 0,
                    span = NULL,
                    precision = NULL,
                    id = NULL,
                    name = NULL,
                    prefix = NULL,
                    durations = NULL,
                    band = NULL,
                    subband = NULL,
                    contrasts = NULL) {
  scale <- match.arg(scale)
  x_label <- paste(deparse(substitute(x), width.cutoff = 500L), collapse = "")

  if (missing(x)) {
    stop("`feature()` requires `x` (a numeric vector, matrix, or per-run list).",
         call. = FALSE)
  }
  if (is.null(dt) == is.null(times)) {
    stop("Supply exactly one of `dt` or `times`.", call. = FALSE)
  }
  is_finite_scalar <- function(v) is.numeric(v) && length(v) == 1L && is.finite(v)
  if (!is.null(dt) && !(is_finite_scalar(dt) && dt > 0)) {
    stop("`dt` must be a single positive finite number.", call. = FALSE)
  }
  if (!(is_finite_scalar(start) && start >= 0)) {
    stop("`start` must be a single non-negative finite number.", call. = FALSE)
  }
  if (!is.logical(center) || length(center) != 1L || is.na(center)) {
    stop("`center` must be a single logical value.", call. = FALSE)
  }
  if (!is.null(precision) && !(is_finite_scalar(precision) && precision > 0)) {
    stop("`precision` must be a single positive finite number.", call. = FALSE)
  }
  if (!is.null(durations) && is.null(times)) {
    stop("`durations` can only be used with `times`.", call. = FALSE)
  }
  if (!is.null(band) &&
      !(is.character(band) && length(band) == 1L && !is.na(band) && nzchar(band))) {
    stop("`band` must be a single non-empty string.", call. = FALSE)
  }
  if (!is.null(subband) &&
      !(is.character(subband) && length(subband) >= 1L && !anyNA(subband) &&
        all(nzchar(subband)))) {
    stop("`subband` must be a character vector of non-empty strings.",
         call. = FALSE)
  }
  contrasts <- .validate_contrasts_arg(contrasts)

  final_id <- id %||% name
  if (is.null(final_id)) {
    final_id <- sanitize(x_label, allow_dot = FALSE)
    if (!nzchar(final_id) || identical(final_id, "x")) {
      final_id <- "feature"
    }
  } else {
    if (!is.character(final_id) || length(final_id) != 1L || !nzchar(final_id)) {
      stop("`id` / `name` must be a single non-empty string.", call. = FALSE)
    }
    final_id <- sanitize(final_id, allow_dot = FALSE)
  }

  hrf_obj <- make_hrf(basis, lag, nbasis = if (missing(nbasis)) NULL else nbasis,
                      label = .basis_fun_label(substitute(basis)))

  ret <- list(
    name = final_id,
    label = sprintf("feature(%s)", x_label),
    id = final_id,
    prefix = prefix,
    vars = list(),
    varnames = final_id,
    x = x,
    x_label = x_label,
    dt = dt,
    times = times,
    durations = durations,
    start = start,
    center = center,
    scale = scale,
    mask = mask,
    hrf = hrf_obj,
    span = span,
    precision = precision,
    band = band,
    subband = subband,
    contrasts = contrasts,
    hrf_fun = NULL,
    summate = TRUE,
    normalize = FALSE
  )
  class(ret) <- c("featurespec", "hrfspec", "list")
  ret
}


#' @keywords internal
#' @noRd
.as_feature_matrix <- function(x, id) {
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }
  if (is.numeric(x) && is.null(dim(x))) {
    x <- matrix(as.numeric(x), ncol = 1L)
  }
  if (!is.matrix(x) || !is.numeric(x)) {
    stop(sprintf(
      "feature '%s': each run must be a numeric vector or matrix.",
      id
    ), call. = FALSE)
  }
  if (nrow(x) == 0L || ncol(x) == 0L) {
    stop(sprintf("feature '%s': `x` must have at least one sample.", id),
         call. = FALSE)
  }
  if (anyNA(x) || any(!is.finite(x))) {
    stop(sprintf("feature '%s': `x` must contain finite numeric values.", id),
         call. = FALSE)
  }
  storage.mode(x) <- "double"
  x
}


#' @keywords internal
#' @noRd
.normalize_feature_x <- function(x, n_blocks, id) {
  if (is.list(x) && !is.data.frame(x)) {
    if (length(x) != n_blocks) {
      stop(sprintf(
        "feature '%s': `x` is a list of length %d but sampling_frame has %d block(s).",
        id, length(x), n_blocks
      ), call. = FALSE)
    }
    mats <- lapply(x, .as_feature_matrix, id = id)
  } else {
    if (n_blocks != 1L) {
      stop(sprintf(
        paste0(
          "feature '%s': a single vector/matrix is only valid for a 1-block ",
          "sampling_frame; got %d blocks. Pass a list with one series per block."
        ),
        id, n_blocks
      ), call. = FALSE)
    }
    mats <- list(.as_feature_matrix(x, id))
  }

  n_feat <- ncol(mats[[1L]])
  if (!all(vapply(mats, ncol, integer(1)) == n_feat)) {
    stop(sprintf(
      "feature '%s': all per-run matrices must have the same number of columns.",
      id
    ), call. = FALSE)
  }

  cn <- colnames(mats[[1L]])
  if (is.null(cn)) {
    cn <- if (n_feat == 1L) id else feature_suffix(seq_len(n_feat), n_feat)
  } else {
    cn <- sanitize(cn, allow_dot = TRUE)
  }
  mats <- lapply(mats, function(m) {
    colnames(m) <- cn
    m
  })
  list(values = mats, cond_tags = cn)
}


#' @keywords internal
#' @noRd
.normalize_feature_times <- function(times, n_blocks, n_samples, id,
                                     strictly = TRUE) {
  if (is.null(times)) {
    return(NULL)
  }

  as_times <- function(tvec, n, block_lab) {
    tvec <- as.numeric(tvec)
    if (length(tvec) != n) {
      stop(sprintf(
        "feature '%s' %s: `times` length %d does not match %d samples.",
        id, block_lab, length(tvec), n
      ), call. = FALSE)
    }
    if (anyNA(tvec) || any(!is.finite(tvec))) {
      stop(sprintf("feature '%s' %s: `times` must be finite.", id, block_lab),
           call. = FALSE)
    }
    if (any(tvec < 0)) {
      stop(sprintf(
        "feature '%s' %s: `times` must be non-negative.",
        id, block_lab
      ), call. = FALSE)
    }
    if (is.unsorted(tvec, strictly = strictly)) {
      stop(sprintf(
        "feature '%s' %s: `times` must be %s.",
        id, block_lab,
        if (strictly) "strictly increasing" else "non-decreasing"
      ), call. = FALSE)
    }
    tvec
  }

  if (is.list(times)) {
    if (length(times) != n_blocks) {
      stop(sprintf(
        "feature '%s': `times` is a list of length %d but sampling_frame has %d block(s).",
        id, length(times), n_blocks
      ), call. = FALSE)
    }
    return(Map(as_times, times, n_samples, sprintf("block %d", seq_len(n_blocks))))
  }

  if (length(unique(n_samples)) != 1L) {
    stop(sprintf(
      paste0(
        "feature '%s': a single `times` vector requires every run to have ",
        "the same number of samples; otherwise pass a list of time vectors."
      ),
      id
    ), call. = FALSE)
  }
  tvec <- as_times(times, n_samples[[1L]], "times")
  replicate(n_blocks, tvec, simplify = FALSE)
}


#' Per-run event durations for an event-timed feature
#'
#' @return `NULL`, or a list with one numeric vector per block, each as long as
#'   that block's series.
#' @keywords internal
#' @noRd
.normalize_feature_durations <- function(durations, n_blocks, n_samples, id) {
  if (is.null(durations)) {
    return(NULL)
  }
  if (!"durations" %in% names(formals(fmrihrf::feature_regressor))) {
    stop(sprintf(
      paste0(
        "feature '%s': `durations` needs fmrihrf::feature_regressor() to have ",
        "a `durations` argument; the installed fmrihrf (%s) does not. ",
        "Update fmrihrf."
      ),
      id, as.character(utils::packageVersion("fmrihrf"))
    ), call. = FALSE)
  }

  as_durations <- function(d, n, block_lab) {
    if (!is.numeric(d) || !(length(d) %in% c(1L, n))) {
      stop(sprintf(
        "feature '%s' %s: `durations` must be a single number or have %d entries.",
        id, block_lab, n
      ), call. = FALSE)
    }
    if (anyNA(d) || any(!is.finite(d)) || any(d < 0)) {
      stop(sprintf(
        "feature '%s' %s: `durations` must be finite and non-negative.",
        id, block_lab
      ), call. = FALSE)
    }
    rep_len(as.numeric(d), n)
  }

  if (is.list(durations)) {
    if (length(durations) != n_blocks) {
      stop(sprintf(
        "feature '%s': `durations` is a list of length %d but sampling_frame has %d block(s).",
        id, length(durations), n_blocks
      ), call. = FALSE)
    }
    return(Map(as_durations, durations, n_samples,
               sprintf("block %d", seq_len(n_blocks))))
  }

  if (length(durations) != 1L && length(unique(n_samples)) != 1L) {
    stop(sprintf(
      paste0(
        "feature '%s': a single `durations` vector requires every run to have ",
        "the same number of samples; otherwise pass a list of duration vectors."
      ),
      id
    ), call. = FALSE)
  }
  Map(as_durations, rep(list(durations), n_blocks), n_samples, "durations")
}


#' Band and per-column sub-band labels of a feature term
#'
#' @return A list with `band` (`NULL` or a string) and `subband` (one entry per
#'   feature column; `NA` where none was declared).
#' @keywords internal
#' @noRd
.normalize_feature_bands <- function(band, subband, cond_tags, id) {
  n_feat <- length(cond_tags)
  if (is.null(subband)) {
    subband <- rep(NA_character_, n_feat)
  } else if (length(subband) == 1L) {
    subband <- rep(subband, n_feat)
  } else if (length(subband) != n_feat) {
    stop(sprintf(
      "feature '%s': `subband` has %d entries but the feature has %d column(s).",
      id, length(subband), n_feat
    ), call. = FALSE)
  }
  list(band = band, subband = as.character(subband))
}


#' @keywords internal
#' @noRd
.normalize_feature_mask <- function(mask, n_blocks, n_samples, id) {
  if (is.null(mask)) {
    return(NULL)
  }

  as_mask <- function(m, n, block_lab) {
    if (!is.logical(m) || length(m) != n || anyNA(m)) {
      stop(sprintf(
        "feature '%s' %s: `mask` must be a non-missing logical vector of length %d.",
        id, block_lab, n
      ), call. = FALSE)
    }
    if (!any(m)) {
      stop(sprintf(
        "feature '%s' %s: `mask` must contain at least one TRUE value.",
        id, block_lab
      ), call. = FALSE)
    }
    m
  }

  if (is.list(mask)) {
    if (length(mask) != n_blocks) {
      stop(sprintf(
        "feature '%s': `mask` is a list of length %d but sampling_frame has %d block(s).",
        id, length(mask), n_blocks
      ), call. = FALSE)
    }
    return(Map(as_mask, mask, n_samples, sprintf("block %d", seq_len(n_blocks))))
  }

  if (n_blocks != 1L) {
    stop(sprintf(
      "feature '%s': for multiple runs, `mask` must be a list (one logical vector per block).",
      id
    ), call. = FALSE)
  }
  list(as_mask(mask, n_samples[[1L]], "mask"))
}


#' @keywords internal
#' @noRd
.feature_block_times <- function(n, dt, start, times) {
  if (!is.null(times)) {
    return(times)
  }
  start + seq(0, by = dt, length.out = n)
}


#' @keywords internal
#' @noRd
.validate_feature_alignment <- function(values, times, dt, start, sampling_frame,
                                        id) {
  bl <- fmrihrf::blocklens(sampling_frame)
  TR <- sampling_frame$TR
  if (is.null(TR)) {
    return(invisible(NULL))
  }
  if (length(TR) == 1L) {
    TR <- rep(TR, length(bl))
  }
  if (length(TR) != length(bl)) {
    return(invisible(NULL))
  }
  run_end <- bl * TR

  for (b in seq_along(values)) {
    n <- nrow(values[[b]])
    tvec <- .feature_block_times(n, dt, start, if (is.null(times)) NULL else times[[b]])
    n_after <- sum(tvec >= run_end[[b]])
    if (n_after > 0L) {
      warning(sprintf(
        "feature '%s' block %d: %d sample(s) at or after run end (%.1f s).",
        id, b, n_after, run_end[[b]]
      ), call. = FALSE)
    }
  }
  invisible(NULL)
}


#' @method construct featurespec
#' @rdname construct
#' @export
construct.featurespec <- function(x, model_spec, ...) {
  sframe <- model_spec$sampling_frame
  if (is.null(sframe)) {
    stop("construct.featurespec() requires a sampling_frame on the model spec.",
         call. = FALSE)
  }
  n_blocks <- length(fmrihrf::blocklens(sframe))
  id <- x$id %||% x$name

  unpacked <- .normalize_feature_x(x$x, n_blocks, id)
  n_samples <- vapply(unpacked$values, nrow, integer(1))
  times <- .normalize_feature_times(x$times, n_blocks, n_samples, id,
                                    strictly = is.null(x$durations))
  durations <- .normalize_feature_durations(x$durations, n_blocks, n_samples, id)
  mask <- .normalize_feature_mask(x$mask, n_blocks, n_samples, id)
  bands <- .normalize_feature_bands(x$band, x$subband, unpacked$cond_tags, id)
  .validate_feature_alignment(unpacked$values, times, x$dt, x$start, sframe, id)

  ret <- list(
    varname = id,
    spec = x,
    values = unpacked$values,
    cond_tags = unpacked$cond_tags,
    times = times,
    durations = durations,
    dt = x$dt,
    start = x$start %||% 0,
    mask = mask,
    hrf = x$hrf,
    center = isTRUE(x$center),
    scale = x$scale %||% "none",
    span = x$span,
    precision = x$precision,
    band = bands$band,
    subband = bands$subband,
    sampling_frame = sframe
  )
  class(ret) <- c("feature_term", "fmri_term", "list")
  ret
}


#' @keywords internal
#' @noRd
.feature_dt_for_precision <- function(term, times_b) {
  dt <- term$dt
  if (!is.null(dt) && is.finite(dt)) {
    return(dt)
  }
  # Event-timed features have onsets, not a sampling grid: gaps between
  # onsets (possibly 0) say nothing about the precision needed.
  if (!is.null(term$durations)) {
    return(NA_real_)
  }
  if (!is.null(times_b) && length(times_b) >= 2L) {
    return(min(diff(times_b)))
  }
  NA_real_
}


#' @keywords internal
#' @noRd
.resolve_feature_precision <- function(term, model_precision, dt_eff) {
  base <- term$precision %||% model_precision
  if (!is.finite(dt_eff)) {
    return(base)
  }
  min(base, dt_eff)
}


#' Evaluate a feature term onto the scan grid (one FeatureReg per column x block).
#'
#' @keywords internal
#' @noRd
.convolve_feature_term_matrix <- function(term, sampling_frame, precision) {
  term_tag <- attr(term, "term_tag") %||% term$varname
  cond_tags <- term$cond_tags
  nb <- fmrihrf::nbasis(term$hrf)
  n_feat <- length(cond_tags)

  sample_times <- fmrihrf::samples(sampling_frame, global = FALSE)
  sample_blockids <- fmrihrf::blockids(sampling_frame)
  n_time <- length(sample_times)
  cmat <- matrix(0, nrow = n_time, ncol = n_feat * nb)

  dt0 <- .feature_dt_for_precision(
    term, if (is.null(term$times)) NULL else term$times[[1L]]
  )
  if (is.null(term$precision) && is.finite(dt0) &&
      precision > dt0 + sqrt(.Machine$double.eps)) {
    warning(sprintf(
      "feature '%s': model precision (%.3f s) is coarser than dt (%.3f s); evaluating at %.3f s.",
      term_tag, precision, dt0, dt0
    ), call. = FALSE)
  }

  for (b in seq_along(term$values)) {
    rows <- which(sample_blockids == b)
    if (length(rows) == 0L) {
      next
    }
    grid <- sample_times[rows]
    times_b <- if (is.null(term$times)) NULL else term$times[[b]]
    mask_b <- if (is.null(term$mask)) NULL else term$mask[[b]]
    dt_eff <- .feature_dt_for_precision(term, times_b)
    prec <- .resolve_feature_precision(term, precision, dt_eff)

    vals <- term$values[[b]]
    for (j in seq_len(n_feat)) {
      fr_args <- list(
        values = vals[, j],
        hrf = term$hrf,
        center = term$center,
        scale = term$scale,
        mask = mask_b,
        span = term$span
      )
      if (!is.null(term$dt) && is.finite(term$dt)) {
        fr_args$dt <- term$dt
        fr_args$start <- term$start
      } else {
        fr_args$times <- times_b
        if (!is.null(term$durations)) {
          fr_args$durations <- term$durations[[b]]
        }
      }
      feat <- do.call(fmrihrf::feature_regressor, fr_args)
      y <- fmrihrf::evaluate(feat, grid, precision = prec)
      if (is.null(dim(y))) {
        y <- matrix(y, ncol = 1L)
      }
      col_idx <- ((j - 1L) * nb + 1L):((j - 1L) * nb + nb)
      cmat[rows, col_idx] <- y
    }
  }

  colnames_final <- make_column_names(term_tag, cond_tags, nb)
  colnames(cmat) <- colnames_final
  attr(cmat, "col_metadata") <- .feature_term_col_metadata(
    term, term$hrf, cond_tags, nb, term_tag, colnames_final
  )
  cmat
}


#' @keywords internal
#' @noRd
.feature_term_col_metadata <- function(term, hrf, base_cnames, nb,
                                       term_tag, colnames_final) {
  n_cond <- length(base_cnames)
  if (n_cond == 0L || nb <= 0L || length(colnames_final) == 0L) {
    return(.empty_col_metadata())
  }
  cond_per_col <- rep(base_cnames, each = nb)
  basis_per_col <- rep(seq_len(nb), times = n_cond)
  basis_name <- .hrf_basis_name(hrf)

  .make_col_metadata(
    name = colnames_final,
    condition = cond_per_col,
    term_tag = if (is.null(term_tag)) NA_character_ else term_tag,
    basis_name = basis_name,
    basis_ix = if (nb > 1L) basis_per_col else NA_integer_,
    basis_total = if (nb > 1L) as.integer(nb) else NA_integer_,
    basis_label = if (nb > 1L) .basis_label(basis_name, basis_per_col) else NA_character_,
    modulation_type = "feature",
    modulation_id = term$varname %||% NA_character_,
    band = term$band %||% NA_character_,
    subband = rep(term$subband %||% NA_character_, each = nb)
  )
}


#' @export
#' @rdname conditions
conditions.feature_term <- function(x, drop.empty = TRUE, expand_basis = FALSE,
                                    style = c("canonical", "display"), ...) {
  style <- match.arg(style)
  tags <- x$cond_tags
  if (isTRUE(expand_basis)) {
    tags <- add_basis(tags, fmrihrf::nbasis(x$hrf))
  }
  tags
}


#' @export
#' @rdname longnames
longnames.feature_term <- function(x, ...) {
  as.character(conditions(x, style = "canonical", ...))
}

#' @export
#' @rdname shortnames
shortnames.feature_term <- function(x, ...) {
  as.character(conditions(x, style = "display", ...))
}


#' @export
#' @rdname condition_map
condition_map.feature_term <- function(x, drop.empty = TRUE,
                                       expand_basis = FALSE, ...) {
  tags <- conditions(x, drop.empty = drop.empty, expand_basis = expand_basis, ...)
  tibble::tibble(display = tags, canonical = tags)
}


#' @export
#' @rdname cells
cells.feature_term <- function(x, ...) {
  tibble::tibble(feature = x$cond_tags)
}


#' @rdname fmrihrf-generics
#' @export
nbasis.feature_term <- function(x, ...) {
  fmrihrf::nbasis(x$hrf)
}


#' @export
#' @rdname contrasts
contrasts.feature_term <- function(x, ...) {
  x$spec$contrasts
}


#' @export
#' @rdname Fcontrasts
#' @details
#' For a `feature_term` the result has the same structure as for an
#' `event_term`: a named list of matrices with one row per feature column
#' (row names are the term's condition tags, `conditions(x)`), before
#' expansion across basis functions. The first element, named after the term,
#' is the omnibus contrast (an identity matrix: all feature columns are tested
#' jointly). It is followed by one element per distinct sub-band declared with
#' `feature(subband = )`, named after the sub-band, whose columns pick out
#' that sub-band's feature columns. A sub-band that has the term's own name is
#' not repeated. `Fcontrasts()` on the `event_model` expands these across the
#' HRF basis and embeds them in the full design, as for any other term.
Fcontrasts.feature_term <- function(x, ...) {
  tags <- x$cond_tags
  n_feat <- length(tags)
  if (n_feat == 0L) {
    return(list())
  }
  select <- function(idx) {
    M <- diag(n_feat)[, idx, drop = FALSE]
    dimnames(M) <- list(tags, paste0("c", seq_along(idx)))
    M
  }
  omnibus <- x$varname
  out <- stats::setNames(list(select(seq_len(n_feat))), omnibus)
  subband <- x$subband %||% rep(NA_character_, n_feat)
  for (sb in setdiff(unique(subband[!is.na(subband)]), omnibus)) {
    out[[sb]] <- select(which(!is.na(subband) & subband == sb))
  }
  out
}


#' @export
is_continuous.feature_term <- function(x, ...) {
  TRUE
}


#' @export
is_categorical.feature_term <- function(x, ...) {
  FALSE
}


#' @export
#' @rdname events
events.feature_term <- function(x, ...) {
  stop("events() is not defined for feature terms (they are sampled series, not trials).",
       call. = FALSE)
}


#' @export
#' @rdname event_table
event_table.feature_term <- function(x, ...) {
  tibble::tibble(feature = x$cond_tags)
}


#' @export
#' @rdname print
print.feature_term <- function(x, ...) {
  n_feat <- length(x$cond_tags)
  n_blocks <- length(x$values)
  n_samp <- vapply(x$values, nrow, integer(1))
  cli::cli_h1("Feature Term: {.field {x$varname}}")
  cli::cli_text("{.info Features:} {n_feat} ({paste(x$cond_tags, collapse = ', ')})")
  cli::cli_text("{.info Blocks:} {n_blocks} ({paste(n_samp, collapse = ', ')} samples)")
  if (!is.null(x$dt) && is.finite(x$dt)) {
    cli::cli_text("{.info Sampling interval:} {x$dt} s")
  } else if (!is.null(x$durations)) {
    cli::cli_text("{.info Timing:} events with explicit durations")
  } else {
    cli::cli_text("{.info Sampling interval:} irregular / per-run times")
  }
  cli::cli_text("{.info Center:} {x$center}; {.info Scale:} {x$scale}")
  invisible(x)
}
