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
#' @param x Numeric vector, matrix (samples x features), or a list of those
#'   (one per run).
#' @param dt Positive sampling interval in seconds. Mutually exclusive with
#'   `times`.
#' @param times Sample times in seconds, or a list of per-run time vectors.
#'   Mutually exclusive with `dt`.
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
#'
#' @return A `featurespec` object for use on the RHS of an [event_model()]
#'   formula or in the list interface.
#'
#' @examples
#' dt <- 0.5
#' rms <- abs(sin(seq(0, 20, by = dt)))
#' sframe <- sampling_frame(blocklens = 15, TR = 2)
#' emod <- event_model(
#'   ~ feature(rms, dt = dt, id = "rms", center = FALSE, scale = "none"),
#'   sampling_frame = sframe
#' )
#' ncol(design_matrix(emod))
#'
#' @seealso [fmrihrf::feature_regressor()], [hrf()], [event_model()]
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
                    prefix = NULL) {
  scale <- match.arg(scale)
  x_label <- paste(deparse(substitute(x), width.cutoff = 500L), collapse = "")

  if (missing(x)) {
    stop("`feature()` requires `x` (a numeric vector, matrix, or per-run list).",
         call. = FALSE)
  }
  if (!is.null(dt) && !is.null(times)) {
    stop("Supply exactly one of `dt` or `times`.", call. = FALSE)
  }
  if (is.null(dt) && is.null(times)) {
    stop("Supply exactly one of `dt` or `times`.", call. = FALSE)
  }
  if (!is.null(dt)) {
    if (!is.numeric(dt) || length(dt) != 1L || is.na(dt) || !is.finite(dt) ||
        dt <= 0) {
      stop("`dt` must be a single positive finite number.", call. = FALSE)
    }
  }
  if (!is.numeric(start) || length(start) != 1L || is.na(start) ||
      !is.finite(start) || start < 0) {
    stop("`start` must be a single non-negative finite number.", call. = FALSE)
  }
  if (!is.logical(center) || length(center) != 1L || is.na(center)) {
    stop("`center` must be a single logical value.", call. = FALSE)
  }
  if (!is.null(precision)) {
    if (!is.numeric(precision) || length(precision) != 1L ||
        is.na(precision) || !is.finite(precision) || precision <= 0) {
      stop("`precision` must be a single positive finite number.", call. = FALSE)
    }
  }

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

  hrf_obj <- make_hrf(basis, lag, nbasis = nbasis)

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
    start = start,
    center = center,
    scale = scale,
    mask = mask,
    hrf = hrf_obj,
    span = span,
    precision = precision,
    contrasts = NULL,
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
.normalize_feature_times <- function(times, n_blocks, n_samples, id) {
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
    if (is.unsorted(tvec, strictly = TRUE)) {
      stop(sprintf(
        "feature '%s' %s: `times` must be strictly increasing.",
        id, block_lab
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
  times <- .normalize_feature_times(x$times, n_blocks, n_samples, id)
  mask <- .normalize_feature_mask(x$mask, n_blocks, n_samples, id)
  .validate_feature_alignment(unpacked$values, times, x$dt, x$start, sframe, id)

  ret <- list(
    varname = id,
    spec = x,
    values = unpacked$values,
    cond_tags = unpacked$cond_tags,
    times = times,
    dt = x$dt,
    start = x$start %||% 0,
    mask = mask,
    hrf = x$hrf,
    center = isTRUE(x$center),
    scale = x$scale %||% "none",
    span = x$span,
    precision = x$precision,
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
  basis_name <- if (!is.null(hrf)) class(hrf)[[1L]] else NA_character_
  if (length(basis_name) == 0L) {
    basis_name <- NA_character_
  }

  .make_col_metadata(
    name = colnames_final,
    condition = cond_per_col,
    term_tag = if (is.null(term_tag)) NA_character_ else term_tag,
    basis_name = basis_name,
    basis_ix = if (nb > 1L) basis_per_col else NA_integer_,
    basis_total = if (nb > 1L) as.integer(nb) else NA_integer_,
    basis_label = if (nb > 1L) {
      vapply(basis_per_col, function(j) .basis_label(basis_name, j), character(1))
    } else {
      NA_character_
    },
    modulation_type = "feature",
    modulation_id = term$varname %||% NA_character_
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


#' @rdname fmrihrf-reexports
#' @export
nbasis.feature_term <- function(x, ...) {
  fmrihrf::nbasis(x$hrf)
}


#' @export
#' @rdname contrasts
contrasts.feature_term <- function(x, ...) {
  NULL
}


#' @export
#' @rdname Fcontrasts
Fcontrasts.feature_term <- function(x, ...) {
  list()
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
  } else {
    cli::cli_text("{.info Sampling interval:} irregular / per-run times")
  }
  cli::cli_text("{.info Center:} {x$center}; {.info Scale:} {x$scale}")
  invisible(x)
}
