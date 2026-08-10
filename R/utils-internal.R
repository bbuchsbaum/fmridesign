#' Default value for NULL
#' @param a Value to use if not NULL
#' @param b Value to use if a is NULL
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

###############################################################################
## Helper: how many *rows* (events) does an arbitrary value-object have?    ###
###############################################################################
#' @keywords internal
#' @noRd
n_events <- function(x) UseMethod("n_events")
#' @keywords internal
#' @noRd
n_events.matrix            <- function(x) nrow(x)
#' @keywords internal
#' @noRd
n_events.ParametricBasis   <- function(x) nrow(x$y)
#' @keywords internal
#' @noRd
n_events.default           <- function(x) length(x)         # vector / factor / list

###############################################################################
## recycle a scalar / vector or abort with a clear message                  ###
###############################################################################
#' @keywords internal
#' @noRd
recycle_or_error <- function(x, target, label) {
  if (length(x) == 1L) rep(x, target)
  else if (length(x) != target)
    stop(sprintf("Length mismatch for %s: got %d, expected %d", label, length(x), target),
         call. = FALSE)
  else x
}

#' Sanitize variable names and store original names
#'
#' Uses `make.names` for robust sanitization and ensures uniqueness.
#' Stores original names in the "orig_names" attribute.
#'
#' @param names A character vector of names to sanitize.
#' @param allow_ = TRUE Passed to `make.names`.
#' @return A character vector of sanitized names, with original names attached
#'         as the "orig_names" attribute.
#' @keywords internal
#' @noRd
.sanitizeName <- function(names, allow_ = TRUE) {
  if (length(names) == 0) return(character(0))
  
  # Store original names before sanitizing
  original_names <- names
  
  # Use make.names for robust sanitization and uniqueness
  sanitized <- make.names(names, unique = TRUE, allow_ = allow_)
  
  # Attach original names as an attribute
  attr(sanitized, "orig_names") <- original_names
  
  sanitized
}

###############################################################################
## Helpers for Consistent Naming (Conditions/Levels)                       ###
###############################################################################

#' Generate a formatted label component for an event and specific level/index
#' 
#' @param ev An event object.
#' @param level Optional: The factor level (character) or column index (integer) to use. If NULL, assumes single column.
#' @return A character string label (e.g., "VarName\\[LevelName\\]" or "VarName\\[1\\]").
#' @keywords internal
#' @noRd
.label_component <- function(ev, level = NULL) {
  base_name <- ev$varname # Use original name before potential sanitization
  if (is.null(level)) {
    base_name
  } else {
    # Revert to sprintf format causing invalid names, but descriptive
    sprintf("%s[%s]", base_name, level)
    # Original change using paste:
    # paste(base_name, level, sep = ".")
  }
}

#' Internal helper to get a vector of formatted labels for a single event
#'
#' Returns `Variable[Level]` for each level of a categorical event, 
#' `Variable[Index]` for each column of a multi-column continuous event, 
#' `Variable` for a single-column continuous event, or `character(0)`.
#' Relies on `levels.event`, `is_categorical`, `.label_component`.
#'
#' @param ev An `event` object.
#' @return A character vector of formatted labels.
#' @keywords internal
#' @noRd
.level_vector <- function(ev) {
  lvls <- levels(ev) # Get levels/colnames via levels.event
  
  if (is_categorical(ev)) {
    # Categorical: Use actual levels
    vapply(lvls, \(l) .label_component(ev, l), character(1))
  } else if (is_continuous(ev) && length(lvls) > 1) {
    # Continuous multi-column (matrix/basis): Use feature names
    vapply(lvls, \(l) .label_component(ev, l), character(1))
  } else if (is_continuous(ev) && length(lvls) == 1) {
    # Single continuous variable (vector): Just the variable name
    .label_component(ev) # level = NULL implicit
  } else {
    # Fallback (e.g., categorical with no levels?) -> empty
    character(0)
  }
}

###############################################################################
## The new .checkEVArgs                                                     ###
###############################################################################
#' Validate event arguments using new helpers.
#'
#' @param name The name of the event.
#' @param value The event values.
#' @param onsets Numeric vector of event onsets.
#' @param blockids Numeric vector of block IDs.
#' @param durations Numeric vector of durations (or a scalar, defaults to 0).
#' @return A list of validated event parameters with sanitized varname.
#' @import assertthat
#' @keywords internal
#' @noRd
.checkEVArgs <- function(name, value, onsets, blockids, durations = 0) {

  vname <- .sanitizeName(name)                    # single place

  ## ---- length consistency --------------------------------------------------
  n <- length(onsets)
  if (n_events(value) != n)
    stop(sprintf("Value length for '%s' is %d but onsets length is %d",
                 vname, n_events(value), n), call. = FALSE)

  if (length(blockids) != n)
    stop(sprintf("blockids length for '%s' is %d but onsets length is %d",
                 vname, length(blockids), n), call. = FALSE)

  ## ---- basic sanity checks -------------------------------------------------
  assertthat::assert_that(all(!is.na(onsets)),   msg = sprintf("NA in onsets (%s)", vname))
  assertthat::assert_that(all(!is.na(blockids)), msg = sprintf("NA in blockids (%s)", vname))
  assertthat::assert_that(!is.unsorted(blockids), msg = sprintf("blockids not non-decreasing (%s)", vname))

  ## strictly increasing onsets *within* each block
  tapply(onsets, blockids, function(ons) {
    if (is.unsorted(ons, strictly = TRUE))
      stop(sprintf("Onsets not strictly increasing within block for '%s'", vname), call. = FALSE)
    NULL
  })

  ## ---- durations -----------------------------------------------------------
  durations <- recycle_or_error(durations, n, sprintf("durations for '%s'", vname))

  ## ---- return cleaned bundle ----------------------------------------------
  list(
    varname   = vname,
    value     = value,
    onsets    = onsets,
    durations = durations,
    blockids  = blockids
  )
}

###############################################################################
## Shared-HRF regressor evaluation (hot path)                               ###
###############################################################################

#' Build the fine-grid HRF matrix used by fmrihrf's convolution engines.
#'
#' Mirrors `fmrihrf`'s internal `.memo_hrf()` using only the public
#' `fmrihrf::evaluate()` API so the result is bit-identical to what
#' `evaluate.Reg()` would prepare via `prep_reg_inputs()`.
#'
#' @keywords internal
#' @noRd
.hrf_fine_matrix <- function(hrf, span, precision) {
  times <- seq(0, span, by = precision)
  val <- fmrihrf::evaluate(hrf, times)
  if (is.null(dim(val))) {
    matrix(val, ncol = 1L)
  } else {
    as.matrix(val)
  }
}

#' Resolve fmrihrf:::evaluate_regressor_cpp (cached).
#'
#' This is the same C++ entry point `evaluate.Reg()` uses after
#' `prep_reg_inputs()`. Calling it directly with a shared HRF fine matrix
#' skips per-column Reg construction and prep overhead while producing
#' bit-identical output.
#'
#' @keywords internal
#' @noRd
.eval_regressor_cpp <- local({
  fun <- NULL
  function(...) {
    if (is.null(fun)) {
      fun <<- utils::getFromNamespace("evaluate_regressor_cpp", "fmrihrf")
    }
    fun(...)
  }
})

#' Evaluate selected design columns onto a TR grid with a shared HRF matrix.
#'
#' Returns an `length(grid) x (length(col_idx) * nb)` matrix of convolved
#' columns (in `col_idx` order). Columns with no in-window nonzero amplitudes
#' are all-zero, matching `evaluate.Reg` on an empty regressor.
#'
#' Onset window filtering matches `fmrihrf:::prep_reg_inputs()` exactly:
#' `onsets >= grid[1] - span` and `onsets <= grid[length(grid)]`.
#'
#' @keywords internal
#' @noRd
.eval_design_cols_shared_hrf <- function(dmat, globons, durations, grid,
                                         hrf_matrix, hrf_span, precision, nb,
                                         col_idx) {
  n_live <- length(col_idx)
  out <- matrix(0, nrow = length(grid), ncol = n_live * nb)
  if (!n_live) {
    return(out)
  }

  onset_min <- grid[1L] - hrf_span
  onset_max <- grid[length(grid)]
  eval_cpp <- .eval_regressor_cpp

  for (k in seq_len(n_live)) {
    j <- col_idx[[k]]
    amp <- dmat[, j]
    nz <- which(amp != 0)
    if (!length(nz)) {
      next
    }
    ons <- globons[nz]
    in_window <- ons >= onset_min & ons <= onset_max
    if (!any(in_window)) {
      next
    }
    nz <- nz[in_window]
    res <- eval_cpp(
      grid = grid,
      onsets = globons[nz],
      durations = durations[nz],
      amplitudes = amp[nz],
      hrf_matrix = hrf_matrix,
      hrf_span = hrf_span,
      precision = precision,
      method = "conv"
    )
    target <- ((k - 1L) * nb + 1L):((k - 1L) * nb + nb)
    if (nb == 1L) {
      out[, target] <- as.numeric(res)
    } else {
      out[, target] <- res
    }
  }
  out
}