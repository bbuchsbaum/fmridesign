## Unified post-processing for contrast specifications. See
## `.expand_and_filter_basis()` for the basis-expansion + filtering helper
## shared by built-in contrast methods, and `contrast_mask()` /
## `contrast_from_mask()` for the extension API used by external packages.

# Escape regex metacharacters so a literal column name can be embedded in a
# pattern. Used for both base->expanded condition matching and basis filtering.
#' @keywords internal
#' @noRd
.regex_escape <- function(x) {
  gsub("([.+*?^$(){}|\\[\\]])", "\\\\\\1", x)
}

#' Expand and basis-filter a base contrast weights matrix
#'
#' Given a weights matrix whose rows correspond to the term's *base*
#' conditions (i.e., before basis expansion), expand it across all basis
#' components and apply optional basis filtering / re-weighting.
#'
#' If the term has a single-basis HRF (or no `hrfspec`), the weights are
#' returned unchanged.
#'
#' @param weights_out Numeric matrix; rows are base condition names.
#' @param term An `event_term` (or compatible) object.
#' @param contrast_name Character scalar used in error messages.
#' @param basis_spec Optional basis index spec (NULL, "all", or integer vector).
#' @param basis_weights Optional numeric vector of basis re-weights.
#' @return A list with elements `weights` and `condnames`.
#' @keywords internal
#' @noRd
.expand_and_filter_basis <- function(weights_out, term, contrast_name,
                                     basis_spec = NULL, basis_weights = NULL) {
  hrfspec <- attr(term, "hrfspec")
  nb_try <- if (!is.null(hrfspec) && !is.null(hrfspec$hrf)) {
    try(fmrihrf::nbasis(hrfspec$hrf), silent = TRUE)
  } else NA
  multi_basis <- !inherits(nb_try, "try-error") && is.numeric(nb_try) && nb_try > 1L

  if (!multi_basis || nrow(weights_out) == 0L) {
    return(list(weights = weights_out, condnames = rownames(weights_out)))
  }

  expanded_condnames <- try(
    conditions(term, drop.empty = FALSE, expand_basis = TRUE),
    silent = TRUE
  )
  if (inherits(expanded_condnames, "try-error") || length(expanded_condnames) == 0L) {
    return(list(weights = weights_out, condnames = rownames(weights_out)))
  }

  # Replicate each base row across matching basis-suffixed expanded names.
  expanded_weights <- matrix(0,
                             nrow = length(expanded_condnames),
                             ncol = ncol(weights_out))
  rownames(expanded_weights) <- expanded_condnames
  colnames(expanded_weights) <- colnames(weights_out)

  base_names <- rownames(weights_out)
  for (i in seq_len(nrow(weights_out))) {
    base_name <- base_names[i]
    if (is.null(base_name) || is.na(base_name) || !nzchar(base_name)) next
    pattern <- paste0("^", .regex_escape(base_name), "(_b\\d+)?$")
    matching_indices <- grep(pattern, expanded_condnames, perl = TRUE)
    if (length(matching_indices) > 0L) {
      expanded_weights[matching_indices, ] <- matrix(
        rep(weights_out[i, ], length(matching_indices)),
        nrow = length(matching_indices),
        byrow = TRUE
      )
    }
  }

  filtered <- .apply_basis_filter(
    expanded_weights, term, basis_spec, contrast_name,
    expanded_condnames, basis_weights
  )

  list(weights = filtered$weights, condnames = filtered$condnames)
}

#' Build a base contrast mask
#'
#' `contrast_mask()` is the extension point for new contrast types: it
#' returns a *base* contrast matrix with one row per base condition (no
#' basis expansion) and one column per contrast column. The unified driver
#' [contrast_from_mask()] handles basis expansion, basis filtering and
#' packaging into a `contrast` object compatible with
#' `contrast_weights.event_model()`.
#'
#' To register a custom contrast type:
#'
#' 1.  Define a constructor that produces a list with class
#'     `c("my_spec", "contrast_spec", "list")`.
#' 2.  Implement `contrast_mask.my_spec()` returning the base weights.
#' 3.  Implement `contrast_weights.my_spec()` as a one-liner:
#'
#'     ```r
#'     contrast_weights.my_spec <- function(x, term, ...) {
#'       contrast_from_mask(contrast_mask(x, term, ...), x, term)
#'     }
#'     ```
#'
#' The built-in spec classes (`pair_contrast_spec`, `oneway_contrast_spec`,
#' `poly_contrast_spec`, ...) implement `contrast_weights()` directly, so
#' they do not require `contrast_mask()` methods.
#'
#' @param x A `contrast_spec` object.
#' @param term An `event_term` against which the mask is computed.
#' @param ... Additional arguments passed to methods.
#' @return A list with elements:
#'   * `weights`   : numeric matrix, `n_base_conditions x n_contrast_cols`,
#'                   row names = base condition names.
#'   * `condnames` : character vector of base condition names.
#' @export
contrast_mask <- function(x, term, ...) UseMethod("contrast_mask")

#' Package a base mask into a `contrast` object
#'
#' Drives the unified post-processing pipeline: expands the base mask across
#' basis functions (when applicable), applies basis filtering, attaches the
#' standard `contrast` list class, and returns an object compatible with
#' downstream `contrast_weights.event_model()`.
#'
#' Custom contrast types implement [contrast_mask()] and call this driver.
#'
#' @param mask    Output of `contrast_mask()` (or compatible list).
#' @param spec    The originating contrast spec (stored on the result).
#' @param term    The `event_term` the contrast is being computed against.
#' @param classes Character vector of extra S3 classes prepended to the
#'   default `c("cell_contrast", "contrast", "list")`.
#' @return A `contrast` object.
#' @export
contrast_from_mask <- function(mask, spec, term, classes = character()) {
  stopifnot(is.list(mask), !is.null(mask$weights))

  weights_base <- mask$weights
  if (is.null(rownames(weights_base)) && !is.null(mask$condnames)) {
    rownames(weights_base) <- mask$condnames
  }

  # Column-targeted specs (column_contrast, contrast_formula) build their
  # mask against the already-expanded design columns; cell-targeted specs
  # build it over base conditions and need basis expansion.
  is_column_targeted <- inherits(spec, c("column_contrast_spec",
                                         "contrast_formula_spec"))

  if (is_column_targeted) {
    weights_out <- weights_base
    condnames   <- rownames(weights_base) %||% character(0)
  } else {
    expanded <- .expand_and_filter_basis(
      weights_out   = weights_base,
      term          = term,
      contrast_name = spec$name,
      basis_spec    = spec$basis,
      basis_weights = spec$basis_weights
    )
    weights_out <- expanded$weights
    condnames   <- expanded$condnames
  }

  ret <- list(
    term          = term,
    name          = spec$name,
    weights       = weights_out,
    condnames     = condnames,
    contrast_spec = spec
  )
  base_class <- if (!is.null(ncol(weights_out)) && ncol(weights_out) > 1L) {
    c("Fcontrast", "cell_contrast", "contrast", "list")
  } else {
    c("cell_contrast", "contrast", "list")
  }
  class(ret) <- unique(c(classes, base_class))
  ret
}
