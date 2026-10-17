# Internal constants
.CONTRAST_TOLERANCE <- 1e-8

#' Argument checks shared by the contrast constructors
#'
#' `basis` and `basis_weights` only get light checks here; their ranges and
#' lengths are validated against the term in `.filter_basis()` and
#' `.apply_basis_filter()`.
#' @keywords internal
#' @noRd
.check_formula_arg <- function(x, arg) {
  assert_that(rlang::is_formula(x), msg = paste(arg, "must be a formula"))
}

.check_name_arg <- function(name) {
  assert_that(is.character(name) && length(name) == 1,
              msg = "name must be a single character string")
}

.check_where_arg <- function(where) {
  if (!is.null(where)) .check_formula_arg(where, "where")
}

.check_basis_args <- function(basis, basis_weights) {
  if (!is.null(basis) && !identical(basis, "all") &&
      (!is.numeric(basis) || any(basis < 1) || anyNA(basis))) {
    stop("basis must be NULL, 'all', or a positive integer vector", call. = FALSE)
  }
  if (!is.null(basis_weights)) {
    if (!is.numeric(basis_weights) || anyNA(basis_weights)) {
      stop("basis_weights must be a numeric vector without NAs", call. = FALSE)
    }
    if (any(basis_weights < 0)) {
      stop("basis_weights must be non-negative", call. = FALSE)
    }
  }
}

#' Get condition names for a term
#' Wraps conditions() with standard arguments for internal use.
#' @param term An event_term object.
#' @param expanded Logical, whether to return basis-expanded names.
#' @return Character vector of condition names.
#' @keywords internal
#' @noRd
.condnames <- function(term, expanded = TRUE) {
  # Assumes conditions() is robust and handles drop.empty=FALSE internally
  tryCatch(conditions(term, drop.empty = FALSE, expand_basis = expanded),
           error = function(e) {
             stop(paste("Error retrieving condition names for term '",
                        term$varname %||% "<unknown>", # Use varname if available
                        "' (expanded=", expanded, "): ", e$message), call. = FALSE)
           })
}

#' Filter condition names by basis indices
#'
#' Given a vector of expanded condition names (with _b## suffixes) and a basis
#' specification, return only the names corresponding to the requested basis functions.
#'
#' @param condnames Character vector of condition names (potentially with _b## suffixes).
#' @param basis NULL (keep all), integer vector (keep specific indices), or "all".
#' @param nbasis Number of basis functions (required for validation).
#' @param contrast_name Name of contrast (for error messages).
#' @return Character vector of filtered condition names.
#' @keywords internal
#' @noRd
.filter_basis <- function(condnames, basis = NULL, nbasis = 1, contrast_name = "contrast") {
  # If no basis filtering requested, or only 1 basis function, return all
  if (is.null(basis) || identical(basis, "all") || nbasis <= 1) {
    return(condnames)
  }

  # Validate basis argument
  if (!is.numeric(basis) || any(basis < 1) || any(basis > nbasis)) {
    stop(sprintf("Contrast '%s': basis must be NULL, 'all', or integer vector with values in 1:%d",
                 contrast_name, nbasis), call. = FALSE)
  }

  # Convert basis indices to the expected suffixes, e.g. basis = 2 -> "_b02"
  basis_suffixes <- basis_suffix(basis, nbasis)

  # Build regex pattern to match any of the requested basis suffixes
  # Pattern: (condition_name)(_b01|_b03)$
  pattern <- paste0("(", paste(basis_suffixes, collapse = "|"), ")$")

  # Filter condition names
  matched_names <- grep(pattern, condnames, value = TRUE, perl = TRUE)

  if (length(matched_names) == 0) {
    warning(sprintf("Contrast '%s': basis filter (indices %s) matched no condition names. Check that nbasis = %d is correct.",
                    contrast_name, paste(basis, collapse = ", "), nbasis), call. = FALSE)
  }

  return(matched_names)
}

#' Number of basis functions of a term's HRF
#'
#' @param term An event_term, possibly carrying an `hrfspec` attribute.
#' @return `fmrihrf::nbasis()` of the term's HRF when it is greater than one;
#'   otherwise (single-basis, no HRF, or an error) `1L`.
#' @keywords internal
#' @noRd
.term_nbasis <- function(term) {
  hrf <- attr(term, "hrfspec")$hrf
  if (is.null(hrf)) return(1L)
  nb <- try(fmrihrf::nbasis(hrf), silent = TRUE)
  if (!inherits(nb, "try-error") && is.numeric(nb) && nb > 1) nb else 1L
}

#' Apply basis filtering and weighting to contrast weights
#'
#' Unified helper function for applying basis filtering across all contrast types.
#' Detects nbasis from term, validates basis parameter, filters expanded names,
#' zeros out non-selected weights, applies basis_weights, and validates sum-to-zero property.
#'
#' @param weights_mat Numeric matrix of weights (rows = conditions, cols = contrasts).
#' @param term An event_term object with hrfspec attribute.
#' @param basis_spec NULL (no filtering), integer vector (basis indices), or "all".
#' @param basis_weights_spec NULL (equal weights), or numeric vector of weights for the
#'   selected bases (all bases when `basis_spec` is NULL or "all").
#' @param contrast_name Name of contrast (for error messages).
#' @param expanded_condnames Character vector of expanded condition names (with _b## suffixes).
#' @return List with:
#'   \item{weights}{Updated weights matrix with basis filtering and weighting applied.}
#'   \item{condnames}{Filtered condition names (only selected basis functions).}
#'   \item{nbasis}{Number of basis functions detected.}
#' @keywords internal
#' @noRd
.apply_basis_filter <- function(weights_mat, term, basis_spec, contrast_name,
                                 expanded_condnames = NULL, basis_weights_spec = NULL) {
  nbasis <- .term_nbasis(term)
  no_filter <- is.null(basis_spec) || identical(basis_spec, "all")

  # Nothing to do without a multi-basis HRF, or without a basis selection or
  # basis weights
  if (nbasis <= 1 || (no_filter && is.null(basis_weights_spec))) {
    if (nbasis <= 1 && !no_filter) {
      warning(sprintf("Contrast '%s': basis filtering requested but term has no multi-basis HRF (nbasis = %d). Ignoring basis filter.",
                      contrast_name, nbasis), call. = FALSE)
    }
    return(list(
      weights = weights_mat,
      condnames = expanded_condnames %||% rownames(weights_mat),
      nbasis = nbasis
    ))
  }

  # Get expanded condition names if not provided
  if (is.null(expanded_condnames)) {
    expanded_condnames <- rownames(weights_mat)
    if (is.null(expanded_condnames)) {
      warning(sprintf("Contrast '%s': Cannot apply basis filtering without condition names.",
                      contrast_name), call. = FALSE)
      return(list(
        weights = weights_mat,
        condnames = character(0),
        nbasis = nbasis
      ))
    }
  }

  # Filter to keep only requested basis functions (all of them if none selected)
  filtered_condnames <- if (no_filter) {
    expanded_condnames
  } else {
    .filter_basis(expanded_condnames, basis = basis_spec,
                  nbasis = nbasis, contrast_name = contrast_name)
  }

  # Zero out weights for non-selected basis functions
  keep_mask <- rownames(weights_mat) %in% filtered_condnames
  weights_mat[!keep_mask, ] <- 0

  # Apply basis_weights if specified
  if (!is.null(basis_weights_spec)) {
    num_selected_bases <- if (no_filter) nbasis else length(basis_spec)

    # Validate length
    if (length(basis_weights_spec) != num_selected_bases) {
      stop(sprintf("Contrast '%s': basis_weights length (%d) must match number of selected basis functions (%d)",
                   contrast_name, length(basis_weights_spec), num_selected_bases), call. = FALSE)
    }

    # Normalize to sum to 1
    weight_sum <- sum(basis_weights_spec)
    if (abs(weight_sum - 1.0) > .CONTRAST_TOLERANCE) {
      warning(sprintf("Contrast '%s': basis_weights sum to %.6f, normalizing to sum to 1.0",
                      contrast_name, weight_sum), call. = FALSE)
      basis_weights_spec <- basis_weights_spec / weight_sum
    }

    # Apply weights to each base condition across all selected bases
    # Get unique base condition names (without _b## suffix)
    base_names <- unique(gsub("_b\\d+$", "", filtered_condnames))

    for (base_name in base_names) {
      # Find all rows corresponding to this base condition with selected bases
      pattern <- paste0("^", .regex_escape(base_name), "_b\\d+$")
      matching_rows <- grep(pattern, rownames(weights_mat), perl = TRUE)
      matching_filtered <- rownames(weights_mat)[matching_rows] %in% filtered_condnames

      # Get the matching rows that are in filtered set
      selected_rows <- matching_rows[matching_filtered]

      if (length(selected_rows) > 0) {
        # For each contrast column, apply basis weights
        for (col_idx in seq_len(ncol(weights_mat))) {
          # Get current weights for this base condition
          current_weights <- weights_mat[selected_rows, col_idx]

          # If any weight is non-zero, apply basis_weights
          if (any(current_weights != 0)) {
            # All should have the same value (e.g., all 1 or all -1)
            base_weight <- current_weights[current_weights != 0][1]
            # Replace with weighted values
            weights_mat[selected_rows, col_idx] <- base_weight * basis_weights_spec
          }
        }
      }
    }
  }

  # Validate sum-to-zero for each contrast column (only for non-zero filtered weights)
  for (col_idx in seq_len(ncol(weights_mat))) {
    nonzero_weights <- weights_mat[keep_mask, col_idx]
    if (length(nonzero_weights) > 0 && abs(sum(nonzero_weights)) > .CONTRAST_TOLERANCE) {
      warning(sprintf("Contrast '%s' (column %d): Weights do not sum to zero after basis filtering (sum = %.6f).",
                      contrast_name, col_idx, sum(nonzero_weights)), call. = FALSE)
    }
  }

  return(list(
    weights = weights_mat,
    condnames = filtered_condnames,
    nbasis = nbasis
  ))
}

#' Calculate contrast weights from logical masks
#' 
#' This is the unified function for weight calculation from masks: conditions
#' in group A get `1/nA`, conditions in group B get `-1/nB`.
#' Returns a *named* numeric vector.
#' 
#' @param names Character vector of all condition names.
#' @param A_mask Logical vector (same length as names) indicating TRUE for conditions in group A.
#' @param B_mask Logical vector (same length as names) indicating TRUE for conditions in group B. Optional.
#' @return Named numeric vector of weights.
#' @keywords internal
#' @noRd
.calculate_mask_weights <- function(names, A_mask, B_mask = NULL) {
  # Input validation
  if (!is.character(names)) {
    stop(".calculate_mask_weights: 'names' must be a character vector", call. = FALSE)
  }
  if (!is.logical(A_mask) || length(A_mask) != length(names)) {
    stop(".calculate_mask_weights: 'A_mask' must be a logical vector with same length as 'names'", call. = FALSE)
  }
  
  nA <- sum(A_mask)
  nB <- 0

  if (!is.null(B_mask)) {
    if (!is.logical(B_mask) || length(B_mask) != length(names)) {
      stop(".calculate_mask_weights: 'B_mask' must be a logical vector with same length as 'names'", call. = FALSE)
    }
    if (any(A_mask & B_mask)) {
      stop(".calculate_mask_weights: Masks for group A and group B overlap.", call. = FALSE)
    }
    nB <- sum(B_mask)
  }

  # Check for completely empty selection: this should be an error.
  if (nA == 0 && nB == 0) {
    stop("Cannot calculate contrast weights: No conditions were selected by the provided mask(s). This usually indicates that the patterns or formulas used to define the contrast did not match any existing conditions. Please check your contrast specification.", call. = FALSE)
  }

  w <- numeric(length(names))
  names(w) <- names
  if (nA > 0) w[A_mask] <- 1 / nA
  if (nB > 0) w[B_mask] <- -1 / nB

  # An A-vs-B contrast with one empty group cannot sum to zero. (A single-group
  # contrast, B_mask = NULL, is not expected to.)
  if (!is.null(B_mask) && (nA == 0 || nB == 0)) {
    empty <- if (nA == 0) c("A", "B") else c("B", "A")
    warning(sprintf(".calculate_mask_weights: For A-vs-B contrast, Mask %s is empty but Mask %s is not. Weights will not sum to zero as expected for a balanced comparison.",
                    empty[1], empty[2]), call. = FALSE)
  }

  w
}

#' Contrast Specification
#'
#' @description
#' Define a linear contrast using a formula expression.
#'
#' @param form A formula describing the contrast.
#' @param name A character label for the contrast.
#' @param where An expression defining the subset over which the contrast is applied (default: NULL).
#'
#' @return A list containing the contrast specification.
#'
#' @examples
#' # A minus B contrast using display labels
#' contrast(~ A - B, name="A_B")
#' 
#' # With subsetting
#' contrast(~ A - B, name="A_B_block1", where = ~ block == 1)
#'
#' @export
contrast <- function(form, name, where=NULL) {
  .check_formula_arg(form, "form")
  .check_name_arg(name)
  .check_where_arg(where)
  structure(list(A = form, B = NULL, where = where, name = name),
            class = c("contrast_formula_spec", "contrast_spec", "list"))
}

#' Build name maps for formula contrasts
#'
#' Display names are the primary interface; canonical names are retained as
#' stable internal aliases.
#'
#' @param term An event term.
#' @return A list with `display` and `canonical` name vectors.
#' @keywords internal
#' @noRd
.contrast_formula_name_map <- function(term) {
  term_cells <- cells(term, drop.empty = TRUE)

  if (is_continuous(term)) {
    display <- conditions(term, drop.empty = TRUE, style = "display")
    canonical <- conditions(term, drop.empty = TRUE, style = "canonical")
  } else {
    display <- if (nrow(term_cells) == 0L) {
      character(0)
    } else {
      apply(term_cells, 1, function(row) paste(row, collapse = ":"))
    }

    canonical <- if (nrow(term_cells) == 0L) {
      character(0)
    } else {
      cell_condition_tags(term_cells)
    }
  }

  if (length(canonical) != length(display)) {
    stop("Internal error: canonical and display contrast name maps have different lengths.", call. = FALSE)
  }

  list(display = display, canonical = canonical)
}

#' Build an evaluation environment for formula contrasts
#'
#' @param display Character vector of display condition names.
#' @param canonical Character vector of canonical condition names.
#' @param parent Parent environment for formula evaluation.
#' @return An environment containing indicator vectors for each available alias.
#' @keywords internal
#' @noRd
.build_formula_contrast_env <- function(display, canonical, parent) {
  eval_env <- new.env(parent = parent)

  for (i in seq_along(display)) {
    indicator <- rep.int(0, length(display))
    indicator[i] <- 1

    assign(display[i], indicator, envir = eval_env)

    if (!identical(canonical[i], display[i]) &&
        !exists(canonical[i], envir = eval_env, inherits = FALSE)) {
      assign(canonical[i], indicator, envir = eval_env)
    }
  }

  eval_env
}

#' Unit Contrast
#'
#' @description
#' Construct a contrast that sums to 1 and is used to define contrasts against the baseline.
#'
#' @param A A formula selecting the cells to average. A logical expression
#'   (e.g. `~ cond == "A"`) selects the matching cells; a bare factor name
#'   (e.g. `~ cond`) selects every cell. For a multi-basis HRF the weights are
#'   repeated on every basis function of the selected cells.
#' @param name A character string specifying the name of the contrast.
#' @param where An optional formula specifying the subset of conditions to apply the contrast to.
#'
#' @return A unit_contrast_spec object containing the contrast that sums to 1.
#'
#' @examples
#' # Test main effect of Face against baseline
#' con <- unit_contrast(~ Face, name="Main_face")
#' 
#' # Test main effect within specific blocks
#' con2 <- unit_contrast(~ Face, name="Face_early", where = ~ block <= 3)
#'
#' @export
unit_contrast <- function(A, name, where=NULL) {
  .check_formula_arg(A, "A")
  .check_name_arg(name)
  .check_where_arg(where)
  structure(list(A = A, B = NULL, where = where, name = name),
            class = c("unit_contrast_spec", "contrast_spec", "list"))
}

#' One Against All Contrast
#'
#' @description
#' Construct contrasts comparing each factor level against the average of the other levels.
#'
#' @param levels A vector of factor levels to be compared.
#' @param facname A character string specifying the name of the factor containing the supplied levels.
#' @param where An optional formula specifying the subset over which the contrast is computed.
#'
#' @return A contrast_set object containing contrasts comparing each factor level against the average of the other levels.
#'
#' @examples
#' fac <- factor(rep(c("A", "B", "C"), 2))
#' con <- one_against_all_contrast(levels(fac), "fac")
#'
#' @export
one_against_all_contrast <- function(levels, facname, where=NULL) {
  .check_where_arg(where)
  ret <- lapply(1:length(levels), function(i) {
    lev1 <- levels[i]
    levother <- levels[-i]
    pair_contrast(as.formula(paste("~", facname, " == ", paste0('"', lev1, '"'))), 
                  as.formula(paste0("~", facname, "!= ", paste0('"', lev1, '"'))), 
                  where=where, name=paste0("con_", lev1, "_vs_", "other"))
  })
  
  do.call(contrast_set, ret)
  
}


#' Create a Set of Contrasts
#'
#' @description
#' Construct a list of contrast_spec objects. Nested `contrast_set` arguments,
#' including results from [one_against_all_contrast()] or [pairwise_contrasts()],
#' are recursively flattened in argument order. Individual specifications and
#' their list names are preserved; names on enclosing sets are not used as
#' prefixes. Empty sets contribute no specifications. Ordinary lists are not
#' flattened; splice those explicitly with `do.call(contrast_set, specs)`.
#'
#' @param ... Individual `contrast_spec` objects or nested `contrast_set` objects.
#'
#' @return A list of contrast_spec objects with class "contrast_set".
#'
#' @examples
#' c1 <- contrast(~ A - B, name="A_B")
#' c2 <- contrast(~ B - C, name="B_C")
#' contrast_set(c1,c2)
#' contrast_set(c1, one_against_all_contrast(c("A", "B", "C"), "condition"))
#'
#' @export
#' @import assertthat
#' @importFrom purrr map_lgl
contrast_set <- function(...) {
  flatten <- function(items, path = NULL) {
    out <- list()
    for (i in seq_along(items)) {
      item <- items[[i]]
      location <- if (is.null(path)) paste("argument", i) else paste0(path, "[[", i, "]]")
      if (inherits(item, "contrast_spec")) {
        # Keep the leaf's list name without descending into its formulas/fields.
        out <- c(out, items[i])
      } else if (inherits(item, "contrast_set") && is.list(item)) {
        out <- c(out, flatten(unclass(item), location))
      } else {
        stop("contrast_set(): ", location, " must be a contrast_spec or contrast_set.",
             if (is.list(item)) " For a plain list of specifications, use do.call(contrast_set, specs).",
             call. = FALSE)
      }
    }
    out
  }
  ret <- flatten(list(...))
  class(ret) <- c("contrast_set", "list")
  ret
}


#' Pairwise Contrasts
#'
#' @description
#' Construct pairwise contrasts for all combinations of factor levels.
#'
#' @param levels A vector of factor levels to be compared.
#' @param facname The name of the factor variable (column name in the design) these levels belong to.
#' @param where An optional formula specifying the subset over which the contrast is computed.
#' @param name_prefix A character string to prefix the generated contrast names (default: "con").
#'
#' @return A contrast_set object containing pairwise contrasts for all combinations of factor levels.
#'
#' @examples
#' # Assuming 'my_factor' is a column name
#' pairwise_contrasts(c("A", "B", "C"), facname = "my_factor")
#' pairwise_contrasts(c("A", "B", "C"), facname = "my_factor", name_prefix = "pair")
#'
#' @export
#' @importFrom utils combn
pairwise_contrasts <- function(levels, facname, where=NULL, name_prefix = "con") {
  assert_that(is.character(facname), length(facname) == 1, msg = "'facname' must be a single string.")
  .check_where_arg(where)
  if (length(levels) < 2) {
    stop("pairwise_contrasts requires at least two levels.")
  }
  
  cbns <- combn(length(levels), 2)
  ret <- lapply(1:ncol(cbns), function(i) {
    lev1 <- levels[cbns[1,i]]
    lev2 <- levels[cbns[2,i]]
    # Construct formulas using the factor name
    formula_A <- as.formula(paste("~", facname, "==", paste0('"', lev1, '"')))
    formula_B <- as.formula(paste("~", facname, "==", paste0('"', lev2, '"')))
    pair_contrast(formula_A, formula_B, 
                  where=where, name=paste0(name_prefix, "_", lev1, "_", lev2))
  })
  
  do.call(contrast_set, ret)
}


#' Sliding-Window Contrasts (Disjoint)
#'
#' @description
#' Generate a set of A-vs-B contrasts where A and B are adjacent, equally sized
#' and disjoint windows over an ordered factor. For window size k, contrast i
#' compares A = \code{levels[i:(i+k-1)]} against B = \code{levels[(i+k):(i+2k-1)]}.
#' This yields \code{length(levels) - 2*k + 1} contrasts that detect local changes
#' across the sequence without overlapping masks.
#'
#' @param levels Character vector of ordered factor levels.
#' @param facname Name of the factor (column in the design).
#' @param window_size Positive integer window size (default 2).
#' @param where Optional formula to subset events used when computing weights.
#' @param name_prefix Prefix for generated contrast names (default "win").
#'
#' @return A `contrast_set` of `pair_contrast` specifications.
#'
#' @examples
#' # For levels 1..5, generate 2 disjoint adjacent-window contrasts (k=2)
#' sliding_window_contrasts(as.character(1:5), facname = "intensity", window_size = 2)
#'
#' # For k=3 with 7 levels (disjoint windows):
#' # A=[1,2,3] vs B=[4,5,6], then A=[2,3,4] vs B=[5,6,7]
#' sliding_window_contrasts(LETTERS[1:7], facname = "difficulty", window_size = 3)
#'
#' @export
sliding_window_contrasts <- function(levels, facname, window_size = 2, where = NULL, name_prefix = "win") {
  assertthat::assert_that(is.character(facname), length(facname) == 1, msg = "'facname' must be a single string.")
  .check_where_arg(where)
  assertthat::assert_that(is.numeric(window_size) && length(window_size) == 1 && window_size >= 1,
                          msg = "'window_size' must be a positive integer.")
  window_size <- as.integer(window_size)

  L <- length(levels)
  if (L < 2) stop("sliding_window_contrasts requires at least two levels.")
  if (2 * window_size > L) stop("'window_size' too large: requires 2*window_size <= length(levels).")

  n_con <- L - 2L * window_size + 1L
  ret <- vector("list", n_con)
  for (i in seq_len(n_con)) {
    A_levels <- levels[i:(i + window_size - 1L)]
    B_levels <- levels[(i + window_size):(i + 2L * window_size - 1L)]
    A_expr <- paste0(facname, " %in% c(", paste0('"', A_levels, '"', collapse = ","), ")")
    B_expr <- paste0(facname, " %in% c(", paste0('"', B_levels, '"', collapse = ","), ")")
    con_name <- paste0(name_prefix, "_", paste(A_levels, collapse = "-"), "_vs_", paste(B_levels, collapse = "-"))
    ret[[i]] <- pair_contrast(as.formula(paste("~", A_expr)), as.formula(paste("~", B_expr)),
                              where = where, name = con_name)
  }
  do.call(contrast_set, ret)
}




#' Pair Contrast
#'
#' @description
#' Construct a sum-to-zero contrast between two logical expressions. This function is
#' particularly useful for comparing specific conditions or combinations of conditions.
#'
#' @param A A formula representing the first logical expression in the contrast.
#' @param B A formula representing the second logical expression in the contrast.
#' @param name A character string specifying the name of the contrast (mandatory).
#' @param where An optional formula specifying the subset over which the contrast is computed.
#' @param basis NULL (default: use all basis functions), an integer vector specifying
#'   which basis function indices to include (e.g., `1`, `2:3`, `c(1,3)`), or `"all"`.
#'   Only relevant when the HRF uses multiple basis functions (e.g., bspline, FIR, Fourier).
#'   Basis indices are 1-based (1 = first basis function, 2 = second, etc.).
#' @param basis_weights NULL (default: equal weights), or a numeric vector of weights to apply
#'   to the selected basis functions. Must have one weight per selected basis function (all of
#'   them when `basis` is NULL or `"all"`) and will
#'   be normalized to sum to 1. Use this to emphasize specific temporal components (e.g.,
#'   `c(.1, .2, .4, .2, .1)` for Gaussian-like weighting emphasizing the peak).
#'
#' @return A pair_contrast_spec object containing:
#'   \item{A}{First logical expression}
#'   \item{B}{Second logical expression}
#'   \item{where}{Subsetting formula (if provided)}
#'   \item{basis}{Basis function specification (if provided)}
#'   \item{basis_weights}{Basis weights (if provided)}
#'   \item{name}{Contrast name}
#'
#' @details
#' The contrast is constructed as (A - B), where A and B are logical expressions that
#' evaluate to TRUE/FALSE for each observation. The resulting contrast weights sum to zero.
#'
#' When using multi-basis HRFs (e.g., bspline with 5 basis functions), the `basis` argument
#' allows you to test specific temporal components of the response. For example:
#' \itemize{
#'   \item{`basis = 1`}: Test only the first basis function (often the canonical/early response)
#'   \item{`basis = 2:3`}: Test the second and third basis functions together
#'   \item{`basis = NULL` or `basis = "all"`}: Test all basis functions (default behavior)
#' }
#'
#' With the default (`basis = NULL`), the single-column t-contrast places the
#' same weight on every basis column of a condition, i.e. it tests the *sum* of
#' the basis coefficients. For informed bases such as `"spmg2"`/`"spmg3"`
#' (canonical plus temporal/dispersion derivatives) that sum is rarely a
#' meaningful quantity; the SPM convention is to contrast the canonical
#' regressor only, so use `basis = 1` there, or test all components jointly
#' with an F-contrast (e.g. [oneway_contrast()]).
#'
#' The `basis_weights` argument allows non-uniform weighting across selected basis functions:
#' \itemize{
#'   \item{`basis_weights = c(.1, .2, .4, .2, .1)`}: Gaussian-like emphasis on peak
#'   \item{`basis_weights = c(1, 0, 0, 0, 0)`}: Isolate first basis (equivalent to `basis = 1`)
#'   \item{Weights are applied within each condition, maintaining contrast sum-to-zero property}
#' }
#'
#' @examples
#' # Compare faces vs scenes (all basis functions)
#' pair_contrast(~ category == "face", ~ category == "scene", name = "face_vs_scene")
#'
#' # Test only the second basis function (e.g., linear component for polynomial HRF)
#' pair_contrast(~ category == "face", ~ category == "scene",
#'              basis = 2, name = "face_vs_scene_basis2")
#'
#' # Test early response components (first 3 basis functions)
#' pair_contrast(~ category == "face", ~ category == "scene",
#'              basis = 1:3, name = "face_vs_scene_early")
#'
#' # Compare with subsetting
#' pair_contrast(~ category == "face", ~ category == "scene",
#'              name = "face_vs_scene_block1",
#'              where = ~ block == 1)
#'
#' # Complex logical expressions
#' pair_contrast(~ stimulus == "face" & emotion == "happy",
#'              ~ stimulus == "face" & emotion == "sad",
#'              name = "happy_vs_sad_faces")
#'
#' @seealso
#' \code{\link{pairwise_contrasts}} for all pairwise comparisons,
#' \code{\link{contrast_set}} for creating sets of contrasts
#'
#' @export
pair_contrast <- function(A, B, name, where = NULL, basis = NULL, basis_weights = NULL) {
  .check_formula_arg(A, "A")
  .check_formula_arg(B, "B")
  .check_name_arg(name)
  .check_where_arg(where)
  .check_basis_args(basis, basis_weights)
  structure(list(A = A, B = B, where = where, basis = basis,
                 basis_weights = basis_weights, name = name),
            class = c("pair_contrast_spec", "contrast_spec", "list"))
}


#' One-way Contrast
#'
#' @description
#' Create a one-way contrast specification
#'
#' @param A A formula specifying the contrast
#' @param name The name of the contrast
#' @param where An optional formula specifying the subset over which the contrast is computed.
#' @param basis NULL (default: use all basis functions), an integer vector specifying
#'   which basis function indices to include, or `"all"`. See \code{\link{pair_contrast}}
#'   for details on basis filtering.
#' @param basis_weights NULL (default: equal weights), or a numeric vector of weights to apply
#'   to the selected basis functions. Must have one weight per selected basis function (all of
#'   them when `basis` is NULL or `"all"`) and will
#'   be normalized to sum to 1. See \code{\link{pair_contrast}} for details on basis weighting.
#' @return A oneway_contrast_spec object that can be used to generate contrast weights
#' @examples
#' # Create a one-way contrast for a factor 'basis'
#' con <- oneway_contrast(~ basis, name = "Main_basis")
#'
#' # Create a one-way contrast with a 'where' clause
#' con <- oneway_contrast(~ basis, name = "Main_basis",
#'                       where = ~ block == 1)
#'
#' # Test only first two basis functions
#' con <- oneway_contrast(~ condition, name = "Main_early", basis = 1:2)
#'
#' @seealso \code{\link{interaction_contrast}} for testing interactions,
#'          \code{\link{pair_contrast}} for pairwise comparisons
#' @export
oneway_contrast <- function(A, name, where = NULL, basis = NULL, basis_weights = NULL) {
  .check_formula_arg(A, "A")
  .check_name_arg(name)
  .check_where_arg(where)
  .check_basis_args(basis, basis_weights)
  structure(list(A = A, B = NULL, where = where, basis = basis,
                 basis_weights = basis_weights, name = name),
            class = c("oneway_contrast_spec", "contrast_spec", "list"))
}

#' Interaction Contrast
#'
#' @description
#' Create an interaction contrast specification. The contrast tests the
#' highest-order interaction of all factors named in `A`; for example
#' `~ A * B * C` gives the three-way interaction.
#'
#' @param A A formula specifying the interaction contrast
#' @param name The name of the contrast
#' @param where An optional formula specifying the subset over which the contrast is computed.
#' @return An interaction_contrast_spec object containing the specification for
#'         generating interaction contrast weights
#' @examples
#' # Create an interaction contrast for factors A and B
#' con <- interaction_contrast(~ A * B, name = "A_by_B")
#'
#' # Create an interaction contrast with a 'where' clause
#' con <- interaction_contrast(~ A * B, name = "A_by_B",
#'                           where = ~ block == 1)
#'
#' @seealso \code{\link{oneway_contrast}} for main effects,
#'          \code{\link{pair_contrast}} for pairwise comparisons
#' @export
interaction_contrast <- function(A, name, where = NULL) {
  .check_formula_arg(A, "A")
  .check_name_arg(name)
  .check_where_arg(where)
  structure(list(A = A, B = NULL, where = where, name = name),
            class = c("interaction_contrast_spec", "contrast_spec", "list"))
}

#' Column Contrast Specification
#'
#' @description
#' Define a contrast by directly targeting design matrix columns using regex patterns.
#' This is useful for contrasts involving continuous variables or specific basis functions.
#'
#' @param pattern_A A character string containing a regex pattern to identify the
#'   columns for the positive (+) part of the contrast.
#' @param pattern_B Optional character string containing a regex pattern for the
#'   negative (-) part (for A-B type contrasts). If NULL, creates a contrast testing
#'   the average of columns matching `pattern_A` against baseline (0).
#' @param name A character string name for the contrast (mandatory).
#' @param where Currently unused for column_contrast, but kept for API consistency.
#'
#' @return A `column_contrast_spec` object containing the specification.
#'
#' @details
#' Weights are assigned as +1/nA to the nA columns matched by `pattern_A` and
#' -1/nB to the nB columns matched by `pattern_B`, so the contrast compares the
#' average effect of the 'A' columns with the average of the 'B' columns (or
#' with baseline if `pattern_B` is NULL) and sums to zero when both groups are
#' present. A contrast is evaluated within the single term it is attached to
#' (via `hrf(..., contrasts = )`); only that term's columns are candidates.
#'
#' \strong{What the patterns are matched against.} Each pattern is matched, with
#' [grepl()] semantics, against two names for every column of the term:
#' \enumerate{
#'   \item the \emph{design-matrix column name}, exactly as shown by
#'     `colnames(design_matrix(model))`: `term_tag_condition_tag`, plus a
#'     `_b##` basis suffix for multi-basis HRFs (e.g. `cond_cond.A`,
#'     `task_cond_task.face_cond.A`, `cond_cond.A_b02`). See
#'     [event_model()] for the naming scheme and how the term tag is chosen;
#'   \item the \emph{term-level condition name}, i.e. the same name without the
#'     `term_tag_` prefix (e.g. `cond.A`, `cond.A_b02`), as returned by
#'     `conditions(term, expand_basis = TRUE)`. This is accepted for backward
#'     compatibility.
#' }
#' The design-matrix names take precedence: a pattern that matches at least one
#' design-matrix column name selects exactly those columns, and the term-level
#' names are consulted only when it matches none. Both routes therefore refer
#' to the same columns (`"^cond_cond\\.A$"` and `"^cond\\.A$"` select the same
#' column). If a pattern matches in both namespaces but selects \emph{different}
#' columns (for example, an unanchored `"h"` against `hf_task.face` and
#' `hf_task.house`, where only `task.house` contains "h" at term level), the
#' pattern is ambiguous and an error is raised; anchor it on the design-matrix
#' names (`"^hf_task\\.house$"`). If a pattern matches nothing in either
#' namespace a warning lists the available column names, and an error follows
#' if neither pattern selected any column.
#'
#' Use standard R regex syntax for the patterns. Remember to escape special
#' characters (e.g., `\\[`, `\\.`, `\\*`), and anchor patterns with `^` and `$`
#' to avoid accidental partial matches.
#'
#' @seealso [event_model()] for the column naming scheme,
#'   [pair_contrast()] with `basis = ` for basis-restricted condition contrasts.
#'
#' @examples
#' des <- data.frame(
#'   onset = seq(0, 70, by = 10),
#'   run = 1,
#'   cond = factor(rep(c("A", "B"), 4))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 50, TR = 2)
#'
#' # Patterns written against the design-matrix column names
#' # (colnames are "cond_cond.A", "cond_cond.B")
#' cset <- contrast_set(
#'   column_contrast(pattern_A = "^cond_cond\\.A$",
#'                   pattern_B = "^cond_cond\\.B$", name = "A_vs_B")
#' )
#' emod <- event_model(onset ~ hrf(cond, contrasts = cset),
#'                     data = des, block = ~run, sampling_frame = sframe)
#' colnames(design_matrix(emod))
#' contrast_weights(emod)[["cond#A_vs_B"]]$offset_weights
#'
#' # Multi-basis HRF: select only the first basis function (_b01) of each
#' # condition. Column names are "cond_cond.A_b01", "cond_cond.A_b02", ...
#' cset_mb <- contrast_set(
#'   column_contrast(pattern_A = "^cond_cond\\.A_b01$",
#'                   pattern_B = "^cond_cond\\.B_b01$", name = "A_vs_B_b01"),
#'   column_contrast(pattern_A = "_b01$", name = "canonical_mean")
#' )
#' emod_mb <- event_model(onset ~ hrf(cond, basis = "spmg3", contrasts = cset_mb),
#'                        data = des, block = ~run, sampling_frame = sframe)
#' colnames(design_matrix(emod_mb))
#' contrast_weights(emod_mb)[["cond#A_vs_B_b01"]]$offset_weights
#'
#' # Legacy term-level patterns select the same columns
#' cc_legacy <- column_contrast(pattern_A = "^cond\\.A_b01$",
#'                              pattern_B = "^cond\\.B_b01$", name = "legacy")
#'
#' @export
column_contrast <- function(pattern_A, pattern_B = NULL, name, where = NULL) {
  # Input validation
  assert_that(is.character(pattern_A) && length(pattern_A) == 1,
              msg = "pattern_A must be a single character string")
  if (!is.null(pattern_B)) {
    assert_that(is.character(pattern_B) && length(pattern_B) == 1,
                msg = "pattern_B must be a single character string")
  }
  .check_name_arg(name)
  if (!is.null(where)) {
      warning("'where' argument is currently ignored for column_contrast.")
  }
  structure(list(pattern_A = pattern_A, pattern_B = pattern_B,
                 where = where, # stored even though unused
                 name = name),
            class = c("column_contrast_spec", "contrast_spec", "list"))
}

#' Polynomial Contrast
#'
#' @description
#' Create polynomial contrasts for testing trends across ordered factor levels. This is
#' particularly useful for analyzing factors with a natural ordering (e.g., time, dose).
#'
#' @param A A formula specifying the ordered factor.
#' @param name A character string identifying the contrast.
#' @param where An optional formula for subsetting the data.
#' @param degree An integer specifying the degree of the polynomial (default: 1).
#' @param value_map An optional list mapping factor levels to numeric values.
#' @param basis NULL (default: use all basis functions), an integer vector specifying
#'   which basis function indices to include, or `"all"`. See \code{\link{pair_contrast}}
#'   for details on basis filtering.
#' @param basis_weights NULL (default: equal weights), or a numeric vector of weights to apply
#'   to the selected basis functions. Must have one weight per selected basis function (all of
#'   them when `basis` is NULL or `"all"`) and will
#'   be normalized to sum to 1. See \code{\link{pair_contrast}} for details on basis weighting.
#'
#' @return A poly_contrast_spec object containing the specification for generating
#'   polynomial contrast weights.
#'
#' @details
#' The function creates orthogonal polynomial contrasts up to the specified degree.
#' These contrasts can test for linear, quadratic, cubic, and higher-order trends
#' in the data. The value_map parameter allows for non-uniform spacing between levels.
#'
#' @examples
#' # Linear trend across time points
#' pcon <- poly_contrast(~ time, name = "linear_time", degree = 1)
#'
#' # Cubic trend with custom spacing
#' pcon <- poly_contrast(~ dose, name = "dose_cubic",
#'                      degree = 3,
#'                      value_map = list("low" = 0, "med" = 2, "high" = 5))
#'
#' # Linear trend for only first basis function
#' pcon <- poly_contrast(~ dose, name = "dose_linear_basis1",
#'                      degree = 1, basis = 1)
#'
#' @seealso
#' \code{\link{oneway_contrast}} for categorical contrasts,
#' \code{\link{interaction_contrast}} for interaction effects
#'
#' @export
poly_contrast <- function(A, name, where = NULL, degree = 1, value_map = NULL, basis = NULL, basis_weights = NULL) {
  .check_formula_arg(A, "A")
  .check_name_arg(name)
  assert_that(is.numeric(degree) && length(degree) == 1 && degree >= 1,
              msg = "degree must be a positive integer")
  .check_where_arg(where)
  if (!is.null(value_map)) {
    assert_that(is.list(value_map),
                msg = "value_map must be a list")
  }
  .check_basis_args(basis, basis_weights)
  structure(list(A = A, B = NULL, where = where, degree = degree,
                 value_map = value_map, basis = basis,
                 basis_weights = basis_weights, name = name),
            class = c("poly_contrast_spec", "contrast_spec", "list"))
}

#' Shared plumbing for the cell-based `contrast_weights()` methods
#'
#' `.eval_cell_formula()` evaluates the right-hand side of a spec formula
#' with the cell table as data and the formula's own environment as the
#' enclosure, so it can use variables from where the spec was created.
#' `.contrast_where_cells()` restricts a term's cells to the spec's `where`
#' subset (and to the optional logical `select`); an error while evaluating
#' `where` becomes a warning and selects no cells.
#' `.contrast_cell_names()` returns the tags of all of the term's cells (for a
#' categorical term, its base condition names; see `.condition_cell_tags()`),
#' or `character(0)` with a warning when they are unavailable.
#' `.new_contrast()` assembles the standard contrast object;
#' `.expanded_contrast()` first moves cell-row weights onto the term's base
#' conditions (`.cell_rows_to_conditions()`) and replicates them across the
#' basis functions of a multi-basis HRF (see `.expand_and_filter_basis()`).
#' `.cell_contrast_class()` is the class of the factorial (F-type) results.
#' @keywords internal
#' @noRd
.eval_cell_formula <- function(f, cells) {
  rlang::eval_tidy(rlang::f_rhs(f), data = cells, env = rlang::f_env(f))
}

.contrast_where_cells <- function(x, term_cells, select = NULL) {
  keep <- if (!is.null(x$where)) {
    tryCatch(.eval_cell_formula(x$where, term_cells), error = function(e) {
      warning(paste("Contrast '", x$name, "': Error evaluating 'where' clause: ", e$message), call. = FALSE)
      rep(FALSE, nrow(term_cells))
    })
  } else {
    rep(TRUE, nrow(term_cells))
  }
  if (!is.null(select)) keep <- keep & select
  relevant_cells <- term_cells[keep, , drop = FALSE]
  if (nrow(relevant_cells) == 0 && nrow(term_cells) > 0) {
    warning(paste("Contrast '", x$name, "' resulted in no relevant cells after applying the 'where' clause."), call. = FALSE)
  }
  relevant_cells
}

.contrast_cell_names <- function(x, term) {
  condnames <- try(conditions(term, drop.empty = FALSE, expand_basis = FALSE), silent = TRUE)
  if (inherits(condnames, "try-error") || length(condnames) == 0) {
    warning(paste("Contrast '", x$name, "': Failed to get condition names for term '", term$varname, "'."), call. = FALSE)
    return(character(0))
  }
  .term_cell_names(term, condnames)
}

.new_contrast <- function(x, term, weights, condnames, class) {
  structure(list(term = term, name = x$name, weights = weights,
                 condnames = condnames, contrast_spec = x),
            class = class)
}

.expanded_contrast <- function(x, term, weights, class,
                               basis_spec = NULL, basis_weights = NULL) {
  expanded <- .expand_and_filter_basis(.cell_rows_to_conditions(weights, term),
                                       term, x$name,
                                       basis_spec = basis_spec,
                                       basis_weights = basis_weights)
  .new_contrast(x, term, expanded$weights, expanded$condnames, class)
}

#' Cell tag of each base condition of a term
#'
#' Cell-based contrasts assign one weight per cell of the term's categorical
#' variables. In a term that also has continuous variables (a parametric
#' modulator, e.g. `hrf(cond, RT)`), the base conditions (`cond.A_RT`) are the
#' cells crossed with the continuous columns, so their names differ from the
#' cell tags (`cond.A`). This rebuilds the condition grid the way
#' `conditions()` does and returns, for each of `condnames`
#' (`conditions(term, drop.empty = FALSE)`), the tag of its cell. For a purely
#' categorical or purely continuous term, or if the grid does not reproduce
#' `condnames`, `condnames` is returned unchanged.
#' @keywords internal
#' @noRd
.condition_cell_tags <- function(term, condnames) {
  evs <- term$events
  if (!is.list(evs) || !length(evs)) return(condnames)
  tryCatch({
    is_cat <- vapply(evs, is_categorical, logical(1))
    if (all(is_cat) || !any(is_cat)) return(condnames)
    tokens <- lapply(evs, function(ev) {
      if (is_categorical(ev)) level_token(ev$varname, levels(ev)) else columns(ev)
    })
    used <- lengths(tokens) > 0
    grid <- expand.grid(tokens[used], stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
    full <- if (nrow(grid)) apply(grid, 1, make_cond_tag) else character(0)
    if (!identical(unname(full), unname(as.character(condnames))) || !any(is_cat[used])) {
      return(condnames)
    }
    unname(apply(grid[, is_cat[used], drop = FALSE], 1, make_cond_tag))
  }, error = function(e) condnames)
}

# The distinct cell tags of `condnames` (`condnames` itself when they coincide)
.term_cell_names <- function(term, condnames) {
  tags <- .condition_cell_tags(term, condnames)
  if (identical(tags, condnames)) condnames else unique(tags)
}

#' Move cell-row weights onto a term's base conditions
#'
#' `weights` has one row per cell tag; each base condition of the term takes
#' the row of its cell (see `.condition_cell_tags()`), so a modulated term gets
#' the cell weights on its modulator columns. Conditions whose cell has no row
#' are dropped. A no-op for terms whose conditions are their cells.
#' @keywords internal
#' @noRd
.cell_rows_to_conditions <- function(weights, term) {
  if (!nrow(weights) || is.null(rownames(weights))) return(weights)
  condnames <- try(conditions(term, drop.empty = FALSE, expand_basis = FALSE), silent = TRUE)
  if (inherits(condnames, "try-error") || !length(condnames)) return(weights)
  cell_tags <- .condition_cell_tags(term, condnames)
  if (identical(cell_tags, condnames)) return(weights)
  idx <- match(cell_tags, rownames(weights))
  keep <- !is.na(idx)
  out <- weights[idx[keep], , drop = FALSE]
  rownames(out) <- condnames[keep]
  out
}

.cell_contrast_class <- function(type, weights) {
  c(type, if (ncol(weights) > 1) "Fcontrast" else "contrast",
    "cell_contrast", "contrast", "list")
}

#' Cell-wise weights for the one-way and interaction contrasts
#'
#' Restricts the term's cells to `x$where`, calls `build(relevant_cells)` for
#' the contrast matrix, and labels its rows with the cells' canonical condition
#' tags (e.g. "task.face_load.low", so `contrast_weights.event_model()` can
#' match them to design-matrix columns) and its columns `<name>_1`, ....
#' Returns a 0 x 0 matrix when the term has no categorical cells (with a
#' warning ending in `no_cells_msg`) or `where` selects none.
#' @keywords internal
#' @noRd
.factorial_cell_weights <- function(x, term, no_cells_msg, build) {
  term_cells <- cells(term)
  if (nrow(term_cells) == 0) {
    warning(paste("Contrast '", x$name, "': Term '", term$varname, no_cells_msg), call. = FALSE)
    return(matrix(numeric(0), nrow = 0, ncol = 0))
  }
  relevant_cells <- .contrast_where_cells(x, term_cells)
  if (nrow(relevant_cells) == 0) {
    return(matrix(numeric(0), nrow = 0, ncol = 0))
  }
  cmat <- build(relevant_cells)
  rownames(cmat) <- cell_condition_tags(relevant_cells)
  colnames(cmat) <- paste(x$name, seq_len(ncol(cmat)), sep = "_")
  cmat
}

#' Unit Contrast Weights
#'
#' @description
#' Compute the contrast weights for a unit_contrast_spec object.
#'
#' @param x A unit_contrast_spec object.
#' @param term A term object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list containing the term, name, weights, condition names, and contrast specification.
#'
#' @rdname contrast_weights
#' @export
contrast_weights.unit_contrast_spec <- function(x, term,...) {
  # All possible cells (for a categorical term, its pre-basis condition names)
  all_condnames <- .contrast_cell_names(x, term)

  weights_out <- matrix(0, nrow = length(all_condnames), ncol = 1)
  rownames(weights_out) <- all_condnames
  colnames(weights_out) <- x$name

  term_cells <- cells(term)
  if (nrow(term_cells) > 0 && length(all_condnames) > 0) {
      # A logical selector in `A` (e.g. `~ cond == "A"`) restricts the cells
      # further; a bare factor name (e.g. `~ cond`) selects every cell.
      sel <- tryCatch(rlang::eval_tidy(rlang::f_rhs(x$A), data = term_cells),
                      error = function(e) NULL)
      sel <- if (is.logical(sel) && length(sel) == nrow(term_cells)) !is.na(sel) & sel
      relevant_cells <- .contrast_where_cells(x, term_cells, select = sel)

      if (nrow(relevant_cells) > 0) {
          idx <- match(cell_condition_tags(relevant_cells), all_condnames)
          mask_A_full <- logical(length(all_condnames))
          mask_A_full[idx[!is.na(idx)]] <- TRUE
          weights_out[,1] <- .calculate_mask_weights(all_condnames, mask_A_full)
      }
  }

  # Replicate across all basis functions of a multi-basis HRF
  .expanded_contrast(x, term, weights_out,
                     c("unit_contrast", "cell_contrast", "contrast", "list"))
}


#' @export
`-.contrast_spec` <- function(e1, e2, ...){
  assert_that(inherits(e2, "contrast_spec"))
  structure(list(
    name=paste0(e1$name, ":", e2$name),
    con1=e1,
    con2=e2),
    class=c("contrast_diff_spec", "contrast_spec", "list")
  )
}


#' One-way Contrast Weights
#'
#' @description
#' Compute the contrast weights for an oneway_contrast_spec object.
#'
#' @param x An oneway_contrast_spec object.
#' @param term A term object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list containing the term, name, weights, condition names, and contrast specification.
#'
#' @rdname contrast_weights
#' @export
contrast_weights.oneway_contrast_spec <- function(x, term,...) {
  weights_out <- .factorial_cell_weights(x, term, "' has no categorical cells.", function(relevant_cells) {
      # Identify the factor for the main effect
      fac_name <- all.vars(rlang::f_rhs(x$A))
      if (length(fac_name) > 1) {
          warning(paste("Contrast '", x$name, "': one-way contrast has >1 factor specified (", 
                        paste(fac_name, collapse=", "), "), using first: ", fac_name[1]), call.=FALSE)
          fac_name <- fac_name[1]
      }
      if (!(fac_name %in% names(relevant_cells))){
           stop(paste("Contrast '", x$name, "': factor ", fac_name, " not found in relevant cells."), call.=FALSE)
      }
      tryCatch(generate_main_effect_contrast(relevant_cells, fac_name), error = function(e) {
          stop(paste("Contrast '", x$name, "': Error generating main effect contrast for factor ", fac_name, ": ", e$message), call.=FALSE)
      })
  })

  .expanded_contrast(x, term, weights_out,
                     .cell_contrast_class("oneway_contrast", weights_out),
                     basis_spec = x$basis, basis_weights = x$basis_weights)
}

#' Interaction Contrast Weights
#'
#' @description
#' Compute the contrast weights for an interaction_contrast_spec object.
#'
#' @param x An interaction_contrast_spec object.
#' @param term A term object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list containing the term, name, weights, condition names, and contrast specification.
#'
#' @rdname contrast_weights
#' @export
contrast_weights.interaction_contrast_spec <- function(x, term,...) {
  weights_out <- .factorial_cell_weights(x, term, "' has no categorical cells for interaction.", function(relevant_cells) {
      # Identify factors for interaction
      factors <- all.vars(rlang::f_rhs(x$A))
      if (length(factors) < 2) {
          stop(paste("Contrast '", x$name, "': Interaction contrast requires at least two factors."), call.=FALSE)
      }
      if (!all(factors %in% names(relevant_cells))) {
          missing_facs <- factors[!factors %in% names(relevant_cells)]
          stop(paste("Contrast '", x$name, "': Factor(s)", paste(missing_facs, collapse=", "), "not found in relevant cells."), call.=FALSE)
      }
      tryCatch(generate_interaction_contrast(relevant_cells, factors), error = function(e) {
          stop(paste("Contrast '", x$name, "': Error generating interaction contrast: ", e$message), call.=FALSE)
      })
  })

  # Replicate across all basis functions of a multi-basis HRF
  .expanded_contrast(x, term, weights_out,
                     .cell_contrast_class("interaction_contrast", weights_out))
}

#' Helmert-coded cell weights for main-effect and interaction contrasts
#'
#' `generate_main_effect_contrast()` returns an n_cells x (k - 1) matrix for
#' one factor. `generate_interaction_contrast()` returns all column-wise
#' products of the main-effect codings of `factors` (the highest-order
#' interaction, prod(k - 1) columns, the last factor's index varying fastest).
#' Callers set the dimnames.
#' @keywords internal
#' @noRd
generate_main_effect_contrast <- function(relevant_cells, fac_name) {
  f <- factor(relevant_cells[[fac_name]])
  if (nlevels(f) < 2) stop("Need at least 2 levels to form a main-effect contrast")
  .helmert_cells(f)
}

generate_interaction_contrast <- function(relevant_cells, factors) {
  if (length(factors) < 2) stop("At least two factors required for interaction contrast")
  facs <- lapply(factors, function(f) factor(relevant_cells[[f]]))
  if (any(vapply(facs, nlevels, integer(1)) < 2)) stop("Each factor needs >= 2 levels for interaction")
  # All products of one column from each factor's coding (n x prod(k - 1))
  Reduce(function(A, f) {
    B <- .helmert_cells(f)
    A[, rep(seq_len(ncol(A)), each = ncol(B)), drop = FALSE] *
      B[, rep(seq_len(ncol(B)), times = ncol(A)), drop = FALSE]
  }, facs[-1], .helmert_cells(facs[[1]]))
}

# n x (k-1) weights per cell: level indicators times the Helmert basis
.helmert_cells <- function(f) {
  model.matrix(~ f - 1) %*% stats::contr.helmert(nlevels(f))
}

#' Polynomial Contrast Weights
#'
#' @description
#' Compute the contrast weights for a poly_contrast_spec object.
#'
#' @param x A poly_contrast_spec object.
#' @param term A term object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list containing the term, name, weights, condition names, and contrast specification.
#'
#' @rdname contrast_weights
#' @export
contrast_weights.poly_contrast_spec <- function(x, term,...) {
  all_condnames <- .contrast_cell_names(x, term)

  weights_out <- matrix(0, nrow = length(all_condnames), ncol = x$degree)
  rownames(weights_out) <- all_condnames
  colnames(weights_out) <- paste(x$name, 1:x$degree, sep="_")

  term_cells <- cells(term)
  if (nrow(term_cells) > 0 && length(all_condnames) > 0) {
      relevant_cells <- .contrast_where_cells(x, term_cells)

      if (nrow(relevant_cells) > 0) {
          vals_fac <- tryCatch(rlang::eval_tidy(rlang::f_rhs(x$A), data = relevant_cells), error = function(e) {
              stop(paste("Contrast '", x$name, "': Error evaluating formula A: ", e$message), call.=FALSE)
          })

          vals_num <- if (is.null(x$value_map)) {
              tryCatch(as.numeric(as.character(vals_fac)), warning = function(w){
                  stop(paste("Contrast '", x$name, "': Cannot coerce factor levels from formula A to numeric for poly contrast. Use value_map? Error: ", w$message), call.=FALSE)
              })
          } else {
              levs <- as.character(vals_fac)
              mapped_vals <- x$value_map[levs]
              if (!all(levs %in% names(x$value_map)) || anyNA(mapped_vals)) {
                  stop(paste("Contrast '", x$name, "': value_map does not cover all factor levels present in relevant cells."), call.=FALSE)
              }
              unlist(mapped_vals)
          }

          if (length(unique(vals_num)) <= x$degree) {
               stop(paste("Contrast '", x$name, "': Polynomial degree (", x$degree,
                           ") is too high for the number of unique points (", length(unique(vals_num)), ") in relevant cells."), call.=FALSE)
          }

          pvals_mat <- tryCatch(stats::poly(vals_num, degree = x$degree), error = function(e){
               stop(paste("Contrast '", x$name, "': Error calculating polynomial weights: ", e$message), call.=FALSE)
          })

          target_cond_names <- cell_condition_tags(relevant_cells)
          idx <- match(target_cond_names, all_condnames)
          valid_idx <- which(!is.na(idx))
          if (length(valid_idx) > 0) {
              weights_out[idx[valid_idx], ] <- pvals_mat[valid_idx, , drop = FALSE]
          }
      }
  }

  .expanded_contrast(x, term, weights_out,
                     .cell_contrast_class("poly_contrast", weights_out),
                     basis_spec = x$basis, basis_weights = x$basis_weights)
}

#' Pair Contrast Weights
#'
#' @description
#' Compute the contrast weights for a pair_contrast_spec object.
#'
#' @param x A pair_contrast_spec object.
#' @param term A term object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list containing the term, name, weights, condition names, and contrast specification.
#'
#' @rdname contrast_weights
#' @export
contrast_weights.pair_contrast_spec <- function(x, term,...) {
  # Get cells (only categorical combinations) for evaluating formulas A & B
  term_cells <- cells(term)

  # The term's cells (for a categorical term, its pre-basis condition names)
  # are the targets for the weights; mapping onto the conditions and basis
  # expansion / filtering happen once the base mask is built.
  base_condnames_all <- try(conditions(term, drop.empty=FALSE, expand_basis=FALSE), silent=TRUE)
  if (inherits(base_condnames_all, "try-error") || length(base_condnames_all) == 0) {
      warning(paste("Contrast '", x$name, "': Failed to get base condition names for term '", term$varname, "'. Skipping."), call. = FALSE)
      return(NULL) # Return NULL if cannot proceed
  }
  base_condnames_all <- .term_cell_names(term, base_condnames_all)
  
  if (nrow(term_cells) == 0) {
      # If term_cells is empty, we can still proceed if base_condnames_all exist,
      # but the contrast formulas A/B likely won't match anything.
      warning(paste("Contrast '", x$name, "': Term '", term$varname, "' has no observed categorical cells. Contrast weights might be all zero."), call. = FALSE)
  }
  
  # Restrict to the 'where' subset; if it is empty the weights will be zero
  relevant_cells <- .contrast_where_cells(x, term_cells)

  # Evaluate A and B formulas on the relevant cells
  keepA_rel <- if(nrow(relevant_cells) > 0) tryCatch(rlang::eval_tidy(rlang::f_rhs(x$A), data = relevant_cells), error = function(e) {
      warning(paste("Contrast '", x$name, "': Error evaluating formula A: ", e$message), call. = FALSE)
      logical(nrow(relevant_cells)) # Return all FALSE on error
  }) else logical(0)
  
  keepB_rel <- if (is.null(x$B) || nrow(relevant_cells) == 0) NULL else {
      tryCatch(rlang::eval_tidy(rlang::f_rhs(x$B), data = relevant_cells), error = function(e) {
          warning(paste("Contrast '", x$name, "': Error evaluating formula B: ", e$message), call. = FALSE)
          logical(nrow(relevant_cells)) # Return all FALSE on error
      })
  }
  
  # Map the per-cell selections onto the base condition names
  idx_all <- if (nrow(relevant_cells) > 0) {
    match(cell_condition_tags(relevant_cells), base_condnames_all)
  } else integer(0)
  to_mask <- function(keep_rel) {
    mask <- logical(length(base_condnames_all))
    n <- min(length(keep_rel), length(idx_all))
    if (n > 0) {
      idx <- idx_all[seq_len(n)]
      mask[idx[keep_rel[seq_len(n)] & !is.na(idx)]] <- TRUE
    }
    mask
  }
  mask_A_full <- to_mask(keepA_rel)
  mask_B_full <- if (!is.null(keepB_rel)) to_mask(keepB_rel)

  # Calculate base weights relative to the full set of base conditions
  base_weights_named <- .calculate_mask_weights(base_condnames_all, mask_A_full, mask_B_full)

  # Package as base mask matrix and delegate basis expansion / filtering.
  weights_base <- matrix(base_weights_named, ncol = 1)
  rownames(weights_base) <- names(base_weights_named)
  colnames(weights_base) <- x$name

  .expanded_contrast(x, term, weights_base, c("cell_contrast", "contrast", "list"),
                     basis_spec = x$basis, basis_weights = x$basis_weights)
}

#' Column Contrast Weights
#'
#' @description
#' Compute contrast weights for a `column_contrast_spec` object by targeting
#' the term's design matrix columns with regex patterns. Patterns are matched
#' against the design-matrix column names (`term_tag_condition_tag[_b##]`)
#' first and, only if they match none, against the term-level condition names;
#' see [column_contrast()] for the precedence rules.
#'
#' @param x A `column_contrast_spec` object.
#' @param term An `event_term` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list containing the contrast details:
#'   \item{term}{The original `event_term` object.}
#'   \item{name}{The name of the contrast.}
#'   \item{weights}{A numeric matrix with one row per column of the term (row
#'                  names are the term-level condition names from
#'                  `conditions(term, expand_basis = TRUE)`, in design-matrix
#'                  column order) and one column per contrast (usually one).}
#'   \item{condnames}{Character vector of all *expanded* condition names from `term`.}
#'   \item{contrast_spec}{The original `column_contrast_spec` object.}
#'
#' @rdname contrast_weights
#' @export
#' @import assertthat
contrast_weights.column_contrast_spec <- function(x, term, ...) {

  # Term-level (expanded) condition names; these index the weight rows.
  all_condnames <- .condnames(term, expanded = TRUE)
  if (length(all_condnames) == 0) {
      # It's possible conditions() returns empty if term has no levels/columns
      warning(paste("Column contrast '", x$name, "': Term '", term$varname %||% "<unknown>",
                    "' resulted in zero condition names. Weights will be empty."), call. = FALSE)
      weights_out <- matrix(numeric(0), nrow = 0, ncol = 1)
      colnames(weights_out) <- x$name
      return(.new_contrast(x, term, weights_out, character(0),
                           c("column_contrast", "contrast", "list")))
  }

  # Full design-matrix column names, aligned one-to-one with all_condnames.
  all_colnames <- .term_design_colnames(term, all_condnames)
  term_label <- attr(term, "term_tag") %||% term$varname %||% "<unknown>"

  idx_A <- .match_column_pattern(x$pattern_A, "pattern_A", all_colnames,
                                 all_condnames, x$name, term_label)
  idx_B <- if (!is.null(x$pattern_B)) {
    .match_column_pattern(x$pattern_B, "pattern_B", all_colnames,
                          all_condnames, x$name, term_label)
  } else integer(0)

  if (any(idx_A %in% idx_B)) {
    stop(paste("Column contrast '", x$name, "': pattern_A and pattern_B match overlapping columns.",
               " Indices A: ", paste(idx_A, collapse=", "),
               "; Indices B: ", paste(idx_B, collapse=", ")),
         call. = FALSE)
  }

  mask_A <- seq_along(all_condnames) %in% idx_A
  mask_B <- if (!is.null(x$pattern_B)) seq_along(all_condnames) %in% idx_B else NULL

  # .calculate_mask_weights handles 1/nA, -1/nB, checks, and warnings
  weights_vec <- .calculate_mask_weights(all_condnames, mask_A, mask_B)

  # Ensure output is a matrix. Rows are term-level condition names, like every
  # other contrast type; contrast_weights.event_model() maps them to the
  # design-matrix columns.
  weights_mat <- matrix(weights_vec, ncol = 1)
  rownames(weights_mat) <- all_condnames
  colnames(weights_mat) <- x$name

  # condnames are the expanded names used for the weights
  .new_contrast(x, term, weights_mat, all_condnames,
                c("column_contrast", "contrast", "list"))
}

#' Design-matrix column names for a term's expanded conditions
#'
#' Maps the term-level expanded condition names (`condition_tag[_b##]`) to the
#' full design-matrix column names (`term_tag_condition_tag[_b##]`) using
#' `make_column_names()`, the same helper the convolution paths use. The result
#' is aligned element-wise with `condnames`. If the term carries no tag (a bare
#' `event_term` or an `Ident()`-only term), the column names equal the
#' condition names. If the basis layout cannot be reconstructed, the condition
#' names are returned unchanged.
#'
#' @param term An event_term (or feature_term) object.
#' @param condnames Expanded condition names, `.condnames(term, TRUE)`.
#' @return Character vector the same length as `condnames`.
#' @keywords internal
#' @noRd
.term_design_colnames <- function(term, condnames) {
  term_tag <- attr(term, "term_tag")
  # Feature terms fall back to their varname as the tag (see
  # .convolve_feature_term_matrix()).
  if (is.null(term_tag) && inherits(term, "feature_term")) {
    term_tag <- term$varname
  }
  if (is.null(term_tag) || length(condnames) == 0L) {
    return(condnames)
  }
  base <- .condnames(term, expanded = FALSE)
  if (length(base) == 0L || length(condnames) %% length(base) != 0L) {
    return(condnames)
  }
  nb <- length(condnames) %/% length(base)
  if (!identical(add_basis(base, nb), condnames)) {
    return(condnames)
  }
  make_column_names(term_tag, base, nb)
}

#' Resolve a column_contrast pattern to column indices
#'
#' The pattern is matched against the full design-matrix column names first.
#' If it matches none, it is matched against the term-level condition names
#' (backward compatibility). A pattern that matches in both namespaces must
#' select the same columns; otherwise it is ambiguous and an error is raised.
#' When nothing matches in either namespace a warning lists the candidates and
#' `integer(0)` is returned.
#'
#' @keywords internal
#' @noRd
.match_column_pattern <- function(pattern, which, colnames_full, condnames,
                                   contrast_name, term_label) {
  idx_full <- grep(pattern, colnames_full)
  same_ns <- identical(colnames_full, condnames)
  idx_cond <- if (same_ns) idx_full else grep(pattern, condnames)

  if (length(idx_full) > 0L) {
    if (length(idx_cond) > 0L && !identical(idx_full, idx_cond)) {
      stop(sprintf(paste0(
        "Column contrast '%s': %s ('%s') is ambiguous for term '%s': it selects ",
        "design-matrix columns [%s] but term-level condition names [%s]. ",
        "Anchor the pattern on the design-matrix column names, e.g. '^%s$'."),
        contrast_name, which, pattern, term_label,
        paste(colnames_full[idx_full], collapse = ", "),
        paste(condnames[idx_cond], collapse = ", "),
        gsub(".", "\\\\.", colnames_full[idx_full[1L]], fixed = TRUE)),
        call. = FALSE)
    }
    return(idx_full)
  }
  if (length(idx_cond) > 0L) {
    return(idx_cond)
  }

  msg <- sprintf(
    "Column contrast '%s': %s ('%s') matched no design-matrix column of term '%s'. Available columns: %s.",
    contrast_name, which, pattern, term_label, .truncate_names(colnames_full)
  )
  if (!same_ns) {
    msg <- paste0(msg, sprintf(
      " The term-level condition names were also tried: %s.",
      .truncate_names(condnames)
    ))
  }
  warning(msg, call. = FALSE)
  integer(0)
}

#' Collapse a name vector for messages, truncating long vectors
#' @keywords internal
#' @noRd
.truncate_names <- function(x, max_n = 10L) {
  if (length(x) <= max_n) {
    return(paste(x, collapse = ", "))
  }
  paste0(paste(x[seq_len(max_n)], collapse = ", "),
         sprintf(", ... and %d more", length(x) - max_n))
}

#' Contrast Formula Weights
#'
#' @description
#' Compute the contrast weights for a contrast_formula_spec object.
#'
#' @param x A contrast_formula_spec object.
#' @param term A term object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list containing the term, name, weights, condition names, and contrast specification.
#'
#' @rdname contrast_weights
#' @export
contrast_weights.contrast_formula_spec <- function(x, term,...) {

  term.cells <- cells(term)
  name_map <- .contrast_formula_name_map(term)
  condnames <- name_map$canonical
  display_names <- name_map$display
  
  if (!is.null(x$where)) {
    keep <- .eval_cell_formula(x$where, term.cells)
    assert_that(sum(keep) > 0)
    term.cells <- term.cells[keep, , drop = FALSE]
    condnames <- condnames[keep]
    display_names <- display_names[keep]
  }
  
  # One row per condition name. (A continuous term has a single cell but may
  # have several conditions, e.g. hrf(Ident(RT1, RT2)).)
  # Display names are primary; canonical names remain available as stable aliases.
  weights <- matrix(0, length(condnames), 1)
  eval_env <- .build_formula_contrast_env(display_names, condnames, rlang::f_env(x$A))
  
  # Evaluate the contrast formula
  res <- tryCatch(rlang::eval_tidy(rlang::f_rhs(x$A), env=eval_env), error = function(e) {
       stop(paste("Contrast formula evaluation failed:", e$message, "\nAvailable display names:", paste(display_names, collapse=", ")), call.=FALSE)
  })
  
  # Apply results to weights matrix
  weights[,1] <- as.vector(res)
  
  # Canonical condition names are the public row labels.
  row.names(weights) <- condnames
  colnames(weights) <- x$name

  # Replicate across all basis functions of a multi-basis HRF
  .expanded_contrast(x, term, weights, c("contrast", "list"))
}

#' Contrast Difference Weights
#'
#' @description
#' Compute the contrast weights for a contrast_diff_spec object.
#'
#' @param x A contrast_diff_spec object.
#' @param term A term object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list containing the term, name, weights, condition names, and contrast specification.
#'
#' @rdname contrast_weights
#' @export
contrast_weights.contrast_diff_spec <- function(x, term,...) {
  wts1 <- contrast_weights(x$con1, term)
  wts2 <- contrast_weights(x$con2, term)
  .new_contrast(x, term, wts1$weights - wts2$weights,
                rownames(wts1$weights) %||% longnames(term),
                c("contrast_diff", "contrast"))
}

#' Print Contrast Set
#'
#' @description
#' Print a contrast set.
#'
#' @param x The contrast set to print.
#' @param ... Additional arguments (currently unused).
#'
#' @export
#' @rdname print
print.contrast_set <- function(x, ...) {
  n_contrasts <- length(x)
  
  # Header
  cat("\n=== Contrast Set ===\n")
  
  # Summary
  cat("\n Overview:\n")
  cat("  * Number of contrasts:", n_contrasts, "\n")
  
  # Group contrasts by type
  types <- sapply(x, function(con) class(con)[1])
  type_table <- table(types)
  if (length(type_table) > 0) {
    cat("  * Types of contrasts:\n")
    for (type in names(type_table)) {
      cat("    -", type, ":", type_table[type], "\n")
    }
  }
  
  # List all contrasts
  cat("\n  Individual Contrasts:\n")
  for (i in seq_along(x)) {
    cat("\n[", i, "] ", x[[i]]$name, " (", class(x[[i]])[1], ")\n", sep="")
    cat("    Formula: ")
    if (!is.null(x[[i]]$A)) cat(deparse(x[[i]]$A))
    if (!is.null(x[[i]]$B)) cat(" vs ", deparse(x[[i]]$B))
    cat("\n")
    if (!is.null(x[[i]]$where)) {
      cat("    Subset: ", deparse(x[[i]]$where), "\n")
    }
  }
  
  cat("\n")
  invisible(x)
}

#' Print Contrast Specification
#'
#' @description
#' Print a contrast specification.
#'
#' @param x The contrast specification to print.
#' @param ... Additional arguments (currently unused).
#'
#' @export
#' @rdname print
print.contrast_spec <- function(x,...) {
  cat("contrast:", x$name, "\n")
  cat(" A: ", Reduce(paste, deparse(x$A)), "\n")
  if (!is.null(x$B))
    cat(" B: ", Reduce(paste, deparse(x$B)), "\n")
  if (!is.null(x$where))
    cat(" where: ", Reduce(paste, deparse(x$where)), "\n")
  

}

#' Print Contrast
#'
#' @description
#' Print a contrast.
#'
#' @param x The contrast to print.
#' @param ... Additional arguments (currently unused).
#'
#' @export
#' @rdname print
print.contrast <- function(x,...) {
  print(x$contrast_spec)
  cat(" term: ", x$term$varname, "\n")
  cat(" weights: ", "\n")
  print(x$weights)
  cat(" conditions: ", x$condnames)
  
}

#' Print Polynomial Contrast Specification
#'
#' @description
#' Print a polynomial contrast specification.
#'
#' @param x The polynomial contrast specification to print.
#' @param ... Additional arguments (currently unused).
#'
#' @export
#' @rdname print
print.poly_contrast_spec <- function(x,...) {
  cat("poly contrast:", "\n")
  cat(" A: ", Reduce(paste, deparse(x$A)), "\n")
  cat(" degree: ", x$degree, "\n")
  if (!is.null(x$where)) {
    cat(" where: ", deparse(x$where), "\n")
  }
  
  if (!is.null(x$value_map)) {
    cat(" values: ", unlist(x$value_map), "\n")
  }
}

#' Print Contrast Difference Specification
#'
#' @description
#' Print a contrast difference specification.
#'
#' @param x The contrast difference specification to print.
#' @param ... Additional arguments (currently unused).
#'
#' @export
#' @rdname print
print.contrast_diff_spec <- function(x,...) {
  cat("contrast difference:", "\n")
  cat("  ", x$con1$name, "-", x$con2$name, "\n")
}

#' plot_contrasts
#'
#' @description
#' Generic function for plotting contrasts.
#'
#' @param x Object containing contrast information
#' @param ... Additional arguments passed to methods
#' @return A plot object (typically ggplot2) displaying the contrasts. The exact type depends on the method used.
#' @examples
#' # Create example data
#' des <- data.frame(
#'   onset = c(1, 3, 5, 7),
#'   cond = factor(c("A", "B", "A", "B")),
#'   run = c(1, 1, 1, 1)
#' )
#'
#' # Create sampling frame and event model
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#'
#' # Create contrast set
#' cset <- contrast_set(
#'   main_A = unit_contrast(~ cond == "A", name = "A_vs_baseline"),
#'   diff = pair_contrast(~ cond == "A", ~ cond == "B", name = "A_vs_B")
#' )
#'
#' # Create event model with contrasts
#' emod <- event_model(onset ~ hrf(cond, contrasts = cset),
#'                     data = des, block = ~run, sampling_frame = sframe)
#'
#' # Plot the contrasts
#' plot_contrasts(emod)
#'
#' @export
plot_contrasts <- function(x, ...) {
  UseMethod("plot_contrasts")
}

#' Contrast Weights for a Contrast Set
#'
#' @description
#' Compute the contrast weights for each contrast specification within a contrast_set object.
#'
#' @param x A contrast_set object (a list of contrast_spec objects).
#' @param term A term object against which weights should be computed.
#' @param ... Additional arguments passed to individual contrast_weights methods.
#'
#' @return A named list where each element is the result of calling contrast_weights 
#'         on the corresponding contrast_spec in the set. The list names are the 
#'         names of the individual contrasts.
#'
#' @rdname contrast_weights
#' @export
#' @importFrom purrr map set_names
contrast_weights.contrast_set <- function(x, term, ...) {
  # Ensure x is a list (contrast_set inherits from list)
  if (!is.list(x)) {
    stop("Input 'x' must be a contrast_set (list).")
  }
  
  # Iterate through each contrast spec in the set
  results_list <- purrr::map(x, function(contrast_spec) {
    # Check if the element is actually a contrast_spec
    if (!inherits(contrast_spec, "contrast_spec")) {
      warning(paste("Element", contrast_spec$name, "is not a contrast_spec object, skipping."))
      return(NULL)
    }
    # Compute weights for the individual contrast spec
    contrast_weights(contrast_spec, term, ...)
  })
  
  # Filter out any NULL results (if any elements weren't contrast_spec)
  results_list <- results_list[!sapply(results_list, is.null)]
  
  # Set the names of the results list based on the names of the contrasts
  contrast_names <- purrr::map_chr(results_list, "name")
  results_list <- purrr::set_names(results_list, contrast_names)
  
  return(results_list)
}
