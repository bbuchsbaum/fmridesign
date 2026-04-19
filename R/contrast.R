# Internal constants
.CONTRAST_TOLERANCE <- 1e-8

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

  # Convert basis indices to expected suffix patterns
  # For nbasis = 5, basis indices are 1-based, so basis = 2 means "_b02"
  pad <- max(2, ceiling(log10(nbasis + 1)))
  basis_suffixes <- sprintf(paste0("_b%0", pad, "d"), basis)

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

#' Apply basis filtering and weighting to contrast weights
#'
#' Unified helper function for applying basis filtering across all contrast types.
#' Detects nbasis from term, validates basis parameter, filters expanded names,
#' zeros out non-selected weights, applies basis_weights, and validates sum-to-zero property.
#'
#' @param weights_mat Numeric matrix of weights (rows = conditions, cols = contrasts).
#' @param term An event_term object with hrfspec attribute.
#' @param basis_spec NULL (no filtering), integer vector (basis indices), or "all".
#' @param basis_weights_spec NULL (equal weights), or numeric vector of weights for selected bases.
#' @param contrast_name Name of contrast (for error messages).
#' @param expanded_condnames Character vector of expanded condition names (with _b## suffixes).
#' @return List with:
#'   \item{weights}{Updated weights matrix with basis filtering and weighting applied.}
#'   \item{condnames}{Filtered condition names (only selected basis functions).}
#'   \item{nbasis}{Number of basis functions detected.}
# Detect nbasis from a term's hrfspec.
# Returns list(nbasis = int, expand_basis = logical).
#' @keywords internal
#' @noRd
.detect_nbasis <- function(term) {
  hrfspec <- attr(term, "hrfspec")
  if (is.null(hrfspec) || is.null(hrfspec$hrf)) {
    return(list(nbasis = 1L, expand_basis = FALSE))
  }
  nb <- try(fmrihrf::nbasis(hrfspec$hrf), silent = TRUE)
  if (inherits(nb, "try-error") || !is.numeric(nb) || nb <= 1L) {
    return(list(nbasis = 1L, expand_basis = FALSE))
  }
  list(nbasis = as.integer(nb), expand_basis = TRUE)
}

# Redistribute a per-base-condition weight across its selected bases. All
# rows matching `<base>_b##` are set to `base_weight * basis_weights_spec`,
# where base_weight is the first nonzero entry for that base. The input
# `basis_weights_spec` has already been length-checked and normalized.
#' @keywords internal
#' @noRd
.apply_basis_weights <- function(weights_mat, filtered_condnames, basis_weights_spec) {
  base_names <- unique(gsub("_b\\d+$", "", filtered_condnames))
  for (base_name in base_names) {
    pattern <- paste0("^", .regex_escape(base_name), "_b\\d+$")
    matching_rows <- grep(pattern, rownames(weights_mat), perl = TRUE)
    selected_rows <- matching_rows[rownames(weights_mat)[matching_rows] %in% filtered_condnames]
    if (length(selected_rows) == 0L) next

    for (col_idx in seq_len(ncol(weights_mat))) {
      current <- weights_mat[selected_rows, col_idx]
      if (any(current != 0)) {
        base_weight <- current[current != 0][1]
        weights_mat[selected_rows, col_idx] <- base_weight * basis_weights_spec
      }
    }
  }
  weights_mat
}

# Warn if any contrast column's kept (non-zero-filtered) weights don't sum
# to zero. Callers are expected to have zeroed out the non-kept rows already.
#' @keywords internal
#' @noRd
.warn_if_not_sum_zero <- function(weights_mat, keep_mask, contrast_name) {
  for (col_idx in seq_len(ncol(weights_mat))) {
    nonzero <- weights_mat[keep_mask, col_idx]
    if (length(nonzero) > 0L && abs(sum(nonzero)) > .CONTRAST_TOLERANCE) {
      warning(sprintf("Contrast '%s' (column %d): Weights do not sum to zero after basis filtering (sum = %.6f).",
                      contrast_name, col_idx, sum(nonzero)), call. = FALSE)
    }
  }
}

# Normalize a user-supplied basis_weights vector to sum to 1, validating its
# length against the number of selected bases.
#' @keywords internal
#' @noRd
.normalize_basis_weights <- function(basis_weights_spec, basis_spec, nbasis, contrast_name) {
  n_selected <- if (is.null(basis_spec) || identical(basis_spec, "all")) {
    nbasis
  } else {
    length(basis_spec)
  }
  if (length(basis_weights_spec) != n_selected) {
    stop(sprintf("Contrast '%s': basis_weights length (%d) must match number of selected basis functions (%d)",
                 contrast_name, length(basis_weights_spec), n_selected), call. = FALSE)
  }
  total <- sum(basis_weights_spec)
  if (abs(total - 1.0) > .CONTRAST_TOLERANCE) {
    warning(sprintf("Contrast '%s': basis_weights sum to %.6f, normalizing to sum to 1.0",
                    contrast_name, total), call. = FALSE)
    basis_weights_spec <- basis_weights_spec / total
  }
  basis_weights_spec
}

#' @keywords internal
#' @noRd
.apply_basis_filter <- function(weights_mat, term, basis_spec, contrast_name,
                                 expanded_condnames = NULL, basis_weights_spec = NULL) {
  nb <- .detect_nbasis(term)

  condnames_out <- function() {
    if (is.null(expanded_condnames)) rownames(weights_mat) else expanded_condnames
  }

  # No multi-basis HRF: nothing to expand or filter. Warn if user tried anyway.
  if (!nb$expand_basis) {
    if (!is.null(basis_spec) && !identical(basis_spec, "all")) {
      warning(sprintf("Contrast '%s': basis filtering requested but term has no multi-basis HRF (nbasis = %d). Ignoring basis filter.",
                      contrast_name, nb$nbasis), call. = FALSE)
    }
    return(list(weights = weights_mat, condnames = condnames_out(), nbasis = nb$nbasis))
  }

  # Multi-basis HRF with no filtering: return as-is.
  if (is.null(basis_spec) || identical(basis_spec, "all")) {
    return(list(weights = weights_mat, condnames = condnames_out(), nbasis = nb$nbasis))
  }

  # Need expanded condition names to filter.
  if (is.null(expanded_condnames)) {
    expanded_condnames <- rownames(weights_mat)
    if (is.null(expanded_condnames)) {
      warning(sprintf("Contrast '%s': Cannot apply basis filtering without condition names.",
                      contrast_name), call. = FALSE)
      return(list(weights = weights_mat, condnames = character(0), nbasis = nb$nbasis))
    }
  }

  filtered_condnames <- .filter_basis(expanded_condnames, basis = basis_spec,
                                      nbasis = nb$nbasis, contrast_name = contrast_name)

  keep_mask <- rownames(weights_mat) %in% filtered_condnames
  weights_mat[!keep_mask, ] <- 0

  if (!is.null(basis_weights_spec)) {
    basis_weights_spec <- .normalize_basis_weights(basis_weights_spec, basis_spec,
                                                   nb$nbasis, contrast_name)
    weights_mat <- .apply_basis_weights(weights_mat, filtered_condnames, basis_weights_spec)
  }

  .warn_if_not_sum_zero(weights_mat, keep_mask, contrast_name)

  list(weights = weights_mat, condnames = filtered_condnames, nbasis = nb$nbasis)
}

#' Calculate contrast weights from logical masks
#' 
#' This is the unified function for weight calculation from masks.
#' Returns a *named* numeric vector.
#' 
#' @param names Character vector of all condition names.
#' @param A_mask Logical vector (same length as names) indicating TRUE for conditions in group A.
#' @param B_mask Logical vector (same length as names) indicating TRUE for conditions in group B. Optional.
#' @param tol Tolerance for sum-to-zero check.
#' @return Named numeric vector of weights.
#' @keywords internal
#' @noRd
.calculate_mask_weights <- function(names, A_mask, B_mask = NULL, tol = .CONTRAST_TOLERANCE) {
  # Input validation
  if (!is.character(names)) {
    stop(".calculate_mask_weights: 'names' must be a character vector", call. = FALSE)
  }
  if (!is.logical(A_mask) || length(A_mask) != length(names)) {
    stop(".calculate_mask_weights: 'A_mask' must be a logical vector with same length as 'names'", call. = FALSE)
  }
  
  nA <- sum(A_mask)
  nB <- 0 # Initialize nB for the case where B_mask is NULL

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
  if (nA == 0 && (is.null(B_mask) || nB == 0)) {
    stop("Cannot calculate contrast weights: No conditions were selected by the provided mask(s). This usually indicates that the patterns or formulas used to define the contrast did not match any existing conditions. Please check your contrast specification.", call. = FALSE)
  }

  # Initialize weights vector
  w <- numeric(length(names))
  if (length(names) > 0) { # Only assign names if 'names' is not empty
      names(w) <- names
  }
  
  # Assign weights if masks are not empty
  if (nA > 0) {
    w[A_mask] <- 1 / nA
  }
  # Ensure B_mask is not NULL before checking nB for assignment
  if (!is.null(B_mask) && nB > 0) {
    w[B_mask] <- -1 / nB
  }
  
  # Warnings for partially defined A-vs-B contrasts (that won't sum to zero as expected)
  if (!is.null(B_mask)) { # These warnings only make sense for A-vs-B type contrasts
    if (nA == 0 && nB > 0) { # A empty, B not (and B_mask was provided)
      warning(".calculate_mask_weights: For A-vs-B contrast, Mask A is empty but Mask B is not. Weights will not sum to zero as expected for a balanced comparison.", call. = FALSE)
    } else if (nB == 0 && nA > 0) { # B empty, A not (and B_mask was provided)
      warning(".calculate_mask_weights: For A-vs-B contrast, Mask B is empty but Mask A is not. Weights will not sum to zero as expected for a balanced comparison.", call. = FALSE)
    } else if (nA > 0 && nB > 0) { # Both A and B are defined and non-empty for A-vs-B
        if (abs(sum(w)) > tol) {
           # This scenario (both non-empty, but sum != 0) should be rare with 1/nA and -1/nB
           warning(".calculate_mask_weights: Weights for A-vs-B contrast (both groups non-empty) do not sum to zero (Sum: ", sum(w), "). This is unexpected.", call. = FALSE)
        }
    }
  } 
  # If B_mask is NULL (single group contrast, e.g. from unit_contrast or pattern_A only column_contrast):
  # - If nA == 0, we would have errored out above.
  # - If nA > 0, weights are 1/nA for A_mask elements. Sum-to-zero is not expected here. No warning needed.
  
  w
}

# Validate the `basis` and `basis_weights` arguments shared by
# `pair_contrast()`, `oneway_contrast()`, and `poly_contrast()`. Detailed
# length validation happens later in `.apply_basis_filter()` once the actual
# number of selected bases is known.
#' @keywords internal
#' @noRd
.validate_basis_args <- function(basis, basis_weights) {
  if (!is.null(basis) && !identical(basis, "all")) {
    if (!is.numeric(basis) || any(basis < 1) || anyNA(basis)) {
      stop("basis must be NULL, 'all', or a positive integer vector", call. = FALSE)
    }
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

# Evaluate a contrast spec's `where` formula against the term's cells.
# Returns a logical vector of length nrow(term_cells); on error, returns
# all-FALSE and warns. NULL `where` => all TRUE.
#' @keywords internal
#' @noRd
.eval_where <- function(spec, term_cells) {
  if (is.null(spec$where)) return(rep(TRUE, nrow(term_cells)))
  tryCatch(
    rlang::eval_tidy(rlang::f_rhs(spec$where), data = term_cells),
    error = function(e) {
      warning(sprintf("Contrast '%s': Error evaluating 'where' clause: %s",
                      spec$name, e$message), call. = FALSE)
      rep(FALSE, nrow(term_cells))
    }
  )
}

# Subset a term's cells by the spec's `where` clause and warn if empty.
# Returns the filtered cells data frame (possibly 0 rows).
#' @keywords internal
#' @noRd
.relevant_cells <- function(spec, term_cells) {
  rc <- term_cells[.eval_where(spec, term_cells), , drop = FALSE]
  if (nrow(rc) == 0L && nrow(term_cells) > 0L) {
    warning(sprintf("Contrast '%s' resulted in no relevant cells after applying the 'where' clause.",
                    spec$name), call. = FALSE)
  }
  rc
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
  # Input validation
  assert_that(rlang::is_formula(form),
              msg = "form must be a formula")
  assert_that(is.character(name) && length(name) == 1,
              msg = "name must be a single character string")
  if (!is.null(where)) {
    assert_that(rlang::is_formula(where),
                msg = "where must be a formula")
  }
  ret <- list(A=form,
              B=NULL,
              where=where,
              name=name)
  
  class(ret) <- c("contrast_formula_spec", "contrast_spec", "list")
  ret
  
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
#' @param A A formula representing the contrast expression.
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
  # Input validation
  assert_that(rlang::is_formula(A),
              msg = "A must be a formula") 
  assert_that(is.character(name) && length(name) == 1,
              msg = "name must be a single character string")
  
  if (!is.null(where)) {
    assert_that(rlang::is_formula(where),
                msg = "where must be a formula")
  }
  
  structure(
    list(A=A,
         B=NULL,
         where=where,
         name=name),
    class=c("unit_contrast_spec", "contrast_spec", "list")
  )
  
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
  if (!is.null(where)) {
    assert_that(rlang::is_formula(where))
  }
  
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
#' Construct a list of contrast_spec objects.
#'
#' @param ... A variable-length list of contrast_spec objects.
#'
#' @return A list of contrast_spec objects with class "contrast_set".
#'
#' @examples
#' c1 <- contrast(~ A - B, name="A_B")
#' c2 <- contrast(~ B - C, name="B_C")
#' contrast_set(c1,c2)
#'
#' @export
#' @import assertthat
#' @importFrom purrr map_lgl
contrast_set <- function(...) {
  ret <- list(...)
  assertthat::assert_that(all(purrr::map_lgl(ret, inherits, "contrast_spec")))
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
  if (!is.null(where)) {
    assert_that(rlang::is_formula(where))
  }
  
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
  if (!is.null(where)) {
    assertthat::assert_that(rlang::is_formula(where))
  }
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
#'   to the selected basis functions. Must have the same length as `basis` selection and will
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
  # Input validation
  assert_that(rlang::is_formula(A),
              msg = "A must be a formula")
  assert_that(rlang::is_formula(B),
              msg = "B must be a formula")
  assert_that(is.character(name) && length(name) == 1,
              msg = "name must be a single character string")

  if (!is.null(where)) {
    assert_that(rlang::is_formula(where),
                msg = "where must be a formula")
  }

  .validate_basis_args(basis, basis_weights)

  ret <- list(A=A,
              B=B,
              where=where,
              basis=basis,
              basis_weights=basis_weights,
              name=name)

  class(ret) <- c("pair_contrast_spec", "contrast_spec", "list")
  ret
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
#'   to the selected basis functions. Must have the same length as `basis` selection and will
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
  # Input validation
  assert_that(rlang::is_formula(A),
              msg = "A must be a formula")
  assert_that(is.character(name) && length(name) == 1,
              msg = "name must be a single character string")

  if (!is.null(where)) {
    assert_that(rlang::is_formula(where),
                msg = "where must be a formula")
  }

  .validate_basis_args(basis, basis_weights)

  structure(
    list(A=A,
         B=NULL,
         where=where,
         basis=basis,
         basis_weights=basis_weights,
         name=name),
    class=c("oneway_contrast_spec", "contrast_spec", "list")
  )
}

#' Interaction Contrast
#'
#' @description
#' Create an interaction contrast specification
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
  # Input validation
  assert_that(rlang::is_formula(A),
              msg = "A must be a formula") 
  assert_that(is.character(name) && length(name) == 1,
              msg = "name must be a single character string")
  
  if (!is.null(where)) {
    assert_that(rlang::is_formula(where),
                msg = "where must be a formula")
  }
  
  
  
  structure(
    list(A=A,
         B=NULL,
         where=where,
         name=name),
    class=c("interaction_contrast_spec", "contrast_spec", "list")
  )
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
#' This contrast type operates by finding design matrix columns whose names match
#' the provided patterns (`pattern_A`, `pattern_B`). It calculates weights such that
#' the average effect of the 'A' columns is compared to the average effect of the
#' 'B' columns (or baseline if `pattern_B` is NULL). Weights are assigned as +1/nA
#' for 'A' columns and -1/nB for 'B' columns, ensuring the contrast sums to zero
#' if both A and B groups are present.
#'
#' Use standard R regex syntax for the patterns. Remember to escape special
#' characters (e.g., `\\[`, `\\.`, `\\*`).
#'
#' @examples
#' # Test the main effect of a continuous modulator 'RT'
#' # Assumes RT is a column name, e.g., from columns(Scale(RT))
#' cc1 <- column_contrast(pattern_A = "^z_RT$", name = "Main_RT")
#'
#' # Compare Condition.A vs Condition.B for the 'RT' modulator effect
#' # Assumes condition names like "Condition.A_z_RT", "Condition.B_z_RT"
#' cc2 <- column_contrast(pattern_A = "^Condition\\.A_z_RT$",
#'                        pattern_B = "^Condition\\.B_z_RT$",
#'                        name = "CondA_vs_CondB_for_RT")
#'
#' # Test a specific basis function (e.g., basis spline #3)
#' # Assumes column names like "TermName_Condition.Tag_b03"
#' cc3 <- column_contrast(pattern_A = "_b03$", name = "Basis_3_Effect")
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
  assert_that(is.character(name) && length(name) == 1,
              msg = "name must be a single character string")
  if (!is.null(where)) {
      warning("'where' argument is currently ignored for column_contrast.")
      # assert_that(rlang::is_formula(where)) # Keep structure if needed later
  }

  ret <- list(
    pattern_A = pattern_A,
    pattern_B = pattern_B,
    where = where, # Store it even if unused for now
    name = name
  )

  class(ret) <- c("column_contrast_spec", "contrast_spec", "list")
  ret
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
#'   to the selected basis functions. Must have the same length as `basis` selection and will
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
  # Input validation
  assert_that(rlang::is_formula(A),
              msg = "A must be a formula")
  assert_that(is.character(name) && length(name) == 1,
              msg = "name must be a single character string")
  assert_that(is.numeric(degree) && length(degree) == 1 && degree >= 1,
              msg = "degree must be a positive integer")

  if (!is.null(where)) {
    assert_that(rlang::is_formula(where),
                msg = "where must be a formula")
  }
  if (!is.null(value_map)) {
    assert_that(is.list(value_map),
                msg = "value_map must be a list")
  }

  .validate_basis_args(basis, basis_weights)

  ret <- list(
    A=A,
    B=NULL,
    where=where,
    degree=degree,
    value_map=value_map,
    basis=basis,
    basis_weights=basis_weights,
    name=name)

  class(ret) <- c("poly_contrast_spec", "contrast_spec", "list")
  ret
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
#' @export
#' @rdname contrast_mask
contrast_mask.unit_contrast_spec <- function(x, term, ...) {
  all_condnames <- try(conditions(term, drop.empty = FALSE, expand_basis = FALSE), silent = TRUE)
  if (inherits(all_condnames, "try-error") || length(all_condnames) == 0) {
    warning(sprintf("Contrast '%s': Failed to get condition names for term '%s'.",
                    x$name, term$varname), call. = FALSE)
    all_condnames <- character(0)
  }

  weights_out <- matrix(0, nrow = length(all_condnames), ncol = 1L,
                        dimnames = list(all_condnames, x$name))

  term_cells <- cells(term)
  if (nrow(term_cells) > 0L && length(all_condnames) > 0L) {
    relevant_cells <- .relevant_cells(x, term_cells)
    if (nrow(relevant_cells) > 0L) {
      idx <- match(cell_condition_tags(relevant_cells), all_condnames)
      mask_A_full <- logical(length(all_condnames))
      mask_A_full[idx[!is.na(idx)]] <- TRUE
      weights_out[, 1] <- .calculate_mask_weights(all_condnames, mask_A_full)
    }
  }

  list(weights = weights_out, condnames = all_condnames)
}

#' @rdname contrast_weights
#' @export
contrast_weights.unit_contrast_spec <- function(x, term, ...) {
  contrast_from_mask(contrast_mask(x, term, ...), x, term,
                     classes = "unit_contrast")
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
#' @export
#' @rdname contrast_mask
contrast_mask.oneway_contrast_spec <- function(x, term, ...) {
  term_cells <- cells(term)
  if (nrow(term_cells) == 0) {
    warning(sprintf("Contrast '%s': Term '%s' has no categorical cells.",
                    x$name, term$varname), call. = FALSE)
    return(list(weights = matrix(numeric(0), nrow = 0, ncol = 0),
                condnames = character(0)))
  }
  relevant_cells <- .relevant_cells(x, term_cells)
  if (nrow(relevant_cells) == 0) {
    return(list(weights = matrix(numeric(0), nrow = 0, ncol = 0),
                condnames = character(0)))
  }

  fac_name <- all.vars(rlang::f_rhs(x$A))
  if (length(fac_name) > 1) {
    warning(sprintf("Contrast '%s': one-way contrast has >1 factor specified (%s), using first: %s",
                    x$name, paste(fac_name, collapse = ", "), fac_name[1]), call. = FALSE)
    fac_name <- fac_name[1]
  }
  if (!(fac_name %in% names(relevant_cells))) {
    stop(sprintf("Contrast '%s': factor %s not found in relevant cells.",
                 x$name, fac_name), call. = FALSE)
  }

  cmat <- tryCatch(generate_main_effect_contrast(relevant_cells, fac_name),
                   error = function(e) {
                     stop(sprintf("Contrast '%s': Error generating main effect contrast for factor %s: %s",
                                  x$name, fac_name, e$message), call. = FALSE)
                   })
  rownames(cmat) <- cell_condition_tags(relevant_cells)
  colnames(cmat) <- paste(x$name, seq_len(ncol(cmat)), sep = "_")
  list(weights = cmat, condnames = rownames(cmat))
}

#' @rdname contrast_weights
#' @export
contrast_weights.oneway_contrast_spec <- function(x, term, ...) {
  mask <- contrast_mask(x, term, ...)
  contrast_from_mask(mask, x, term, classes = "oneway_contrast")
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
#' @export
#' @rdname contrast_mask
contrast_mask.interaction_contrast_spec <- function(x, term, ...) {
  term_cells <- cells(term)
  if (nrow(term_cells) == 0L) {
    warning(sprintf("Contrast '%s': Term '%s' has no categorical cells for interaction.",
                    x$name, term$varname), call. = FALSE)
    return(list(weights = matrix(numeric(0), nrow = 0, ncol = 0),
                condnames = character(0)))
  }
  relevant_cells <- .relevant_cells(x, term_cells)
  if (nrow(relevant_cells) == 0L) {
    return(list(weights = matrix(numeric(0), nrow = 0, ncol = 0),
                condnames = character(0)))
  }

  factors <- all.vars(rlang::f_rhs(x$A))
  if (length(factors) < 2L) {
    stop(sprintf("Contrast '%s': Interaction contrast requires at least two factors.",
                 x$name), call. = FALSE)
  }
  missing_facs <- setdiff(factors, names(relevant_cells))
  if (length(missing_facs) > 0L) {
    stop(sprintf("Contrast '%s': Factor(s) %s not found in relevant cells.",
                 x$name, paste(missing_facs, collapse = ", ")), call. = FALSE)
  }

  cmat <- tryCatch(generate_interaction_contrast(relevant_cells, factors),
                   error = function(e) {
                     stop(sprintf("Contrast '%s': Error generating interaction contrast: %s",
                                  x$name, e$message), call. = FALSE)
                   })
  rownames(cmat) <- apply(relevant_cells, 1, paste, collapse = "_")
  colnames(cmat) <- paste(x$name, seq_len(ncol(cmat)), sep = "_")
  list(weights = cmat, condnames = rownames(cmat))
}

#' @rdname contrast_weights
#' @export
contrast_weights.interaction_contrast_spec <- function(x, term, ...) {
  contrast_from_mask(contrast_mask(x, term, ...), x, term,
                     classes = "interaction_contrast")
}

# Per-cell Helmert weights for one factor: indicator x Helmert basis.
# Returns an n x (k-1) matrix where n = nrow(cells), k = nlevels(factor).
#' @keywords internal
#' @noRd
.helmert_weights <- function(cells_df, fac_name) {
  f <- factor(cells_df[[fac_name]])
  k <- nlevels(f)
  if (k < 2L) stop(sprintf("Need at least 2 levels for factor '%s'", fac_name), call. = FALSE)
  model.matrix(~ f - 1) %*% stats::contr.helmert(k)
}

# Internal: build main-effect contrast matrix over cells
#' @keywords internal
generate_main_effect_contrast <- function(relevant_cells, fac_name) {
  W <- .helmert_weights(relevant_cells, fac_name)
  colnames(W) <- paste0(fac_name, "_h", seq_len(ncol(W)))
  W
}

# Internal: build interaction contrast matrix over cells for two+ factors
#' @keywords internal
generate_interaction_contrast <- function(relevant_cells, factors) {
  if (length(factors) < 2) stop("At least two factors required for interaction contrast")
  A <- .helmert_weights(relevant_cells, factors[1])
  B <- .helmert_weights(relevant_cells, factors[2])
  W <- do.call(cbind, lapply(seq_len(ncol(A)), function(i) {
    A[, i] * B   # n x ncol(B), column-wise multiplication
  }))
  colnames(W) <- paste0(factors[1], "_h", rep(seq_len(ncol(A)), each = ncol(B)),
                        ":", factors[2], "_h", rep(seq_len(ncol(B)), times = ncol(A)))
  W
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
#' @export
#' @rdname contrast_mask
contrast_mask.poly_contrast_spec <- function(x, term, ...) {
  all_condnames <- try(conditions(term, drop.empty = FALSE, expand_basis = FALSE), silent = TRUE)
  if (inherits(all_condnames, "try-error") || length(all_condnames) == 0) {
    warning(sprintf("Contrast '%s': Failed to get condition names for term '%s'.",
                    x$name, term$varname), call. = FALSE)
    all_condnames <- character(0)
  }

  weights_out <- matrix(0, nrow = length(all_condnames), ncol = x$degree,
                        dimnames = list(all_condnames,
                                        paste(x$name, seq_len(x$degree), sep = "_")))

  term_cells <- cells(term)
  if (nrow(term_cells) == 0 || length(all_condnames) == 0) {
    return(list(weights = weights_out, condnames = all_condnames))
  }

  relevant_cells <- .relevant_cells(x, term_cells)
  if (nrow(relevant_cells) == 0) {
    return(list(weights = weights_out, condnames = all_condnames))
  }

  vals_fac <- tryCatch(rlang::eval_tidy(rlang::f_rhs(x$A), data = relevant_cells),
                       error = function(e) {
                         stop(sprintf("Contrast '%s': Error evaluating formula A: %s",
                                      x$name, e$message), call. = FALSE)
                       })

  vals_num <- if (is.null(x$value_map)) {
    tryCatch(as.numeric(as.character(vals_fac)),
             warning = function(w) {
               stop(sprintf("Contrast '%s': Cannot coerce factor levels from formula A to numeric for poly contrast. Use value_map? Error: %s",
                            x$name, w$message), call. = FALSE)
             })
  } else {
    mapped_vals <- x$value_map[as.character(vals_fac)]
    if (anyNA(mapped_vals) || length(mapped_vals) != length(vals_fac)) {
      stop(sprintf("Contrast '%s': value_map does not cover all factor levels present in relevant cells.",
                   x$name), call. = FALSE)
    }
    unlist(mapped_vals)
  }

  if (length(unique(vals_num)) <= x$degree) {
    stop(sprintf("Contrast '%s': Polynomial degree (%d) is too high for the number of unique points (%d) in relevant cells.",
                 x$name, x$degree, length(unique(vals_num))), call. = FALSE)
  }

  pvals_mat <- tryCatch(stats::poly(vals_num, degree = x$degree),
                        error = function(e) {
                          stop(sprintf("Contrast '%s': Error calculating polynomial weights: %s",
                                       x$name, e$message), call. = FALSE)
                        })

  idx <- match(cell_condition_tags(relevant_cells), all_condnames)
  valid_idx <- which(!is.na(idx))
  if (length(valid_idx) > 0L) {
    weights_out[idx[valid_idx], ] <- pvals_mat[valid_idx, , drop = FALSE]
  }
  list(weights = weights_out, condnames = all_condnames)
}

#' @rdname contrast_weights
#' @export
contrast_weights.poly_contrast_spec <- function(x, term, ...) {
  mask <- contrast_mask(x, term, ...)
  contrast_from_mask(mask, x, term, classes = "poly_contrast")
}

#' @export
#' @rdname contrast_mask
contrast_mask.pair_contrast_spec <- function(x, term, ...) {
  term_cells <- cells(term)
  base_condnames_all <- try(conditions(term, drop.empty = FALSE, expand_basis = FALSE), silent = TRUE)
  if (inherits(base_condnames_all, "try-error") || length(base_condnames_all) == 0) {
    warning(sprintf("Contrast '%s': Failed to get base condition names for term '%s'. Skipping.",
                    x$name, term$varname), call. = FALSE)
    return(NULL)
  }
  if (nrow(term_cells) == 0) {
    warning(sprintf("Contrast '%s': Term '%s' has no observed categorical cells. Contrast weights might be all zero.",
                    x$name, term$varname), call. = FALSE)
  }

  relevant_cells <- .relevant_cells(x, term_cells)

  keepA_rel <- if (nrow(relevant_cells) > 0) tryCatch(
    rlang::eval_tidy(rlang::f_rhs(x$A), data = relevant_cells),
    error = function(e) {
      warning(sprintf("Contrast '%s': Error evaluating formula A: %s", x$name, e$message), call. = FALSE)
      logical(nrow(relevant_cells))
    }
  ) else logical(0)

  keepB_rel <- if (is.null(x$B) || nrow(relevant_cells) == 0) NULL else {
    tryCatch(rlang::eval_tidy(rlang::f_rhs(x$B), data = relevant_cells),
             error = function(e) {
               warning(sprintf("Contrast '%s': Error evaluating formula B: %s", x$name, e$message), call. = FALSE)
               logical(nrow(relevant_cells))
             })
  }

  mask_A_full <- logical(length(base_condnames_all))
  mask_B_full <- if (!is.null(keepB_rel)) logical(length(base_condnames_all)) else NULL

  if (nrow(relevant_cells) > 0) {
    idx_all <- match(cell_condition_tags(relevant_cells), base_condnames_all)
    len_A <- min(length(keepA_rel), length(idx_all))
    if (len_A > 0) {
      idx_A <- idx_all[seq_len(len_A)]
      mask_A_full[idx_A[keepA_rel[seq_len(len_A)] & !is.na(idx_A)]] <- TRUE
    }
    if (!is.null(mask_B_full)) {
      len_B <- min(length(keepB_rel), length(idx_all))
      if (len_B > 0) {
        idx_B <- idx_all[seq_len(len_B)]
        mask_B_full[idx_B[keepB_rel[seq_len(len_B)] & !is.na(idx_B)]] <- TRUE
      }
    }
  }

  base_weights_named <- .calculate_mask_weights(base_condnames_all, mask_A_full, mask_B_full)
  weights_base <- matrix(base_weights_named, ncol = 1,
                         dimnames = list(names(base_weights_named), x$name))
  list(weights = weights_base, condnames = rownames(weights_base))
}

#' @rdname contrast_weights
#' @export
contrast_weights.pair_contrast_spec <- function(x, term, ...) {
  mask <- contrast_mask(x, term, ...)
  if (is.null(mask)) return(NULL)
  contrast_from_mask(mask, x, term)
}

#' Column Contrast Weights
#'
#' @description
#' Compute contrast weights for a `column_contrast_spec` object by targeting
#' design matrix columns based on regex patterns.
#'
#' @param x A `column_contrast_spec` object.
#' @param term An `event_term` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list containing the contrast details:
#'   \item{term}{The original `event_term` object.}
#'   \item{name}{The name of the contrast.}
#'   \item{weights}{A numeric matrix where rows correspond to the full design
#'                  matrix columns (from `.condnames(term, expanded = TRUE)`)
#'                  and columns represent the contrast(s). Usually one column.}
#'   \item{condnames}{Character vector of all potential *expanded* condition names from `term`.}
#'   \item{contrast_spec}{The original `column_contrast_spec` object.}
#'
#' @rdname contrast_weights
#' @export
#' @import assertthat
contrast_weights.column_contrast_spec <- function(x, term, ...) {

  # --- Use .condnames helper to get expanded names --- 
  all_colnames <- .condnames(term, expanded = TRUE)
  # Note: .condnames() already includes error handling for conditions()
  if (length(all_colnames) == 0) {
      # It's possible conditions() returns empty if term has no levels/columns
      warning(paste("Column contrast '", x$name, "': Term '", term$varname %||% "<unknown>",
                    "' resulted in zero condition names. Weights will be empty."), call. = FALSE)
      # Return structure with empty weights
      weights_out <- matrix(numeric(0), nrow = 0, ncol = 1)
      colnames(weights_out) <- x$name
      ret <- list(
          term = term,
          name = x$name,
          weights = weights_out,
          condnames = character(0),
          contrast_spec = x
      )
      class(ret) <- c("column_contrast", "contrast", "list")
      return(ret)
  }
  num_all_conds <- length(all_colnames)

  # --- Find indices using grep directly --- 
  # No need for .col_index intermediary anymore if we have names
  idx_A <- grep(x$pattern_A, all_colnames, value = FALSE)
  nA <- length(idx_A)
  if (nA == 0) {
    warning(paste("Column contrast '", x$name, "': pattern_A ('", x$pattern_A,
                  "') did not match any design matrix columns for term '", term$varname %||% "<unknown>", "'."),
            call. = FALSE)
  }

  idx_B <- integer(0)
  nB <- 0
  if (!is.null(x$pattern_B)) {
    idx_B <- grep(x$pattern_B, all_colnames, value = FALSE)
    nB <- length(idx_B)
    if (nB == 0) {
      warning(paste("Column contrast '", x$name, "': pattern_B ('", x$pattern_B,
                    "') did not match any design matrix columns for term '", term$varname %||% "<unknown>", "'."),
              call. = FALSE)
    }
  }

  # --- Retain overlap check --- 
  if (nA > 0 && nB > 0 && any(idx_A %in% idx_B)) {
    stop(paste("Column contrast '", x$name, "': pattern_A and pattern_B match overlapping columns.",
               " Indices A: ", paste(idx_A, collapse=", "),
               "; Indices B: ", paste(idx_B, collapse=", ")), 
         call. = FALSE)
  }

  # --- Calculate weights using weight helper --- 
  mask_A <- seq_along(all_colnames) %in% idx_A
  mask_B <- if (!is.null(x$pattern_B)) seq_along(all_colnames) %in% idx_B else NULL
  
  # .calculate_mask_weights handles 1/nA, -1/nB, checks, and warnings
  weights_vec <- .calculate_mask_weights(all_colnames, mask_A, mask_B)
  
  # Ensure output is a matrix
  weights_mat <- matrix(weights_vec, ncol = 1)
  rownames(weights_mat) <- all_colnames
  colnames(weights_mat) <- x$name

  # Return structure
  ret <- list(
    term = term,
    name = x$name,
    weights = weights_mat,
    condnames = all_colnames, # These are the expanded names used for weights
    contrast_spec = x
  )

  # Classify appropriately
  class(ret) <- c("column_contrast", "contrast", "list")
  ret
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
    keep <- rlang::eval_tidy(rlang::f_rhs(x$where), data=term.cells)
    assert_that(sum(keep) > 0)
    term.cells <- term.cells[keep, , drop = FALSE]
    condnames <- condnames[keep]
    display_names <- display_names[keep]
  }
  
  if (is_continuous(term)) {
    ## hack to handle contrasts with continuous terms
    facs <- !sapply(term$events, is_continuous)
    term.cells[,!facs] <- 1
  } 

  # Display names are primary; canonical names remain available as stable aliases.
  weights <- matrix(0, NROW(term.cells), 1)
  eval_env <- .build_formula_contrast_env(display_names, condnames, rlang::f_env(x$A))
  
  # Evaluate the contrast formula
  res <- tryCatch(rlang::eval_tidy(rlang::f_rhs(x$A), env=eval_env), error = function(e) {
       stop(paste("Contrast formula evaluation failed:", e$message, "\nAvailable display names:", paste(display_names, collapse=", ")), call.=FALSE)
  })
  
  # Apply results to weights matrix
  weights[,1] <- as.vector(res)
  
  # Canonical condition names are the public row labels.
  row.names(weights) <- condnames

  # Return structure
  ret <- list(
    term=term,
    name=x$name,
    weights=weights,
    condnames=condnames,
    contrast_spec=x)
  
  class(ret) <- c("contrast", "list")
  ret  
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

  ret <- structure(
    list(
      term=term,
      name=x$name,
      weights=wts1$weights - wts2$weights,
      condnames=longnames(term),
      contrast_spec=x),
    class=c("contrast_diff", "contrast")
  )

  ret  
}

# Estimated Contrast (internal function, commented out)
#
# @description
# Compute the estimated contrast for a given fit and indices.
#
# @param x The contrast to estimate.
# @param fit The fit object.
# @param indices The indices to use.
# @param ... Additional arguments (currently unused).
#
# @return The estimated contrast.
#
# @noRd
# @keywords internal
# estcon.contrast <- function(x, fit, indices, ...) {
#   wts <- numeric(length(fit$assign))
#   wts[indices] <- x$weights
#   
#   gmodels::estimable(fit, wts)
# }

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

#' plot_contrasts.event_model
#'
#' @description
#' Produces a heatmap of all contrasts defined for an \code{event_model}.
#' Rows = each contrast (or column of an F-contrast), columns = each regressor in
#' the full design matrix, and the fill color = the contrast weight.
#'
#' @param x An \code{event_model} with (lazily) defined contrasts.
#' @param absolute_limits Logical; if \code{TRUE}, the color scale is fixed at (-1,1).
#'   If \code{FALSE}, the range is set to (min, max) of the weights.
#' @param rotate_x_text Logical; if \code{TRUE}, rotate x-axis labels for readability.
#' @param scale_mode Character; 'auto', 'diverging', or 'one_sided' color scaling.
#' @param coord_fixed Logical; if TRUE, use fixed aspect ratio.
#' @param ... Further arguments passed to \code{geom_tile}, e.g. \code{color="grey80"}.
#'
#' @return A \code{ggplot2} object (a heatmap).
#' @examples
#' # Create event model with contrasts
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30, 40, 50),
#'   run = 1,
#'   cond = factor(c("A", "B", "C", "A", "B", "C"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 60, TR = 1)
#' cset <- contrast_set(
#'   A_vs_B = pair_contrast(~ cond == "A", ~ cond == "B", name = "A_vs_B"),
#'   B_vs_C = pair_contrast(~ cond == "B", ~ cond == "C", name = "B_vs_C")
#' )
#' emod <- event_model(onset ~ hrf(cond, contrasts = cset),
#'                     data = des, block = ~run, sampling_frame = sframe)
#' plot_contrasts(emod)
#' @import ggplot2
#' @export
plot_contrasts.event_model <- function(
    x,
    absolute_limits = FALSE,
    rotate_x_text   = TRUE,
    scale_mode      = c("auto", "diverging", "one_sided"),
    coord_fixed     = TRUE,
    ...
) {
  # 1) Extract all the design-matrix column names
  dm <- design_matrix(x)
  regressor_names <- colnames(dm)
  
  # 2) Gather contrast weights (the nested list by term, then by contrast)
  cws <- contrast_weights(x)
  
  # Flatten everything into one big matrix of contrast weights
  big_mat  <- NULL
  rownames <- character(0)
  
  add_contrast_row <- function(vec, row_name) {
    if (is.null(big_mat)) {
      big_mat <<- matrix(vec, nrow = 1)
      colnames(big_mat) <<- regressor_names
      rownames <<- row_name
    } else {
      big_mat <<- rbind(big_mat, vec)
      rownames <<- c(rownames, row_name)
    }
  }
  
  for (contrast_nm in names(cws)) {
    cw_obj <- cws[[contrast_nm]]
    if (is.null(cw_obj)) next

    # By default, we store offset_weights in cw_obj$offset_weights
    W <- cw_obj$offset_weights
    if (is.null(W)) next  # skip if no offset_weights

    # (#designCols x #contrastCols)
    ncols <- ncol(W)
    for (k in seq_len(ncols)) {
      this_col <- W[, k]
      if (ncols > 1) {
        row_label <- paste0(contrast_nm, "_component", k)
      } else {
        row_label <- contrast_nm
      }
      add_contrast_row(this_col, row_label)
    }
  }
  
  if (is.null(big_mat)) {
    stop("No contrasts found in this event_model.")
  }
  
  rownames(big_mat) <- rownames
  
  # 3) Convert big_mat to a long data frame using modern base R approach
  # Create indices for row and column positions
  row_indices <- rep(seq_len(nrow(big_mat)), ncol(big_mat))
  col_indices <- rep(seq_len(ncol(big_mat)), each = nrow(big_mat))
  
  # Create the long format data frame
  df_long <- data.frame(
    ContrastName = rownames(big_mat)[row_indices],
    Regressor = colnames(big_mat)[col_indices],
    Weight = as.vector(big_mat),
    stringsAsFactors = FALSE
  )
  
  # 4) Build the ggplot
  plt <- ggplot2::ggplot(
    df_long,
    ggplot2::aes(
      x    = ReorderFactor(Regressor),
      y    = ReorderFactor(ContrastName, reverse = TRUE),
      fill = Weight
    )
  ) +
    ggplot2::geom_tile(...)
  
  scale_mode <- match.arg(scale_mode)
  wmin <- min(df_long$Weight, na.rm = TRUE)
  wmax <- max(df_long$Weight, na.rm = TRUE)
  effective_mode <- if (scale_mode == "auto") {
    if (wmin < 0) "diverging" else "one_sided"
  } else {
    scale_mode
  }

  plt <- plt + .contrast_fill_scale(effective_mode, wmin, wmax, absolute_limits)
  
  # 5) Theming
  plt <- plt +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(
      x    = "Regressor",
      y    = "Contrast",
      fill = "Weight"
    ) +
    ggplot2::theme(
      panel.grid  = ggplot2::element_blank(),
      axis.ticks  = ggplot2::element_blank()
    )
  
  if (rotate_x_text) {
    plt <- plt + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
  
  # 6) Optionally fix the coordinate ratio to keep tiles square
  if (coord_fixed) {
    plt <- plt +
      ggplot2::scale_x_discrete(expand = c(0, 0)) +
      ggplot2::scale_y_discrete(expand = c(0, 0)) +
      ggplot2::coord_fixed()
  }
  
  plt
}

#' A small utility to preserve factor order in ggplot
#'
#' Makes a factor from a character vector but preserves the order of appearance.
#' If `reverse=TRUE`, it reverses that order.
#' @keywords internal
#' @noRd
ReorderFactor <- function(x, reverse=FALSE) {
  levs <- unique(as.character(x))
  if (reverse) levs <- rev(levs)
  factor(x, levels=levs)
}

# Build a ggplot fill scale for `plot_contrasts()`. `mode` is "diverging"
# (blue-white-red, midpoint 0) or "one_sided" (white-red). `absolute_limits`
# clamps to [-1, 1] or [0, 1] respectively.
#' @keywords internal
#' @noRd
.contrast_fill_scale <- function(mode, wmin, wmax, absolute_limits) {
  if (mode == "diverging") {
    lo <- if (absolute_limits) -1 else wmin
    hi <- if (absolute_limits)  1 else wmax
    ggplot2::scale_fill_gradient2(limits = c(lo, hi), midpoint = 0,
                                   low = "blue", mid = "white", high = "red")
  } else {
    lo <- if (absolute_limits) 0 else wmin
    hi <- if (absolute_limits) 1 else wmax
    ggplot2::scale_fill_gradient(limits = c(lo, hi),
                                  low = "white", high = "red")
  }
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
