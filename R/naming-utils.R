# Naming Utilities for fmrireg Design Matrix Column Names

# Internal helper for zero-padding numbers
#' @export
#' @noRd
zeropad <- function(i, n_total) {
  # Handle edge case n_total = 0 or 1 gracefully
  #width <- if (n_total <= 1) 1 else ceiling(log10(n_total + 1e-9)) # OLD width calc
  # Calculate width needed based on magnitude of the largest number (n_total)
  log_width <- if (n_total < 1) 1L else ceiling(log10(n_total + 1e-9))
  # Ensure minimum width is 2 if there's more than one item, otherwise use calculated width
  final_width <- if (n_total > 1) max(2L, as.integer(log_width)) else as.integer(log_width) 
  sprintf(paste0("%0", final_width, "d"), i)
}

#' Sanitize Strings for Use in R Names
#'
#' Wraps `make.names` but allows control over dot replacement.
#'
#' @param x A character vector.
#' @param allow_dot Logical, if `FALSE`, dots (`.`) are replaced with underscores (`_`).
#' @return A sanitized character vector.
#' @export
#' @examples
#' \dontrun{
#' sanitize("a.b c")
#' sanitize("a.b c", allow_dot = FALSE)
#' }
sanitize <- function(x, allow_dot = TRUE) {
  # Initial sanitization using make.names (handles invalid starting chars, spaces, etc.)
  sanitized <- make.names(x, unique = FALSE)

  if (!allow_dot) {
    # Replace dots introduced by make.names (or pre-existing) with underscores.
    # CRITICAL FIX: Replace sequences of one or more dots/underscores with a single underscore.
    # This prevents ".." becoming "__".
    # First, replace all dots with underscores to normalize separators
    sanitized <- gsub(".", "_", sanitized, fixed = TRUE)
    # Then, replace multiple consecutive underscores with a single one
    sanitized <- gsub("_+", "_", sanitized)
    # Optional: remove leading/trailing underscores that might result
    sanitized <- gsub("^_|_$", "", sanitized)
  }
  sanitized
}

#' Sanitize Factor Level
#'
#' Similar to `sanitize()` but if the original level begins with a digit,
#' remove the leading `"X"` that `make.names` would prepend.
#'
#' @param lev A factor level string.
#' @return Sanitized level string.
#' @keywords internal
#' @noRd
sanitize_level <- function(lev) {
  sanitized <- sanitize(lev, allow_dot = TRUE)
  needs_strip <- grepl("^[0-9]", lev) & startsWith(sanitized, "X")
  sanitized[needs_strip] <- sub("^X", "", sanitized[needs_strip])
  sanitized
}

#' Create Basis Function Suffix
#'
#' Generates the `_b##` suffix for HRF basis functions.
#'
#' @param j Integer vector of basis indices (1-based).
#' @param nb Total number of basis functions.
#' @return Character vector of suffixes (e.g., `_b01`, `_b02`).
#' @export
#' @examples
#' \dontrun{
#' basis_suffix(1:3, 5)
#' basis_suffix(1:10, 10)
#' }
basis_suffix <- function(j, nb) {
  paste0("_b", zeropad(j, nb))
}

#' Create Feature Suffix
#'
#' Generates the `f##` suffix for multi-column continuous events.
#'
#' @param j Integer vector of feature indices (1-based).
#' @param nf Total number of features.
#' @return Character vector of suffixes (e.g., `f01`, `f02`).
#' @export
#' @examples
#' \dontrun{
#' feature_suffix(1:3, 5)
#' }
feature_suffix <- function(j, nf) {
  paste0("f", zeropad(j, nf))
}

#' Make Tags Unique with Hash Separator
#'
#' Wraps `make.unique` using `#` as the separator.
#'
#' @param tags Character vector of tags.
#' @return Character vector with unique tags (e.g., `tag`, `tag#1`).
#' @keywords internal
#' @noRd
make_unique_tags <- function(tags) {
  make.unique(tags, sep = "#")
}

#' Generate Term Tag
#'
#' Creates a sanitized, unique tag for an event term based on its spec.
#'
#' @param hrfspec The HRF specification list for the term.
#' @param existing_tags Character vector of already assigned term tags.
#' @return A single, unique, sanitized term tag.
#' @importFrom rlang as_label
#' @keywords internal
#' @noRd
make_term_tag <- function(hrfspec, existing_tags = character()) {
  # List of basis function names that inherit from ParametricBasis
  # This list is derived from all classes in basis.R that extend ParametricBasis
  parametric_basis_functions <- c("Poly", "BSpline", "Scale", "Standardized", 
                                  "ScaleWithin", "RobustScale")

  if (!is.null(hrfspec$id)) {
    tag_base <- sanitize(hrfspec$id, allow_dot = FALSE)
  } else if (!is.null(hrfspec$prefix)) {
    # If no explicit id but prefix is provided, use prefix as the term tag
    tag_base <- sanitize(hrfspec$prefix, allow_dot = FALSE)
  } else {
    is_ident_only <- FALSE
    is_parametric_basis <- FALSE
    parsed_basis_tag_base <- NULL

    if (length(hrfspec$vars) == 1) {
       expr <- rlang::quo_get_expr(hrfspec$vars[[1]])
       target_expr <- if (rlang::is_formula(expr, lhs = FALSE)) rlang::f_rhs(expr) else expr

       if (rlang::is_call(target_expr)) {
          call_nm <- rlang::call_name(target_expr)
          if (!is.null(call_nm)) {
            if (call_nm == "Ident") {
              is_ident_only <- TRUE
            } else if (call_nm %in% parametric_basis_functions) {
              is_parametric_basis <- TRUE
              # Attempt to parse: BasisFunction(VarName, ...)
              if (length(rlang::call_args(target_expr)) >= 1) {
                var_expr <- rlang::call_args(target_expr)[[1]]
                var_name_label <- rlang::as_label(var_expr)
                
                # Generate appropriate prefix based on the basis type
                # This mirrors the naming convention used in each basis class
                basis_prefix <- switch(call_nm,
                  "Poly" = "poly",
                  "BSpline" = "bs", 
                  "Scale" = "z",
                  "Standardized" = "std",
                  "ScaleWithin" = "z",  # ScaleWithin uses z_ prefix like Scale
                  "RobustScale" = "robz",
                  tolower(call_nm)  # fallback to lowercase
                )
                
                sanitized_var_name <- sanitize(var_name_label, allow_dot = FALSE)
                parsed_basis_tag_base <- paste(basis_prefix, sanitized_var_name, sep = "_")
              } else {
                # Fallback if parsing fails
                is_parametric_basis <- FALSE # Treat as generic
              }
            }
          }
       }
    }
    
    if (is_ident_only) {
       return(NULL) 
    } else if (is_parametric_basis && !is.null(parsed_basis_tag_base)) {
        tag_base <- parsed_basis_tag_base
    } else {
       # Default: build tag from variable names/labels in original quosures
       tag_base <- paste(vapply(hrfspec$vars, rlang::as_label, character(1)), collapse = "_")
       tag_base <- sanitize(tag_base, allow_dot = FALSE)
    }
  }
  
  # Handle vector inputs - collapse to single tag
  if (length(tag_base) > 1) {
    # For vector tags, use the first element
    # This ensures we always have a scalar for comparison
    tag_base <- tag_base[1]
  }
  
  # Handle NA values that might result from subsetting
  if (is.na(tag_base)) {
    tag_base <- "empty_id_tag"
  }
  
  # Now safe to do scalar comparison
  if (tag_base == "") {
      tag_base <- "empty_id_tag" 
  }

  tag <- tag_base 

  all_tags <- c(existing_tags, tag)
  unique_tags <- make_unique_tags(all_tags)
  final_tag <- unique_tags[length(all_tags)]
  return(final_tag)
}

#' Create Factor Level Token
#'
#' Generates the `Var.Level` token.
#'
#' @param var Variable name (character).
#' @param lev Level name (character).
#' @return Sanitized token (e.g., `cond.A`).
#' @keywords internal
#' @noRd
level_token <- function(var, lev) {
  # Sanitize variable and level parts
  s_var <- sanitize(var, allow_dot = TRUE)
  s_lev <- sanitize_level(lev)
  paste0(s_var, ".", s_lev)
}

#' Sanitize Continuous Column Name Token
#'
#' Ensures a pre-formatted continuous column name (like `poly_RT_01`) is a valid R name.
#'
#' @param colname The proposed column name string.
#' @return Sanitized token.
#' @keywords internal
#' @noRd
continuous_token <- function(colname) {
  sanitize(colname, allow_dot = TRUE)
}

#' Combine Tokens into Condition Tag
#'
#' Pastes tokens together with underscores for interaction terms.
#'
#' @param tokens Character vector of tokens (factor levels or continuous names).
#' @return Single string representing the combined condition tag.
#' @keywords internal
#' @noRd
make_cond_tag <- function(tokens) {
  paste(tokens, collapse = "_")
}

#' Add Basis Suffix to Condition Tags
#'
#' Optionally expands condition tags with basis suffixes.
#'
#' @param cond_tags Character vector of condition tags.
#' @param nb Number of basis functions.
#' @return Character vector, expanded with suffixes if `nb > 1`.
#' @keywords internal
#' @noRd
add_basis <- function(cond_tags, nb) {
  if (nb <= 1L) {
    return(cond_tags)
  }
  suffixes <- basis_suffix(seq_len(nb), nb)
  as.vector(outer(cond_tags, suffixes, paste0))
}

#' Compose Final Column Names
#'
#' The single source of truth for creating final design matrix column names.
#'
#' @param term_tag The sanitized, unique term tag.
#' @param cond_tags Character vector of base condition tags (without basis suffix).
#' @param nb Number of basis functions.
#' @return Character vector of final column names `term_tag_condition_tag[_b##]`.
#' @keywords internal
#' @noRd
make_column_names <- function(term_tag, cond_tags, nb) {
  # Internal consistency check: term_tag should not contain double underscores
  # Allow NULL term_tag for Ident()-only direct variables
  if (!is.null(term_tag)) {
    stopifnot(!grepl("__", term_tag))
  }
  
  # Add basis suffix if needed
  full_cond_tags <- add_basis(cond_tags, nb)
  
  # Combine term tag and condition tag, or just use condition tags if term_tag is NULL
  if (is.null(term_tag)) {
    # This is the case for Ident()-only terms where variables become direct column names
    return(full_cond_tags)
  } else {
    # Default case: term_tag_condition_tag[_b##]
    return(paste0(term_tag, "_", full_cond_tags))
  }
}

# Null-coalescing operator (from rlang)
# Avoids direct dependency if only used here, but rlang is needed for as_label anyway
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
} 
