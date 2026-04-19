## First-class column metadata for design matrices. The schema lives in
## `.empty_col_metadata()` and is the single source of truth for the columns
## returned by `design_colmap()` and `design_meta()`.

#' @keywords internal
#' @noRd
.empty_col_metadata <- function() {
  tibble::tibble(
    col              = integer(0),
    name             = character(0),
    term_tag         = character(0),
    term_index       = integer(0),
    condition        = character(0),
    run              = integer(0),
    role             = character(0),
    model_source     = character(0),
    basis_name       = character(0),
    basis_ix         = integer(0),
    basis_total      = integer(0),
    basis_label      = character(0),
    is_block_diagonal = logical(0),
    modulation_type  = character(0),
    modulation_id    = character(0)
  )
}

#' Determine modulation type and id for a realised event term
#'
#' Inspects the term's events; if it contains exactly one ParametricBasis event,
#' the basis registry decides the modulation type (`parametric` /
#' `amplitude` / `covariate`) and the id is the variable name. Otherwise
#' falls back to `"amplitude"`.
#'
#' @param term An `event_term` (or covariate_convolved_term).
#' @return Length-1 list with `modulation_type` and `modulation_id`.
#' @keywords internal
#' @noRd
.event_term_modulation <- function(term) {
  if (inherits(term, "covariate_convolved_term")) {
    return(list(
      modulation_type = "covariate",
      modulation_id   = term$varname %||% NA_character_
    ))
  }

  basis_ev <- Find(function(ev) !is.null(ev$meta$basis), term$events %||% list())
  if (!is.null(basis_ev)) {
    basis <- basis_ev$meta$basis
    entry <- get_basis_entry(class(basis))
    return(list(
      modulation_type = if (!is.null(entry)) entry$modulation else "parametric",
      modulation_id   = basis$argname %||% basis_ev$varname %||% NA_character_
    ))
  }

  list(modulation_type = "amplitude", modulation_id = NA_character_)
}

#' Human-friendly label for a basis component
#'
#' @param basis_name Class name of the HRF basis (e.g. `"SPMG3"`, `"FIR"`).
#' @param basis_ix   1-based component index.
#' @return Character scalar label.
#' @keywords internal
#' @noRd
.basis_label <- function(basis_name, basis_ix) {
  if (is.na(basis_name) || is.na(basis_ix)) return(NA_character_)
  if (grepl("SPMG3", basis_name, fixed = TRUE)) {
    return(c("canonical", "derivative", "dispersion")[pmin(pmax(basis_ix, 1L), 3L)])
  }
  if (grepl("SPMG2", basis_name, fixed = TRUE)) {
    return(c("canonical", "derivative")[pmin(pmax(basis_ix, 1L), 2L)])
  }
  if (grepl("FIR", basis_name, fixed = TRUE)) {
    return(sprintf("lag_%02d", basis_ix - 1L))
  }
  sprintf("component_%02d", basis_ix)
}

#' Build column metadata for a single convolved event term
#'
#' Called from `convolve.event_term()` once the final column names are known.
#'
#' @param term       The `event_term` being convolved.
#' @param hrf        The HRF object used for convolution.
#' @param base_cnames Character vector of base condition names (without `_b##`).
#' @param nb         Total number of basis functions.
#' @param term_tag   Term tag (may be NULL for Ident-only terms).
#' @param colnames_final Final column names produced by `make_column_names()`.
#' @return A tibble of metadata, one row per column.
#' @keywords internal
#' @noRd
.term_col_metadata <- function(term, hrf, base_cnames, nb,
                               term_tag, colnames_final) {
  n_cond <- length(base_cnames)
  if (n_cond == 0L || nb <= 0L || length(colnames_final) == 0L) {
    return(.empty_col_metadata())
  }

  cond_per_col  <- rep(base_cnames, each = nb)
  basis_per_col <- rep(seq_len(nb), times = n_cond)
  basis_name    <- if (!is.null(hrf)) class(hrf)[[1L]] else NA_character_
  if (length(basis_name) == 0L) basis_name <- NA_character_
  modu <- .event_term_modulation(term)

  .make_col_metadata(
    name            = colnames_final,
    condition       = cond_per_col,
    term_tag        = if (is.null(term_tag)) NA_character_ else term_tag,
    basis_name      = basis_name,
    basis_ix        = if (nb > 1L) basis_per_col else NA_integer_,
    basis_total     = if (nb > 1L) as.integer(nb) else NA_integer_,
    basis_label     = if (nb > 1L) {
      vapply(basis_per_col, function(j) .basis_label(basis_name, j), character(1))
    } else NA_character_,
    modulation_type = modu$modulation_type,
    modulation_id   = modu$modulation_id %||% NA_character_
  )
}

#' Build column metadata for a covariate term
#'
#' Covariate terms bypass `convolve.event_term()`; produce metadata directly
#' from the term's design matrix.
#'
#' @param term A `covariate_convolved_term`.
#' @return A tibble of metadata.
#' @keywords internal
#' @noRd
.covariate_term_col_metadata <- function(term) {
  cn <- colnames(term$design_matrix) %||% character(0)
  if (length(cn) == 0L) return(.empty_col_metadata())
  .make_col_metadata(
    name            = cn,
    condition       = cn,
    modulation_type = "covariate",
    modulation_id   = term$varname %||% NA_character_
  )
}

# Construct a per-term metadata tibble matching the canonical schema in
# `.empty_col_metadata()`. Any columns omitted take their schema default.
#' @keywords internal
#' @noRd
.make_col_metadata <- function(name,
                               condition       = name,
                               term_tag        = NA_character_,
                               basis_name      = NA_character_,
                               basis_ix        = NA_integer_,
                               basis_total     = NA_integer_,
                               basis_label     = NA_character_,
                               role            = "task",
                               model_source    = "event",
                               modulation_type = "amplitude",
                               modulation_id   = NA_character_,
                               is_block_diagonal = FALSE) {
  tibble::tibble(
    col              = integer(0),  # filled by combiner
    name             = name,
    term_tag         = term_tag,
    term_index       = NA_integer_,
    condition        = condition,
    run              = NA_integer_,
    role             = role,
    model_source     = model_source,
    basis_name       = basis_name,
    basis_ix         = basis_ix,
    basis_total      = basis_total,
    basis_label      = basis_label,
    is_block_diagonal = is_block_diagonal,
    modulation_type  = modulation_type,
    modulation_id    = modulation_id
  )
}

#' Combine per-term metadata tibbles into a design-wide metadata tibble
#'
#' Stamps `term_index` and the absolute `col` index. Intended for use in
#' [build_event_model_design_matrix()].
#'
#' @param meta_list   Named list of per-term metadata tibbles (or NULL entries).
#' @param term_names  Character vector of term tags aligned with `meta_list`.
#' @param term_indices Optional integer vector giving the original term index
#'   for each entry of `meta_list`. Defaults to `seq_along(meta_list)`.
#' @return A tibble with one row per design matrix column.
#' @keywords internal
#' @noRd
.combine_col_metadata <- function(meta_list, term_names, term_indices = NULL) {
  if (length(meta_list) == 0L) return(.empty_col_metadata())
  if (is.null(term_indices)) term_indices <- seq_along(meta_list)

  parts <- vector("list", length(meta_list))
  cursor <- 0L
  for (i in seq_along(meta_list)) {
    md <- meta_list[[i]]
    if (is.null(md) || nrow(md) == 0L) next
    md$term_index <- as.integer(term_indices[i])
    if (length(term_names) >= i && !is.na(term_names[i])) {
      md$term_tag <- term_names[i]
    }
    md$col <- cursor + seq_len(nrow(md))
    cursor <- cursor + nrow(md)
    parts[[i]] <- md
  }
  parts <- purrr::compact(parts)
  if (length(parts) == 0L) return(.empty_col_metadata())
  dplyr::bind_rows(parts)
}

#' Accessor: column metadata for a design matrix
#'
#' Returns the `col_metadata` tibble attached to a design matrix produced by
#' `event_model()`. If the attribute is missing (e.g., on a manually
#' constructed matrix) returns `NULL`.
#'
#' @param x A design matrix or any object with a `design_matrix()` method.
#' @return A tibble, or `NULL`.
#' @export
#' @examples
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30),
#'   run = 1,
#'   cond = factor(c("A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#' emod <- event_model(onset ~ hrf(cond), data = des,
#'                     block = ~run, sampling_frame = sframe)
#' design_meta(emod)
design_meta <- function(x) {
  dm <- if (inherits(x, "data.frame") || is.matrix(x)) x else design_matrix(x)
  attr(dm, "col_metadata")
}
