#' Column metadata for a design matrix
#'
#' Returns a tibble with one row per column of the design matrix and
#' structured metadata derived from existing package attributes and
#' naming conventions. This avoids ad-hoc string parsing in user code
#' and provides a consistent, queryable view of the design.
#'
#' @param x An object containing or producing a design matrix
#'          (e.g., `event_model`, `baseline_model`).
#' @param ... Unused; reserved for future extensions.
#' @return A tibble with columns:
#'   - `col` (integer): 1-based column index
#'   - `name` (character): column name
#'   - `term_tag` (character): term identifier (event tag or baseline term name)
#'   - `term_index` (integer): position within `terms(x)`
#'   - `condition` (character): condition/event label without basis suffix (if applicable)
#'   - `run` (integer): block/run id when the column is block-specific; `NA` if pooled
#'   - `role` (character): e.g., "task", "drift", "intercept", "nuisance"
#'   - `model_source` (character): "event" or "baseline"
#'   - `basis_name` (character): HRF or baseline basis identifier when available
#'   - `basis_ix` (integer): within-condition basis index (HRF component); `NA` if not applicable
#'   - `basis_total` (integer): total number of basis components for the column's term; `NA` if not applicable
#'   - `basis_label` (character): human-readable label for the basis component when known
#'   - `is_block_diagonal` (logical): TRUE when the regressor is per-run/block
#'   - `modulation_type` (character): "amplitude", "parametric", or "covariate"
#'   - `modulation_id` (character): modulator identifier when applicable (e.g., "RT", "RT_by_group")
#'
#' @export
design_colmap.event_model <- function(x, ...) {
  dm <- design_matrix(x)
  n_cols <- ncol(dm)
  if (is.null(n_cols) || n_cols == 0L) {
    return(tibble::tibble(
      col = integer(0), name = character(0), term_tag = character(0), term_index = integer(0),
      condition = character(0), run = integer(0), role = character(0), model_source = character(0),
      basis_name = character(0), basis_ix = integer(0), basis_total = integer(0), basis_label = character(0),
      is_block_diagonal = logical(0), modulation_type = character(0), modulation_id = character(0)
    ))
  }

  cn <- colnames(dm)
  terms_list <- event_terms(x)
  col_inds <- attr(dm, "col_indices")

  # Map each column to its term index and term tag
  term_index_by_col <- rep(NA_integer_, n_cols)
  term_tag_by_col   <- rep(NA_character_, n_cols)
  if (is.list(col_inds) && length(col_inds) > 0) {
    for (i in seq_along(col_inds)) {
      idx <- col_inds[[i]]
      if (length(idx)) {
        term_index_by_col[idx] <- i
        term_tag_by_col[idx]   <- names(col_inds)[i]
      }
    }
  }

  # Per-term basis info (from HRF object when available)
  per_term_basis_name  <- rep(NA_character_, length(terms_list))
  per_term_basis_total <- rep(NA_integer_,  length(terms_list))
  per_term_mod_type    <- rep(NA_character_, length(terms_list))
  per_term_mod_id      <- rep(NA_character_, length(terms_list))

  known_param_prefix <- c("poly_", "bs_", "z_", "std_", "robz_")

  for (i in seq_along(terms_list)) {
    ti <- terms_list[[i]]
    # Default modulation classification from term tag
    ttag <- names(terms_list)[i]
    if (!is.null(ttag) && isTRUE(any(startsWith(ttag, known_param_prefix)))) {
      per_term_mod_type[i] <- "parametric"
      # Strip the first prefix and underscore to recover variable identifier
      per_term_mod_id[i] <- sub("^[^_]+_", "", ttag)
    } else if (inherits(ti, "covariate_convolved_term")) {
      per_term_mod_type[i] <- "covariate"
      per_term_mod_id[i]   <- ti$varname %||% NA_character_
    } else {
      per_term_mod_type[i] <- "amplitude"
      per_term_mod_id[i]   <- NA_character_
    }

    if (inherits(ti, "covariate_convolved_term")) {
      per_term_basis_name[i]  <- NA_character_
      per_term_basis_total[i] <- NA_integer_
    } else {
      hrfspec <- attr(ti, "hrfspec")
      if (!is.null(hrfspec) && !is.null(hrfspec$hrf)) {
        per_term_basis_total[i] <- tryCatch(fmrihrf::nbasis(hrfspec$hrf), error = function(...) NA_integer_)
        # Use the primary class name as a compact basis identifier
        bname <- class(hrfspec$hrf)
        per_term_basis_name[i] <- if (length(bname)) as.character(bname[[1]]) else NA_character_
      } else {
        per_term_basis_name[i]  <- NA_character_
        per_term_basis_total[i] <- NA_integer_
      }
    }
  }

  # Parse basis_ix and condition per column using term tag knowledge
  basis_ix <- rep(NA_integer_, n_cols)
  base_no_basis <- sub("_b([0-9]+)$", "", cn)
  has_basis <- grepl("_b([0-9]+)$", cn)
  if (any(has_basis)) {
    basis_ix[has_basis] <- suppressWarnings(as.integer(sub(".*_b([0-9]+)$", "\\1", cn[has_basis])))
  }

  condition <- base_no_basis
  # Remove `term_tag_` prefix when present to recover condition tag
  to_strip <- !is.na(term_tag_by_col) & startsWith(base_no_basis, paste0(term_tag_by_col, "_"))
  condition[to_strip] <- substring(base_no_basis[to_strip], nchar(term_tag_by_col[to_strip]) + 2L)

  # basis_label heuristics for common HRFs
  basis_label <- rep(NA_character_, n_cols)
  for (i in seq_len(n_cols)) {
    bi <- term_index_by_col[i]
    if (is.na(bi)) next
    bname <- per_term_basis_name[bi]
    bix   <- basis_ix[i]
    if (is.na(bix)) next
    if (!is.na(bname) && grepl("SPMG3", bname, fixed = TRUE)) {
      basis_label[i] <- c("canonical", "derivative", "dispersion")[pmin(pmax(bix, 1L), 3L)]
    } else if (!is.na(bname) && grepl("SPMG2", bname, fixed = TRUE)) {
      basis_label[i] <- c("canonical", "derivative")[pmin(pmax(bix, 1L), 2L)]
    } else if (!is.na(bname) && grepl("FIR", bname, fixed = TRUE)) {
      basis_label[i] <- sprintf("lag_%02d", bix - 1L)
    } else {
      basis_label[i] <- sprintf("component_%02d", bix)
    }
  }

  tibble::tibble(
    col = seq_len(n_cols),
    name = cn,
    term_tag = term_tag_by_col,
    term_index = term_index_by_col,
    condition = condition,
    run = as.integer(NA),
    role = rep("task", n_cols),
    model_source = rep("event", n_cols),
    basis_name = per_term_basis_name[term_index_by_col],
    basis_ix = basis_ix,
    basis_total = per_term_basis_total[term_index_by_col],
    basis_label = basis_label,
    is_block_diagonal = rep(FALSE, n_cols),
    modulation_type = per_term_mod_type[term_index_by_col],
    modulation_id   = per_term_mod_id[term_index_by_col]
  )
}

#' @export
design_colmap.baseline_model <- function(x, ...) {
  dm <- design_matrix(x)
  n_cols <- ncol(dm)
  if (is.null(n_cols) || n_cols == 0L) {
    return(tibble::tibble(
      col = integer(0), name = character(0), term_tag = character(0), term_index = integer(0),
      condition = character(0), run = integer(0), role = character(0), model_source = character(0),
      basis_name = character(0), basis_ix = integer(0), basis_total = integer(0), basis_label = character(0),
      is_block_diagonal = logical(0), modulation_type = character(0), modulation_id = character(0)
    ))
  }

  cn <- colnames(dm)

  # Iterate terms in order; bind_cols preserves order
  tlist <- baseline_terms(x)
  term_tag_by_col <- character(n_cols)
  term_index_by_col <- integer(n_cols)
  role_by_col <- character(n_cols)
  basis_name_by_col <- character(n_cols)
  basis_ix <- rep(NA_integer_, n_cols)
  basis_total_by_term <- rep(NA_integer_, length(tlist))
  basis_total_by_col <- rep(NA_integer_, n_cols)
  basis_label <- rep(NA_character_, n_cols)
  run_by_col <- rep(NA_integer_, n_cols)
  is_block_diag <- rep(TRUE, n_cols) # most baseline columns are per-run

  col_cursor <- 1L
  for (i in seq_along(tlist)) {
    term <- tlist[[i]]
    tcols <- ncol(term$design_matrix)
    if (tcols == 0L) next
    idx <- col_cursor:(col_cursor + tcols - 1L)
    termname <- names(tlist)[i] %||% term$varname %||% sprintf("baseline_term_%d", i)
    term_tag_by_col[idx] <- termname
    term_index_by_col[idx] <- i

    # Role + basis name heuristics
    if (identical(termname, "drift") || grepl("^base_", colnames(term$design_matrix)[1])) {
      role_by_col[idx] <- "drift"
      basis_name_by_col[idx] <- if (!is.null(x$drift_spec)) x$drift_spec$basis else "baseline"
      # Parse component index (k) and run id from names like base_<basis><k>_block_<r>
      nm <- colnames(term$design_matrix)
      k <- suppressWarnings(as.integer(sub("^.*?([0-9]+)_block_.*$", "\\1", nm)))
      r <- suppressWarnings(as.integer(sub("^.*_block_([0-9]+)$", "\\1", nm)))
      basis_ix[idx] <- k
      run_by_col[idx] <- r
      if (any(is.finite(k))) basis_total_by_term[i] <- max(k, na.rm = TRUE)
      is_block_diag[idx] <- TRUE
      basis_label[idx] <- ifelse(is.na(k), NA_character_, sprintf("component_%02d", k))
    } else if (identical(termname, "block") || grepl("_global$|_[0-9]+$", colnames(term$design_matrix)[1])) {
      role_by_col[idx] <- "intercept"
      basis_name_by_col[idx] <- "constant"
      nm <- colnames(term$design_matrix)
      # global intercept ends with _global; runwise ends with _<run>
      is_global <- grepl("_global$", nm)
      if (all(is_global)) {
        is_block_diag[idx] <- FALSE
        run_by_col[idx] <- NA_integer_
        basis_ix[idx] <- 1L
        basis_label[idx] <- "intercept"
        basis_total_by_term[i] <- 1L
      } else {
        # parse run number from suffix
        r <- suppressWarnings(as.integer(sub("^.*_([0-9]+)$", "\\1", nm)))
        run_by_col[idx] <- r
        is_block_diag[idx] <- TRUE
        # basis_ix not meaningful here; leave NA
        basis_label[idx] <- "intercept"
        basis_total_by_term[i] <- length(unique(stats::na.omit(r)))
      }
    } else if (identical(termname, "nuisance") || grepl("#", colnames(term$design_matrix)[1], fixed = TRUE)) {
      role_by_col[idx] <- "nuisance"
      basis_name_by_col[idx] <- "nuisance"
      nm <- colnames(term$design_matrix)
      # Format: prefix#<block>_<col>
      r <- suppressWarnings(as.integer(sub("^.*#([0-9]+)_.*$", "\\1", nm)))
      k <- suppressWarnings(as.integer(sub("^.*_([0-9]+)$", "\\1", nm)))
      run_by_col[idx] <- r
      basis_ix[idx] <- k
      is_block_diag[idx] <- TRUE
      basis_label[idx] <- ifelse(is.na(k), NA_character_, sprintf("component_%02d", k))
      # basis_total per term is not reliably inferable; leave NA
    } else {
      # Fallback: treat as baseline component, per-run if name suggests block
      role_by_col[idx] <- "baseline"
      basis_name_by_col[idx] <- "baseline"
      nm <- colnames(term$design_matrix)
      if (any(grepl("block", nm, fixed = TRUE))) {
        r <- suppressWarnings(as.integer(sub("^.*_block_([0-9]+)$", "\\1", nm)))
        run_by_col[idx] <- r
        is_block_diag[idx] <- TRUE
      } else {
        run_by_col[idx] <- NA_integer_
        is_block_diag[idx] <- FALSE
      }
      basis_label[idx] <- NA_character_
    }

    basis_total_by_col[idx] <- basis_total_by_term[i]
    col_cursor <- col_cursor + tcols
  }

  tibble::tibble(
    col = seq_len(n_cols),
    name = cn,
    term_tag = term_tag_by_col,
    term_index = term_index_by_col,
    condition = rep(NA_character_, n_cols),
    run = run_by_col,
    role = role_by_col,
    model_source = rep("baseline", n_cols),
    basis_name = basis_name_by_col,
    basis_ix = basis_ix,
    basis_total = basis_total_by_col,
    basis_label = basis_label,
    is_block_diagonal = is_block_diag,
    modulation_type = rep(NA_character_, n_cols),
    modulation_id   = rep(NA_character_, n_cols)
  )
}
