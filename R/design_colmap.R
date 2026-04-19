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
#'   - `pretty_name` (character): concise, human-friendly label for display (e.g., "RT" or "RT_lag_02")
#'   - `is_block_diagonal` (logical): TRUE when the regressor is per-run/block
#'   - `modulation_type` (character): "amplitude", "parametric", or "covariate"
#'   - `modulation_id` (character): modulator identifier when applicable (e.g., "RT", "RT_by_group")
#' @examples
#' # Create event model
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30),
#'   run = 1,
#'   cond = factor(c("A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#' emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
#'
#' # Get column metadata
#' colmap <- design_colmap(emod)
#' head(colmap)
#'
#' # Query columns by condition
#' colmap[colmap$condition == "A", ]
#' @export
design_colmap.event_model <- function(x, ...) {
  dm <- design_matrix(x)
  n_cols <- ncol(dm)
  if (is.null(n_cols) || n_cols == 0L) return(.empty_col_metadata())

  cm <- attr(dm, "col_metadata")
  if (is.null(cm) || nrow(cm) != n_cols) {
    warning("design_colmap.event_model: design matrix missing 'col_metadata' attribute ",
            "or metadata doesn't match column count. Returning schema-only metadata; ",
            "was this design matrix built outside event_model()?", call. = FALSE)
    out <- .empty_col_metadata()[rep(NA_integer_, n_cols), , drop = FALSE]
    out$col <- seq_len(n_cols)
    out$name <- colnames(dm)
    out$model_source <- "event"
    out$role <- "task"
    out$pretty_name <- out$name
    return(out)
  }

  out <- cm
  out$name <- colnames(dm)
  out$pretty_name <- .pretty_names_from_metadata(out)
  out
}

#' Derive concise display names from a metadata tibble
#'
#' Single-basis parametric columns become the bare modulator id (e.g., "RT");
#' multi-basis parametric columns get appended with the basis label (or
#' `_b##`). Other columns are returned unchanged.
#'
#' @param meta A metadata tibble with at least
#'   `name`, `modulation_type`, `modulation_id`, `basis_ix`, `basis_label`.
#' @return Character vector of pretty names aligned with `meta$name`.
#' @keywords internal
#' @noRd
.pretty_names_from_metadata <- function(meta) {
  pretty_name <- meta$name
  is_param <- !is.na(meta$modulation_type) & meta$modulation_type == "parametric"
  if (!any(is_param)) return(pretty_name)
  has_id <- !is.na(meta$modulation_id)

  mask_single <- is_param & has_id & (is.na(meta$basis_ix) | !is.finite(meta$basis_ix))
  pretty_name[mask_single] <- meta$modulation_id[mask_single]

  mask_multi <- is_param & has_id & is.finite(meta$basis_ix)
  total_for_pad <- suppressWarnings(max(meta$basis_total[mask_multi], 1L, na.rm = TRUE))
  fallback <- paste0(meta$modulation_id[mask_multi],
                     basis_suffix(meta$basis_ix[mask_multi], total_for_pad))
  has_label <- !is.na(meta$basis_label[mask_multi]) & nzchar(meta$basis_label[mask_multi])
  pretty_name[mask_multi] <- ifelse(
    has_label,
    paste0(meta$modulation_id[mask_multi], "_", meta$basis_label[mask_multi]),
    fallback
  )
  pretty_name
}

#' @export
design_colmap.baseline_model <- function(x, ...) {
  dm <- design_matrix(x)
  n_cols <- ncol(dm)
  if (is.null(n_cols) || n_cols == 0L) {
    return(.empty_col_metadata())
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
