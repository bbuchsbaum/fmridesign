###############################################################################
## BASELINE_MODEL.R
##
## This file implements baseline model construction for fMRI analyses.
## It includes helper routines for:
##   - Extracting column indices from a list of matrices.
##   - Constructing baseline models that account for drift, block-wise
##     intercepts, and nuisance regressors.
##   - Creating design matrices and terms for baseline models.
##   - Specifying block and nuisance terms.
##   - Printing and plotting baseline models.
##
## The file has been organized into sections with additional inline comments
## for clarity. Functionality is preserved exactly as in the original file.
###############################################################################

## ============================================================================
## Section 1: Helper Functions
## ============================================================================

# Helper to get number of columns, returning 0 for NULL
.nz <- function(x) if (is.null(x)) 0 else ncol(design_matrix(x))

.nuisance_default_tol <- function() sqrt(.Machine$double.eps)

.nuisance_colnames <- function(mat) {
  cn <- colnames(mat)
  if (is.null(cn)) {
    cn <- paste0("V", seq_len(ncol(mat)))
  }
  missing <- is.na(cn) | cn == ""
  cn[missing] <- paste0("V", which(missing))
  make.unique(cn, sep = "_")
}

.as_nuisance_matrices <- function(nuisance_list, sframe) {
  if (!is.list(nuisance_list)) {
    stop("nuisance_list must be a list.", call. = FALSE)
  }
  if (!all(vapply(nuisance_list,
                  function(x) is.matrix(x) || is.data.frame(x),
                  logical(1)))) {
    stop("Each nuisance_list element must be a numeric matrix or data frame.",
         call. = FALSE)
  }

  nb <- length(fmrihrf::blocklens(sframe))
  bl_lens <- fmrihrf::blocklens(sframe)

  if (length(nuisance_list) != nb) {
    stop("length(nuisance_list) must equal number of blocks in sframe.",
         call. = FALSE)
  }

  mats <- lapply(seq_along(nuisance_list), function(i) {
    mat <- as.matrix(nuisance_list[[i]])
    if (!(is.numeric(mat) || is.logical(mat))) {
      stop(sprintf("nuisance_list[[%d]] must contain only numeric, integer, or logical columns.", i),
           call. = FALSE)
    }
    storage.mode(mat) <- "double"
    if (nrow(mat) != bl_lens[i]) {
      stop("Each nuisance matrix must have nrow == block length for its block.",
           call. = FALSE)
    }
    colnames(mat) <- .nuisance_colnames(mat)
    mat
  })

  mats
}

.qr_rank <- function(x, tol = .nuisance_default_tol()) {
  if (ncol(x) == 0) return(0L)
  qr(x, tol = tol)$rank
}

.block_rows <- function(sframe) {
  split(seq_len(sum(fmrihrf::blocklens(sframe))), fmrihrf::blockids(sframe))
}

.baseline_matrix_for_block <- function(baseline_terms, rows, tol) {
  if (length(baseline_terms) == 0) {
    return(matrix(numeric(length(rows) * 0), nrow = length(rows)))
  }

  mats <- lapply(baseline_terms, function(term) {
    if (is.null(term)) return(NULL)
    mat <- as.matrix(design_matrix(term))[rows, , drop = FALSE]
    active <- vapply(seq_len(ncol(mat)), function(j) {
      any(is.finite(mat[, j]) & abs(mat[, j]) > tol)
    }, logical(1))
    mat[, active, drop = FALSE]
  })
  mats <- purrr::compact(mats)

  if (length(mats) == 0) {
    matrix(numeric(length(rows) * 0), nrow = length(rows))
  } else {
    do.call(cbind, mats)
  }
}

.zero_variance_columns <- function(mat, tol) {
  vapply(seq_len(ncol(mat)), function(j) {
    x <- mat[, j]
    if (any(!is.finite(x))) return(FALSE)
    rng <- range(x)
    scale <- max(1, max(abs(x)))
    abs(diff(rng)) <= tol * scale
  }, logical(1))
}

.duplicate_pairs <- function(mat, zero_variance, duplicate_threshold) {
  keep <- which(!zero_variance)
  if (length(keep) < 2) {
    return(data.frame(column = character(0),
                      duplicates = character(0),
                      correlation = numeric(0),
                      stringsAsFactors = FALSE))
  }

  x <- mat[, keep, drop = FALSE]
  cc <- suppressWarnings(stats::cor(x))
  diag(cc) <- 0
  idx <- which(abs(cc) >= duplicate_threshold, arr.ind = TRUE)
  idx <- idx[idx[, 1] < idx[, 2], , drop = FALSE]
  if (nrow(idx) == 0) {
    return(data.frame(column = character(0),
                      duplicates = character(0),
                      correlation = numeric(0),
                      stringsAsFactors = FALSE))
  }

  data.frame(
    column = colnames(x)[idx[, 2]],
    duplicates = colnames(x)[idx[, 1]],
    correlation = cc[idx],
    stringsAsFactors = FALSE
  )
}

.incremental_rank_keep <- function(mat, baseline_mat, zero_variance, non_finite, tol) {
  keep <- rep(TRUE, ncol(mat))
  names(keep) <- colnames(mat)
  current <- baseline_mat
  current_rank <- .qr_rank(current, tol = tol)

  for (j in seq_len(ncol(mat))) {
    if (zero_variance[j] || non_finite[j]) {
      keep[j] <- FALSE
      next
    }

    candidate <- cbind(current, mat[, j, drop = FALSE])
    candidate_rank <- .qr_rank(candidate, tol = tol)
    if (candidate_rank > current_rank) {
      current <- candidate
      current_rank <- candidate_rank
    } else {
      keep[j] <- FALSE
    }
  }

  keep
}

.problem_row <- function(block, issue, columns, detail) {
  data.frame(block = block,
             issue = issue,
             columns = paste(columns, collapse = ", "),
             detail = detail,
             stringsAsFactors = FALSE)
}

.check_nuisance_internal <- function(nuisance_list,
                                     sframe,
                                     baseline_terms = list(),
                                     tol = .nuisance_default_tol(),
                                     duplicate_threshold = 1 - .nuisance_default_tol()) {
  mats <- .as_nuisance_matrices(nuisance_list, sframe)
  rows_by_block <- .block_rows(sframe)
  by_block <- vector("list", length(mats))
  problems <- list()

  for (i in seq_along(mats)) {
    mat <- mats[[i]]
    cn <- colnames(mat)
    baseline_mat <- .baseline_matrix_for_block(baseline_terms, rows_by_block[[i]], tol)

    non_finite <- vapply(seq_len(ncol(mat)), function(j) any(!is.finite(mat[, j])), logical(1))
    finite_mat <- mat
    finite_mat[!is.finite(finite_mat)] <- 0
    zero_variance <- .zero_variance_columns(finite_mat, tol = tol)
    dup_pairs <- .duplicate_pairs(finite_mat, zero_variance | non_finite, duplicate_threshold)

    nuisance_rank <- .qr_rank(finite_mat, tol = tol)
    baseline_rank <- .qr_rank(baseline_mat, tol = tol)
    rank_with_baseline <- .qr_rank(cbind(baseline_mat, finite_mat), tol = tol)
    n_with_baseline <- ncol(baseline_mat) + ncol(finite_mat)
    keep <- .incremental_rank_keep(finite_mat, baseline_mat, zero_variance, non_finite, tol)
    aliased <- cn[!keep & !zero_variance & !non_finite]

    if (any(non_finite)) {
      problems[[length(problems) + 1]] <- .problem_row(
        i, "non_finite", cn[non_finite],
        "contains NA, NaN, or infinite values"
      )
    }
    if (any(zero_variance)) {
      problems[[length(problems) + 1]] <- .problem_row(
        i, "zero_variance", cn[zero_variance],
        "column has no within-run variance"
      )
    }
    if (nrow(dup_pairs) > 0) {
      detail <- paste(sprintf("%s duplicates %s (r = %.6g)",
                              dup_pairs$column,
                              dup_pairs$duplicates,
                              dup_pairs$correlation),
                      collapse = "; ")
      problems[[length(problems) + 1]] <- .problem_row(
        i, "duplicate", dup_pairs$column, detail
      )
    }
    if (nuisance_rank < ncol(finite_mat)) {
      problems[[length(problems) + 1]] <- .problem_row(
        i, "rank_deficient_nuisance", cn,
        sprintf("nuisance rank: %d < %d columns", nuisance_rank, ncol(finite_mat))
      )
    }
    if (rank_with_baseline < n_with_baseline) {
      problems[[length(problems) + 1]] <- .problem_row(
        i, "rank_deficient_with_baseline", cn,
        sprintf("rank with baseline terms: %d < %d columns",
                rank_with_baseline, n_with_baseline)
      )
    }
    if (length(aliased) > 0) {
      problems[[length(problems) + 1]] <- .problem_row(
        i, "aliased_columns", aliased,
        "columns do not increase rank after baseline and earlier nuisance columns"
      )
    }

    by_block[[i]] <- list(
      block = i,
      baseline_rank = baseline_rank,
      baseline_columns = ncol(baseline_mat),
      nuisance_rank = nuisance_rank,
      nuisance_columns = ncol(finite_mat),
      rank_with_baseline = rank_with_baseline,
      columns_with_baseline = n_with_baseline,
      non_finite = cn[non_finite],
      zero_variance = cn[zero_variance],
      duplicate_pairs = dup_pairs,
      aliased_columns = aliased,
      keep = keep
    )
  }

  problems_df <- if (length(problems) == 0) {
    data.frame(block = integer(0),
               issue = character(0),
               columns = character(0),
               detail = character(0),
               stringsAsFactors = FALSE)
  } else {
    do.call(rbind, problems)
  }

  ret <- list(
    ok = nrow(problems_df) == 0,
    problems = problems_df,
    by_block = by_block,
    nuisance_list = mats
  )
  class(ret) <- c("nuisance_check", "list")
  ret
}

.format_nuisance_check <- function(x, dropped = FALSE) {
  if (isTRUE(x$ok)) return("baseline_model(): nuisance_list passed validation.")

  lines <- c("baseline_model(): nuisance_list has column/rank problems.")
  for (block in x$by_block) {
    block_lines <- character()
    if (length(block$non_finite) > 0) {
      block_lines <- c(block_lines,
                       sprintf("  Non-finite columns: %s.",
                               paste(block$non_finite, collapse = ", ")))
    }
    if (length(block$zero_variance) > 0) {
      block_lines <- c(block_lines,
                       sprintf("  Zero-variance columns: %s.",
                               paste(block$zero_variance, collapse = ", ")))
    }
    if (nrow(block$duplicate_pairs) > 0) {
      dup <- paste(sprintf("%s duplicates %s (r = %.6g)",
                           block$duplicate_pairs$column,
                           block$duplicate_pairs$duplicates,
                           block$duplicate_pairs$correlation),
                   collapse = "; ")
      block_lines <- c(block_lines,
                       sprintf("  Duplicate or near-duplicate columns: %s.", dup))
    }
    if (length(block$aliased_columns) > 0) {
      block_lines <- c(block_lines,
                       sprintf("  Aliased columns: %s.",
                               paste(block$aliased_columns, collapse = ", ")))
    }
    if (block$rank_with_baseline < block$columns_with_baseline) {
      block_lines <- c(block_lines,
                       sprintf("  Rank with baseline terms: %d < %d columns.",
                               block$rank_with_baseline,
                               block$columns_with_baseline))
    }
    if (length(block_lines) > 0) {
      lines <- c(lines, sprintf("nuisance_list[[%d]]:", block$block), block_lines)
    }
  }
  if (dropped) {
    lines <- c(lines, "Dropped non-finite, zero-variance, and rank-aliased nuisance columns.")
  } else {
    lines <- c(lines, "Use nuisance_check = \"error\" to stop or nuisance_check = \"drop\" to remove columns that do not increase rank.")
  }
  paste(lines, collapse = "\n")
}

.drop_nuisance_columns <- function(report) {
  lapply(seq_along(report$nuisance_list), function(i) {
    mat <- report$nuisance_list[[i]]
    keep <- report$by_block[[i]]$keep
    mat[, keep, drop = FALSE]
  })
}

.baseline_terms_for_nuisance_check <- function(basis, degree, sframe, intercept) {
  drift_spec <- baseline(degree = degree, basis = basis, intercept = intercept)
  list(
    drift = construct(drift_spec, sframe),
    block = if (intercept != "none" && basis != "constant") {
      construct_block_term("constant", sframe, intercept)
    }
  )
}

#' Check nuisance regressors for rank and column problems
#'
#' Inspects a block-wise nuisance regressor list before it is added to a
#' baseline model. The check is run per block and compares nuisance columns
#' against the baseline terms that would be constructed from `basis`, `degree`,
#' and `intercept`.
#'
#' @param nuisance_list A list of numeric matrices or data frames, one per block.
#' @param sframe A sampling frame.
#' @param basis,degree,intercept Baseline model settings used to construct the
#'   comparison baseline terms.
#' @param tol Numeric tolerance passed to QR rank checks.
#' @param duplicate_threshold Absolute correlation threshold used to flag
#'   duplicate or near-duplicate columns.
#'
#' @return A `nuisance_check` object with `ok`, `problems`, `by_block`, and
#'   normalized `nuisance_list` elements.
#' @export
check_nuisance <- function(nuisance_list,
                           sframe,
                           basis = c("constant", "poly", "bs", "ns"),
                           degree = 1,
                           intercept = c("runwise", "global", "none"),
                           tol = sqrt(.Machine$double.eps),
                           duplicate_threshold = 1 - sqrt(.Machine$double.eps)) {
  basis <- match.arg(basis)
  intercept <- match.arg(intercept)
  if (basis %in% c("bs", "ns")) {
    assert_that(degree > 2, msg = "'bs' and 'ns' bases must have degree >= 3")
  }
  baseline_terms <- .baseline_terms_for_nuisance_check(basis, degree, sframe, intercept)
  .check_nuisance_internal(nuisance_list, sframe, baseline_terms,
                           tol = tol,
                           duplicate_threshold = duplicate_threshold)
}

#' Clean nuisance regressors by dropping rank-useless columns
#'
#' Drops nuisance columns that are non-finite, zero-variance, or fail to
#' increase QR rank after the block's baseline terms and earlier nuisance
#' columns. Column order is respected, so when two columns are aliased the
#' earlier column is kept.
#'
#' @inheritParams check_nuisance
#'
#' @return A list with `nuisance_list` and `report` elements. Pass
#'   `result$nuisance_list` to `baseline_model()`.
#' @export
clean_nuisance <- function(nuisance_list,
                           sframe,
                           basis = c("constant", "poly", "bs", "ns"),
                           degree = 1,
                           intercept = c("runwise", "global", "none"),
                           tol = sqrt(.Machine$double.eps),
                           duplicate_threshold = 1 - sqrt(.Machine$double.eps)) {
  report <- check_nuisance(nuisance_list, sframe,
                           basis = basis,
                           degree = degree,
                           intercept = intercept,
                           tol = tol,
                           duplicate_threshold = duplicate_threshold)
  ret <- list(
    nuisance_list = .drop_nuisance_columns(report),
    report = report
  )
  class(ret) <- c("cleaned_nuisance", "list")
  ret
}

#' @export
print.nuisance_check <- function(x, ...) {
  cat(.format_nuisance_check(x), "\n")
  invisible(x)
}

#' Calculate column indices for a list of block matrices
#'
#' Given a list of matrices (e.g., one per block), this function calculates
#' the corresponding column indices for each matrix as they would appear
#' when combined into a block-diagonal structure.
#'
#' @param mat_list A list of matrices. Each element must be a matrix.
#' @return A list where each element is an integer vector of column indices
#'         corresponding to the matrix in the input list.
#' @noRd
#' @keywords internal
get_col_inds <- function(mat_list) {
  # mat_list must be a list containing only matrices
  if (!is.list(mat_list)) {
    stop("mat_list must be a list")
  }
  if (!all(vapply(mat_list, is.matrix, logical(1)))) {
    stop("All elements of mat_list must be matrices")
  }
  ncols_per_block <- vapply(mat_list, ncol, integer(1))
  if (any(ncols_per_block < 0)) {
      stop("Matrices in mat_list must have non-negative number of columns.")
  }
  
  # Cumulative sum of columns, starting from 0
  cum_ncols <- c(0, cumsum(ncols_per_block))
  
  # Generate sequences of column indices for each block
  lapply(seq_along(ncols_per_block), function(i) {
    if (ncols_per_block[i] > 0) {
      # Indices are from (cumulative cols before this block + 1) to (cumulative cols up to this block)
      (cum_ncols[i] + 1):cum_ncols[i + 1]
    } else {
      # Return an empty integer vector if a block has 0 columns
      integer(0)
    }
  })
}

#' Build a baseline_term from a list of block-wise nuisance matrices
#'
#' @param nuisance_list list of numeric matrices or data frames, **one per run/block**.
#' @param sframe        the sampling_frame used in the model.
#' @param prefix        prefix used when auto-naming the columns.
#'
#' @return a baseline_term object (class c("baseline_term","matrix_term",...))
#' @noRd
make_nuisance_term <- function(nuisance_list,
                               sframe,
                               prefix = "nuis") {

  nuisance_list <- .as_nuisance_matrices(nuisance_list, sframe)

  ## --- assemble block-diagonal matrix ------------------------------------
  # Ensure all elements are plain matrices without special attributes
  nuisance_mats <- lapply(nuisance_list, function(x) {
    mat <- as.matrix(x)
    # Remove any special class attributes that might interfere with bdiag
    class(mat) <- "matrix"
    attributes(mat) <- list(dim = dim(mat), dimnames = dimnames(mat))
    mat
  })
  full_mat <- as.matrix(Matrix::bdiag(nuisance_mats))
  ncols    <- ncol(full_mat)

  ## names:  prefix#<block>_<col>
  colnames(full_mat) <-
    unlist(purrr::imap(nuisance_list, function(mat, i)
      sprintf("%s#%02d_%d",
              prefix, as.integer(i), seq_len(ncol(mat)))))

  ## bookkeeping lists
  colind <- get_col_inds(lapply(nuisance_list, as.matrix))
  rowind <- split(seq_len(nrow(full_mat)), fmrihrf::blockids(sframe))

  baseline_term("nuisance", full_mat, colind, rowind)
}


## ============================================================================
## Section 2: Baseline Model Construction and Specification
## ============================================================================

#' Construct a Baseline Model
#'
#' Builds a baseline model to account for noise and non-event-related variance.
#' This model may include a drift term, a block intercept term, and nuisance regressors.
#'
#' @param basis Character; type of basis function ("constant", "poly", "bs", or "ns").
#' @param degree Integer; degree of the spline/polynomial function.
#' @param sframe A sampling_frame object.
#' @param intercept Character; whether to include an intercept ("runwise", "global", or "none").
#'   Ignored when \code{basis == "constant"} because the drift term already
#'   provides the constant baseline.
#' @param nuisance_list Optional list of nuisance matrices or data frames (one per fMRI block).
#' @param nuisance_check Character; how to handle nuisance diagnostics. `"warn"`
#'   warns on construction-time problems, `"error"` stops, `"drop"` removes
#'   non-finite, zero-variance, and rank-aliased columns with a warning, and
#'   `"none"` skips these checks.
#'
#' @return An object of class "baseline_model".
#'
#' @examples 
#' sframe <- fmrihrf::sampling_frame(blocklens = c(100, 100), TR = 2)
#' bmod <- baseline_model(basis = "bs", degree = 3, sframe = sframe)
#' bmod_global <- baseline_model(basis = "bs", degree = 3, sframe = sframe, intercept = "global")
#' bmod_nointercept <- baseline_model(basis = "bs", degree = 3, sframe = sframe, intercept = "none")
#' stopifnot(ncol(design_matrix(bmod)) == 8)
#' @export
#' @importFrom purrr compact
baseline_model <- function(basis = c("constant", "poly", "bs", "ns"), degree = 1, sframe, 
                           intercept = c("runwise", "global", "none"), nuisance_list = NULL,
                           nuisance_check = c("warn", "error", "drop", "none")) {
  
  basis <- match.arg(basis)
  intercept <- match.arg(intercept)
  nuisance_check <- match.arg(nuisance_check)
  
  if (basis %in% c("bs", "ns")) {
    assert_that(degree > 2, msg ="'bs' and 'ns' bases must have degree >= 3")
  }
  
  # Construct the drift term specification
  drift_spec <- baseline(degree = degree, basis = basis, intercept = intercept)
  drift_term <- construct(drift_spec, sframe)
  block_term <- if (intercept != "none" && basis != "constant") {
    construct_block_term("constant", sframe, intercept)
  }

  nuisance_report <- NULL
  if (!is.null(nuisance_list) && nuisance_check != "none") {
    baseline_terms <- purrr::compact(list(drift = drift_term, block = block_term))
    nuisance_report <- .check_nuisance_internal(nuisance_list, sframe, baseline_terms)
    if (!nuisance_report$ok) {
      if (nuisance_check == "error") {
        stop(.format_nuisance_check(nuisance_report), call. = FALSE)
      } else if (nuisance_check == "drop") {
        nuisance_list <- .drop_nuisance_columns(nuisance_report)
        warning(.format_nuisance_check(nuisance_report, dropped = TRUE), call. = FALSE)
      } else {
        warning(.format_nuisance_check(nuisance_report), call. = FALSE)
      }
    }
  }
  
  # List potential terms, using compact later to remove NULLs
  terms_list <- list(
    # Drift term always constructed based on spec
    drift = drift_term,
    # Block term constructed only if intercept is needed and basis isn't constant
    block = block_term,
    # Nuisance term constructed only if list provided
    nuisance = if (!is.null(nuisance_list)) {
                 make_nuisance_term(nuisance_list, sframe)
               }
  )

  # Remove NULL terms and store in the final list
  ret <- list(
    terms = purrr::compact(terms_list),
    # Keep drift_spec for potential inspection? (Optional, but consistent with review example)
    drift_spec = drift_spec, 
    sampling_frame = sframe,
    nuisance_check = nuisance_report
  )
  
  class(ret) <- c("baseline_model", "list")
  ret
}

#' Create a Baseline Specification
#'
#' Generates a baselinespec for modeling low-frequency drift in fMRI time series.
#'
#' @param degree Number of basis terms per image block (ignored for "constant").
#' @param basis Type of basis ("constant", "poly", "bs", or "ns").
#' @param name Optional name for the term.
#' @param intercept Type of intercept to include ("runwise", "global", or "none").
#'
#' @return A baselinespec list instance.
#' @examples
#' baseline(degree = 3, basis = "bs")
#' @export
baseline <- function(degree = 1, basis = c("constant", "poly", "bs", "ns"), name = NULL,
                     intercept = c("runwise", "global", "none")) {
  
  basis <- match.arg(basis)
  intercept <- match.arg(intercept)
  
  if (basis == "constant") {
    degree <- 1
  }
  
  bfun <- switch(basis,
                 bs = splines::bs,
                 ns = splines::ns,
                 poly = poly,
                 constant = function(x, degree) { matrix(rep(1, length(x))) })
  
  if (is.null(name)) {
    name <- paste0("baseline_", basis, "_", degree)
  }
  
  ret <- list(
    degree = degree,
    basis = basis,
    fun = bfun,
    intercept = intercept,
    name = name
  )
  
  class(ret) <- c("baselinespec", "nuisancespec")
  ret
}


## ============================================================================
## Section 3: Design Matrix and Term Functions for Baseline Models
## ============================================================================

#' @method design_matrix baseline_model
#' @rdname design_matrix
#' @export
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#' @examples
#' sframe <- fmrihrf::sampling_frame(blocklens = 6, TR = 1)
#' bmod <- baseline_model(sframe = sframe)
#' head(design_matrix(bmod))
design_matrix.baseline_model <- function(x, blockid = NULL, allrows = FALSE, ...) {
  # Map design_matrix over the terms list, passing blockid and allrows
  mats <- purrr::map(x$terms, design_matrix, blockid = blockid, allrows = allrows)
  # Combine the resulting matrices column-wise using bind_cols for safety and tibble output
  dplyr::bind_cols(mats)
}

#' @method terms baseline_model
#' @export
#' @importFrom purrr map_lgl
terms.baseline_model <- function(x, ...) {
  # Simply return the terms list
  x$terms
}

#' @method baseline_terms baseline_model
#' @rdname baseline_terms
#' @export
baseline_terms.baseline_model <- function(x, ...) {
  x$terms
}

#' @export
term_matrices.baseline_model <- function(x, ...) {
  # Get the design matrix and check for col_indices attribute
  dm <- design_matrix(x)
  col_indices <- attr(dm, "col_indices")

  if (is.null(col_indices)) {
    # Fallback: construct indices by computing design matrices for each term
    result <- vector("list", length(x$terms))
    names(result) <- names(x$terms)

    for (i in seq_along(x$terms)) {
      term_dm <- design_matrix(x$terms[[i]])
      if (ncol(term_dm) > 0) {
        result[[i]] <- term_dm
      } else {
        result[[i]] <- NULL
      }
    }
    return(result)
  }

  # Use col_indices if available
  result <- vector("list", length(col_indices))
  names(result) <- names(col_indices)

  for (i in seq_along(col_indices)) {
    indices <- col_indices[[i]]
    if (length(indices) > 0) {
      result[[i]] <- dm[, indices, drop = FALSE]
    } else {
      result[[i]] <- NULL
    }
  }

  result
}

#' @method cells baseline_model
#' @rdname cells
#' @export
#' @importFrom dplyr mutate tibble
#' @importFrom stringr str_pad
cells.baseline_model <- function(x, drop.empty = TRUE, ...) {
  # Use lapply over x$terms
  cells_list <- lapply(x$terms, function(term) {
    conds <- conditions(term)
    ncond <- length(conds)
    # Handle case with zero conditions gracefully
    if (ncond == 0) return(dplyr::tibble(term = character(0), level = character(0), basis = character(0)))
    # Use str_pad for consistent zero-padding
    basis_names <- stringr::str_pad(1:ncond, width = ceiling(log10(ncond + 1e-6)), pad = "0")
    dplyr::tibble(term = term$varname, level = conds, basis = paste0("basis", basis_names))
  })
  # Use do.call(rbind, ...) to combine
  combined_cells <- do.call(rbind, cells_list)
  # Add index column if there are rows
  if(nrow(combined_cells) > 0) {
     dplyr::mutate(combined_cells, index = 1:dplyr::n())
  } else {
     dplyr::mutate(combined_cells, index = integer(0))
  }
}

## ============================================================================
## Section 4: Block and Nuisance Specification Helpers
## ============================================================================

#' Create a Block Variable
#'
#' Returns a block variable that is constant over the span of a scanning run.
#'
#' @param x The block variable.
#' @return An object of class "blockspec".
#' @examples
#' block(run)
#' @export
block <- function(x) {
  varname <- substitute(x)
  pterm <- parse_term(as.list(substitute(x)), "block")
  ret <- list(
    name = varname,
    label = pterm$label
  )
  class(ret) <- "blockspec"
  ret
}

#' @method construct baselinespec
#' @rdname construct
#' @export
construct.baselinespec <- function(x, model_spec, ...) {
  sampling_frame <- if (!is.null(model_spec$sampling_frame)) model_spec$sampling_frame else model_spec
  
  bl <- fmrihrf::blocklens(sampling_frame) # Normalize block lengths usage
  
  # Validate block lengths
  if (length(bl) == 0 || any(bl <= 0)) {
    stop("Invalid block lengths: must have at least one block with positive length")
  }
  
  # Compute baseline covariates for each block, passing correct argument name
  ret_list <- lapply(bl, function(block_len) {
    if (x$basis == "ns") {
      x$fun(seq(1, block_len), df = x$degree)
    } else if (x$basis %in% c("poly", "bs")) {
      x$fun(seq(1, block_len), degree = x$degree)
    } else { 
      x$fun(seq(1, block_len))
    }
  })
  
  # Check that ret_list is not empty
  if (length(ret_list) == 0) {
    stop("No baseline terms could be constructed: ret_list is empty")
  }
  
  # Simplified handling for global constant intercept
  if (x$basis == "constant" && x$intercept == "global") {
    mat <- matrix(1, nrow = sum(bl), ncol = 1)
    cnames <- paste0("base_", x$basis)
    colnames(mat) <- cnames
    # column index is a single value, but rows are tracked per block
    colind <- list(1)
    rowind <- split(seq_len(nrow(mat)), fmrihrf::blockids(sampling_frame))
    return(baseline_term(x$name, mat, colind, rowind))
  }
  
  # Standard block-wise construction
  nc_per_block <- ncol(ret_list[[1]])
  total_cols <- length(ret_list) * nc_per_block
  mat <- matrix(0, sum(bl), total_cols)
  
  colind <- vector("list", length(ret_list))
  rowind <- vector("list", length(ret_list))
  all_cnames <- vector("character", total_cols)
  
  current_col <- 1
  current_row <- 1
  for (i in seq_along(ret_list)) {
    rows_this_block <- bl[i]
    cols_this_block <- nc_per_block
    
    row_indices <- current_row:(current_row + rows_this_block - 1)
    col_indices <- current_col:(current_col + cols_this_block - 1)
    
    mat[row_indices, col_indices] <- ret_list[[i]]
    colind[[i]] <- col_indices
    rowind[[i]] <- row_indices
    
    # Generate column names for this block
    cnames_block <- paste0("base_", x$basis, 1:cols_this_block, "_block_", i)
    all_cnames[col_indices] <- cnames_block
    
    current_row <- current_row + rows_this_block
    current_col <- current_col + cols_this_block
  }
  
  colnames(mat) <- all_cnames
  baseline_term(x$name, mat, colind, rowind)
}

#' Construct a Baseline Term
#'
#' Creates a baseline_term object given a covariate matrix and its associated
#' column and row indices.
#'
#' @param varname The name of the term.
#' @param mat A matrix (or data frame) of covariates.
#' @param colind A list of column indices.
#' @param rowind A list of row indices.
#' @return A baseline_term object.
#' @importFrom tibble as_tibble
#' @noRd
#' @keywords internal
#' Construct a baseline term
#' 
#' @param varname Variable name for the term
#' @param mat Design matrix
#' @param colind Column indices
#' @param rowind Row indices
#' @return A baseline_term object
#' @keywords internal
#' @noRd
baseline_term <- function(varname, mat, colind, rowind) {
  stopifnot(inherits(mat, "matrix") || is.data.frame(mat) || inherits(mat, "Matrix"))
  ret <- list(varname = varname, 
              design_matrix = suppressMessages(tibble::as_tibble(as.matrix(mat), .name_repair = "minimal")), 
              colind = colind, 
              rowind = rowind)
  class(ret) <- c("baseline_term", "matrix_term", "fmri_term", "list")
  ret
}

#' @method design_matrix baseline_term
#' @rdname design_matrix
#' @export
design_matrix.baseline_term <- function(x, blockid = NULL, allrows = FALSE, ...) {
  if (is.null(blockid)) {
    x$design_matrix
  } else {
    if (!allrows) {
      x$design_matrix[unlist(x$rowind[blockid]), unlist(x$colind[blockid]), drop = FALSE]
    } else {
      x$design_matrix[, unlist(x$colind[blockid]), drop = FALSE]
    }
  }
}

#' @export
conditions.baseline_term <- function(x, ...) {
  colnames(x$design_matrix)
}

#' Construct a Block Term.
#'
#' Constructs a constant block intercept term based on block IDs.
#'
#' @param vname The name of the block variable.
#' @param sframe A sampling_frame object.
#' @param intercept Type of intercept ("global" or "runwise").
#' @return A block_term object.
#' @noRd
construct_block_term <- function(vname, sframe, intercept = c("global", "runwise")) {
  intercept <- match.arg(intercept)
  blockids_vec <- fmrihrf::blockids(sframe)
  blockord <- sort(unique(blockids_vec))
  n_total_scans <- length(blockids_vec)
  n_blocks <- length(blockord)
  
  if (n_blocks == 1 || intercept == "global") {
    # Simple global intercept: single column of 1s
    mat <- matrix(1, nrow = n_total_scans, ncol = 1)
    cnames <- paste0(vname, "_global")
    colnames(mat) <- cnames
    # Even for global intercept, we need to respect block structure for row indices
    colind <- rep(list(1), n_blocks)  # Same column for all blocks
    rowind <- split(1:n_total_scans, blockids_vec)  # Split rows by block
  } else {
    # Runwise intercept: use model.matrix once
    # Create the factor directly for model.matrix
    expanded_blockids_fac <- factor(blockids_vec, levels = blockord) 
    mat <- model.matrix(~ expanded_blockids_fac - 1)
    cnames <- paste0(vname, "_", blockord)
    colnames(mat) <- cnames
    # colind and rowind reflect the block structure
    colind <- as.list(1:n_blocks) 
    rowind <- split(1:n_total_scans, blockids_vec)
  }
  
  # Use baseline_term constructor directly
  baseline_term(vname, mat, colind, rowind) 
}

#' @export
term_names.baseline_model <- function(x, ...) {
  xt <- terms(x)
  unlist(lapply(xt, function(term) term$varname))
}

#' Create a Nuisance Specification
#'
#' Returns a nuisance term specification from a numeric matrix.
#'
#' @param x A matrix.
#' @return An object of class "nuisancespec".
#' @examples
#' mat <- matrix(rnorm(10), nrow = 5)
#' nuisance(mat)
#' @export
nuisance <- function(x) {
  varname <- substitute(x)
  ret <- list(name = varname)
  class(ret) <- "nuisancespec"
  ret
}

# Simple constructor for matrix terms
# @keywords internal
matrix_term <- function(varname, mat) {
  stopifnot(is.matrix(mat))
  ret <- list(varname = varname, design_matrix = suppressMessages(tibble::as_tibble(mat)))
  class(ret) <- c("baseline_term", "matrix_term", "fmri_term", "list")
  ret
}

#' @export
construct.nuisancespec <- function(x, model_spec, ...) {
  expr <- rlang::parse_expr(as.character(x$varname))
  mat <- rlang::eval_tidy(expr, data = model_spec$aux_data, env = parent.frame())
  matrix_term(x$name, mat)
}

#' @export
construct.blockspec <- function(x, model_spec, ...) {
  construct_block_term(x$name, model_spec$sampling_frame)
}


## ============================================================================
## Section 4: Print and Plot Methods for Baseline Models
## ============================================================================

#' Print a Baseline Model
#'
#' Displays key information about the baseline model components and a preview
#' of the design matrix.
#'
#' @param x A baseline_model object.
#' @param ... Additional arguments (ignored).
#' @return The input object, invisibly.
#' @examples
#' sframe <- fmrihrf::sampling_frame(blocklens = 5, TR = 1)
#' bmod <- baseline_model(sframe = sframe)
#' print(bmod)
#' @export
#' @rdname print
print.baseline_model <- function(x, ...) {
  # Extract component information using helper
  drift_cols <- .nz(x$terms$drift)
  const_cols <- .nz(x$terms$block)
  nuis_cols  <- .nz(x$terms$nuisance)
  total_cols <- sum(drift_cols, const_cols, nuis_cols) # Summing is safer now
  
  # Get drift spec details (assuming drift_spec is still stored, adjust if removed)
  basis_type <- if (!is.null(x$drift_spec)) x$drift_spec$basis else "N/A"
  degree     <- if (!is.null(x$drift_spec)) x$drift_spec$degree else "N/A"
  drift_name <- if (!is.null(x$terms$drift)) x$terms$drift$varname else "N/A"
  
  # Print header.
  cat("================================================\n")
  cat("           Baseline Model                       \n")
  cat("================================================\n")
  
  # Drift term info.
  if (drift_cols > 0) {
      cat("  Drift Components                           \n")
      cat(sprintf("    * %-35s\n", paste("Name:", drift_name)))
      cat(sprintf("    * %-35s\n", paste("Basis type:", basis_type)))
      cat(sprintf("    * %-35s\n", paste("Degree:", degree)))
      cat(sprintf("    * %-35s\n", paste("Drift columns:", drift_cols)))
  }
  
  # Additional components.
  cat("\n") 
  cat("  Additional Components                    \n")
  cat(sprintf("    * %-35s\n", paste("Constant columns:", const_cols)))
  cat(sprintf("    * %-35s\n", paste("Nuisance columns:", nuis_cols)))
  
  # Summary.
  cat("\n") # Blank line
  cat("  Model Summary                            \n")
  cat(sprintf("    * %-35s\n", paste("Total columns:", total_cols)))
  
  # Preview design matrix.
  cat("\n") # Blank line
  cat("  Design Matrix Preview                    \n")
  if (total_cols > 0) {
      dm <- head(design_matrix(x), 3)
      for (i in 1:min(3, nrow(dm))) {
        row_preview <- paste(sprintf("%6.3f", as.numeric(dm[i, 1:min(4, ncol(dm))])), collapse = " ")
        if (ncol(dm) > 4) row_preview <- paste0(row_preview, " ...")
        # Adjust padding to 37 for design matrix preview lines
        cat(sprintf("    %-37s\n", row_preview)) 
      }
      # Adjust padding for '...' line
      if (nrow(dm) > 3) cat("    ...                                    \n") 
  } else {
      # Adjust padding for 'no terms' line
      cat("    (No baseline terms in model)           \n") 
  }
  
  cat("================================================\n")
}

#' Plot a Baseline Model
#'
#' Creates a detailed ggplot2 visualization of the baseline model design matrix.
#' Each non-constant term is plotted over time. The plot includes separate panels
#' for each block and supports customization of titles, axis labels, line size, and color palette.
#'
#' @param x A baseline_model object.
#' @param term_name Optional term name (a character string) specifying which term to plot.
#'   If omitted, the first non-constant term is plotted.
#' @param title Optional title for the plot. If not provided, a default title is generated.
#' @param xlab Label for the x-axis (default: "Time").
#' @param ylab Label for the y-axis (default: "Design Matrix Value").
#' @param line_size Numeric value for line thickness (default: 1).
#' @param color_palette A palette name for the line colors (default: "Set1").
#' @param ... Additional arguments passed to ggplot2::geom_line.
#' @return A ggplot2 plot object.
#' @examples
#' sframe <- fmrihrf::sampling_frame(blocklens = 5, TR = 1)
#' bmod <- baseline_model(sframe = sframe)
#' if (requireNamespace("ggplot2", quietly = TRUE)) plot(bmod)
#'
#' @importFrom ggplot2 ggplot aes_string geom_line facet_wrap labs theme_minimal scale_color_brewer
#' @importFrom tidyr pivot_longer
#' @keywords internal
#' @export
plot.baseline_model <- function(x, term_name = NULL, title = NULL, 
                                xlab = "Time", ylab = "Design Matrix Value",
                                line_size = 1, color_palette = "Set1", ...) {
  # Extract terms and term names from the baseline model using the terms() S3 method
  all_terms <- terms(x)
  if (length(all_terms) == 0) {
      stop("Baseline model contains no terms.")
  }
  term_names <- names(all_terms)
  
  # Remove constant terms from plotting (e.g., block intercept)
  # Need a reliable way to identify constant terms - check varname? Or add a flag?
  # Let's assume terms named "constant" or similar are constant for now.
  # A more robust approach might be needed.
  const_idx <- grep("^constant", term_names, ignore.case = TRUE)
  if (length(const_idx) > 0) {
    plotting_terms <- all_terms[-const_idx]
    plotting_term_names <- term_names[-const_idx]
  } else {
    plotting_terms <- all_terms
    plotting_term_names <- term_names
  }
  
  # Check if any non-constant terms remain.
  if (length(plotting_terms) == 0) {
    stop("No non-constant baseline terms available for plotting.")
  }
  
  # Derive time and block IDs from the sampling frame without mutating it
  # Use run-relative sample times so each facet starts at 0 per block
  time_vec    <- tryCatch(fmrihrf::samples(x$sampling_frame, global = FALSE), silent = TRUE)
  blockids_vec <- tryCatch(fmrihrf::blockids(x$sampling_frame), silent = TRUE)
  if (inherits(time_vec, "try-error") || inherits(blockids_vec, "try-error") ||
      is.null(time_vec) || is.null(blockids_vec)) {
    stop("Could not derive sample times or block IDs from the sampling_frame.", call. = FALSE)
  }
  
  # For each term to plot, convert its design matrix into a long-format tibble.
  dflist <- lapply(plotting_terms, function(term) {
    dm <- design_matrix(term) # Get matrix for this specific term
    dm_tib <- suppressMessages(tibble::as_tibble(dm, .name_repair = "check_unique"))
    # Add block and time info - ensure dimensions match!
    if (nrow(dm_tib) != length(blockids_vec)) {
        stop(paste("Row mismatch between design matrix for term", term$varname, "and sampling frame."))
    }
    dm_tib$.block <- blockids_vec
    dm_tib$.time  <- time_vec
    tidyr::pivot_longer(dm_tib, cols = -c(.time, .block),
                        names_to = "condition", values_to = "value")
  })
  names(dflist) <- plotting_term_names
  
  # Select the term to plot, allowing for partial matching against plottable terms.
  if (is.null(term_name)) {
    plot_term_idx <- 1
    plot_term <- plotting_term_names[plot_term_idx]
    message(paste("No term_name specified, plotting the first available non-constant term:", plot_term))
  } else {
    exact_match <- which(plotting_term_names == term_name)
    if (length(exact_match) == 1) {
      plot_term_idx <- exact_match
      plot_term <- plotting_term_names[plot_term_idx]
    } else {
      # Try partial matching if no exact match
      partial_matches <- grep(term_name, plotting_term_names, ignore.case = TRUE)
      if (length(partial_matches) == 1) {
        plot_term_idx <- partial_matches
        plot_term <- plotting_term_names[plot_term_idx]
        message(paste("Found unique partial match for '", term_name, "': using term '", plot_term, "'", sep=""))
      } else if (length(partial_matches) == 0) {
        stop("Specified term_name '", term_name, "' not found among plottable terms. Available: ", 
             paste(plotting_term_names, collapse=", "))
      } else {
        # Multiple partial matches
        stop("Specified term_name '", term_name, "' matches multiple terms: ", 
             paste(plotting_term_names[partial_matches], collapse=", "), ". Please be more specific.")
      }
    }
  }
  
  # Get the data for the selected term and ensure stable ordering
  dfx <- dflist[[plot_term]]
  dfx <- dfx[order(dfx$.block, dfx$condition, dfx$.time), ]
  # Coerce types explicitly to avoid downstream surprises
  dfx$.block    <- factor(dfx$.block)
  dfx$condition <- as.factor(dfx$condition)
  dfx$value     <- as.numeric(dfx$value)
  dfx$.time     <- as.numeric(dfx$.time)
  n_cond <- length(levels(dfx$condition))
  # Ensure block is a factor for facetting stability
  dfx$.block <- as.factor(dfx$.block)
  
  # Define scale function outside the pipe
  # Use a robust default color scale that supports many categories
  scale_fn <- function(...) ggplot2::scale_color_hue(...)
  
  # Create the ggplot (handle single vs multi condition for robust legends/scales).
  if (n_cond <= 1) {
    p <- ggplot2::ggplot(dfx, ggplot2::aes(x = .time, y = value, group = 1)) +
      ggplot2::geom_line(size = line_size, na.rm = TRUE, colour = "#2c7fb8", ...) +
      ggplot2::facet_wrap(ggplot2::vars(.block), ncol = 1, scales = "free_x") +
      ggplot2::labs(title = if (!is.null(title)) title else paste("Baseline Model:", plot_term),
                    x = xlab, y = ylab) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(legend.position = "none",
                     plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                     axis.title = ggplot2::element_text(face = "bold"))
  } else {
    p <- ggplot2::ggplot(dfx, ggplot2::aes(x = .time, y = value, colour = condition, group = condition)) +
      ggplot2::geom_line(size = line_size, na.rm = TRUE, ...) +
      ggplot2::facet_wrap(ggplot2::vars(.block), ncol = 1, scales = "free_x") +
      ggplot2::labs(title = if (!is.null(title)) title else paste("Baseline Model:", plot_term),
                    x = xlab, y = ylab, colour = "Condition") +
      scale_fn() +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(legend.position = "bottom",
                     plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                     axis.title = ggplot2::element_text(face = "bold"))
  }
  
  p
}

#' correlation_map.baseline_model
#'
#' @description
#' Generates a correlation heatmap of the columns in a \code{baseline_model}'s design matrix.
#'
#' @param x A \code{baseline_model}.
#' @param method Correlation method (e.g., "pearson", "spearman").
#' @param half_matrix Logical; if TRUE, display only the lower triangle of the matrix.
#' @param absolute_limits Logical; if TRUE, set color scale limits from -1 to 1.
#' @param ... Additional arguments passed to internal plotting functions.
#' @return A ggplot2 plot object.
#' @examples
#' sframe <- fmrihrf::sampling_frame(blocklens = 5, TR = 1)
#' bmod <- baseline_model(sframe = sframe)
#' if (requireNamespace("ggplot2", quietly = TRUE)) correlation_map(bmod)
#' @export
correlation_map.baseline_model <- function(x,
                                           method          = c("pearson", "spearman"),
                                           half_matrix     = FALSE,
                                           absolute_limits = TRUE,
                                           ...) {
  DM <- as.matrix(design_matrix(x))
  .correlation_map_common(DM, method=method, half_matrix=half_matrix,
                          absolute_limits=absolute_limits, ...)
}


#' Heatmap visualization of the baseline_model design matrix
#'
#' @description
#' Produces a heatmap of all columns in the design matrix for a `baseline_model` object,
#' with rows corresponding to scans and columns corresponding to regressors. By default,
#' it draws horizontal lines separating runs (blocks), and rotates the column labels diagonally.
#'
#' @param x A `baseline_model` object.
#' @param block_separators Logical; if `TRUE`, draw white horizontal lines between blocks.
#' @param rotate_x_text Logical; if `TRUE`, rotate x-axis labels by 45 degrees.
#' @param fill_midpoint Numeric or `NULL`; if not `NULL`, used as the `midpoint` in
#'   [ggplot2::scale_fill_gradient2()] to center the color scale (for example at 0).
#' @param fill_limits Numeric vector of length 2 or `NULL`; passed to the fill scale
#'   `limits` argument. Can clip or expand the color range.
#' @param ... Additional arguments forwarded to [ggplot2::geom_tile()].
#'
#' @import ggplot2
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @return A ggplot2 plot object.
#' @examples
#' sframe <- fmrihrf::sampling_frame(blocklens = 5, TR = 1)
#' bmod <- baseline_model(sframe = sframe)
#' if (requireNamespace("ggplot2", quietly = TRUE)) design_map(bmod)
#' @export
design_map.baseline_model <- function(x,
                                      block_separators = TRUE,
                                      rotate_x_text    = TRUE,
                                      fill_midpoint    = NULL,
                                      fill_limits      = NULL,
                                      ...) {
  # 1) Extract the design matrix
  DM <- design_matrix(x)
  n_scans <- nrow(DM)
  
  # 2) Convert to long format
  df_long <- tibble::as_tibble(DM, .name_repair = "unique")
  df_long$scan_number <- seq_len(n_scans)
  df_long <- tidyr::pivot_longer(
    df_long,
    cols      = -scan_number,
    names_to  = "Regressor",
    values_to = "Value"
  )
  
  # 3) Build the base ggplot
  plt <- ggplot(df_long, aes(x = Regressor, y = scan_number, fill = Value)) +
    geom_tile(...)
  
  # 4) Reverse the y-axis so that scan #1 is at top
  plt <- plt + scale_y_reverse()
  
  # 5) Decide on color scale
  #    - If fill_midpoint is set, use scale_fill_gradient2 to center the scale
  #    - Otherwise, use a default 3-color gradient
  if (is.null(fill_midpoint)) {
    plt <- plt + scale_fill_gradientn(
      colours = c("navy", "white", "firebrick"),
      limits  = fill_limits
    )
  } else {
    plt <- plt + scale_fill_gradient2(
      midpoint = fill_midpoint,
      low      = "navy",
      mid      = "white",
      high     = "firebrick",
      limits   = fill_limits
    )
  }
  
  # 6) Optionally draw white horizontal lines to separate blocks
  if (block_separators) {
    block_ids  <- tryCatch(fmrihrf::blockids(x$sampling_frame), silent = TRUE)
    if (inherits(block_ids, "try-error") || is.null(block_ids)) {
      return(plt)
    }
    run_info   <- rle(block_ids)             # lengths of each block
    row_breaks <- cumsum(run_info$lengths)   # boundary after each block
    ncols      <- ncol(DM)
    
    # Add horizontal lines
    for (rb in row_breaks[-length(row_breaks)]) {
      plt <- plt + 
        annotate("segment",
                 x    = 0.5,
                 xend = ncols + 0.5,
                 y    = rb + 0.5,
                 yend = rb + 0.5,
                 color = "white", size = 1)
    }
  }
  
  # 7) Clean up theme
  plt <- plt + 
    theme_minimal(base_size = 14) +
    labs(x = "Regressors", y = "Scan Number", fill = "Value") +
    theme(
      panel.grid  = element_blank(),
      axis.text.x = if (rotate_x_text) element_text(angle = 45, hjust = 1) else element_text()
    )
  
  plt
}


## ============================================================================
## Section 5: End of File
###############################################################################
