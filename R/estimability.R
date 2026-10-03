#' Check full-design estimability and contrast precision
#'
#' Diagnoses dependencies involving many regressors, including event regressors
#' and the baseline. Unlike [check_collinearity()], this uses the singular values
#' of the full design rather than pairwise correlations.
#'
#' @param x An `event_model`, or a finite numeric matrix/data frame (including
#'   a sparse `Matrix`) with observations in rows.
#' @param contrasts A numeric vector, a matrix with contrast vectors in columns,
#'   or a list of these. Names on vectors or row names on matrices are matched
#'   strictly to design columns; omitted columns receive zero weight. Unnamed
#'   weights must span the input design or the full design including appended
#'   baseline columns. For an event model, `NULL` uses the declared contrasts
#'   returned by [contrast_weights()].
#' @param baseline `NULL` (default) adds run intercepts to an event model and
#'   leaves a matrix input unchanged. `TRUE` requests run intercepts for an event
#'   model, or a global intercept for a matrix (reusing an existing constant
#'   nonzero column). `FALSE` adds nothing. Alternatively, supply a
#'   `baseline_model` or numeric matrix of the actual baseline/nuisance columns,
#'   with rows in the same order as `x`. Supplied columns are never dropped.
#' @param absolute Include diagnostics for individual coefficients of the input
#'   design (before appending a baseline). Default `TRUE`.
#' @param condition_threshold Condition-number warning threshold, default 10.
#'   This is a screening heuristic, not a universal precision cutoff.
#' @param coverage_threshold Flag runs whose event coverage exceeds this
#'   fraction, default 0.9. Coverage is context, not an estimability test.
#' @param tol Relative singular-value cutoff for numerical rank and relative
#'   null-space tolerance for contrast estimability. Default `1e-8`.
#'
#' @details
#' Columns are scaled to unit Euclidean norm without centring, retaining the
#' intercept. `condition_number` is the exact spectral ratio (largest/smallest
#' singular value), or `Inf` for a numerically rank-deficient design. It is not
#' the default approximation returned by `kappa()`. Zero columns are retained.
#'
#' `weakest_direction` gives named loadings in the scaled coordinates and the
#' corresponding coefficient direction in the original units. Its sign is
#' arbitrary; when the smallest singular value is repeated, the direction is
#' not unique. `baseline_pattern` labels a possible baseline-versus-regressors
#' dependency when all loadings outside the baseline have one sign and baseline
#' loadings have the opposite sign, in an ill-conditioned or deficient design.
#' This is a heuristic interpretation, not proof of a particular task structure.
#'
#' A contrast is estimable when it is orthogonal to the design's null space.
#' Estimable contrasts receive a `variance_factor`, equal to
#' \eqn{c'(X'X)^{-1}c} for full-rank designs and the corresponding generalized
#' inverse quadratic form otherwise. Non-estimable contrasts receive `NA`,
#' never a misleading finite pseudoinverse variance. These are variances per
#' unit independent residual variance in the original coefficient units;
#' rescaling a contrast rescales its variance quadratically. F-contrast columns
#' are assessed separately, not as an omnibus F statistic. For inference with
#' temporal filtering or correlated errors, supply the actual filtered/whitened
#' full design and matching contrasts.
#'
#' Event coverage is the union of modelled event intervals, clipped to each
#' run's `[0, n_scans * TR)` window. Overlapping events or repeated terms count
#' once; zero-duration events occupy no time. Coverage uses retained event
#' terms, including term-specific subsets/durations, and is unavailable (`NULL`)
#' for raw matrices or models without event timing. High coverage suggests
#' inspecting scientifically appropriate centred or reference-condition
#' contrasts, which estimate different quantities from absolute coefficients.
#'
#' @return An `estimability_check` list with `ok`, `rank`, `n_columns`,
#'   `n_observations`, `full_rank`, `condition_number`, `condition_threshold`,
#'   `ill_conditioned`, `singular_values`, `column_norms`, `weakest_direction`,
#'   `baseline_pattern`, `baseline` (source and column names), `contrasts`
#'   (name, quantity, estimable, variance_factor, null_fraction), `coverage`
#'   (run, duration, covered, fraction, high_coverage), and explanatory `messages`.
#'   `ok` means full numerical rank and condition number within the threshold;
#'   it does not certify adequate precision for a scientific question.
#'
#' @examples
#' # Every observation belongs to one task: absolute effects alias the intercept.
#' X <- cbind(task_A = c(1, 1, 0, 0), task_B = c(0, 0, 1, 1), intercept = 1)
#' check_collinearity(X)$ok
#' check_estimability(X, contrasts = list(A_vs_B = c(1, -1, 0)))
#'
#' @seealso [validate_contrasts()], [baseline_model()]
#' @export
check_estimability <- function(x, contrasts = NULL, baseline = NULL,
                               absolute = TRUE, condition_threshold = 10,
                               coverage_threshold = 0.9, tol = 1e-8) {
  scalar_in_range <- function(z, lo, hi, label) {
    if (!is.numeric(z) || length(z) != 1L || !is.finite(z) || z <= lo || z > hi) {
      stop(label, " must be a finite number in (", lo, ", ", hi, "].", call. = FALSE)
    }
  }
  scalar_in_range(tol, 0, 0.1, "tol")
  scalar_in_range(condition_threshold, 1, Inf, "condition_threshold")
  scalar_in_range(coverage_threshold, 0, 1, "coverage_threshold")
  if (!is.logical(absolute) || length(absolute) != 1L || is.na(absolute)) {
    stop("absolute must be TRUE or FALSE.", call. = FALSE)
  }
  is_model <- inherits(x, "event_model")
  X <- .estimability_matrix(if (is_model) design_matrix(x) else x, "x")
  input_columns <- ncol(X)
  if (is.null(colnames(X))) colnames(X) <- paste0("column", seq_len(input_columns))
  .estimability_names(colnames(X), "Design column names")
  base <- .estimability_baseline(x, X, baseline, is_model)
  X <- base$design
  p <- ncol(X)
  norms <- apply(X, 2L, .estimability_norm)
  if (any(!is.finite(norms))) stop("Design column norms overflow.", call. = FALSE)
  scales <- ifelse(norms == 0, 1, norms)
  Z <- sweep(X, 2L, scales, "/")
  # Full V is necessary when there are more columns than observations.
  decomp <- svd(Z, nu = 0L, nv = p)
  singular_values <- c(decomp$d, rep(0, p - length(decomp$d)))
  retained <- which(singular_values > tol * max(singular_values))
  rank <- length(retained)
  null <- setdiff(seq_len(p), retained)
  condition <- if (rank < p) Inf else singular_values[1L] / singular_values[p]
  ill <- condition > condition_threshold
  weak <- decomp$v[, p]
  weak <- weak * if (weak[which.max(abs(weak))] < 0) -1 else 1
  base_idx <- match(base$columns, colnames(X))
  other_idx <- setdiff(seq_len(p), base_idx)
  same_sign <- function(z) length(z) > 0L && (all(z > tol) || all(z < -tol))
  pattern <- if (ill && same_sign(weak[base_idx]) && same_sign(weak[other_idx]) &&
                 sign(weak[base_idx[1L]]) != sign(weak[other_idx[1L]])) {
    "baseline_minus_regressors"
  } else NULL

  if (is.null(contrasts) && is_model) {
    declared <- contrast_weights(x)
    contrasts <- lapply(declared, function(z) z$offset_weights)
    if (any(vapply(contrasts, is.null, logical(1)))) {
      stop("Declared contrast is missing full-design offset_weights.", call. = FALSE)
    }
  }
  weights <- .estimability_weights(contrasts, colnames(X), input_columns)
  quantities <- rep("contrast", ncol(weights))
  if (absolute) {
    W <- diag(p)[, seq_len(input_columns), drop = FALSE]
    colnames(W) <- paste0("beta:", colnames(X)[seq_len(input_columns)])
    weights <- cbind(weights, W)
    quantities <- c(quantities, rep("absolute", input_columns))
  }
  metrics <- lapply(seq_len(ncol(weights)), function(j) {
    cvec <- weights[, j] / scales
    size <- .estimability_norm(cvec)
    if (!is.finite(size)) stop("Scaled contrast weights overflow.", call. = FALSE)
    fraction <- if (size == 0 || !length(null)) 0 else {
      .estimability_norm(crossprod(decomp$v[, null, drop = FALSE], cvec / size))
    }
    estimable <- fraction <= tol
    variance <- if (!estimable) NA_real_ else if (!rank) 0 else {
      sum((crossprod(decomp$v[, retained, drop = FALSE], cvec) /
             singular_values[retained])^2)
    }
    data.frame(name = colnames(weights)[j], quantity = quantities[j],
               estimable = estimable, variance_factor = variance, null_fraction = fraction)
  })
  contrast_report <- if (length(metrics)) do.call(rbind, metrics) else {
    data.frame(name = character(), quantity = character(), estimable = logical(),
               variance_factor = numeric(), null_fraction = numeric())
  }
  coverage <- if (is_model) .estimability_coverage(x, coverage_threshold) else NULL
  messages <- character()
  if (rank < p) messages <- c(messages, sprintf("Design is rank deficient (%d of %d columns).", rank, p))
  if (ill && rank == p) messages <- c(messages, sprintf(
    "Full-rank design has condition number %.3g above the screening threshold %.3g; inspect contrast precision.",
    condition, condition_threshold))
  if (!is.null(pattern)) messages <- c(messages,
    "Weakest direction trades the baseline against same-sign event/regressor loadings (baseline_minus_regressors).")
  if (any(!contrast_report$estimable)) messages <- c(messages,
    "Non-estimable contrasts have no identifiable coefficient variance (variance_factor = NA).")
  if (!length(base$columns)) messages <- c(messages,
    "No baseline columns identified: supply the actual baseline/nuisance design to assess its dependencies.")
  if (!is.null(coverage) && any(coverage$high_coverage)) messages <- c(messages, sprintf(
    "Run(s) %s have little or no implicit baseline (>%.0f%% event coverage); consider centred contrasts or contrasts against a modelled condition if appropriate to the scientific question.",
    paste(coverage$run[coverage$high_coverage], collapse = ", "), 100 * coverage_threshold))
  structure(list(ok = rank == p && !ill, rank = rank, n_columns = p,
    n_observations = nrow(X), full_rank = rank == p, condition_number = condition,
    condition_threshold = condition_threshold, ill_conditioned = ill,
    singular_values = singular_values, column_norms = norms,
    weakest_direction = data.frame(column = colnames(X), loading = weak,
      coefficient_loading = weak / scales, baseline = seq_len(p) %in% base_idx,
      row.names = NULL), baseline_pattern = pattern,
    baseline = list(source = base$source, columns = base$columns),
    contrasts = contrast_report, coverage = coverage, messages = messages),
    class = "estimability_check")
}

.estimability_matrix <- function(x, label) {
  if (!(is.matrix(x) || is.data.frame(x) || inherits(x, "Matrix"))) {
    stop(label, " must be a numeric design matrix or data frame.", call. = FALSE)
  }
  x <- as.matrix(x)
  if (!is.numeric(x) || any(!is.finite(x)) || nrow(x) == 0L || ncol(x) == 0L) {
    stop(label, " must be nonempty, numeric and finite.", call. = FALSE)
  }
  x
}

.estimability_names <- function(x, label) {
  if (anyNA(x) || any(!nzchar(x)) || anyDuplicated(x)) {
    stop(label, " must be nonempty and unique.", call. = FALSE)
  }
}

.estimability_norm <- function(x) {
  largest <- max(abs(x), 0)
  if (largest == 0) 0 else largest * sqrt(sum((x / largest)^2))
}

.estimability_baseline <- function(x, X, baseline, is_model) {
  if (is.null(baseline)) baseline <- is_model
  constants <- which(apply(X, 2L, function(z) all(z == z[1L]) && z[1L] != 0))
  source <- "supplied design"
  added <- character()
  if (is.logical(baseline)) {
    if (length(baseline) != 1L || is.na(baseline)) stop("baseline must be TRUE or FALSE.", call. = FALSE)
    B <- NULL
    if (baseline && is_model) {
      ids <- fmrihrf::blockids(x$sampling_frame)
      if (length(ids) != nrow(X)) stop("Sampling frame does not match design rows.", call. = FALSE)
      B <- outer(ids, seq_along(fmrihrf::blocklens(x$sampling_frame)), "==") * 1
      colnames(B) <- paste0("intercept_run", seq_len(ncol(B)))
      source <- "run intercepts"
    } else if (baseline && !length(constants)) {
      B <- matrix(1, nrow(X), 1L, dimnames = list(NULL, "intercept"))
      source <- "global intercept"
    }
    # Reuse an existing proportional intercept column; never drop user columns.
    if (!is.null(B)) {
      keep <- vapply(seq_len(ncol(B)), function(j) {
        b <- B[, j]
        !any(vapply(seq_len(ncol(X)), function(k) {
          z <- X[, k]
          any(b != 0) && z[which(b != 0)[1L]] != 0 &&
            all(z == b * z[which(b != 0)[1L]])
        }, logical(1)))
      }, logical(1))
      reused <- which(vapply(seq_len(ncol(X)), function(k) any(vapply(seq_len(ncol(B)), function(j) {
        b <- B[, j]
        z <- X[, k]
        z[which(b != 0)[1L]] != 0 && all(z == b * z[which(b != 0)[1L]])
      }, logical(1))), logical(1)))
      constants <- union(constants, reused)
      B <- B[, keep, drop = FALSE]
      # Generated names must not collide with supplied column identities.
      colnames(B) <- utils::tail(make.unique(c(colnames(X), colnames(B))), ncol(B))
    }
  } else {
    if (inherits(baseline, "baseline_model")) {
      if (is_model && !identical(x$sampling_frame, baseline$sampling_frame)) {
        stop("baseline_model and event_model must use the same sampling frame.", call. = FALSE)
      }
      B <- .estimability_matrix(design_matrix(baseline), "baseline")
      source <- "baseline_model"
    } else {
      B <- .estimability_matrix(baseline, "baseline")
      source <- "baseline matrix"
    }
    if (nrow(B) != nrow(X)) stop("baseline must have the same number of rows as x.", call. = FALSE)
    if (is.null(colnames(B))) colnames(B) <- paste0("baseline", seq_len(ncol(B)))
  }
  if (!is.null(B) && ncol(B)) {
    added <- colnames(B)
    .estimability_names(c(colnames(X), added), "Combined design column names")
    X <- cbind(X, B)
  }
  list(design = X, source = source, columns = c(colnames(X)[constants], added))
}

.estimability_weights <- function(contrasts, columns, input_columns) {
  out <- matrix(numeric(), length(columns), 0L)
  if (is.null(contrasts) || (is.list(contrasts) && !length(contrasts))) return(out)
  if (!is.list(contrasts)) contrasts <- list(contrast = contrasts)
  labels <- names(contrasts)
  if (is.null(labels)) labels <- rep("", length(contrasts))
  if (anyNA(labels)) stop("Contrast names must not be NA.", call. = FALSE)
  labels[!nzchar(labels)] <- paste0("contrast", which(!nzchar(labels)))
  .estimability_names(labels, "Contrast names")
  for (i in seq_along(contrasts)) {
    W <- contrasts[[i]]
    if (is.numeric(W) && is.null(dim(W))) W <- matrix(W, ncol = 1L, dimnames = list(names(W), NULL))
    W <- .estimability_matrix(W, "Contrast weights")
    rn <- rownames(W)
    aligned <- matrix(0, length(columns), ncol(W))
    if (!is.null(rn)) {
      .estimability_names(rn, "Contrast row names")
      where <- match(rn, columns)
      if (anyNA(where)) stop("Unknown contrast column names: ", paste(rn[is.na(where)], collapse = ", "), call. = FALSE)
    } else if (nrow(W) %in% c(input_columns, length(columns))) {
      where <- seq_len(nrow(W))
    } else {
      stop("Unnamed contrast weights must span the input or full design columns.", call. = FALSE)
    }
    aligned[where, ] <- W
    suffix <- colnames(W)
    if (is.null(suffix)) suffix <- as.character(seq_len(ncol(W)))
    .estimability_names(suffix, "Contrast column names")
    colnames(aligned) <- if (ncol(W) == 1L) labels[i] else paste0(labels[i], "#", suffix)
    out <- cbind(out, aligned)
  }
  .estimability_names(colnames(out), "Expanded contrast names")
  out
}

.estimability_coverage <- function(x, threshold) {
  events <- lapply(terms(x), function(term) {
    ev <- if (inherits(term, "event_term")) term else term$evterm
    if (!inherits(ev, "event_term")) return(NULL)
    data.frame(onset = ev$onsets, duration = ev$durations, run = ev$blockids)
  })
  events <- Filter(Negate(is.null), events)
  if (!length(events)) return(NULL)
  events <- do.call(rbind, events)
  durations <- fmrihrf::blocklens(x$sampling_frame) * x$sampling_frame$TR
  if (any(!is.finite(as.matrix(events))) || any(events$duration < 0) ||
      any(!events$run %in% seq_along(durations))) {
    stop("Event coverage requires finite, nonnegative durations and valid run IDs.", call. = FALSE)
  }
  covered <- vapply(seq_along(durations), function(run) {
    ev <- events[events$run == run, , drop = FALSE]
    start <- pmax(0, ev$onset)
    end <- pmin(durations[run], ev$onset + ev$duration)
    keep <- end > start
    start <- start[keep]
    end <- end[keep]
    if (!length(start)) return(0)
    order_idx <- order(start)
    start <- start[order_idx]
    end <- end[order_idx]
    previous_end <- c(0, head(cummax(end), -1L))
    sum(pmax(0, end - pmax(start, previous_end)))
  }, numeric(1))
  data.frame(run = seq_along(durations), duration = durations, covered = covered,
             fraction = covered / durations, high_coverage = covered / durations > threshold)
}

#' @rdname check_estimability
#' @param ... Unused.
#' @export
print.estimability_check <- function(x, ...) {
  cat(sprintf("Design estimability: rank %d/%d; condition number %.4g\n",
              x$rank, x$n_columns, x$condition_number))
  cat("Baseline:", x$baseline$source, "\n")
  for (msg in x$messages) cat("-", msg, "\n")
  if (nrow(x$contrasts)) print(x$contrasts, row.names = FALSE)
  if (!is.null(x$coverage)) print(x$coverage, row.names = FALSE)
  invisible(x)
}
