#' Validate contrast weights against a design matrix or event model
#'
#' Provides basic diagnostics for t- and F-contrasts once the design matrix
#' is available. You can either pass an `event_model` (to validate all attached
#' contrasts) or a design matrix plus custom weights.
#'
#' Checks include:
#' - Estimability: whether each contrast column lies in the row space of `X`.
#' - Sum-to-zero: whether the weights sum to ~0 (t-contrasts only).
#' - Intercept orthogonality: whether weights on intercept-like columns are ~0.
#' - Full-rank (F only): whether an F-contrast matrix has full column rank.
#'
#' @param x An `event_model` or a numeric matrix/data.frame design matrix.
#' @param weights Optional contrast weights. May be a numeric vector (t-contrast),
#'   a numeric matrix (F-contrast with columns as contrast vectors), or a named
#'   list mapping names to vectors/matrices. If `NULL` and `x` is an `event_model`,
#'   all attached t- and F-contrasts are validated.
#' @param tol Numeric tolerance for zero checks. Default `1e-8`.
#'
#' @return A data.frame with one row per validated contrast column and the
#'   following columns: `name`, `type` ("t" or "F"), `estimable`,
#'   `sum_to_zero`, `orthogonal_to_intercept`, `full_rank` (F only), and
#'   `nonzero_weights`.
#'
#' @examples
#' # Create a simple event model
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30),
#'   run = 1,
#'   cond = factor(c("A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#' emodel <- event_model(onset ~ hrf(cond), data = des, block = ~run,
#'                       sampling_frame = sframe)
#'
#' # Validate all attached contrasts on a model
#' res <- validate_contrasts(emodel)
#'
#' # Validate a custom vector against a model
#' v <- rep(0, ncol(design_matrix(emodel)))
#' v[1] <- 1
#' v[2] <- -1
#' res2 <- validate_contrasts(emodel, weights = v)
#'
#' # Validate a custom matrix against a design matrix
#' X <- as.matrix(design_matrix(emodel))
#' C <- cbind(c(1, -1, rep(0, ncol(X) - 2)), c(0, 1, -1, rep(0, ncol(X) - 3)))
#' res3 <- validate_contrasts(X, weights = C)
#' @export
validate_contrasts <- function(x, weights = NULL, tol = 1e-8) {
  # Helper: normalize X
  as_X <- function(obj) {
    if (inherits(obj, "event_model")) {
      as.matrix(design_matrix(obj))
    } else if (is.matrix(obj) || is.data.frame(obj)) {
      as.matrix(obj)
    } else {
      stop("validate_contrasts: 'x' must be an event_model or a design matrix.")
    }
  }

  # Helper: intercept-like column indices
  intercept_idx <- function(X) {
    cn <- colnames(X) %||% character(0)
    which(cn %in% c("(Intercept)", "Intercept", "constant", "const"))
  }

  # Helper: estimability of a single vector cvec
  is_estimable_vec <- function(X, cvec, tol) {
    if (length(cvec) != ncol(X)) return(FALSE)
    # Check if cvec lies in row-space(X) by testing rank does not increase
    # when appending it as an extra row.
    rX  <- qr(X)$rank
    rXa <- qr(rbind(X, cvec))$rank
    abs(rXa - rX) < .Machine$double.eps^0.5 || rXa == rX
  }

  # Helper: build a named list of contrast matrices to validate
  gather_from_model <- function(model) {
    out <- list()
    # t-contrasts via contrast_weights(model)
    cw <- try(contrast_weights(model), silent = TRUE)
    if (!inherits(cw, "try-error") && length(cw) > 0) {
      for (nm in names(cw)) {
        obj <- cw[[nm]]
        W <- obj$offset_weights %||% obj$weights
        if (is.null(W)) next
        # Ensure matrix shape (#cols of X x k)
        W <- as.matrix(W)
        out[[nm]] <- W
      }
    }
    # F-contrasts via Fcontrasts(model)
    FC <- try(Fcontrasts(model), silent = TRUE)
    if (!inherits(FC, "try-error") && length(FC) > 0) {
      for (nm in names(FC)) {
        M <- FC[[nm]]
        if (is.null(M)) next
        out[[nm]] <- as.matrix(M)
      }
    }
    out
  }

  # Normalize inputs
  X <- as_X(x)
  ic <- intercept_idx(X)

  # Build a list of weights to validate
  wlist <- list()
  if (is.null(weights)) {
    if (inherits(x, "event_model")) {
      wlist <- gather_from_model(x)
    } else {
      stop("validate_contrasts: when 'x' is a matrix, 'weights' must be provided.")
    }
  } else if (is.list(weights)) {
    wlist <- weights
  } else if (is.numeric(weights)) {
    # vector or matrix
    if (is.null(dim(weights))) {
      wlist[["contrast"]] <- matrix(as.numeric(weights), ncol = 1)
    } else {
      wlist[["contrast"]] <- as.matrix(weights)
    }
  } else {
    stop("validate_contrasts: 'weights' must be a numeric vector, matrix, or list of such.")
  }

  # Iterate and validate
  res <- do.call(rbind, lapply(names(wlist), function(nm) {
    W <- wlist[[nm]]
    if (nrow(W) != ncol(X)) {
      # Try to align by rownames if available
      if (!is.null(rownames(W)) && !is.null(colnames(X))) {
        m <- match(rownames(W), colnames(X))
        keep <- !is.na(m)
        if (any(keep)) {
          W2 <- matrix(0, nrow = ncol(X), ncol = ncol(W))
          W2[m[keep], , drop = FALSE] <- W[keep, , drop = FALSE]
          rownames(W2) <- colnames(X)
          colnames(W2) <- colnames(W)
          W <- W2
        } else {
          warning(sprintf("validate_contrasts: Dimension mismatch for '%s' and no matching rownames to map onto design columns. Skipping.", nm), call. = FALSE)
          return(NULL)
        }
      } else {
        warning(sprintf("validate_contrasts: Dimension mismatch for '%s' (nrow(weights)=%d, ncol(X)=%d). Skipping.", nm, nrow(W), ncol(X)), call. = FALSE)
        return(NULL)
      }
    }

    # Decide type: if multiple columns and appears like F-contrast, call it "F"
    type <- if (ncol(W) > 1) "F" else "t"

    # Prepare per-column metrics
    col_metrics <- lapply(seq_len(ncol(W)), function(j) {
      cvec <- as.numeric(W[, j])
      list(
        estimable = is_estimable_vec(X, cvec, tol),
        sum_to_zero = abs(sum(cvec)) < tol,
        orth_int = if (length(ic) > 0) all(abs(cvec[ic]) < tol) else TRUE,
        nonzero = sum(abs(cvec) > tol)
      )
    })

    # Aggregate for F: full-rank and all-cols estimable
    full_rank <- if (type == "F") {
      qr(W)$rank == ncol(W)
    } else NA

    data.frame(
      name = if (ncol(W) == 1) nm else paste0(nm, "#", seq_len(ncol(W))),
      type = type,
      estimable = vapply(col_metrics, `[[`, logical(1), "estimable"),
      sum_to_zero = vapply(col_metrics, `[[`, logical(1), "sum_to_zero"),
      orthogonal_to_intercept = vapply(col_metrics, `[[`, logical(1), "orth_int"),
      full_rank = rep(full_rank, length(col_metrics)),
      nonzero_weights = vapply(col_metrics, `[[`, numeric(1), "nonzero"),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  }))

  res[order(res$name), , drop = FALSE]
}


#' Check design matrix for multicollinearity
#'
#' Convenience helper to quickly flag highly correlated regressors.
#'
#' @param X A numeric design matrix (or an `event_model`).
#' @param threshold Absolute correlation above which a pair is flagged. Default 0.9.
#'
#' @return A list with elements: `ok` (logical), `pairs` (data.frame with
#'   offending pairs and their correlations). Invisibly returns the same list.
#'
#' @examples
#' # Create a simple event model
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30),
#'   run = 1,
#'   cond = factor(c("A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#' emodel <- event_model(onset ~ hrf(cond), data = des, block = ~run,
#'                       sampling_frame = sframe)
#'
#' # Check for multicollinearity
#' res <- check_collinearity(design_matrix(emodel), threshold = 0.95)
#' if (!res$ok) print(res$pairs)
#' @export
check_collinearity <- function(X, threshold = 0.9) {
  if (inherits(X, "event_model")) X <- as.matrix(design_matrix(X))
  if (!(is.matrix(X) || is.data.frame(X))) stop("check_collinearity: 'X' must be a matrix, data.frame or event_model.")
  X <- as.matrix(X)

  # Drop constant columns (including intercept-like)
  cn <- colnames(X) %||% character(0)
  ic <- which(cn %in% c("(Intercept)", "Intercept", "constant", "const"))
  keep <- setdiff(seq_len(ncol(X)), ic)
  if (length(keep) == 0) return(invisible(list(ok = TRUE, pairs = data.frame())))
  Xk <- X[, keep, drop = FALSE]

  # Drop zero-variance columns
  v <- apply(Xk, 2, function(z) var(z, na.rm = TRUE))
  Xk <- Xk[, which(is.finite(v) & v > 0), drop = FALSE]
  if (ncol(Xk) < 2) return(invisible(list(ok = TRUE, pairs = data.frame())))

  C <- suppressWarnings(cor(Xk))
  diag(C) <- 0
  idx <- which(abs(C) > threshold, arr.ind = TRUE)
  idx <- idx[idx[, 1] < idx[, 2], , drop = FALSE]

  if (nrow(idx) == 0) {
    return(invisible(list(ok = TRUE, pairs = data.frame())))
  } else {
    pairs <- data.frame(
      regressor_1 = colnames(Xk)[idx[, 1]],
      regressor_2 = colnames(Xk)[idx[, 2]],
      r = mapply(function(i, j) C[i, j], idx[, 1], idx[, 2]),
      stringsAsFactors = FALSE
    )
    return(invisible(list(ok = FALSE, pairs = pairs)))
  }
}

