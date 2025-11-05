#' Residualize methods
#'
#' See `residualize()` generic in `design_generics.R`.
#'
#' @name residualize-methods
#' @keywords internal
NULL

# Internal workhorse: residualize with a numeric design matrix
.residualize_with_matrix <- function(X, Y) {
  if (!is.matrix(X)) X <- as.matrix(X)
  if (inherits(Y, "data.frame")) Y <- as.matrix(Y)
  if (is.vector(Y)) Y <- matrix(Y, ncol = 1)
  stopifnot(is.numeric(X), is.numeric(Y))
  if (nrow(Y) != nrow(X)) {
    stop(sprintf("Row mismatch: nrow(data)=%d, nrow(design)=%d", nrow(Y), nrow(X)), call. = FALSE)
  }
  QR <- qr(X)
  qr.resid(QR, Y)
}

#' @export
residualize.matrix <- function(x, data, cols = NULL, ...) {
  X <- x
  if (!is.null(cols)) {
    if (is.character(cols)) {
      X <- X[, cols, drop = FALSE]
    } else {
      X <- X[, cols, drop = FALSE]
    }
  }
  .residualize_with_matrix(X, data)
}

#' @export
residualize.event_model <- function(x, data, cols = NULL, ...) {
  dm <- design_matrix(x)
  X <- as.matrix(dm)
  if (!is.null(cols)) {
    if (is.character(cols)) {
      X <- X[, cols, drop = FALSE]
    } else {
      X <- X[, cols, drop = FALSE]
    }
  }
  .residualize_with_matrix(X, data)
}

#' @export
residualize.baseline_model <- function(x, data, cols = NULL, ...) {
  dm <- design_matrix(x)
  X <- as.matrix(dm)
  if (!is.null(cols)) {
    if (is.character(cols)) {
      X <- X[, cols, drop = FALSE]
    } else {
      X <- X[, cols, drop = FALSE]
    }
  }
  .residualize_with_matrix(X, data)
}

