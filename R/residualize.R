#' Residualize methods
#'
#' S3 methods for the `residualize()` generic. These methods project data
#' onto the orthogonal complement of a design, returning OLS residuals.
#'
#' @param x A design object: matrix, event_model, or baseline_model.
#' @param data Numeric vector/matrix/data.frame of observations Y.
#' @param cols Optional integer or character vector selecting columns to project out.
#' @param ... Additional arguments (currently unused).
#'
#' @return A numeric matrix of residuals with the same dimensions as `data`.
#'
#' @examples
#' # Residualize with a raw matrix
#' X <- cbind(1, 1:10)
#' Y <- matrix(rnorm(20), ncol = 2)
#' R <- residualize(X, Y)
#' dim(R)  # 10 x 2
#'
#' # Residualize with an event model
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30),
#'   run = 1,
#'   cond = factor(c("A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#' emod <- event_model(onset ~ hrf(cond), data = des, block = ~run,
#'                     sampling_frame = sframe)
#' Y_sim <- matrix(rnorm(40 * 2), ncol = 2)
#' R_emod <- residualize(emod, Y_sim)
#' dim(R_emod)  # 40 x 2
#'
#' @name residualize-methods
#' @rdname residualize-methods
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
#' @rdname residualize-methods
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
#' @rdname residualize-methods
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
#' @rdname residualize-methods
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

