.basis_arg_label <- function(expr) {
  deparse1(expr)
}

# Guard a scale estimate (sd, MAD) against zero or NA.
.safe_scale <- function(s) {
  if (is.na(s) || s == 0) 1e-6 else s
}

# (x - center) / scale as a named one-column matrix; NAs become 0. Shared by
# the scaling bases and their predict() methods.
.zscore_column <- function(x, center, scale, name) {
  z <- (x - center) / scale
  z[is.na(z)] <- 0
  matrix(z, ncol = 1, dimnames = list(NULL, name))
}


#' sub_basis
#'
#' Subset a parametric basis regressor.
#'
#' @param x the object
#' @param subset the subset (logical or integer indices)
#' @return An object of the same class as `x` with subset applied.
#' @examples
#' # Create some sample data
#' x_vals <- 1:10
#' rt_vals <- rnorm(10, 500, 50)
#'
#' # Create different basis objects
#' poly_basis <- Poly(x_vals, degree = 3)
#' scale_basis <- Scale(rt_vals)
#' bspline_basis <- BSpline(x_vals, degree = 2)
#'
#' # Subset with integer indices
#' poly_sub <- sub_basis(poly_basis, 1:5)
#' scale_sub <- sub_basis(scale_basis, c(1, 3, 5, 7, 9))
#'
#' # Subset with logical indices
#' logical_idx <- x_vals <= 5
#' bspline_sub <- sub_basis(bspline_basis, logical_idx)
#'
#' # Check dimensions
#' nrow(poly_basis$y)  # 10
#' nrow(poly_sub$y)    # 5
#' nrow(scale_sub$y)   # 5
#' nrow(bspline_sub$y) # 5
#' @export
sub_basis <-  function(x, subset) UseMethod("sub_basis")


#' Predict from a ParametricBasis object
#'
#' Dispatch to the appropriate method for transforming new data
#' according to a specific parametric basis.
#'
#' @param object ParametricBasis object.
#' @param newdata Numeric vector to transform.
#' @param ... Additional arguments.
#' @param newgroup Optional factor for group-dependent bases.
#' @return A numeric matrix with transformed values (one column per basis component).
#' @examples
#' # Create polynomial basis from training data
#' train_x <- 1:10
#' poly_basis <- Poly(train_x, degree = 2)
#'
#' # Predict on new data
#' new_x <- c(5.5, 7.3, 11.2)
#' predict(poly_basis, new_x)
#'
#' # Create scaling basis
#' train_vals <- c(10, 20, 30, 40, 50)
#' scale_basis <- Scale(train_vals)
#'
#' # Apply same scaling to new data
#' new_vals <- c(15, 25, 35)
#' predict(scale_basis, new_vals)
#' @method predict ParametricBasis
#' @rdname predict
#' @export
predict.ParametricBasis <- function(object, newdata, ...) {
  UseMethod("predict", object)
}


#' Ident
#'
#' A basis that applies identity transform to a set of raw variables.
#'
#' @param ... a list of variable names
#' @return an instance of class \code{Ident} extending \code{ParametricBasis}
#' @examples
#' # Create identity basis from numeric vectors
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(2, 4, 6, 8, 10)
#' ident_basis <- Ident(x, y)
#' print(ident_basis$y)
#' @export 
Ident <- function(...) {
  vlist <- list(...)

  varnames <- as.list(substitute(list(...)))[-1]
  varnames <- unlist(lapply(varnames, as.character))
  names(vlist) <- varnames
  y <- do.call(cbind, vlist)
  
  ret <- list(vlist=vlist, y=y, varnames=varnames, name=paste(varnames, collapse="_"))
  class(ret) <- c("Ident", "ParametricBasis")
  ret
}


#' Polynomial basis
#'
#' Orthogonal polynomial expansion of a linear term based on \code{\link[stats]{poly}}
#'
#' @param x a numeric vector at which to evaluate the polynomial. Missing values are not allowed in x.
#' @param degree the degree of the polynomial. Must be less than the number of unique points.
#' @return an instance of class \code{Poly} extending \code{ParametricBasis}
#' @examples
#' # Create a 3rd degree polynomial basis
#' x_vals <- c(1, 2, 3, 4, 5, 6)
#' poly_basis <- Poly(x_vals, degree = 3)
#' print(poly_basis$y)
#' @seealso \link[stats]{poly}
#' @export 
Poly <- function(x, degree) {
  mc <- match.call()
  argname <- .basis_arg_label(mc[["x"]])
  pres <- poly(x,degree)
  n <- paste0("poly_", argname)
  ret <- list(x=x,y=pres,fun="poly",argname=argname, 
              name=n,
              degree=degree)
  class(ret) <- c("Poly", "ParametricBasis")
  ret
}

#' Standardized basis
#'
#' Standardize a numeric vector by centering and scaling, handling NAs appropriately.
#' If the computed standard deviation is \code{NA} or zero, a small constant
#' (\code{1e-6}) is used instead to avoid division by zero.
#' The returned basis matrix has one column with this standardized name.
#'
#' @param x a numeric vector to standardize. Missing values are allowed and will be replaced with 0 after standardization.
#' @return an instance of class \code{Standardized} extending \code{ParametricBasis}
#' @examples
#' # Standardize a numeric vector
#' x_vals <- c(10, 20, 30, 40, 50)
#' std_basis <- Standardized(x_vals)
#' print(std_basis$y)
#' print(std_basis$mean)
#' print(std_basis$sd)
#' @export 
Standardized <- function(x) {
  mc <- match.call()
  argname <- .basis_arg_label(mc[["x"]])
  
  # Remove NAs for computing mean and sd
  x_clean <- x[!is.na(x)]
  x_mean <- mean(x_clean)
  x_sd <- .safe_scale(stats::sd(x_clean))
  
  n <- paste0("std", "_", argname)
  ret <- list(x=x, y=.zscore_column(x, x_mean, x_sd, n),
              mean=x_mean, sd=x_sd,
              fun="standardized", 
              argname=argname, 
              name=n)
  class(ret) <- c("Standardized", "ParametricBasis")
  ret
}

#' @export
#' @rdname predict
predict.Standardized <- function(object, newdata, ...) {
  .zscore_column(newdata, object$mean, .safe_scale(object$sd), object$name)
}

#' @export
sub_basis.Standardized <- function(x, subset) {
  ret <- list(x=x$x[subset],
              y=x$y[subset, , drop = FALSE],
              fun=x$fun,
              argname=x$argname,
              name=x$name,
              mean=x$mean,
              sd=x$sd)
  class(ret) <- c("Standardized", "ParametricBasis")
  ret
}

#' @export
levels.Standardized <- function(x) {
  x$argname 
}

#' @export
columns.Standardized <- function(x, ...) {
  continuous_token(x$name)
}


#' B-spline basis
#' 
#' Generate the B-spline basis matrix for a polynomial spline.
#' 
#' @param x a numeric vector at which to evaluate the spline. Missing values are not allowed in x
#' @param degree the degree of the piecewise polynomial
#' @importFrom splines bs
#' @seealso \link[splines]{bs}
#' @export
#' @return an \code{BSpline} list instance
#' @examples
#' x_vals <- seq(0, 1, length.out = 6)
#' bs_obj <- BSpline(x_vals, degree = 3)
#' dim(bs_obj$y)
#' bs_obj$name
BSpline <- function(x, degree) {
  mc <- match.call()
  argname <- .basis_arg_label(mc[["x"]])
  pres <- bs(x, degree = degree)
  n <- paste0("bs_", argname)
  ret <- list(x=x, y=pres, fun="bs", argname=argname, 
              name=n,
              degree=degree)
  class(ret) <- c("BSpline", "ParametricBasis")
  
  ret
}


#' @method predict Poly
#' @export
#' @rdname predict
predict.Poly <- function(object,newdata,...) {
  predict(object$y, newdata)
}

#' @export
sub_basis.Poly <- function(x, subset) {
  ret <- list(x=x$x[subset],
       y=x$y[subset, , drop = FALSE],
       fun=x$fun,
       argname=x$argname,
       degree=x$degree)
  class(ret) <- c("Poly", "ParametricBasis")
  ret
}

#' @export
sub_basis.BSpline <- function(x, subset) {
  ret <- list(x=x$x[subset],
              y=x$y[subset, , drop=FALSE],
              fun=x$fun,
              argname=x$argname,
              degree=x$degree)
  class(ret) <- c("BSpline", "ParametricBasis")
  ret
}

#' @export
sub_basis.Ident <- function(x, subset) {
  ret <- list(y = x$y[subset, , drop = FALSE], 
              varnames = x$varnames, 
              fun = "ident")
  class(ret) <- c("Ident", "ParametricBasis")
  ret
}

#' @export
#' @rdname predict
predict.BSpline <- function(object, newdata, ...) {
  # Rebuild using bs() and stored attributes
  splines::bs(newdata, degree = object$degree,
     knots = attr(object$y, "knots"),
     Boundary.knots = attr(object$y, "Boundary.knots"))
}

#' @export
#' @rdname predict
predict.Ident <- function(object,newdata,...) {
  if (!is.matrix(newdata) && !is.data.frame(newdata)) {
      stop("newdata for predict.Ident should be matrix or data.frame containing necessary columns")
  }
  return(as.matrix(newdata[, object$varnames, drop=FALSE]))
}

#' @export
levels.Ident <- function(x) {
  x$varnames
}

#' @export
levels.BSpline <- function(x) {
  ncols <- ncol(x$y)
  if (is.null(ncols) || ncols == 0) {
    return(character(0))
  }
  indices <- seq_len(ncols)
  zeropad(indices, ncols) # Pad based on the number of columns
}

#' @export
levels.Poly <- function(x) {
  ncols <- ncol(x$y) 
  if (is.null(ncols) || ncols == 0) {
    return(character(0))
  }
  indices <- seq_len(ncols) # Or seq_len(x$degree)
  zeropad(indices, x$degree) 
}

#' @export
columns.Poly <- function(x, ...) {
  # Use degree for padding width
  indices <- seq_len(x$degree)
  padded_indices <- zeropad(indices, x$degree) # Enforces min width 
  # Return only the padded index as the condition tag component
  continuous_token(padded_indices)
}

#' @export
columns.BSpline <- function(x, ...) columns.Poly(x, ...)

#' @export
columns.Ident <- function(x, ...) {
  continuous_token(x$varnames)
}


#' @export
nbasis.BSpline <- function(x, ...) {
  x$degree
}

#' @export
nbasis.Poly <- function(x,...) {
  x$degree
}

#' Z-score (global) basis
#'
#' @param x numeric vector (NAs allowed)
#' @return object of class c("Scale","ParametricBasis")
#' @examples
#' # Create a z-score transformed basis
#' x_vals <- c(1, 3, 5, 7, 9, 11)
#' scale_basis <- Scale(x_vals)
#' print(scale_basis$y)
#' print(scale_basis$mean)
#' print(scale_basis$sd)
#' @export
Scale <- function(x) {
  mc <- match.call()
  varname <- .basis_arg_label(mc[["x"]])
  mu <- mean(x, na.rm = TRUE)
  sd_ <- .safe_scale(stats::sd(x, na.rm = TRUE))
  
  final_name <- paste0("z_", varname)
  ret <- list(x = x,
              y = .zscore_column(x, mu, sd_, final_name),
              mean = mu,
              sd   = sd_,
              fun  = "scale",
              argname = varname,
              name = final_name)
  class(ret) <- c("Scale","ParametricBasis")
  ret
}

#' @export
#' @rdname predict
predict.Scale <- function(object, newdata, ...) {
  .zscore_column(newdata, object$mean, object$sd, object$name)
}

#' @export
#' @rdname sub_basis
sub_basis.Scale <- function(x, subset) {
  x$x <- x$x[subset]
  x$y <- x$y[subset, , drop = FALSE]
  x
}

#' @method levels Scale
#' @rdname levels.event
#' @export
levels.Scale  <- function(x, ...) x$argname

#' @method columns Scale
#' @rdname columns
#' @export
columns.Scale <- function(x, ...) {
  continuous_token(x$name)
}


#' Z-score within groups
#'
#' @param x numeric vector
#' @param g grouping factor / character / integer of same length as x
#' @return An object of class `ScaleWithin` (a `ParametricBasis`).
#' @examples
#' # Create a within-group z-score transformed basis
#' x_vals <- c(1, 2, 3, 10, 11, 12)
#' groups <- c("A", "A", "A", "B", "B", "B")
#' scale_within_basis <- ScaleWithin(x_vals, groups)
#' print(scale_within_basis$y)
#' print(scale_within_basis$means)
#' print(scale_within_basis$sds)
#' @export
ScaleWithin <- function(x, g) {
  mc <- match.call()
  varname <- .basis_arg_label(mc[["x"]])
  grpname <- .basis_arg_label(mc[["g"]])
  stopifnot(length(x) == length(g))
  g <- as.factor(g)
  
  # pre-compute means/sds per group (ignoring NAs)
  stats <- tapply(seq_along(x), g, function(idx) {
    # Groups with sd = 0 or a single non-NA value get a small sd
    c(mean = mean(x[idx], na.rm = TRUE),
      sd = .safe_scale(stats::sd(x[idx], na.rm = TRUE)))
  })
  mus <- sapply(stats, `[[`, "mean")
  sds <- sapply(stats, `[[`, "sd")
  
  z <- mapply(function(val, grp) {
              grp_name <- as.character(grp) # Ensure factor level name is used
              mu <- mus[grp_name]; sd_ <- sds[grp_name]
              if (is.na(val) || is.na(mu) || is.na(sd_)) 0
              else (val - mu) / sd_
            },
            x, g, SIMPLIFY = TRUE)
  
  final_name <- paste0("z_", varname, "_by_", grpname)
  ret <- list(x = x,
              group = g,
              y = matrix(z, ncol = 1, dimnames=list(NULL, final_name)),
              means = mus,
              sds   = sds,
              fun   = "scale_within",
              argname = varname,
              grpname = grpname,
              name = final_name)
  class(ret) <- c("ScaleWithin","ParametricBasis")
  ret
}

#' @export
#' @rdname predict
predict.ScaleWithin <- function(object, newdata, newgroup, ...) {
  stopifnot(length(newdata) == length(newgroup))
  newgroup <- as.factor(newgroup)
  
  z <- mapply(function(val, grp) {
      grp_name <- as.character(grp) # Ensure factor level name used for indexing
      mu <- object$means[grp_name]; sd_ <- object$sds[grp_name]
      if (is.na(val) || is.na(mu) || is.na(sd_)) 0 # Handle missing group stats in newdata
      else (val - mu)/sd_
    },
    newdata, newgroup, SIMPLIFY = TRUE)
  matrix(z, ncol = 1, dimnames=list(NULL, object$name))
}

#' @export
#' @rdname sub_basis
sub_basis.ScaleWithin <- function(x, subset) {
  ret <- x
  ret$x     <- x$x[subset]
  ret$group <- x$group[subset]
  ret$y     <- x$y[subset,,drop = FALSE]
  class(ret) <- class(x)
  ret
}

#' @export
#' @rdname levels.event
levels.ScaleWithin  <- function(x, ...) x$argname

#' @export
#' @rdname columns
columns.ScaleWithin <- function(x, ...) {
  continuous_token(x$name)
}


#' Robust Scaling (Median/MAD)
#'
#' @param x numeric vector (NAs allowed)
#' @return object of class c("RobustScale","ParametricBasis")
#' @examples
#' # Create a robust scale transformed basis using median and MAD
#' x_vals <- c(1, 2, 3, 4, 100)  # Note the outlier
#' robust_basis <- RobustScale(x_vals)
#' print(robust_basis$y)
#' print(robust_basis$median)
#' print(robust_basis$mad)
#' @export
RobustScale <- function(x) {
  mc <- match.call()
  varname <- .basis_arg_label(mc[["x"]])
  med <- stats::median(x, na.rm = TRUE)
  mad_ <- .safe_scale(stats::mad(x, na.rm = TRUE))  # Default constant=1.4826
  
  final_name <- paste0("robz_", varname)
  ret <- list(x = x,
              y = .zscore_column(x, med, mad_, final_name),
              median = med,
              mad    = mad_,
              fun = "robust_scale",
              argname = varname,
              name = final_name)
  class(ret) <- c("RobustScale","ParametricBasis")
  ret
}

#' @export
#' @rdname predict
predict.RobustScale <- function(object, newdata, ...) {
  .zscore_column(newdata, object$median, object$mad, object$name)
}

#' @export
#' @rdname sub_basis
# Use the same logic as Scale
sub_basis.RobustScale <- sub_basis.Scale

#' @export
#' @rdname levels.event
levels.RobustScale  <- function(x, ...) x$argname

#' @export
#' @rdname columns
columns.RobustScale <- columns.Scale

# Add nbasis methods -----

#' @export
nbasis.Scale        <- function(x,...) 1L

#' @export
nbasis.ScaleWithin  <- function(x,...) 1L

#' @export
nbasis.RobustScale  <- function(x,...) 1L

#' @export
nbasis.Standardized <- function(x,...) 1L

#' @export
nbasis.Ident        <- function(x, ...) ncol(x$y)
