#' Functions re-exported from fmrihrf
#' 
#' @description
#' These functions are re-exported from the fmrihrf package.
#' Note: When both packages are loaded, R will show masking warnings.
#' This is expected and harmless - the functions work identically.
#' 
#' @details
#' The following generics are re-exported:
#' \itemize{
#'   \item \code{\link[fmrihrf]{onsets}}: Extract onset times from event objects
#'   \item \code{\link[fmrihrf]{durations}}: Extract durations from event objects  
#'   \item \code{\link[fmrihrf]{blockids}}: Extract block identifiers
#' }
#' 
#' fmridesign adds S3 methods for these generics to work with:
#' \itemize{
#'   \item event_term objects
#'   \item convolved_term objects
#'   \item event_model objects
#' }
#'
#' @name fmrihrf-reexports
#' @keywords internal
#' @importFrom fmrihrf onsets durations blockids
NULL

# Note: We define these as identical generics rather than direct re-exports
# This ensures S3 dispatch works for methods from both packages

#' @export
#' @rdname fmrihrf-reexports
onsets <- function(x, ...) UseMethod("onsets")

#' @export
#' @rdname fmrihrf-reexports
durations <- function(x, ...) UseMethod("durations")

#' @export
#' @rdname fmrihrf-reexports
blockids <- function(x, ...) UseMethod("blockids")

