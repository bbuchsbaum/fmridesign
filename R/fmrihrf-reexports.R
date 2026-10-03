#' Accessor generics shared with fmrihrf
#'
#' @description
#' `onsets()`, `durations()`, `blockids()` and `nbasis()` are fmrihrf's
#' generics, re-exported by fmridesign. fmridesign registers methods on them
#' for its own classes, so `fmrihrf::onsets(term)` and `fmridesign::onsets(term)`
#' are the same function and dispatch to the same methods, whichever package
#' is attached first.
#'
#' fmridesign provides methods for:
#' \itemize{
#'   \item `onsets()`, `durations()`: `event_term`, `convolved_term`
#'   \item `blockids()`: `event_term`, `convolved_term`, `event_model`
#'   \item `nbasis()`: `hrfspec`, `convolved_term` subclasses, `feature_term`
#'     and the parametric basis classes (`Poly`, `BSpline`, `Scale`, ...)
#' }
#' Methods for fmrihrf's own classes (`Reg`, `HRF`, `sampling_frame`) are
#' documented in fmrihrf.
#'
#' @param x An object.
#' @param ... Passed to methods.
#' @return See [fmrihrf::onsets()], [fmrihrf::durations()],
#'   [fmrihrf::blockids()] and [fmrihrf::nbasis()].
#'
#' @name fmrihrf-generics
#' @importFrom fmrihrf onsets durations blockids nbasis
#' @examples
#' term <- event_term(
#'   list(condition = factor(c("A", "B", "A"))),
#'   onsets = c(0, 10, 20),
#'   blockids = c(1, 1, 1),
#'   durations = c(2, 2, 2)
#' )
#' onsets(term)
#' durations(term)
#' blockids(term)
#' identical(onsets, fmrihrf::onsets)
NULL

#' @export
fmrihrf::onsets

#' @export
fmrihrf::durations

#' @export
fmrihrf::blockids

#' @export
fmrihrf::nbasis
