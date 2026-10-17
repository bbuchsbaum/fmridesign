# fmrihrf is imported wholesale (explicit importFrom() lines would be
# redundant); package code calls it as fmrihrf::name().
#' @import fmrihrf
NULL

# Re-export commonly used functions for backward compatibility. Generics shared
# with fmrihrf are re-exported in R/fmrihrf-reexports.R.
#' @export
fmrihrf::HRF

#' @export
fmrihrf::as_hrf

#' @export
fmrihrf::gen_hrf

#' @export
fmrihrf::sampling_frame

#' @export
fmrihrf::hrf_spmg1

#' @export
fmrihrf::regressor

#' @export
fmrihrf::feature_regressor

#' @export
fmrihrf::blocklens

#' @export
fmrihrf::samples

#' @export
fmrihrf::global_onsets

#' @export
fmrihrf::evaluate
