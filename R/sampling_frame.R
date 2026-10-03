#' Print and Plot for sampling_frame
#'
#' Custom print and plot methods for `sampling_frame` objects (from fmrihrf).
#' The print method provides a concise summary focused on runs/blocks, TR and
#' total scans without mentioning lower-level evaluation precision. The plot
#' method visualizes runs over time.
#'
#' @param x A `sampling_frame` object created by `fmrihrf::sampling_frame()`.
#' @param style Plot style for `plot()`. One of `"timeline"` (default),
#'   `"grid"` or `"lane"`. `"timeline"` draws one bar per run on a shared
#'   time axis; `"grid"` shows the scans of each run as cells on a
#'   within-run scan axis; `"lane"` is a compact single row of adjacent run
#'   segments, annotated above each segment.
#' @param show_ticks Logical; for `plot()`, whether to show per-TR tick marks
#'   along each run (timeline style only). Default `FALSE`.
#' @param tick_every Integer; draw a tick every `tick_every` TRs when
#'   `show_ticks = TRUE` (timeline), or alternate cell shading every
#'   `tick_every` scans (grid; defaults to 10 for runs over 100 scans).
#'   Default `5`.
#' @param ... Unused.
#'
#' @return For `print()`, returns `x` invisibly. For `plot()`, a ggplot object.
#'
#' @examples
#' sf <- fmrihrf::sampling_frame(blocklens = c(60, 120), TR = 2)
#' print(sf)
#' plot(sf)
#'
#' @importFrom stats median
#' @export
print.sampling_frame <- function(x, ...) {
  # Extract block lengths (scans per run)
  bl <- fmrihrf::blocklens(x)
  n_blocks <- length(bl)
  n_scans <- sum(bl)

  # Estimate TR and total duration from sample times if available
  t_global <- tryCatch(fmrihrf::samples(x, global = TRUE), error = function(e) NULL)
  TR_est <- if (!is.null(t_global) && length(t_global) > 1) stats::median(diff(t_global)) else NA_real_
  dur_sec <- if (!is.null(t_global) && length(t_global) > 0) max(t_global) else NA_real_

  cat("Sampling frame\n")
  cat("- Blocks:", n_blocks, "\n")
  cat("- Scans:", n_scans, "(per block:", paste(bl, collapse = ", "), ")\n", sep = " ")
  if (is.finite(TR_est)) cat("- TR:", format(TR_est), "s\n")
  if (is.finite(dur_sec)) cat("- Duration:", format(dur_sec), "s\n")
  invisible(x)
}



