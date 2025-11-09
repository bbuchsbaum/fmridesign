#' Print and Plot for sampling_frame
#'
#' Custom print and plot methods for `sampling_frame` objects (from fmrihrf).
#' The print method provides a concise summary focused on runs/blocks, TR and
#' total scans without mentioning lower-level evaluation precision. The plot
#' method visualizes runs over time.
#'
#' @param x A `sampling_frame` object created by `fmrihrf::sampling_frame()`.
#' @param style Plot style for `plot()`. One of `"timeline"` (default) or
#'   `"grid"`. `"timeline"` draws a horizontal bar per run; `"grid"` shows a
#'   scan grid by run.
#' @param show_ticks Logical; for `plot()`, whether to show per-TR tick marks
#'   along each run (timeline style only). Default `FALSE`.
#' @param tick_every Integer; draw a tick every `tick_every` TRs when
#'   `show_ticks = TRUE`. Default `5`.
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

#' @rdname print.sampling_frame
#' @importFrom ggplot2 ggplot aes geom_segment geom_tile geom_text scale_y_reverse
#' @importFrom ggplot2 scale_x_continuous theme_minimal labs theme element_blank
#' @method plot sampling_frame
#' @export
plot.sampling_frame <- function(x, style = c("timeline", "grid"), show_ticks = FALSE, tick_every = 5, ...) {
  style <- match.arg(style)
  bl <- fmrihrf::blocklens(x)
  n_blocks <- length(bl)

  t_global <- tryCatch(fmrihrf::samples(x, global = TRUE), error = function(e) NULL)
  TR_est <- if (!is.null(t_global) && length(t_global) > 1) stats::median(diff(t_global)) else 1

  # Build run timing data
  run_df <- data.frame(
    run = seq_len(n_blocks),
    scans = bl,
    start = c(0, cumsum(head(bl, -1)) * TR_est),
    end = cumsum(bl) * TR_est
  )

  if (style == "timeline") {
    p <- ggplot2::ggplot(run_df, ggplot2::aes(y = factor(run))) +
      ggplot2::geom_segment(ggplot2::aes(x = start, xend = end, yend = factor(run)), linewidth = 6) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::labs(x = "Time (s)", y = "Run")

    if (isTRUE(show_ticks)) {
      tick_df <- do.call(rbind, lapply(seq_len(n_blocks), function(r) {
        n <- bl[r]
        t0 <- run_df$start[r]
        tks <- seq(0, n, by = tick_every)
        data.frame(run = r, x = t0 + tks * TR_est)
      }))
      p <- p + ggplot2::geom_segment(data = tick_df,
                                     ggplot2::aes(x = x, xend = x, y = as.numeric(factor(run)) - 0.25, yend = as.numeric(factor(run)) + 0.25),
                                     inherit.aes = FALSE,
                                     linewidth = 0.3,
                                     alpha = 0.6)
    }

    p <- p + ggplot2::scale_y_reverse(breaks = seq_len(n_blocks), labels = seq_len(n_blocks))
    return(p)
  }

  # grid style: scans by run as tiles
  grid_df <- do.call(rbind, lapply(seq_len(n_blocks), function(r) {
    n <- bl[r]
    data.frame(run = r, scan = seq_len(n))
  }))

  p <- ggplot2::ggplot(grid_df, ggplot2::aes(x = scan, y = factor(run))) +
    ggplot2::geom_tile(fill = "grey60", color = "white", width = 0.95, height = 0.8) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(x = "Scan index (within run)", y = "Run") +
    ggplot2::theme(panel.grid = ggplot2::element_blank())

  p <- p + ggplot2::scale_y_reverse(breaks = seq_len(n_blocks), labels = seq_len(n_blocks))
  p
}

