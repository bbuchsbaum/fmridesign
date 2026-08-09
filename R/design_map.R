#' Design Matrix Visualization Methods
#'
#' This file contains methods for visualizing design matrices as heatmaps
#' using ggplot2.

# Internal helper used by baseline and event correlation maps
# Not exported
#' @keywords internal
#' @return A ggplot2 heatmap of the correlation matrix.
#' @noRd
.correlation_map_common <- function(DM,
                                    method = c("pearson", "spearman"),
                                    half_matrix = FALSE,
                                    absolute_limits = TRUE,
                                    ...) {
  method <- match.arg(method)
  cor_mat <- stats::cor(DM, method = method, use = "pairwise.complete.obs")

  if (isTRUE(half_matrix)) {
    cor_mat[upper.tri(cor_mat, diag = FALSE)] <- NA
  }

  df_long <- as.data.frame(as.table(cor_mat))
  names(df_long) <- c("Var1", "Var2", "Correlation")

  limits <- if (absolute_limits) c(-1, 1) else range(df_long$Correlation, na.rm = TRUE)

  plt <- ggplot2::ggplot(df_long, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
    ggplot2::geom_tile(...) +
    ggplot2::scale_fill_gradient2(
      midpoint = 0,
      low = "blue",
      mid = "white",
      high = "red",
      limits = limits
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(x = "", y = "", fill = "Correlation") +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::coord_fixed()
  plt
}

#' Visualize Event Model Design Matrix
#' 
#' Creates a heatmap visualization of the design matrix for an event_model object.
#' 
#' @param x An \code{event_model} object.
#' @param block_separators Logical. Whether to draw separators between blocks/runs. Default is TRUE.
#' @param rotate_x_text Logical. Whether to rotate x-axis labels. Default is TRUE.
#' @param fill_midpoint Numeric. Midpoint for color scale. If NULL, uses gradient scale.
#' @param fill_limits Numeric vector of length 2. Limits for fill scale.
#' @param ... Additional arguments passed to geom_tile.
#' 
#' @return A ggplot2 object showing the design matrix heatmap.
#' 
#' @importFrom ggplot2 ggplot aes geom_tile scale_y_reverse scale_fill_gradientn 
#' @importFrom ggplot2 scale_fill_gradient2 theme_minimal labs theme element_blank element_text
#' @importFrom ggplot2 annotate
#' @importFrom tidyr pivot_longer
#' @importFrom tibble as_tibble
#' @method design_map event_model
#' @export
#' @examples
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30),
#'   run = 1,
#'   cond = factor(c("A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#' emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
#' design_map(emod)
design_map.event_model <- function(x,
                                   block_separators = TRUE,
                                   rotate_x_text = TRUE,
                                   fill_midpoint = NULL,
                                   fill_limits = NULL,
                                   ...) {
  # Extract the design matrix
  DM <- design_matrix(x)
  n_scans <- nrow(DM)
  
  # Convert to long format for ggplot
  df_long <- tibble::as_tibble(DM, .name_repair = "unique")
  df_long$scan_number <- seq_len(n_scans)
  df_long <- tidyr::pivot_longer(
    df_long,
    cols = -scan_number,
    names_to = "Regressor",
    values_to = "Value"
  )
  
  # Create the base plot
  plt <- ggplot(df_long, aes(x = Regressor, y = scan_number, fill = Value)) +
    geom_tile(...)
  
  # Reverse y-axis so scan 1 is at top
  plt <- plt + scale_y_reverse()
  
  # Apply color scale
  if (is.null(fill_midpoint)) {
    plt <- plt + scale_fill_gradientn(
      colours = c("navy", "white", "firebrick"),
      limits = fill_limits
    )
  } else {
    plt <- plt + scale_fill_gradient2(
      midpoint = fill_midpoint,
      low = "navy",
      mid = "white",
      high = "firebrick",
      limits = fill_limits
    )
  }
  
  # Add block separators if requested
  if (!is.null(x$blockids) && block_separators) {
    block_ids <- x$blockids
    run_info <- rle(block_ids)
    row_breaks <- cumsum(run_info$lengths)
    num_cols <- ncol(DM)
    
    # Draw white lines at block boundaries
    for (rb in row_breaks[-length(row_breaks)]) {
      plt <- plt + 
        annotate("segment",
                 x = 0.5, 
                 xend = num_cols + 0.5,
                 y = rb + 0.5,
                 yend = rb + 0.5,
                 color = "white", linewidth = 1)
    }
  }
  
  # Apply theming
  plt <- plt + 
    theme_minimal(base_size = 14) +
    labs(x = "Regressors", y = "Scan Number", fill = "Value") +
    theme(
      panel.grid = element_blank(),
      axis.text.x = if (rotate_x_text) element_text(angle = 45, hjust = 1) else element_text()
    )
  
  plt
}

#' Visualize Regressor Correlations
#' 
#' Creates a heatmap visualization of the correlation matrix between regressors
#' in an event_model object.
#' 
#' @param x An \code{event_model} object.
#' @param rotate_x_text Logical. Whether to rotate x-axis labels. Default is TRUE.
#' @param ... Additional arguments passed to geom_tile.
#'
#' @return A ggplot2 object showing the correlation matrix heatmap.
#'
#' @importFrom stats cor
#' @importFrom ggplot2 coord_fixed
#' @method correlation_map event_model
#' @export
#' @examples
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30),
#'   run = 1,
#'   cond = factor(c("A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#' emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
#' correlation_map(emod)
correlation_map.event_model <- function(x, rotate_x_text = TRUE, ...) {
  # Get the design matrix
  DM <- design_matrix(x)
  
  # Compute correlation matrix
  cor_mat <- cor(DM)
  
  # Convert to long format
  df_long <- as.data.frame(as.table(cor_mat))
  names(df_long) <- c("Var1", "Var2", "Correlation")
  
  # Create the heatmap
  plt <- ggplot(df_long, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile(...) +
    scale_fill_gradient2(
      midpoint = 0,
      low = "blue",
      mid = "white",
      high = "red",
      limits = c(-1, 1)
    ) +
    theme_minimal(base_size = 14) +
    labs(x = "", y = "", fill = "Correlation") +
    theme(
      panel.grid = element_blank(),
      axis.text.x = if (rotate_x_text) element_text(angle = 45, hjust = 1) else element_text(),
      axis.text.y = element_text()
    )
  
  # Make it square
  plt <- plt + coord_fixed()
  
  plt
}

#' Plot Event Model
#'
#' Creates a line plot visualization of the predicted BOLD response for each
#' regressor in an event_model object.
#'
#' This method attempts to keep labels readable when there are many
#' regressors (e.g., trial-wise designs) by switching to faceting and either
#' abbreviating or suppressing labels depending on thresholds. You can control
#' this behavior via `label_mode`, `max_labels`, and `abbrev_min`.
#'
#' @param x An \code{event_model} object.
#' @param term_name Character. Name of specific term to plot. If NULL, plots all terms.
#' @param facet_threshold Integer. Switch to faceting when number of regressors exceeds this value. Default 6.
#' @param label_mode Character. One of `"auto"`, `"compact"`, `"none"`. In `"auto"` mode
#'   the method abbreviates labels for moderate counts and suppresses labels entirely
#'   when they are excessive (> `max_labels`). `"compact"` always abbreviates labels.
#'   `"none"` suppresses legend and facet strip labels.
#' @param max_labels Integer. When `label_mode = "auto"` and the number of regressors
#'   exceeds this value, labels are suppressed. Default 30.
#' @param abbrev_min Integer. Minimum length used by [base::abbreviate()] when compacting labels. Default 10.
#' @param strip_text_size Numeric. Strip label text size when faceting with labels. Default 8.
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot2 object showing the predicted BOLD timecourses.
#' @examples
#' # Create a simple event model
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30),
#'   run = 1,
#'   cond = factor(c("A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#' emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
#'
#' # Plot all regressors
#' plot(emod)
#'
#' # Plot specific term only
#' plot(emod, term_name = "cond")
#'
#' @param block_x Time axis to use for multi-run designs. `"global"` (default)
#'   uses concatenated time so each block occupies a distinct x-range;
#'   `"run"` uses run-relative time that restarts each block. In either case
#'   line segments are grouped by block so a regressor is never connected across
#'   a run boundary (this is what prevents the spurious high-frequency
#'   oscillations that appear when all blocks share one block-relative axis).
#' @param facet_by_block Logical; if `TRUE`, draw one panel per block. Defaults
#'   to `FALSE`. Useful for multi-run designs where overlaid runs are cluttered.
#' @param show_block_bounds Logical; if `TRUE` (default), draw dashed vertical
#'   rules at each run's start/end (`blocklen * TR`). These make late starts and
#'   overruns obvious and complement the `event_model()` onset bounds check.
#'   Drawn only when a `sampling_frame` is available.
#' @importFrom ggplot2 ggplot aes geom_line geom_vline facet_wrap facet_grid labs theme_minimal as_labeller scale_color_discrete
#' @importFrom tidyr pivot_longer
#' @method plot event_model
#' @export
plot.event_model <- function(x,
                             term_name = NULL,
                             facet_threshold = Inf,
                             label_mode = c("auto", "compact", "none"),
                             max_labels = 30,
                             abbrev_min = 10,
                             strip_text_size = 8,
                             block_x = c("global", "run"),
                             facet_by_block = FALSE,
                             show_block_bounds = TRUE,
                             ...) {
  block_x <- match.arg(block_x)
  # Get the design matrix
  DM <- design_matrix(x)

  # Per-timepoint block ids and time axis from the sampling frame.
  if (!is.null(x$sampling_frame)) {
    blockid_vec <- fmrihrf::blockids(x$sampling_frame)
    # global = concatenated time (distinct x-range per block);
    # run-relative time restarts each block.
    time_var <- fmrihrf::samples(x$sampling_frame, global = (block_x == "global"))
  } else {
    blockid_vec <- rep(1L, nrow(DM))
    time_var <- seq_len(nrow(DM))
  }
  # Guard against any length mismatch between the frame and the design matrix.
  if (length(blockid_vec) != nrow(DM)) blockid_vec <- rep(1L, nrow(DM))
  if (length(time_var) != nrow(DM)) time_var <- seq_len(nrow(DM))
  n_blocks <- length(unique(blockid_vec))

  # Select columns to plot
  if (!is.null(term_name)) {
    # Find columns matching the term name
    # Pattern matches: term_name followed by underscore, dot, bracket, or end of string
    term_cols <- grep(paste0("^", term_name, "[_\\.\\[]|^", term_name, "$"),
                      colnames(DM), value = TRUE)
    if (length(term_cols) == 0) {
      stop("No columns found matching term name: ", term_name)
    }
    DM <- DM[, term_cols, drop = FALSE]
  }

  # Convert to long format
  df_long <- as.data.frame(DM)
  df_long$Time <- time_var
  df_long$.block <- factor(blockid_vec, levels = unique(blockid_vec))
  df_long <- tidyr::pivot_longer(
    df_long,
    cols = -c(Time, .block),
    names_to = "Regressor",
    values_to = "Response"
  )
  # Group each line by Regressor *within* a block so the polyline never doubles
  # back between runs (the root cause of the spurious oscillation in #6).
  df_long$.group <- interaction(df_long$Regressor, df_long$.block,
                                drop = TRUE, lex.order = TRUE)
  # Ensure proper ordering for line drawing
  df_long <- df_long[order(df_long$Regressor, df_long$.block, df_long$Time), ]

  # Label handling
  label_mode <- match.arg(label_mode)
  regs <- unique(df_long$Regressor)
  n_regressors <- length(regs)
  use_facets <- n_regressors > facet_threshold
  
  # Build label map (possibly compacted)
  label_map <- stats::setNames(regs, regs)
  should_compact <- (label_mode == "compact") || (label_mode == "auto" && n_regressors > 8)
  if (should_compact) {
    abbr <- base::abbreviate(regs, minlength = abbrev_min)
    # ensure uniqueness to avoid duplicate facet labels or legend entries
    abbr <- base::make.unique(abbr, sep = "_")
    label_map[] <- abbr
  }
  
  # Only facet by block when it actually disambiguates (more than one block).
  facet_by_block <- isTRUE(facet_by_block) && n_blocks > 1L
  x_lab <- if (block_x == "run") "Time (seconds, run-relative)" else "Time (seconds)"

  # Run start/end boundaries (at blocklen * TR), in the chosen time units.
  bound_df <- NULL
  if (isTRUE(show_block_bounds) && !is.null(x$sampling_frame)) {
    bl <- tryCatch(fmrihrf::blocklens(x$sampling_frame), error = function(e) NULL)
    TR <- tryCatch(x$sampling_frame$TR, error = function(e) NULL)
    if (!is.null(bl) && length(bl) > 0L && is.numeric(TR)) {
      if (length(TR) == 1L) TR <- rep(TR, length(bl))
      if (length(TR) == length(bl)) {
        run_len <- bl * TR
        if (block_x == "global") {
          ends   <- cumsum(run_len)
          starts <- c(0, utils::head(ends, -1L))
        } else {
          ends   <- run_len
          starts <- rep(0, length(bl))
        }
        if (facet_by_block) {
          # One set of rules per panel (keyed by .block) so each run is bracketed.
          bound_df <- data.frame(
            .block = factor(rep(seq_along(bl), times = 2L),
                            levels = levels(df_long$.block)),
            xintercept = c(starts, ends)
          )
        } else {
          # Single shared axis: de-duplicated boundary positions.
          bound_df <- data.frame(xintercept = sort(unique(c(starts, ends))))
        }
      }
    }
  }

  # Create the plot. Lines are grouped by Regressor-within-block via `.group`.
  plt <- ggplot(df_long, aes(x = Time, y = Response, color = Regressor, group = .group))
  if (!is.null(bound_df)) {
    # Drawn first so the dashed rules sit underneath the regressor lines.
    plt <- plt + ggplot2::geom_vline(
      data = bound_df, ggplot2::aes(xintercept = xintercept),
      inherit.aes = FALSE, linetype = "dashed", colour = "grey60", linewidth = 0.35)
  }
  plt <- plt +
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    theme_minimal(base_size = 14) +
    labs(x = x_lab, y = "Predicted Response")

  suppress_labels <- label_mode == "none" ||
    (label_mode == "auto" && n_regressors > max_labels)

  if (use_facets && facet_by_block) {
    # Grid: regressors x blocks.
    labeller <- if (suppress_labels) "label_value" else ggplot2::as_labeller(label_map)
    plt <- plt +
      ggplot2::facet_grid(Regressor ~ .block, scales = "free_y",
                          labeller = ggplot2::labeller(Regressor = labeller)) +
      ggplot2::theme(legend.position = "none",
                     strip.text = if (suppress_labels) ggplot2::element_blank()
                                  else ggplot2::element_text(size = strip_text_size))
  } else if (use_facets) {
    # Facet by regressor; control labels via labeller and theme
    if (suppress_labels) {
      plt <- plt +
        ggplot2::facet_wrap(~ Regressor, scales = "free_y") +
        ggplot2::theme(legend.position = "none", strip.text = ggplot2::element_blank())
    } else {
      plt <- plt +
        ggplot2::facet_wrap(~ Regressor, scales = "free_y", labeller = ggplot2::as_labeller(label_map)) +
        ggplot2::theme(legend.position = "none", strip.text = ggplot2::element_text(size = strip_text_size))
    }
  } else if (facet_by_block) {
    # One panel per block (each block keeps its own x-range).
    plt <- plt + ggplot2::facet_wrap(~ .block, scales = "free_x",
                                     labeller = ggplot2::labeller(.block = function(b) paste0("block ", b)))
    if (suppress_labels) {
      plt <- plt + ggplot2::theme(legend.position = "none")
    } else {
      plt <- plt + ggplot2::scale_color_discrete(labels = function(x) unname(ifelse(x %in% names(label_map), label_map[x], x)))
    }
  } else {
    # Not faceting; show legend unless suppressed
    if (suppress_labels) {
      plt <- plt + ggplot2::theme(legend.position = "none")
    } else {
      # Apply compacted labels to legend if requested
      plt <- plt + ggplot2::scale_color_discrete(labels = function(x) unname(ifelse(x %in% names(label_map), label_map[x], x)))
    }
  }

  plt
}
