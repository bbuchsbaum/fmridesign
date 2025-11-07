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
                 color = "white", size = 1)
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
#' 
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap labs theme_minimal as_labeller scale_color_discrete
#' @importFrom tidyr pivot_longer
#' @method plot event_model
#' @export
plot.event_model <- function(x,
                             term_name = NULL,
                             facet_threshold = 6,
                             label_mode = c("auto", "compact", "none"),
                             max_labels = 30,
                             abbrev_min = 10,
                             strip_text_size = 8,
                             ...) {
  # Get the design matrix
  DM <- design_matrix(x)
  
  # Create time variable based on sampling frame
  if (!is.null(x$sampling_frame)) {
    # Use run-relative time (restarts each block) for clearer per-block plots
    time_var <- fmrihrf::samples(x$sampling_frame, global = FALSE)
  } else {
    time_var <- seq_len(nrow(DM))
  }
  
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
  df_long <- tidyr::pivot_longer(
    df_long,
    cols = -Time,
    names_to = "Regressor",
    values_to = "Response"
  )
  # Ensure proper ordering and grouping for line drawing
  df_long <- df_long[order(df_long$Regressor, df_long$Time), ]
  
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
  
  # Create the plot
  plt <- ggplot(df_long, aes(x = Time, y = Response, color = Regressor, group = Regressor)) +
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    theme_minimal(base_size = 14) +
    labs(x = "Time (seconds)", y = "Predicted Response")
  
  if (use_facets) {
    # Facet by regressor; control labels via labeller and theme
    if (label_mode == "none" || (label_mode == "auto" && n_regressors > max_labels)) {
      plt <- plt +
        ggplot2::facet_wrap(~ Regressor, scales = "free_y") +
        ggplot2::theme(legend.position = "none", strip.text = ggplot2::element_blank())
    } else {
      plt <- plt +
        ggplot2::facet_wrap(~ Regressor, scales = "free_y", labeller = ggplot2::as_labeller(label_map)) +
        ggplot2::theme(legend.position = "none", strip.text = ggplot2::element_text(size = strip_text_size))
    }
  } else {
    # Not faceting; show legend unless suppressed
    if (label_mode == "none" || (label_mode == "auto" && n_regressors > max_labels)) {
      plt <- plt + ggplot2::theme(legend.position = "none")
    } else {
      # Apply compacted labels to legend if requested
      plt <- plt + ggplot2::scale_color_discrete(labels = function(x) ifelse(x %in% names(label_map), label_map[x], x))
    }
  }
  
  plt
}
