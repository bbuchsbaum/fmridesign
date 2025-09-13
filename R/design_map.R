#' Design Matrix Visualization Methods
#'
#' This file contains methods for visualizing design matrices as heatmaps
#' using ggplot2.

# Internal helper used by baseline and event correlation maps
# Not exported
#' @keywords internal
#' @return A ggplot2 heatmap of the correlation matrix.
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
#' @param x An \code{event_model} object.
#' @param term_name Character. Name of specific term to plot. If NULL, plots all terms.
#' @param ... Additional arguments (currently unused).
#' 
#' @return A ggplot2 object showing the predicted BOLD timecourses.
#' 
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap labs theme_minimal
#' @importFrom tidyr pivot_longer
#' @method plot event_model
#' @export
plot.event_model <- function(x, term_name = NULL, ...) {
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
  
  # Create the plot
  plt <- ggplot(df_long, aes(x = Time, y = Response, color = Regressor, group = Regressor)) +
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    theme_minimal(base_size = 14) +
    labs(x = "Time (seconds)", y = "Predicted Response")
  
  # If many regressors, use facets
  n_regressors <- length(unique(df_long$Regressor))
  if (n_regressors > 6) {
    plt <- plt + 
      facet_wrap(~ Regressor, scales = "free_y") +
      theme(legend.position = "none")
  }
  
  # Explicitly print to ensure rendering on first call
  print(plt)
  invisible(plt)
}
