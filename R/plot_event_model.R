#' Plot Event Model
#'
#' Draws the convolved regressors of an `event_model` against time, with the
#' events that generate them marked underneath.
#'
#' The default `style = "auto"` picks the layout that stays readable for the
#' design at hand:
#'
#' * `"stacked"`: one row per condition on a shared time axis. Each row is
#'   scaled on its own so response shapes stay visible, and a rug under each
#'   trace marks the onsets of that condition's events. Basis sets (e.g.
#'   SPMG3) are drawn in the same row, distinguished by line type.
#' * `"heatmap"`: one raster row per regressor, used automatically for large
#'   designs such as trialwise (beta-series) models, where overlapping traces
#'   are unreadable. Rows are ordered by first onset.
#' * `"overlay"`: all regressors on one shared amplitude axis, useful for
#'   comparing magnitudes directly.
#'
#' Runs are shown as alternating light bands labelled along the top edge, with
#' thin rules at each run's start and end so late starts and overruns are
#' easy to spot. Lines never connect across a run boundary.
#'
#' @param x An \code{event_model} object.
#' @param term_name Character. Name of a specific term to plot. If `NULL`,
#'   plots all terms.
#' @param style Layout; one of `"auto"`, `"stacked"`, `"overlay"`, `"heatmap"`.
#' @param show_events Logical; draw event onset marks (rugs in stacked and
#'   overlay styles, ticks in heatmap style). Default `TRUE`.
#' @param facet_threshold Integer. In `"overlay"` style, switch to one panel
#'   per regressor when the number of regressors exceeds this value.
#' @param label_mode Character. One of `"auto"`, `"compact"`, `"none"`.
#'   `"compact"` abbreviates condition labels, `"none"` suppresses them, and
#'   `"auto"` suppresses them when there are more than `max_labels`.
#' @param max_labels Integer. Label limit used by `label_mode = "auto"`.
#' @param abbrev_min Integer. Minimum length used by [base::abbreviate()] when
#'   compacting labels.
#' @param strip_text_size Numeric. Size of row and panel labels.
#' @param block_x Time axis for multi-run designs. `"global"` (default) uses
#'   concatenated time so each run occupies its own x-range; `"run"` uses
#'   run-relative time that restarts each run (combine with
#'   `facet_by_block = TRUE` to avoid overlaying runs).
#' @param facet_by_block Logical; if `TRUE`, draw one column of panels per run.
#' @param show_block_bounds Logical; draw rules at each run's start and end.
#'   Drawn only when a `sampling_frame` is available.
#' @param y_scale How stacked rows share their amplitude axis. `"term"`
#'   (default) gives rows of the same term (and basis function) one common
#'   scale, so conditions can be compared directly while terms with different
#'   units (e.g. a parametric modulator) keep their own; `"row"` scales every
#'   row independently; `"shared"` uses one scale for all rows.
#' @param time_range Optional numeric length-2 vector (seconds, in the units of
#'   the time axis) to zoom into, e.g. `c(0, 60)` to inspect basis shapes.
#' @param title,subtitle Optional plot title and subtitle. `NULL` uses an
#'   informative default; `NA` removes it.
#' @param ... Unused; accepted for compatibility.
#'
#' @return A ggplot2 object.
#' @examples
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30),
#'   run = 1,
#'   cond = factor(c("A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#' emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
#'
#' plot(emod)
#' plot(emod, style = "overlay")
#' plot(emod, term_name = "cond")
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_vline facet_wrap facet_grid labs
#' @method plot event_model
#' @export
plot.event_model <- function(x,
                             term_name = NULL,
                             style = c("auto", "stacked", "overlay", "heatmap"),
                             show_events = TRUE,
                             facet_threshold = Inf,
                             label_mode = c("auto", "compact", "none"),
                             max_labels = 30,
                             abbrev_min = 10,
                             strip_text_size = 8.5,
                             block_x = c("global", "run"),
                             facet_by_block = FALSE,
                             show_block_bounds = TRUE,
                             y_scale = c("term", "row", "shared"),
                             time_range = NULL,
                             title = NULL,
                             subtitle = NULL,
                             ...) {
  style <- match.arg(style)
  y_scale <- match.arg(y_scale)
  if (!is.null(time_range)) {
    if (!is.numeric(time_range) || length(time_range) != 2L || anyNA(time_range) ||
        time_range[1] >= time_range[2]) {
      stop("`time_range` must be an increasing numeric vector of length 2.", call. = FALSE)
    }
  }
  block_x <- match.arg(block_x)
  label_mode <- match.arg(label_mode)

  DM <- as.matrix(design_matrix(x))
  ci <- .fd_colinfo(x)
  tl <- .fd_timeline(x$sampling_frame, nrow(DM), block_x = block_x)
  has_frame <- !is.null(x$sampling_frame) && identical(tl$unit, "s")

  # ---- column selection ---------------------------------------------------
  keep <- seq_len(ncol(DM))
  if (!is.null(term_name)) {
    pat <- paste0("^", term_name, "[_\\.\\[]|^", term_name, "$")
    keep <- which(grepl(pat, colnames(DM)) | ci$term == term_name)
    if (length(keep) == 0) {
      stop("No columns found matching term name: ", term_name, call. = FALSE)
    }
  }
  ci <- ci[keep, , drop = FALSE]
  DM <- DM[, keep, drop = FALSE]

  n_blocks <- length(unique(tl$run))
  facet_by_block <- isTRUE(facet_by_block) && n_blocks > 1L

  # ---- display labels -----------------------------------------------------
  row_key <- .fd_ev_row_key(ci)
  n_rows <- length(unique(row_key))
  n_basis <- max(c(1L, ci$basis_ix), na.rm = TRUE)
  if (style == "auto") style <- if (n_rows > 12L || n_basis > 3L) "heatmap" else "stacked"
  suppress_labels <- label_mode == "none" ||
    (label_mode == "auto" && ncol(DM) > max_labels && style != "heatmap")
  disp <- ci$full_label
  row_disp <- row_key
  if (label_mode == "compact") {
    row_disp <- unname(make.unique(base::abbreviate(row_key, minlength = abbrev_min), sep = "_"))
    disp <- unname(make.unique(base::abbreviate(disp, minlength = abbrev_min), sep = "_"))
  }
  row_levels <- unique(row_key)
  row_lab_map <- stats::setNames(row_disp[match(row_levels, row_key)], row_levels)

  # ---- long data ----------------------------------------------------------
  basis_lab <- ifelse(is.na(ci$basis_ix), NA_character_,
                      .fd_short_basis(ci$basis_label, ci$basis_ix))
  df_long <- data.frame(
    Time      = rep(tl$time, times = ncol(DM)),
    .block    = factor(rep(tl$run, times = ncol(DM)), levels = unique(tl$run)),
    Regressor = rep(colnames(DM), each = nrow(DM)),
    Response  = as.vector(DM),
    .row      = factor(rep(row_key, each = nrow(DM)), levels = row_levels),
    .basis    = factor(rep(basis_lab, each = nrow(DM)), levels = unique(basis_lab[!is.na(basis_lab)])),
    stringsAsFactors = FALSE
  )
  df_long$.group <- interaction(df_long$Regressor, df_long$.block,
                                drop = TRUE, lex.order = TRUE)
  df_long <- df_long[order(df_long$Regressor, df_long$.block, df_long$Time), ]

  # Colour: one hue per row (condition), falling back to one hue per term
  # when there are more rows than distinguishable hues.
  # Factorial terms get hue families (first factor) with lightness steps
  # (remaining factors), so the design structure reads without labels.
  struct <- .fd_structured_colours(ci, row_key)
  if (!is.null(struct)) {
    colour_keys <- row_levels
    colour_of_row <- stats::setNames(row_levels, row_levels)
    pal <- struct[row_levels]
  } else {
    colour_keys <- unique(ci$term)
    colour_of_row <- stats::setNames(ci$term[match(row_levels, row_key)], row_levels)
    pal <- .fd_colour_map(colour_keys)
  }
  df_long$.colour <- factor(colour_of_row[as.character(df_long$.row)], levels = colour_keys)

  # ---- events -------------------------------------------------------------
  ev <- if (isTRUE(show_events)) .fd_ev_events(x, ci, row_key, tl, block_x) else NULL
  if (!is.null(ev) && nrow(ev)) {
    ev$.row <- factor(ev$.row, levels = row_levels)
    ev$.colour <- factor(colour_of_row[as.character(ev$.row)], levels = colour_keys)
  }

  # ---- run furniture ------------------------------------------------------
  runs <- tl$runs
  bound_df <- NULL
  if (isTRUE(show_block_bounds) && has_frame) {
    if (facet_by_block) {
      bound_df <- data.frame(
        .block = factor(rep(runs$run, times = 2L), levels = levels(df_long$.block)),
        xintercept = c(runs$start, runs$end)
      )
    } else {
      bound_df <- data.frame(xintercept = sort(unique(c(runs$start, runs$end))))
    }
  }

  # ---- titles -------------------------------------------------------------
  n_ev <- if (!is.null(ev)) length(unique(ev$.event)) else NA
  default_sub <- paste0(
    ncol(DM), if (ncol(DM) == 1) " regressor" else " regressors",
    if (has_frame) paste0(.fd_u(" {dot} "), n_blocks, if (n_blocks == 1) " run" else " runs",
                          .fd_u(" {dot} "), nrow(DM), " scans (TR ", .fd_fmt_tr(tl$TR), ")") else "",
    if (!is.na(n_ev) && n_ev > 0) paste0(.fd_u(" {dot} "), n_ev, " events") else ""
  )
  title <- .fd_title(title, "Event regressors")
  subtitle <- .fd_title(subtitle, default_sub)

  x_scale <- .fd_ev_x_scale(runs, has_frame, block_x, facet_by_block)
  x_lab <- if (!has_frame) "Scan" else .fd_time_label(block_x)

  if (style == "heatmap") {
    return(.fd_ev_heatmap(df_long, ci, row_key, ev, runs, bound_df, x_scale, x_lab,
                          title, subtitle, suppress_labels, strip_text_size,
                          facet_by_block, time_range, DM))
  }

  row_facet <- ".row"
  has_basis <- any(!is.na(df_long$.basis))
  if (has_basis && style == "stacked") {
    # One facet row per condition x basis; only the first basis row names
    # the condition, the rest name just the basis function.
    bl <- levels(df_long$.basis)
    rb_levels <- as.vector(t(outer(row_levels, bl, paste, sep = "\r")))
    rb_labels <- as.vector(t(outer(row_lab_map[row_levels], bl, function(r, b) {
      ifelse(b == bl[1], paste0(r, .fd_u(" {dot} "), b), b)
    })))
    df_long$.rowb <- factor(paste(df_long$.row, df_long$.basis, sep = "\r"), levels = rb_levels)
    if (!is.null(ev) && nrow(ev)) {
      # Onset marks only under the first basis row of each condition.
      ev$.basis <- bl[1]
      ev$.rowb <- factor(paste(ev$.row, ev$.basis, sep = "\r"), levels = rb_levels)
    }
    row_lab_map <- stats::setNames(rb_labels, rb_levels)
    row_levels <- rb_levels
    row_facet <- ".rowb"
  }
  plt <- ggplot2::ggplot(df_long, ggplot2::aes(x = Time, y = Response, group = .group))
  if (style == "stacked" && y_scale != "row") {
    # Give rows in the same scale group identical y-limits via invisible
    # points at the group's extremes.
    grp <- if (y_scale == "shared") rep("all", nrow(df_long)) else
      paste(ci$term[match(df_long$Regressor, ci$name)], df_long$.basis)
    ext <- stats::aggregate(df_long$Response, list(g = grp, f = df_long[[row_facet]]),
                            function(v) c(min(v, 0, na.rm = TRUE), max(v, 0, na.rm = TRUE)))
    lim_g <- tapply(seq_len(nrow(ext)), ext$g, function(i) range(ext$x[i, ]))
    blank <- do.call(rbind, lapply(seq_len(nrow(ext)), function(i) {
      d <- data.frame(Time = df_long$Time[1], Response = lim_g[[ext$g[i]]])
      d[[row_facet]] <- ext$f[i]
      d
    }))
    blank[[row_facet]] <- factor(blank[[row_facet]], levels = levels(df_long[[row_facet]]))
    plt <- plt + ggplot2::geom_blank(data = blank, ggplot2::aes(x = Time, y = Response),
                                     inherit.aes = FALSE)
  }
  if (!facet_by_block && block_x == "global") plt <- plt + .fd_run_layers(runs)
  if (!is.null(bound_df)) {
    plt <- plt + ggplot2::geom_vline(
      data = bound_df, ggplot2::aes(xintercept = xintercept),
      inherit.aes = FALSE, colour = .fd_ink$zero, linewidth = 0.3)
  }
  plt <- plt + ggplot2::geom_hline(yintercept = 0, colour = .fd_ink$rule, linewidth = 0.3)

  if (style == "stacked") {
    # Basis sets get one sub-row per basis function (nested strips), each on
    # its own scale, so derivatives are as legible as the canonical.
    plt <- plt +
      ggplot2::geom_area(ggplot2::aes(fill = .colour), alpha = 0.12,
                         position = "identity", na.rm = TRUE) +
      ggplot2::geom_line(ggplot2::aes(colour = .colour), linewidth = 0.5, na.rm = TRUE) +
      ggplot2::scale_fill_manual(values = pal, guide = "none")
    if (!is.null(ev) && nrow(ev)) {
      plt <- plt + ggplot2::geom_rug(
        data = ev, ggplot2::aes(x = onset, colour = .colour), inherit.aes = FALSE,
        sides = "b", length = grid::unit(0.16, "npc"), linewidth = 0.35, alpha = 0.9)
      dur <- ev[ev$duration > 0, , drop = FALSE]
      if (nrow(dur)) {
        plt <- plt + ggplot2::geom_segment(
          data = dur, ggplot2::aes(x = onset, xend = onset + duration, colour = .colour),
          y = -Inf, yend = -Inf, inherit.aes = FALSE, linewidth = 2.2, lineend = "butt")
      }
    }
    plt <- plt + ggplot2::scale_colour_manual(values = pal, guide = "none")
    row_labeller <- ggplot2::as_labeller(if (suppress_labels) {
      stats::setNames(rep("", length(row_levels)), row_levels)
    } else row_lab_map)
    plt <- plt +
      ggplot2::facet_grid(
        rows = ggplot2::vars(!!rlang::sym(row_facet)),
        cols = if (facet_by_block) ggplot2::vars(.block) else NULL,
        scales = if (facet_by_block) "free" else "free_y",
        space = if (facet_by_block) "free_x" else "fixed",
        switch = "y",
        labeller = ggplot2::labeller(.row = row_labeller, .rowb = row_labeller,
                                     .block = function(b) paste("Run", b))) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.22, 0.08)),
                                  position = "right", breaks = .fd_peak_breaks,
                                  labels = function(v) .fd_num(v, 2)) +
      x_scale +
      ggplot2::coord_cartesian(xlim = time_range) +
      ggplot2::labs(x = x_lab, y = NULL, title = title, subtitle = subtitle,
                    caption = .fd_ev_caption(ev, y_scale)) +
      theme_fmridesign() +
      .fd_run_axis_theme() +
      ggplot2::theme(
        axis.text.y.right = ggplot2::element_text(size = strip_text_size - 1,
                                                  colour = .fd_ink$muted),
        axis.ticks.y = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        strip.placement = "outside",
        strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1, face = "plain",
                                                  size = strip_text_size + 0.5,
                                                  colour = .fd_ink$text),
        strip.text.x = ggplot2::element_text(size = strip_text_size + 0.5),
        panel.spacing.y = grid::unit(3, "pt"),
        legend.position = "none"
      )
    return(plt)
  }

  # ---- overlay ------------------------------------------------------------
  n_reg <- ncol(DM)
  reg_levels <- colnames(DM)
  reg_lab <- stats::setNames(disp, reg_levels)
  reg_pal <- if (n_reg <= length(.fd_cat)) {
    stats::setNames(.fd_cat[seq_len(n_reg)], reg_levels)
  } else {
    stats::setNames(unname(pal[as.character(colour_of_row[row_key])]), reg_levels)
  }
  df_long$Regressor <- factor(df_long$Regressor, levels = reg_levels)
  plt <- plt +
    ggplot2::geom_line(ggplot2::aes(colour = Regressor), linewidth = 0.55, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = reg_pal, labels = reg_lab, name = NULL) +
    x_scale +
    ggplot2::coord_cartesian(xlim = time_range) +
    ggplot2::labs(x = x_lab, y = "Regressor amplitude (a.u.)", title = title,
                  subtitle = subtitle) +
    theme_fmridesign() +
    .fd_run_axis_theme() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  if (!is.null(ev) && nrow(ev)) {
    plt <- plt + ggplot2::geom_rug(
      data = ev, ggplot2::aes(x = onset), inherit.aes = FALSE, sides = "b",
      colour = .fd_ink$muted, length = grid::unit(0.03, "npc"), linewidth = 0.3)
  }
  use_facets <- n_reg > facet_threshold
  if (use_facets && facet_by_block) {
    plt <- plt + ggplot2::facet_grid(Regressor ~ .block, scales = "free",
                                     labeller = ggplot2::labeller(
                                       Regressor = if (suppress_labels) ggplot2::label_value
                                                   else ggplot2::as_labeller(reg_lab),
                                       .block = function(b) paste("Run", b)))
  } else if (use_facets) {
    plt <- plt + ggplot2::facet_wrap(~ Regressor, scales = "free_y",
                                     labeller = ggplot2::as_labeller(reg_lab))
  } else if (facet_by_block) {
    plt <- plt + ggplot2::facet_wrap(~ .block, scales = "free_x",
                                     labeller = ggplot2::labeller(.block = function(b) paste("Run", b)))
  }
  if (use_facets || suppress_labels) {
    plt <- plt + ggplot2::theme(legend.position = "none")
  }
  if (use_facets) {
    plt <- plt + ggplot2::theme(
      strip.text = if (suppress_labels) ggplot2::element_blank()
                   else ggplot2::element_text(size = strip_text_size, hjust = 0))
  }
  plt
}

# One display row per condition: basis functions of a condition share a row.
.fd_ev_row_key <- function(ci) {
  key <- ci$label
  multi <- vapply(split(ci$term, key), function(t) length(unique(t)) > 1L, logical(1))
  clash <- key %in% names(multi)[multi]
  key[clash] <- paste0(ci$term_label[clash], ": ", key[clash])
  key
}

# Event onsets per display row, in plot time units.
.fd_ev_events <- function(x, ci, row_key, tl, block_x) {
  cidx <- attr(design_matrix(x), "col_indices")
  if (is.null(cidx) || is.null(x$terms) || !identical(tl$unit, "s")) return(NULL)
  runs <- tl$runs
  out <- list()
  for (tt in names(cidx)) {
    term <- x$terms[[tt]]
    all_cols <- cidx[[tt]]
    if (is.null(term) || !any(all_cols %in% ci$col) || is.null(term$onsets)) next
    raw <- tryCatch(as.matrix(design_matrix(term)), error = function(e) NULL)
    if (is.null(raw) || nrow(raw) != length(term$onsets)) next
    ncond <- ncol(raw)
    nb <- length(all_cols) %/% max(ncond, 1L)
    if (nb < 1L || ncond * nb != length(all_cols)) next
    cond_of_col <- rep(seq_len(ncond), each = nb)
    onset <- term$onsets
    if (block_x == "global") {
      run_ix <- match(term$blockids, sort(unique(x$blockids)))
      if (anyNA(run_ix) || max(run_ix) > nrow(runs)) next
      onset <- onset + runs$start[run_ix]
    }
    for (k in seq_len(ncond)) {
      hit <- which(ci$col %in% all_cols[cond_of_col == k])
      if (!length(hit)) next
      on_rows <- which(raw[, k] != 0 & !is.na(raw[, k]))
      if (!length(on_rows)) next
      out[[length(out) + 1L]] <- data.frame(
        onset = onset[on_rows], .row = row_key[hit[1]],
        duration = if (length(term$durations) == length(term$onsets))
          term$durations[on_rows] else 0,
        .block = factor(term$blockids[on_rows]),
        .event = paste(term$blockids[on_rows], term$onsets[on_rows]),
        stringsAsFactors = FALSE)
    }
  }
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}

.fd_ev_x_scale <- function(runs, has_frame, block_x, facet_by_block) {
  if (!has_frame) {
    return(ggplot2::scale_x_continuous(expand = c(0, 0)))
  }
  sec <- ggplot2::waiver()
  if (!facet_by_block && block_x == "global") sec <- .fd_run_sec_axis(runs)
  ggplot2::scale_x_continuous(labels = .fd_mmss, expand = c(0, 0),
                              breaks = if (facet_by_block) .fd_time_breaks_inner else .fd_time_breaks,
                              sec.axis = sec)
}

# Breaks at round minute multiples, about six per axis.
.fd_time_breaks <- function(lims) {
  span <- diff(lims)
  step <- c(15, 30, 60, 120, 300, 600, 1200)
  step <- step[which.min(abs(span / step - 6))]
  seq(ceiling(lims[1] / step) * step, lims[2], by = step)
}

# Interior breaks only, so adjacent facet panels never print colliding labels
# at their shared edge.
.fd_time_breaks_inner <- function(lims) {
  b <- .fd_time_breaks(lims)
  pad <- 0.04 * diff(lims)
  b[b > lims[1] + pad & b < lims[2] - pad]
}

# Two y breaks per stacked row: zero and a round value near the row's peak,
# so every row states its own magnitude.
.fd_peak_breaks <- function(lims) {
  # Undo the stacked rows' expansion (mult = c(0.22, 0.08)) to recover the
  # data extremes, then label zero and those extremes (rounded towards zero
  # so a break never falls outside the data).
  r <- diff(lims) / 1.30
  hi <- lims[2] - 0.08 * r
  lo <- lims[1] + 0.22 * r
  trunc2 <- function(v) {
    if (!is.finite(v) || v == 0) return(0)
    d <- 10^(floor(log10(abs(v))) - 1)
    trunc(v / d) * d
  }
  b <- 0
  if (hi > 0.02 * r) b <- c(b, trunc2(hi))
  if (lo < -0.02 * r) b <- c(trunc2(lo), b)
  unique(b)
}

.fd_ev_caption <- function(ev, y_scale = "row") {
  scale_txt <- switch(y_scale,
    term = "Rows of the same term share a scale (right axis)",
    row = "Each row has its own scale (right axis)",
    shared = "All rows share one scale (right axis)")
  if (is.null(ev) || !nrow(ev)) return(paste0(scale_txt, "."))
  dur <- any(ev$duration > 0)
  paste0(scale_txt, ". Ticks mark event onsets",
         if (dur) "; bars show event durations" else "", ".")
}

.fd_fmt_tr <- function(TR) {
  paste0(paste(format(unique(TR)), collapse = "/"), " s")
}

.fd_title <- function(value, default) {
  if (is.null(value)) return(default)
  if (length(value) == 1L && is.na(value)) return(NULL)
  value
}

.fd_ev_heatmap <- function(df_long, ci, row_key, ev, runs, bound_df, x_scale, x_lab,
                           title, subtitle, suppress_labels, strip_text_size,
                           facet_by_block, time_range = NULL, DM = NULL) {
  # Order rows by first event onset when events are known, else design order.
  reg_levels <- ci$name
  lab <- stats::setNames(ci$full_label, ci$name)
  # Order by first onset for one-column-per-event designs (trialwise); keep
  # design order when rows are basis functions of a few conditions.
  if (!is.null(ev) && nrow(ev) && all(is.na(ci$basis_ix))) {
    first_on <- tapply(ev$onset, as.character(ev$.row), min)
    ord <- order(first_on[row_key], na.last = TRUE)
    reg_levels <- reg_levels[ord]
  }
  df_long$Regressor <- factor(df_long$Regressor, levels = rev(reg_levels))
  peak <- max(abs(df_long$Response), na.rm = TRUE)
  # HRF undershoots are small; only treat the data as signed when negative
  # values are substantial (e.g. parametric modulators or derivatives).
  signed <- any(df_long$Response < -0.01 * peak, na.rm = TRUE)
  dt <- stats::median(diff(sort(unique(df_long$Time))))
  plt <- ggplot2::ggplot(df_long, ggplot2::aes(x = Time, y = Regressor, fill = Response)) +
    ggplot2::geom_tile(width = dt, height = 0.92)
  plt <- plt + if (signed) {
    .fd_scale_signed(limit = peak, name = "Amplitude\n(a.u.)")
  } else {
    .fd_scale_magnitude(limit = peak, name = "Amplitude\n(a.u.)")
  }
  if (!is.null(bound_df)) {
    plt <- plt + ggplot2::geom_vline(data = bound_df, ggplot2::aes(xintercept = xintercept),
                                     inherit.aes = FALSE, colour = .fd_ink$muted, linewidth = 0.3)
  }
  if (!is.null(ev) && nrow(ev)) {
    ev$Regressor <- factor(ci$name[match(as.character(ev$.row), row_key)],
                           levels = levels(df_long$Regressor))
    ev <- ev[!is.na(ev$Regressor), , drop = FALSE]
    plt <- plt + ggplot2::geom_point(data = ev, ggplot2::aes(x = onset, y = Regressor),
                                     inherit.aes = FALSE, shape = "|", size = 3,
                                     colour = .fd_ink$text)
  }
  n <- length(reg_levels)
  # Overlap diagnostic: each regressor's largest |r| with any other column.
  # High values mean neighbouring trials will be hard to separate (beta-series
  # collinearity).
  show_r <- !is.null(DM) && ncol(DM) > 1L && ncol(DM) <= 80L && !facet_by_block
  if (show_r) {
    R <- suppressWarnings(stats::cor(DM))
    diag(R) <- NA
    max_r <- apply(abs(R), 2, max, na.rm = TRUE)
    names(max_r) <- colnames(DM)
    x_right <- if (!is.null(time_range)) time_range[2] else max(df_long$Time)
    span <- diff(if (!is.null(time_range)) time_range else range(df_long$Time))
    rdf <- data.frame(Regressor = factor(names(max_r), levels = levels(df_long$Regressor)),
                      r = max_r, x = x_right + 0.012 * span)
    rdf$lab <- sub("^0", "", formatC(rdf$r, format = "f", digits = 2))
    plt <- plt +
      ggplot2::geom_text(data = rdf, ggplot2::aes(x = x, y = Regressor, label = lab,
                                                  colour = r >= 0.5),
                         inherit.aes = FALSE, hjust = 0, size = strip_text_size / ggplot2::.pt * 0.95) +
      ggplot2::scale_colour_manual(values = c(`FALSE` = .fd_ink$muted, `TRUE` = .fd_cat[2]),
                                   guide = "none") +
      ggplot2::annotate("text", x = x_right + 0.012 * span, y = n + 0.9, label = "max r",
                        hjust = 0, vjust = 0, size = strip_text_size / ggplot2::.pt,
                        colour = .fd_ink$text2, fontface = "bold")
  }
  step <- if (n > 40) ceiling(n / 25) else 1L
  shown <- reg_levels[seq(1, n, by = step)]
  plt <- plt +
    x_scale +
    ggplot2::scale_y_discrete(labels = function(b) unname(lab[b]), breaks = if (suppress_labels) NULL else shown,
                              expand = c(0, 0)) +
    ggplot2::labs(x = x_lab, y = NULL, title = title, subtitle = subtitle,
                  caption = paste0(
                    if (!is.null(ev) && nrow(ev) && all(is.na(ci$basis_ix))) "Rows ordered by first onset; ticks mark event onsets. " else if (!is.null(ev) && nrow(ev)) "Ticks mark event onsets. " else "",
                    "Cells are sampled at the TR.",
                    if (show_r) " max r: largest correlation with any other regressor (orange when 0.5 or more)." else "")) +
    ggplot2::coord_cartesian(xlim = time_range, clip = "off") +
    theme_fmridesign() +
    .fd_run_axis_theme() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = strip_text_size),
                   legend.position = "top",
                   legend.justification = "right",
                   legend.key.width = grid::unit(1.6, "lines"),
                   legend.key.height = grid::unit(0.45, "lines"),
                   plot.margin = ggplot2::margin(10, if (show_r) 42 else 14, 8, 10))
  if (facet_by_block) {
    plt <- plt + ggplot2::facet_grid(cols = ggplot2::vars(.block), scales = "free_x",
                                     space = "free_x",
                                     labeller = ggplot2::labeller(.block = function(b) paste("Run", b)))
  }
  plt
}

utils::globalVariables(c("Time", "Response", "Regressor", ".group", ".block", ".row",
                         ".basis", ".colour", "onset", "xintercept", "duration", "lab", "r", "x"))
