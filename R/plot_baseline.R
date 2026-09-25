#' Plot a Baseline Model
#'
#' Draws every time-varying term of a baseline (nuisance) model on a shared
#' time axis: one panel per term, with thin rules at run boundaries and run
#' labels along the top. Lines never connect across run boundaries.
#'
#' Drift basis functions share one ordered colour ramp (darkest = first basis
#' function), so a basis function keeps its colour in every run. Nuisance
#' regressors keep the column names you supplied. When those names look like
#' rigid-body motion parameters (`tx`/`ty`/`tz` and `rx`/`ry`/`rz`,
#' `trans_x`/`rot_x`, `x`/`y`/`z` and `pitch`/`roll`/`yaw`), translations and
#' rotations get separate panels, because they are usually in different units
#' and rotations look flat on a shared axis. Each panel carries a key at its
#' right edge in column order; panels with more than 7 series use a legend
#' instead.
#'
#' Terms that are constant within every run (run intercepts, a global
#' intercept, a constant drift basis) carry no time course and are left out;
#' the caption lists what was omitted. Request one explicitly with
#' `term_name` to draw it anyway. For spline and polynomial drift the caption
#' also gives a rough equivalent high-pass period, `2 * run duration / k` for
#' `k` drift columns per run (the cutoff a discrete cosine basis with `k`
#' regressors would have); treat it as a guide, not an exact filter.
#'
#' @param x A `baseline_model` object.
#' @param term_name Optional name of a single term to plot (one of
#'   `names(terms(x))`, e.g. `"drift"`, `"block"` or `"nuisance"`). An exact
#'   match is used if there is one; otherwise a unique case-insensitive
#'   partial match. Constant terms can be requested this way. If `NULL` (the
#'   default), all non-constant terms are drawn.
#' @param title Plot title. Defaults to `"Baseline model"`.
#' @param xlab Label for the x-axis. Defaults to a time label with units.
#' @param ylab Label for the y-axis. By default `"Regressor value"`, or no
#'   y title when motion parameters are split into translation and rotation
#'   panels (their units differ; the panel titles name them).
#' @param line_size Line width in mm (default `0.5`).
#' @param color_palette Ignored; retained for backward compatibility. Colours
#'   follow the package palette (see [fmridesign_palette()]).
#' @param block_x How to lay out runs on the x-axis: `"global"` (default)
#'   concatenates runs on one time axis; `"run"` gives each run its own
#'   column of panels with run-relative time.
#' @param drift_scale `"raw"` (default) plots drift columns as they enter the
#'   design matrix. `"unit"` divides each drift column by its peak absolute
#'   value within its run so basis shapes can be compared; the caption then
#'   reports the raw peaks and flags near-flat columns.
#' @param subtitle Plot subtitle. Defaults to a summary of the model (drift
#'   basis and size, nuisance regressors per run, runs, scans and TR).
#' @param ... Additional arguments passed to [ggplot2::geom_line()].
#' @return A ggplot2 object.
#' @examples
#' sframe <- fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 2)
#' nuis <- lapply(1:2, function(r) {
#'   m <- matrix(cumsum(rnorm(160, sd = 0.1)), 40, 4)
#'   colnames(m) <- c("tx", "ty", "rx", "ry")
#'   m
#' })
#' bmod <- baseline_model(basis = "bs", degree = 4, sframe = sframe,
#'                        nuisance_list = nuis)
#' plot(bmod)
#' plot(bmod, drift_scale = "unit")
#' plot(bmod, term_name = "nuisance", block_x = "run")
#' @method plot baseline_model
#' @export
plot.baseline_model <- function(x, term_name = NULL, title = NULL,
                                xlab = NULL, ylab = NULL,
                                line_size = 0.5, color_palette = NULL,
                                block_x = c("global", "run"),
                                drift_scale = c("raw", "unit"),
                                subtitle = NULL, ...) {
  block_x <- match.arg(block_x)
  drift_scale <- match.arg(drift_scale)
  all_terms <- terms(x)
  if (length(all_terms) == 0) {
    stop("Baseline model contains no terms.")
  }
  term_names <- names(all_terms)

  sframe <- x$sampling_frame
  blockids_vec <- tryCatch(fmrihrf::blockids(sframe), error = function(e) NULL)
  if (is.null(blockids_vec)) {
    stop("Could not derive sample times or block IDs from the sampling_frame.",
         call. = FALSE)
  }

  term_dms <- lapply(all_terms, function(term) as.matrix(design_matrix(term)))
  is_constant <- vapply(term_dms, .baseline_term_is_constant, logical(1),
                        blockids = blockids_vec)

  if (is.null(term_name)) {
    plot_terms <- if (all(is_constant)) term_names else term_names[!is_constant]
    if (all(is_constant)) {
      message("All baseline terms are constant within runs; plotting: ",
              paste(plot_terms, collapse = ", "))
    }
  } else {
    plot_terms <- .fd_bl_match_term(term_name, term_names)
  }

  # Column metadata (labels, run membership) for the full design matrix,
  # split back into terms by their column counts.
  info <- .fd_colinfo(x)
  ncols <- vapply(term_dms, ncol, integer(1))
  if (sum(ncols) != nrow(info)) {
    stop("Column mismatch between baseline terms and design matrix.", call. = FALSE)
  }
  info$term_name <- rep(term_names, ncols)

  n_rows <- length(blockids_vec)
  tl <- .fd_timeline(sframe, n_rows, block_x = block_x)
  runs <- tl$runs
  n_runs <- nrow(runs)

  long <- do.call(rbind, lapply(plot_terms, function(tn) {
    .fd_bl_term_long(term_dms[[tn]], info[info$term_name == tn, , drop = FALSE],
                     tl, tn)
  }))
  if (is.null(long) || nrow(long) == 0L) {
    stop("Nothing to plot: the selected baseline terms have no columns.",
         call. = FALSE)
  }

  scale_note <- NULL
  if (drift_scale == "unit" && any(long$term == "drift")) {
    sc <- .fd_bl_unit_scale(long)
    long <- sc$long
    scale_note <- sc$note
  }

  # Panels ("lanes"): one per term, with motion-like nuisance columns split
  # into translation and rotation lanes.
  long$lane <- .fd_bl_lanes(long)
  lane_levels <- .fd_bl_lane_order(long$lane)
  long$facet <- factor(long$lane, levels = lane_levels)
  long$run_lab <- factor(paste("Run", long$run), levels = paste("Run", runs$run))

  # Colours: drift bases get an ordered ramp keyed by basis index; other
  # lanes use the categorical palette in column order, so a regressor keeps
  # its colour in every run (and x/y/z match across motion lanes).
  long$colour <- NA_character_
  for (ln in lane_levels) {
    sel <- long$lane == ln
    keys <- unique(long$label[sel])
    cmap <- if (all(long$term[sel] == "drift")) .fd_bl_ramp(keys) else .fd_colour_map(keys)
    long$colour[sel] <- cmap[long$label[sel]]
  }

  kk <- .fd_bl_keys(long)
  keys <- kk$keys
  big <- kk$unlabelled

  motion_split <- any(long$lane %in% c("Translation", "Rotation"))
  if (is.null(ylab) && !motion_split) ylab <- "Regressor value"
  unit <- tl$unit
  if (is.null(xlab)) {
    xlab <- if (identical(unit, "scan")) "Scan" else .fd_time_label(block_x)
  }
  span <- max(runs$end) - min(runs$start)
  x_labels <- if (identical(unit, "s")) .fd_mmss else ggplot2::waiver()
  x_breaks <- if (identical(unit, "s")) .fd_bl_time_breaks(span) else ggplot2::waiver()

  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$time, y = .data$value,
                                          group = .data$group))
  if (block_x == "global" && n_runs > 1L) {
    p <- p + ggplot2::geom_vline(xintercept = runs$start[-1],
                                 colour = .fd_ink$zero, linewidth = 0.3)
  }
  p <- p +
    ggplot2::geom_line(ggplot2::aes(colour = .data$colour),
                       linewidth = line_size, na.rm = TRUE, ...)

  if (length(big)) {
    # Lanes share colours by position (e.g. trans_x and rot_x), so one legend
    # entry per colour lists every series drawn in it.
    lg <- unique(long[long$lane %in% big, c("colour", "label")])
    lg_lab <- tapply(lg$label, factor(lg$colour, levels = unique(lg$colour)),
                     function(v) paste(unique(v), collapse = " / "))
    p <- p + ggplot2::scale_colour_identity(
      guide = ggplot2::guide_legend(ncol = if (length(lg_lab) > 8L) 4L else NULL),
      breaks = names(lg_lab), labels = unname(lg_lab), name = NULL)
  } else {
    p <- p + ggplot2::scale_colour_identity()
  }

  if (!is.null(keys) && nrow(keys)) {
    p <- p +
      ggplot2::geom_segment(
        data = keys,
        ggplot2::aes(x = .data$x_key0, xend = .data$x_key1, y = .data$y_lab,
                     yend = .data$y_lab, colour = .data$colour),
        inherit.aes = FALSE, linewidth = 1.6) +
      ggplot2::geom_text(
        data = keys,
        ggplot2::aes(x = .data$x_txt, y = .data$y_lab, label = .data$label),
        inherit.aes = FALSE, hjust = 0, size = 3.1, colour = .fd_ink$text)
  }

  if (block_x == "global") {
    p <- p +
      ggplot2::facet_wrap(ggplot2::vars(.data$facet), ncol = 1,
                          scales = "free_y") +
      ggplot2::scale_x_continuous(breaks = x_breaks, labels = x_labels,
                                  limits = c(min(runs$start), max(runs$end)),
                                  oob = function(v, range) v,
                                  expand = c(0, 0),
                                  sec.axis = .fd_run_sec_axis(runs))
  } else {
    p <- p +
      ggplot2::facet_grid(rows = ggplot2::vars(.data$facet),
                          cols = ggplot2::vars(.data$run_lab),
                          scales = "free", space = "free_x", switch = "y") +
      ggplot2::scale_x_continuous(breaks = x_breaks, labels = x_labels,
                                  expand = c(0, 0))
  }

  caption <- c(
    if (any(long$term == "drift")) .fd_bl_cutoff_note(x, info, runs, unit),
    scale_note,
    if (motion_split) "Translation and rotation are plotted in the units supplied.",
    if (is.null(term_name)) .fd_bl_omitted_caption(info, is_constant, plot_terms))

  p +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = title %||% "Baseline model",
      subtitle = subtitle %||% .fd_bl_subtitle(x, info, runs, tl),
      caption = if (length(caption)) paste(caption, collapse = "\n"),
      x = xlab, y = ylab) +
    theme_fmridesign() +
    .fd_run_axis_theme() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(
        10, if (is.null(keys)) 14 else 34 + 5.6 * max(nchar(keys$label)), 8, 10),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.spacing.y = grid::unit(12, "pt"),
      panel.spacing.x = grid::unit(if (block_x == "run") 10 else 6, "pt"),
      strip.placement = if (block_x == "run") "outside" else "inside",
      strip.text.y.left = ggplot2::element_text(angle = 90, hjust = 0.5),
      legend.position = if (length(big)) "bottom" else "none")
}

# Resolve `term_name` against the model's term names: exact match first, then
# a unique case-insensitive partial match.
.fd_bl_match_term <- function(term_name, term_names) {
  exact_match <- which(term_names == term_name)
  if (length(exact_match) == 1) return(term_names[exact_match])
  partial_matches <- grep(term_name, term_names, ignore.case = TRUE)
  if (length(partial_matches) == 1) {
    plot_term <- term_names[partial_matches]
    message("Found unique partial match for '", term_name, "': using term '",
            plot_term, "'")
    return(plot_term)
  }
  if (length(partial_matches) == 0) {
    stop("Specified term_name '", term_name, "' not found. Available terms: ",
         paste(term_names, collapse = ", "))
  }
  stop("Specified term_name '", term_name, "' matches multiple terms: ",
       paste(term_names[partial_matches], collapse = ", "),
       ". Please be more specific.")
}

# Long data for one term. Block-diagonal columns (one run's drift or
# nuisance column) are kept only within their own run, so other runs do not
# show flat zero lines; lines are grouped per column and run so nothing
# connects across run boundaries.
.fd_bl_term_long <- function(dm, info, tl, term) {
  if (ncol(dm) == 0L) return(NULL)
  n <- nrow(dm)
  out <- lapply(seq_len(ncol(dm)), function(j) {
    keep <- if (!is.na(info$run[j])) tl$run == info$run[j] else rep(TRUE, n)
    if (!any(keep)) return(NULL)
    data.frame(term = term, col = info$col[j], label = info$label[j],
               run = tl$run[keep], time = tl$time[keep],
               value = as.numeric(dm[keep, j]),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  if (is.null(out)) return(NULL)
  out$group <- paste(out$term, out$col, out$run, sep = ":")
  out
}

# Divide each drift series (one column within one run) by its peak |value|.
# Returns the rescaled data and a caption line that states the denominator,
# the raw peaks, and any series that were near-flat before scaling.
.fd_bl_unit_scale <- function(long) {
  sel <- long$term == "drift"
  peak <- stats::ave(abs(long$value), long$group, FUN = function(v) max(v, na.rm = TRUE))
  rng <- stats::ave(long$value, long$group, FUN = function(v) diff(range(v, na.rm = TRUE)))
  ok <- sel & peak > 0
  long$value[ok] <- long$value[ok] / peak[ok]
  peaks <- tapply(peak[sel], long$group[sel], `[`, 1)
  rngs <- tapply(rng[sel], long$group[sel], `[`, 1)
  flat <- names(rngs)[rngs < 0.01 * max(rngs)]
  note <- .fd_u(sprintf(
    "Drift scaled: each basis function divided by its peak |value| in its run (raw peaks %s{ndash}%s).",
    trimws(.fd_num(min(peaks))), trimws(.fd_num(max(peaks)))))
  if (length(flat)) {
    fl <- unique(long[long$group %in% flat, c("label", "run")])
    note <- paste0(note, " Near-flat before scaling: ",
                   paste(sprintf("%s (run %d)", fl$label, fl$run), collapse = ", "), ".")
  }
  list(long = long, note = note)
}

# Recognise rigid-body motion parameters by name. Returns "translation",
# "rotation" or NA for each label.
.fd_bl_motion_kind <- function(labels) {
  l <- tolower(labels)
  rot <- grepl("^(r|rot)[_.]?[xyz]($|[_.])", l) | grepl("^rot", l) |
    grepl("(^|[_.])(pitch|roll|yaw)($|[_.])", l)
  trans <- !rot & (grepl("^(t|trans)[_.]?[xyz]($|[_.])", l) | grepl("^trans", l) |
                     grepl("^[xyz]($|[_.])", l))
  out <- rep(NA_character_, length(l))
  out[trans] <- "translation"
  out[rot] <- "rotation"
  out
}

# Lane (panel) for each row: the term's display name, with nuisance columns
# split into Translation / Rotation (/ Other nuisance) when both motion kinds
# are present.
.fd_bl_lanes <- function(long) {
  lane <- long$term
  lane[long$term == "drift"] <- "Drift"
  lane[long$term == "block"] <- "Intercept"
  nz <- long$term == "nuisance"
  lane[nz] <- "Nuisance"
  if (any(nz)) {
    kind <- .fd_bl_motion_kind(unique(long$label[nz]))
    if (any(kind %in% "translation") && any(kind %in% "rotation")) {
      k <- .fd_bl_motion_kind(long$label[nz])
      lane[nz] <- ifelse(is.na(k), "Other nuisance",
                         ifelse(k == "translation", "Translation", "Rotation"))
    }
  }
  lane
}

# Lane order: order of appearance, except that motion lanes always read
# Translation, Rotation, Other nuisance.
.fd_bl_lane_order <- function(lanes) {
  lanes <- unique(lanes)
  motion <- c("Translation", "Rotation", "Other nuisance")
  is_motion <- lanes %in% motion
  if (!any(is_motion)) return(lanes)
  first <- which(is_motion)[1]
  rest <- lanes[!is_motion]
  before <- rest[rest %in% lanes[seq_len(first - 1L)]]
  c(before, intersect(motion, lanes), setdiff(rest, before))
}

# Colours for drift bases: the categorical palette in basis order while it
# lasts (every basis clearly distinct), otherwise an ordered ramp from dark
# indigo through blue and teal to green.
.fd_bl_ramp <- function(keys) {
  keys <- unique(as.character(keys))
  n <- length(keys)
  cols <- if (n <= length(.fd_cat)) .fd_cat[seq_len(n)] else {
    grDevices::colorRampPalette(c("#1B2A5E", "#1F6FB2", "#3A9FA8", "#86B84F"),
                                space = "Lab")(n)
  }
  stats::setNames(cols, keys)
}

# In-panel keys at the right edge of each lane, listed top-down in column
# order (like a legend) rather than by end value. Lanes with more than 7
# series (too many to stack legibly in one panel) get no key and are listed
# in `unlabelled` so the caller can draw a legend for them.
.fd_bl_keys <- function(long) {
  span <- diff(range(long$time))
  lanes <- unique(long$lane)
  big <- character(0)
  out <- lapply(lanes, function(ln) {
    d <- long[long$lane == ln, , drop = FALSE]
    lab <- unique(d$label)
    n <- length(lab)
    if (n > 7L) {
      big <<- c(big, ln)
      return(NULL)
    }
    rng <- range(d$value, na.rm = TRUE)
    h <- max(diff(rng), 1e-8)
    gap <- if (n > 1L) max(0.13 * h, min(0.22 * h, h / (n - 1))) else 0
    mid <- mean(rng)
    last <- d[d$run == max(d$run), , drop = FALSE]
    data.frame(facet = d$facet[1], run_lab = last$run_lab[1], label = lab,
               colour = d$colour[match(lab, d$label)],
               x = max(last$time),
               y_lab = mid + gap * ((n - 1) / 2 - (seq_len(n) - 1)),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  if (is.null(out) || nrow(out) == 0L) {
    return(list(keys = NULL, unlabelled = big))
  }
  out$x_key0 <- out$x + 0.012 * span
  out$x_key1 <- out$x_key0 + 0.012 * span
  out$x_txt <- out$x_key1 + 0.006 * span
  list(keys = out, unlabelled = big)
}

# Round m:ss breaks for a time axis of `span` seconds.
.fd_bl_time_breaks <- function(span) {
  steps <- c(10, 15, 30, 60, 120, 180, 300, 600, 900, 1200, 1800, 3600)
  step <- steps[which(span / steps <= 9)[1]]
  if (is.na(step)) step <- 3600
  seq(0, span, by = step)
}

.fd_bl_basis_name <- function(basis) {
  switch(basis %||% "",
         bs = "B-spline", ns = "natural spline", poly = "polynomial",
         constant = "constant", basis)
}

# Rough equivalent high-pass period for spline/polynomial drift: a discrete
# cosine basis with k regressors over a run of duration T removes periods
# longer than 2T/k. Only stated when drift is block-diagonal by run.
.fd_bl_cutoff_note <- function(x, info, runs, unit) {
  basis <- x$drift_spec$basis %||% ""
  if (!basis %in% c("bs", "ns", "poly") || !identical(unit, "s")) return(NULL)
  d <- info[info$term_name == "drift", , drop = FALSE]
  if (!nrow(d) || anyNA(d$run)) return(NULL)
  k <- as.integer(table(factor(d$run, levels = runs$run)))
  dur <- runs$end - runs$start
  ok <- k > 0
  if (!any(ok)) return(NULL)
  period <- 2 * dur[ok] / k[ok]
  num <- function(v) trimws(.fd_num(v, 3))
  span_txt <- function(v) {
    if (diff(range(v)) < 1) paste(num(v[1]), "s") else
      .fd_u(sprintf("%s{ndash}%s s", num(min(v)), num(max(v))))
  }
  k_txt <- if (length(unique(k[ok])) == 1L) k[ok][1] else
    .fd_u(sprintf("%d{ndash}%d", min(k[ok]), max(k[ok])))
  # "bs" drift is built with splines::bs(degree = k) and no interior knots,
  # i.e. a degree-k polynomial basis; say so, since the DCT analogy is loose.
  what <- switch(basis,
                 bs = "B-spline columns (degree-%s polynomial, no interior knots)",
                 ns = "natural-spline columns",
                 poly = "polynomial columns")
  if (basis == "bs") what <- sprintf(what, k_txt)
  .fd_u(sprintf(paste0(
    "Drift: %s %s per %s run.\nRoughly comparable to a DCT high-pass with cutoff ",
    "~%s (DCT rule: 2 {times} run duration / columns)."),
    k_txt, what, span_txt(dur[ok]), span_txt(period)))
}

# e.g. "Drift: B-spline, 5 per run, 6 nuisance per run, 3 runs, 480 scans (TR 2 s)",
# joined by middle dots.
.fd_bl_subtitle <- function(x, info, runs, tl) {
  count <- function(tn) {
    d <- info[info$term_name == tn, , drop = FALSE]
    if (!nrow(d)) return(NULL)
    if (all(is.na(d$run))) return(list(n = nrow(d), per_run = FALSE))
    k <- as.integer(table(factor(d$run, levels = runs$run)))
    k <- k[k > 0]
    list(n = if (length(unique(k)) == 1L) k[1] else
      paste0(min(k), .fd_u("{ndash}"), max(k)), per_run = TRUE)
  }
  parts <- character(0)
  dr <- count("drift")
  if (!is.null(dr)) {
    parts <- c(parts, sprintf("Drift: %s, %s%s",
                              .fd_bl_basis_name(x$drift_spec$basis), dr$n,
                              if (dr$per_run) " per run" else ""))
  }
  nz <- count("nuisance")
  if (!is.null(nz)) {
    parts <- c(parts, sprintf("%s nuisance%s", nz$n,
                              if (nz$per_run) " per run" else ""))
  }
  n_runs <- nrow(runs)
  parts <- c(parts, sprintf("%d run%s", n_runs, if (n_runs == 1L) "" else "s"))
  if (identical(tl$unit, "s")) {
    parts <- c(parts, sprintf("%d scans (TR %s)", sum(runs$scans),
                              .fd_sf_tr_text(unique(tl$TR))))
  }
  paste(parts, collapse = .fd_u(" {dot} "))
}

# Caption listing constant terms left out of the default view.
.fd_bl_omitted_caption <- function(info, is_constant, plot_terms) {
  hidden <- setdiff(names(is_constant)[is_constant], plot_terms)
  if (!length(hidden)) return(NULL)
  desc <- vapply(hidden, function(tn) {
    k <- sum(info$term_name == tn)
    what <- if (tn == "block") {
      if (k == 1L) "global intercept" else "run intercepts"
    } else if (tn == "drift") "constant drift" else tn
    sprintf("%s (%d column%s)", what, k, if (k == 1L) "" else "s")
  }, character(1))
  paste0("Not shown (constant within each run): ",
         paste(desc, collapse = ", "), ".")
}

#' @rdname print.sampling_frame
#' @param events For `plot()`, optional events to overlay as a coverage
#'   check: an `event_model`, or a data frame with an `onset` column (seconds,
#'   relative to the start of each run) and a `run` (or `block`) column. Each
#'   onset is drawn as a small tick under its run, so runs without events or
#'   with an empty tail stand out; onsets that fall outside their run are
#'   counted in the caption.
#' @param title,subtitle For `plot()`, optional title and subtitle; the
#'   defaults describe the acquisition (runs, scans, TR, total duration).
#' @method plot sampling_frame
#' @export
plot.sampling_frame <- function(x, style = c("timeline", "grid", "lane"),
                                show_ticks = FALSE, tick_every = 5,
                                events = NULL, title = NULL, subtitle = NULL,
                                ...) {
  style <- match.arg(style)
  bl <- fmrihrf::blocklens(x)
  runs <- .fd_timeline(x, sum(bl), block_x = "global")$runs
  runs$dur <- runs$end - runs$start
  n_runs <- nrow(runs)
  # The grid's default ruler groups 10 scans once runs get long.
  if (missing(tick_every) && style == "grid" && max(bl) > 100) tick_every <- 10
  tick_every <- max(1L, as.integer(tick_every))
  TR <- unique(runs$TR)
  per_run_tr <- length(TR) > 1L
  total <- sum(runs$dur)

  sub_default <- sprintf(.fd_u("%d run%s {dot} %d scans {dot} TR %s {dot} total %s"),
                         n_runs, if (n_runs == 1L) "" else "s", sum(runs$scans),
                         .fd_sf_tr_text(TR), .fd_mmss(total))
  # Runs whose length differs from the most common length are flagged with
  # a bold label and the difference in scans.
  tab <- table(runs$scans)
  modal <- as.integer(names(tab)[which.max(tab)])
  # Only meaningful when most runs share one length.
  has_mode <- n_runs > 2L && max(tab) > 1L && max(tab) >= n_runs / 2
  runs$odd <- has_mode & runs$scans != modal
  delta <- ifelse(runs$odd, .fd_u(sprintf(" (%s%d)", ifelse(runs$scans > modal, "+", "{minus}"),
                                          abs(runs$scans - modal))), "")
  runs$lab <- paste0(runs$scans, " scans", delta, .fd_u(" {dot} "),
                     if (per_run_tr) paste0("TR ", .fd_sf_num(runs$TR), .fd_u(" s {dot} ")) else "",
                     .fd_mmss(runs$dur))
  runs$face <- ifelse(runs$odd, "bold", "plain")
  odd_note <- if (any(runs$odd)) {
    sprintf("Bold: run length differs from the most common length (%d scans).", modal)
  }

  ev <- if (!is.null(events)) .fd_sf_events(events, runs) else NULL
  ev_note <- if (!is.null(ev)) attr(ev, "note")

  # Run 1 at the top (timeline and grid).
  runs$y <- n_runs - runs$run + 1
  y_scale <- ggplot2::scale_y_continuous(breaks = runs$y,
                                         labels = paste("Run", runs$run),
                                         expand = ggplot2::expansion(
                                           add = 0.5 + max(0, 4 - n_runs) * 0.3))
  half <- 0.36
  # Approximate width of one label character in data units (3.2 mm text on
  # a panel about 8 in wide).
  char_w <- total / 115
  pad <- total * 0.008

  if (style == "timeline") {
    # One placement for every label: inside the bars when all fit, else
    # outside to the right of every bar.
    need <- nchar(runs$lab) * char_w
    inside <- all(need < runs$dur * 0.85)
    runs$lx <- if (inside) runs$start + pad else runs$end + pad
    tcol <- if (inside) "#FFFFFF" else .fd_ink$text2
    right_room <- if (inside) 0.015 * total else max(runs$end + pad + need) - total + 0.01 * total

    p <- ggplot2::ggplot(runs) +
      ggplot2::geom_rect(ggplot2::aes(xmin = .data$start, xmax = .data$end,
                                      ymin = .data$y - half, ymax = .data$y + half),
                         fill = .fd_cat[1], colour = NA)
    if (isTRUE(show_ticks)) {
      ticks <- do.call(rbind, lapply(seq_len(n_runs), function(r) {
        if (runs$scans[r] <= tick_every) return(NULL)
        k <- seq(tick_every, runs$scans[r] - 1L, by = tick_every)
        data.frame(x = runs$start[r] + k * runs$TR[r], y = runs$y[r])
      }))
      if (!is.null(ticks)) {
        p <- p + ggplot2::geom_segment(
          data = ticks,
          ggplot2::aes(x = .data$x, xend = .data$x, y = .data$y - half,
                       yend = .data$y - half * 0.4),
          colour = "#FFFFFF", linewidth = 0.3, alpha = 0.85)
      }
    }
    if (!is.null(ev) && nrow(ev)) {
      ev$x <- runs$start[ev$run] + ev$onset
      ev$y <- runs$y[ev$run]
      p <- p + ggplot2::geom_segment(
        data = ev,
        ggplot2::aes(x = .data$x, xend = .data$x, y = .data$y - half - 0.04,
                     yend = .data$y - half - 0.16),
        colour = .fd_ink$text, linewidth = 0.3)
    }
    p <- p +
      ggplot2::geom_text(ggplot2::aes(x = .data$lx, y = .data$y, label = .data$lab,
                                      fontface = .data$face),
                         hjust = 0, size = 3.2, colour = tcol) +
      ggplot2::scale_x_continuous(
        breaks = .fd_sf_breaks(runs, total), labels = .fd_mmss,
        expand = ggplot2::expansion(add = c(0.01 * total, right_room))) +
      y_scale +
      ggplot2::labs(title = title %||% "Acquisition timeline",
                    subtitle = subtitle %||% sub_default,
                    x = .fd_time_label("global"), y = NULL,
                    caption = .fd_sf_caption(c(
                      if (n_runs > 1L) "Runs are concatenated on one time axis.",
                      if (isTRUE(show_ticks)) sprintf("Ticks every %d scans.", tick_every),
                      ev_note, odd_note)))
  } else if (style == "lane") {
    # One lane of adjacent run segments, annotated above each segment.
    # Annotations that would collide are stacked on further rows, with a
    # leader line down to their segment.
    # Two-line annotations ("Run k" over the details) when they fit side by
    # side; otherwise one-line annotations stacked on rows.
    runs$mid <- (runs$start + runs$end) / 2
    cw <- total / 125
    w <- pmax(nchar(runs$lab), nchar(paste("Run", runs$run))) * cw
    two_line <- all(w <= runs$dur * 0.95)
    if (two_line) {
      runs$lx <- runs$mid
      runs$row <- 0L
      runs$ly <- 1.5
    } else {
      runs$lab1 <- paste0("Run ", runs$run, ": ", runs$lab)
      w <- nchar(runs$lab1) * cw
      runs$lx <- pmin(pmax(runs$mid, w / 2), total - w / 2)
      runs$row <- .fd_sf_stack(runs$lx - w / 2, runs$lx + w / 2, gap = total * 0.01)
      runs$ly <- 1.55 + runs$row * 0.55
    }
    stagger <- !two_line
    p <- ggplot2::ggplot(runs) +
      ggplot2::geom_rect(ggplot2::aes(xmin = .data$start, xmax = .data$end,
                                      ymin = 0.6, ymax = 1.4),
                         fill = .fd_cat[1], colour = "#FFFFFF", linewidth = 0.8)
    if (stagger) {
      p <- p + ggplot2::geom_segment(
        ggplot2::aes(x = .data$mid, xend = .data$lx, y = 1.42, yend = .data$ly - 0.05),
        colour = .fd_ink$rule, linewidth = 0.3)
    }
    if (!is.null(ev) && nrow(ev)) {
      ev$x <- runs$start[ev$run] + ev$onset
      p <- p + ggplot2::geom_segment(
        data = ev, ggplot2::aes(x = .data$x, xend = .data$x, y = 0.55, yend = 0.35),
        colour = .fd_ink$text, linewidth = 0.3)
    }
    p <- if (two_line) {
      p +
        ggplot2::geom_text(ggplot2::aes(x = .data$lx, y = .data$ly + 0.42,
                                        label = paste("Run", .data$run)),
                           vjust = 0, size = 3.3, fontface = "bold", colour = .fd_ink$text) +
        ggplot2::geom_text(ggplot2::aes(x = .data$lx, y = .data$ly, label = .data$lab,
                                        fontface = .data$face),
                           vjust = 0, size = 3, colour = .fd_ink$text2)
    } else {
      p + ggplot2::geom_text(ggplot2::aes(x = .data$lx, y = .data$ly, label = .data$lab1,
                                          fontface = .data$face),
                             vjust = 0, size = 3, colour = .fd_ink$text2)
    }
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = .fd_sf_breaks(runs, total), labels = .fd_mmss,
        expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
      ggplot2::scale_y_continuous(breaks = NULL,
                                  limits = c(if (is.null(ev)) 0.5 else 0.3,
                                             max(runs$ly) + if (two_line) 0.95 else 0.5),
                                  expand = c(0, 0)) +
      ggplot2::labs(title = title %||% "Acquisition timeline",
                    subtitle = subtitle %||% sub_default,
                    x = .fd_time_label("global"), y = NULL,
                    caption = .fd_sf_caption(c(
                      if (n_runs > 1L) "Runs are concatenated on one time axis.",
                      ev_note, odd_note)))
  } else {
    # Grid: one cell per scan on a run-relative scan index; cells alternate
    # shade in groups of `tick_every` scans so counts read like a ruler.
    cells <- do.call(rbind, lapply(seq_len(n_runs), function(r) {
      s <- seq_len(runs$scans[r])
      data.frame(y = runs$y[r], scan = s,
                 grp = ((s - 1L) %/% tick_every) %% 2L)
    }))
    cells$fill <- ifelse(cells$grp == 0L, .fd_cat[1], "#5D9BCB")
    fine <- max(runs$scans) <= 60
    n_max <- max(runs$scans)
    runs$lx <- runs$scans + 0.5 + n_max * 0.01
    p <- ggplot2::ggplot(cells) +
      ggplot2::geom_tile(ggplot2::aes(x = .data$scan, y = .data$y, fill = .data$fill),
                         width = 1, height = half * 2,
                         colour = if (fine) "#FFFFFF" else NA,
                         linewidth = if (fine) 0.4 else 0) +
      ggplot2::scale_fill_identity()
    if (!is.null(ev) && nrow(ev)) {
      # Scan k covers [(k - 1) TR, k TR) within its run.
      ev$x <- ev$onset / runs$TR[ev$run] + 0.5
      ev$y <- runs$y[ev$run]
      p <- p + ggplot2::geom_segment(
        data = ev,
        ggplot2::aes(x = .data$x, xend = .data$x, y = .data$y - half - 0.04,
                     yend = .data$y - half - 0.16),
        colour = .fd_ink$text, linewidth = 0.3)
    }
    p <- p +
      ggplot2::geom_text(data = runs,
                         ggplot2::aes(x = .data$lx, y = .data$y, label = .data$lab,
                                      fontface = .data$face),
                         hjust = 0, size = 3.2, colour = .fd_ink$text2) +
      ggplot2::scale_x_continuous(
        breaks = function(l) {
          b <- pretty(c(1, n_max))
          b[b >= 1 & b <= n_max]
        },
        expand = ggplot2::expansion(mult = c(0.005, 0.2))) +
      y_scale +
      ggplot2::labs(title = title %||% "Scans per run",
                    subtitle = subtitle %||% sub_default,
                    x = "Scan (within run)", y = NULL,
                    caption = .fd_sf_caption(c(
                      sprintf("Shading alternates every %d scans.", tick_every),
                      ev_note, odd_note)))
  }

  p +
    theme_fmridesign() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = if (style == "grid") {
        ggplot2::element_line(colour = .fd_ink$grid, linewidth = 0.3)
      } else {
        ggplot2::element_blank()
      },
      axis.text.y = ggplot2::element_text(colour = .fd_ink$text, face = "bold",
                                          size = 11 * 0.85),
      legend.position = "none")
}

# Greedy interval stacking: the lowest row (0, 1, ...) on which each
# [left, right] interval does not overlap an earlier one.
.fd_sf_stack <- function(left, right, gap = 0) {
  row_end <- numeric(0)
  out <- integer(length(left))
  for (i in order(left)) {
    free <- which(row_end + gap <= left[i])
    r <- if (length(free)) free[1] else length(row_end) + 1L
    row_end[r] <- right[i]
    out[i] <- r - 1L
  }
  out
}

.fd_sf_caption <- function(parts) {
  parts <- parts[nzchar(parts)]
  if (length(parts)) paste(parts, collapse = " ") else NULL
}

# Onsets (run-relative seconds) and run indices for the event overlay, from
# an event_model or a data frame with onset and run/block columns. Onsets
# outside their run, or in runs the frame does not have, are dropped and
# counted in attr "note", which also names runs without any events.
.fd_sf_events <- function(events, runs) {
  if (inherits(events, "event_model")) {
    ev <- do.call(rbind, lapply(events$terms, function(t) {
      if (is.null(t$onsets) || is.null(t$blockids)) return(NULL)
      data.frame(onset = as.numeric(t$onsets), run = as.integer(t$blockids))
    }))
    if (is.null(ev)) stop("Could not find event onsets in the event_model.", call. = FALSE)
    ev <- unique(ev)
  } else if (is.data.frame(events)) {
    run_col <- intersect(c("run", "block"), names(events))[1]
    if (!"onset" %in% names(events) || is.na(run_col)) {
      stop("`events` must be an event_model or a data frame with `onset` and ",
           "`run` (or `block`) columns.", call. = FALSE)
    }
    ev <- data.frame(onset = as.numeric(events$onset),
                     run = as.integer(factor(events[[run_col]])))
    if (is.numeric(events[[run_col]])) ev$run <- as.integer(events[[run_col]])
  } else {
    stop("`events` must be an event_model or a data frame with `onset` and ",
         "`run` (or `block`) columns.", call. = FALSE)
  }
  ev <- ev[is.finite(ev$onset) & !is.na(ev$run), , drop = FALSE]
  known <- ev$run %in% runs$run
  inside <- known
  inside[known] <- ev$onset[known] >= 0 &
    ev$onset[known] < runs$dur[match(ev$run[known], runs$run)]
  n_out <- sum(!inside)
  kept <- ev[inside, , drop = FALSE]
  empty <- setdiff(runs$run, kept$run)
  note <- c(
    sprintf("Ticks below each run: %d event onset%s.", nrow(kept),
            if (nrow(kept) == 1L) "" else "s"),
    if (n_out) sprintf("%d onset%s outside the sampled runs not shown.", n_out,
                       if (n_out == 1L) "" else "s"),
    if (length(empty)) sprintf("No events in run%s %s.", if (length(empty) == 1L) "" else "s",
                               paste(empty, collapse = ", ")))
  attr(kept, "note") <- paste(note, collapse = " ")
  kept
}

.fd_sf_num <- function(v) vapply(v, function(z) format(z, trim = TRUE), character(1))

# "2 s" for a single TR, "1-2 s (varies by run)" otherwise.
.fd_sf_tr_text <- function(TR) {
  if (length(TR) == 1L) return(paste0(.fd_sf_num(TR), " s"))
  paste0(.fd_sf_num(min(TR)), .fd_u("{ndash}"), .fd_sf_num(max(TR)), " s (varies by run)")
}

# Timeline breaks at run boundaries, thinned so labels do not collide. A
# single run gets round breaks instead.
.fd_sf_breaks <- function(runs, total) {
  if (nrow(runs) == 1L) {
    if (total < 150) return(seq(0, total, by = if (total > 60) 30 else 10))
    return(.fd_bl_time_breaks(total))
  }
  b <- sort(unique(c(runs$start, runs$end)))
  keep <- b[1]
  for (v in b[-1]) if (v - keep[length(keep)] >= total * 0.06) keep <- c(keep, v)
  if (utils::tail(keep, 1) != utils::tail(b, 1)) keep[length(keep)] <- utils::tail(b, 1)
  keep
}
