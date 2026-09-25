# Heatmap views of a design: design_map(), correlation_map(), plot_contrasts().
#
# All three draw one matrix cell per (row, design column) on a single panel,
# with columns in design order, grouped by term. Term spanners (a label over a
# thin rule) sit above the panel, thin rules separate groups, and every label
# comes from `.fd_colinfo()` so the same regressor reads identically across
# the family. Each view also carries a diagnostic: column magnitudes
# (design_map), variance inflation (correlation_map), and contrast standard
# errors (plot_contrasts).

# ---------------------------------------------------------------------------
# Shared heatmap helpers
# ---------------------------------------------------------------------------

# Runs of consecutive equal keys: one row per group with its first/last
# position and centre, used for spanners and separators.
.fd_hm_groups <- function(key, label = key) {
  key <- as.character(key)
  if (length(key) == 0L) {
    return(data.frame(start = integer(0), end = integer(0), centre = numeric(0),
                      label = character(0), stringsAsFactors = FALSE))
  }
  r <- rle(key)
  end <- cumsum(r$lengths)
  start <- end - r$lengths + 1L
  data.frame(start = start, end = end, centre = (start + end) / 2,
             label = as.character(label)[start], stringsAsFactors = FALSE)
}

# Positions (in cell units) of the rules between consecutive groups.
.fd_hm_boundaries <- function(groups) {
  if (nrow(groups) < 2L) return(numeric(0))
  groups$end[-nrow(groups)] + 0.5
}

# Which of n labels to show: all when they fit, otherwise every k-th.
.fd_hm_thin <- function(n, max_labels) {
  if (n <= max_labels) return(seq_len(n))
  k <- ceiling(n / max_labels)
  seq(1L, n, by = k)
}

# Axis-text element for column labels. Horizontal when the labels plausibly
# fit their cells (assuming a panel `panel_in` inches wide), otherwise angled
# or vertical. Multi-line labels are measured by their longest line.
.fd_hm_x_text <- function(labels, rotate = TRUE, panel_in = 6) {
  n <- length(labels)
  if (!isTRUE(rotate) || n == 0L) return(ggplot2::element_text(lineheight = 1.05))
  lines <- unlist(strsplit(labels, "\n", fixed = TRUE))
  widest_in <- max(nchar(lines), 1L) * 0.075
  if (widest_in <= panel_in / n * 0.9) {
    ggplot2::element_text(lineheight = 1.05)
  } else if (n <= 16L) {
    ggplot2::element_text(angle = 40, hjust = 1, vjust = 1)
  } else {
    ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
  }
}

# The base pdf() and postscript() devices cannot draw some typographic
# characters (they warn and substitute), so labels fall back to ASCII there.
# `options(fmridesign.plot_unicode = TRUE/FALSE)` overrides the guess.
.fd_hm_unicode <- function() {
  opt <- getOption("fmridesign.plot_unicode")
  if (!is.null(opt)) return(isTRUE(opt))
  !names(grDevices::dev.cur()) %in% c("pdf", "postscript", "xfig", "pictex")
}

.fd_hm_ascii <- function(s) {
  if (.fd_hm_unicode()) return(s)
  s <- gsub(.fd_u("{minus}"), "-", s, fixed = TRUE)
  s <- gsub(.fd_u("{ndash}"), "-", s, fixed = TRUE)
  s <- gsub(intToUtf8(0x03C3L), "sigma", s, fixed = TRUE)
  gsub(intToUtf8(0x2265L), ">=", s, fixed = TRUE)
}

.fd_hm_num <- function(x, digits = 2) {
  out <- .fd_hm_ascii(.fd_num(x, digits))
  out[is.na(x)] <- ""
  out
}

# Legend tick labels with a true minus sign.
.fd_hm_tick_labels <- function(b) .fd_hm_num(b, 2)

.fd_hm_ge <- function() intToUtf8(0x2265L)
.fd_hm_pm <- function() intToUtf8(0x00B1L)
.fd_hm_sep <- function() .fd_u(" {dot} ")

# Correlations without the leading zero: ".28", "-.3".
.fd_hm_fmt_r <- function(r, digits = 2) {
  s <- formatC(r, format = "f", digits = digits)
  s <- sub("^(-?)0\\.", "\\1.", s)
  s[grepl("^-?\\.0+$", s)] <- "0"
  s[is.na(r)] <- ""
  .fd_hm_ascii(gsub("-", .fd_u("{minus}"), s, fixed = TRUE))
}

# Contrast weights as "+1", "-0.5", "+0.333" (signed, trailing zeros dropped);
# exact zeros print as "0".
.fd_hm_fmt_w <- function(w, digits = 3) {
  s <- trimws(formatC(abs(w), digits = digits, format = "fg"))
  s <- ifelse(w < 0, paste0(.fd_u("{minus}"), s), paste0("+", s))
  s[!is.na(w) & w == 0] <- "0"
  s[is.na(w)] <- ""
  .fd_hm_ascii(s)
}

# A column's raw range as "0 to 1.9". A minimum (or maximum) on the other
# side of zero is shown only when it exceeds 5% of the column's peak, so HRF
# undershoots do not make unsigned regressors look signed.
.fd_hm_range_label <- function(lo, hi) {
  peak <- pmax(abs(lo), abs(hi))
  lo_d <- ifelse(lo < 0 & hi > 0 & abs(lo) <= 0.05 * peak, 0, lo)
  hi_d <- ifelse(hi > 0 & lo < 0 & abs(hi) <= 0.05 * peak, 0, hi)
  paste0(.fd_hm_num(lo_d), " to ", .fd_hm_num(hi_d))
}

# Text colour that stays readable on a diverging fill of relative strength v.
.fd_hm_text_col <- function(v, threshold = 0.6) {
  out <- ifelse(abs(v) >= threshold, "#FFFFFF", .fd_ink$text)
  out[is.na(out)] <- .fd_ink$text
  out
}

.fd_hm_bar <- function() {
  ggplot2::guide_colourbar(barwidth = grid::unit(7, "lines"),
                           barheight = grid::unit(0.45, "lines"),
                           title.vjust = 0.9)
}

# Signed fill on the family ramp. The shared `.fd_scale_signed()` covers the
# usual case (symmetric about 0); explicit limits or a non-zero midpoint use
# the same ramp with the midpoint at the centre of `limits`.
.fd_hm_fill_signed <- function(limit, name, limits = NULL, ...) {
  if (is.null(limits)) {
    return(.fd_scale_signed(limit = limit, name = name, guide = .fd_hm_bar(),
                            na.value = .fd_ink$band, labels = .fd_hm_tick_labels, ...))
  }
  ggplot2::scale_fill_gradientn(colours = .fd_div, limits = limits, name = name,
                                oob = .fd_squish, na.value = .fd_ink$band,
                                labels = .fd_hm_tick_labels, guide = .fd_hm_bar(), ...)
}

# Theme additions common to the heatmaps. The top axis carries no labels;
# its text margin reserves room for the spanners, which are drawn as
# annotations (see `.fd_hm_spanners()`).
.fd_hm_theme <- function(x_text, reserve_pt = 18) {
  ggplot2::theme(
    panel.grid.major   = ggplot2::element_blank(),
    panel.grid.minor   = ggplot2::element_blank(),
    axis.ticks         = ggplot2::element_blank(),
    axis.text.x.bottom = x_text,
    axis.text.x.top    = ggplot2::element_text(size = 1,
                                               margin = ggplot2::margin(b = reserve_pt)),
    axis.text.y        = ggplot2::element_text(colour = .fd_ink$text2),
    axis.title         = ggplot2::element_blank()
  )
}

# Secondary x axis with blank labels: it exists only to reserve the spanner
# band above the panel.
.fd_hm_reserve_axis <- function() {
  ggplot2::dup_axis(labels = function(b) rep("", length(b)), name = NULL)
}

# Fix the panel size in inches where the installed ggplot2 supports it
# (>= 3.5.2); otherwise leave sizing to ggplot2 (optionally via an aspect
# ratio).
.fd_hm_panel_size <- function(width_in = NULL, height_in = NULL, aspect = NULL) {
  if (utils::packageVersion("ggplot2") >= "3.5.2") {
    args <- list()
    if (!is.null(width_in)) args$panel.widths <- grid::unit(width_in, "in")
    if (!is.null(height_in)) args$panel.heights <- grid::unit(height_in, "in")
    return(do.call(ggplot2::theme, args))
  }
  if (!is.null(aspect)) ggplot2::theme(aspect.ratio = aspect) else ggplot2::theme()
}

.fd_hm_plural <- function(n, word) {
  paste0(format(n, big.mark = ","), " ", word, if (n == 1L) "" else "s")
}

# Name a few items, count many.
.fd_hm_name_or_count <- function(items, word, max_named = 3) {
  if (length(items) <= max_named) paste(items, collapse = ", ")
  else .fd_hm_plural(length(items), word)
}

# Column metadata ordered for display. Baseline designs are regrouped by term
# and then run so each term's block-diagonal staircase reads as one unit.
.fd_hm_columns <- function(x) {
  ci <- .fd_colinfo(x)
  if (inherits(x, "baseline_model")) {
    term_rank <- match(ci$term, unique(ci$term))
    run <- ifelse(is.na(ci$run), 0L, ci$run)
    ci <- ci[order(term_rank, run, ci$col), , drop = FALSE]
    ci$term_label[ci$term == "block"] <- "intercept"
  }
  ci
}

# Per-scan run ids from the model's sampling frame.
.fd_hm_scan_runs <- function(x, n) .fd_timeline(x$sampling_frame, n)$run

# ---------------------------------------------------------------------------
# Spanners: a label above a thin rule, spanning a group of columns
# ---------------------------------------------------------------------------

# A spanner is drawn in the panel's coordinate system for its x-range (via
# annotation_custom) but offset in points above the panel, so its position
# does not depend on the figure size. At draw time a label that does not fit
# the group (allowing `overflow_pt` of spill into the gaps beside it) falls
# back to `short`, then to nothing; the size never changes, so every spanner
# in a tier reads alike.
.fd_hm_spanner_grob <- function(label, short, text_pt, rule_pt, size, face, col,
                                overflow_pt) {
  grid::gTree(label = label, short = short, text_pt = text_pt, rule_pt = rule_pt,
              size = size, face = face, col = col, overflow_pt = overflow_pt,
              cl = "fd_hm_spanner")
}

#' @exportS3Method grid::makeContent
makeContent.fd_hm_spanner <- function(x) {
  gp <- grid::gpar(fontsize = x$size, fontface = x$face, col = x$col)
  width <- grid::convertWidth(grid::unit(1, "npc"), "pt", valueOnly = TRUE)
  fits <- function(s) {
    w <- grid::convertWidth(grid::grobWidth(grid::textGrob(s, gp = gp)), "pt",
                            valueOnly = TRUE)
    w <= width - 4 + x$overflow_pt
  }
  label <- x$label
  if (!fits(label)) label <- if (fits(x$short)) x$short else ""
  kids <- grid::gList(grid::textGrob(
    label, x = grid::unit(0.5, "npc"),
    y = grid::unit(1, "npc") + grid::unit(x$text_pt, "pt"), vjust = 0, gp = gp))
  if (!is.na(x$rule_pt)) {
    pad <- grid::unit(max(0, min(3, width / 4)), "pt")
    y <- grid::unit(1, "npc") + grid::unit(x$rule_pt, "pt")
    kids <- grid::gList(kids, grid::segmentsGrob(
      x0 = pad, x1 = grid::unit(1, "npc") - pad, y0 = y, y1 = y,
      gp = grid::gpar(col = .fd_ink$muted, lwd = 0.9, lineend = "butt")))
  }
  grid::setChildren(x, kids)
}

# Annotation layers for one tier of spanners. `groups` has start/end/label
# (and optionally short); `text_pt`/`rule_pt` are offsets above the panel.
.fd_hm_spanners <- function(groups, text_pt, rule_pt = NA, size = 9.4,
                            face = "bold", col = .fd_ink$text, overflow_pt = 24) {
  if (is.null(groups) || nrow(groups) == 0L) return(list())
  short <- groups$short %||%
    ifelse(nchar(groups$label) > 5, paste0(substr(groups$label, 1, 3), "."), groups$label)
  lapply(seq_len(nrow(groups)), function(i) {
    ggplot2::annotation_custom(
      .fd_hm_spanner_grob(groups$label[i], short[i], text_pt, rule_pt, size, face, col,
                          overflow_pt),
      xmin = groups$start[i] - 0.5, xmax = groups$end[i] + 0.5,
      ymin = -Inf, ymax = Inf)
  })
}

# A text label placed in points to the right of the panel: one per row (for
# the margin columns of plot_contrasts), or (top = TRUE) a column header.
.fd_hm_margin_text <- function(label, y, col, x_pt = 10, size = 9, face = "plain",
                               top = FALSE) {
  g <- grid::textGrob(
    label, x = grid::unit(1, "npc") + grid::unit(x_pt, "pt"),
    y = if (top) grid::unit(1, "npc") + grid::unit(6, "pt") else grid::unit(0.5, "npc"),
    hjust = 0, vjust = if (top) 0 else 0.5,
    gp = grid::gpar(col = col, fontsize = size, fontface = face))
  if (top) {
    ggplot2::annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
  } else {
    ggplot2::annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = y - 0.5, ymax = y + 0.5)
  }
}

# ---------------------------------------------------------------------------
# Diagnostics
# ---------------------------------------------------------------------------

# Columns whose peak abs value is tiny next to the largest in their term (or
# zero): per-column rescaling would otherwise make them look healthy.
.fd_hm_near_flat <- function(mx, term, rel = 0.05) {
  ref <- stats::ave(mx, term, FUN = max)
  mx < .Machine$double.eps | mx < rel * ref
}

# Variance inflation factors of the design columns after removing each run's
# mean (i.e. with per-run intercepts in the model). VIF_j = 1 / (1 - R2_j),
# where R2_j comes from regressing column j on all the others. Columns that
# are an exact linear combination of others get Inf ("aliased"); columns that
# are constant within every run get NA.
.fd_hm_vif <- function(DM, run) {
  DM <- as.matrix(DM)
  Z <- DM - apply(DM, 2, function(v) stats::ave(v, run))
  p <- ncol(Z)
  tss <- colSums(Z^2)
  const <- tss <= 1e-12 * max(1, max(tss))
  vif <- rep(NA_real_, p)
  keep <- which(!const)
  for (j in keep) {
    others <- setdiff(keep, j)
    rss <- if (length(others)) {
      sum(qr.resid(qr(Z[, others, drop = FALSE]), Z[, j])^2)
    } else {
      tss[j]
    }
    vif[j] <- if (rss / tss[j] < 1e-8) Inf else tss[j] / rss
  }
  vif
}

# Moore-Penrose inverse of a symmetric positive semi-definite matrix.
.fd_hm_pinv <- function(A, tol = 1e-10) {
  s <- svd(A)
  pos <- s$d > tol * max(s$d)
  s$v[, pos, drop = FALSE] %*% (t(s$u[, pos, drop = FALSE]) / s$d[pos])
}

# Standard-error multiplier of each contrast: sqrt(c' (X'X)^+ c) with X the
# design columns plus one intercept per run, so SE(c'b) = multiplier * sigma.
# Returns NA for contrasts that are not estimable (c outside the row space of
# X) and 0 for all-zero contrasts.
.fd_hm_contrast_se <- function(DM, run, W) {
  DM <- as.matrix(DM)
  W <- as.matrix(W)
  runs <- sort(unique(run))
  X <- cbind(DM, vapply(runs, function(r) as.numeric(run == r), numeric(nrow(DM))))
  XtX <- crossprod(X)
  G <- .fd_hm_pinv(XtX)
  P <- XtX %*% G
  Ca <- rbind(W, matrix(0, length(runs), ncol(W)))
  vapply(seq_len(ncol(Ca)), function(k) {
    ca <- Ca[, k]
    if (all(ca == 0)) return(0)
    resid <- ca - drop(P %*% ca)
    if (sqrt(sum(resid^2)) > 1e-6 * sqrt(sum(ca^2))) return(NA_real_)
    sqrt(max(0, drop(crossprod(ca, G %*% ca))))
  }, numeric(1))
}

# ---------------------------------------------------------------------------
# design_map
# ---------------------------------------------------------------------------

.fd_hm_design_subtitle <- function(p, tl, n_scans) {
  tr_txt <- if (identical(tl$unit, "s")) {
    tr <- unique(tl$runs$TR)
    paste0(" (TR ", paste(format(tr, trim = TRUE), collapse = "/"), " s)")
  } else ""
  paste0(.fd_hm_plural(p, "regressor"), .fd_hm_sep(),
         .fd_hm_plural(nrow(tl$runs), "run"), .fd_hm_sep(),
         .fd_hm_plural(n_scans, "scan"), tr_txt)
}

.fd_hm_design_map <- function(x,
                              block_separators = TRUE,
                              rotate_x_text = TRUE,
                              fill_midpoint = NULL,
                              fill_limits = NULL,
                              scale = c("column", "none"),
                              palette = c("diverging", "grey"),
                              y_axis = c("scan", "time"),
                              title = "Design matrix",
                              subtitle = NULL,
                              max_labels = 60,
                              ...) {
  scale <- match.arg(scale)
  palette <- match.arg(palette)
  y_axis <- match.arg(y_axis)

  DM <- as.matrix(design_matrix(x))
  ci <- .fd_hm_columns(x)
  DM <- DM[, ci$col, drop = FALSE]
  n <- nrow(DM)
  p <- ncol(DM)
  tl <- .fd_timeline(x$sampling_frame, n)
  runs <- tl$runs
  is_baseline <- inherits(x, "baseline_model")

  mx <- apply(abs(DM), 2, max, na.rm = TRUE)
  mx[!is.finite(mx)] <- 0
  vals <- DM
  if (scale == "column") {
    vals <- sweep(DM, 2, ifelse(mx < .Machine$double.eps, 1, mx), "/")
  }
  flat <- .fd_hm_near_flat(mx, ci$term)
  term_max <- stats::ave(mx, ci$term, FUN = max)
  rel <- ifelse(term_max > 0, mx / term_max, 0)

  # Nuisance columns are drawn quieter so the block structure dominates.
  quiet <- is_baseline & ci$term == "nuisance"
  df <- data.frame(x = rep(seq_len(p), each = n),
                   y = rep(seq_len(n), times = p),
                   value = as.vector(vals))

  # Columns: groups by term, with light sub-rules inside a term between runs
  # (baseline) or between conditions that each carry several basis functions.
  groups <- .fd_hm_groups(ci$term, ci$term_label)
  bounds <- .fd_hm_boundaries(groups)
  sub_part <- if (is_baseline) {
    ifelse(is.na(ci$run), "", ci$run)
  } else {
    ifelse(is.na(ci$basis_ix), "", ci$label)
  }
  sub_groups <- .fd_hm_groups(paste(ci$term, sub_part))
  sub_rules <- setdiff(.fd_hm_boundaries(sub_groups), bounds)

  # Column labels, with each column's raw range beneath when there is room.
  col_labels <- ci$full_label
  if (is_baseline) {
    col_labels <- ci$label
    ic <- ci$term == "block" & !is.na(ci$run)
    col_labels[ic] <- paste("Run", ci$run[ic])
  }
  if (p <= 12L) {
    lo <- apply(DM, 2, min, na.rm = TRUE)
    hi <- apply(DM, 2, max, na.rm = TRUE)
    col_labels <- paste0(col_labels, "\n", .fd_hm_range_label(lo, hi))
  }
  x_show <- .fd_hm_thin(p, max_labels)

  # Magnitude strip under the matrix: one bar per column, its height the
  # column's peak abs value relative to the largest in its term.
  gap <- max(1, 0.012 * n)
  strip_h <- max(2, 0.035 * n)
  s_top <- n + 0.5 + gap
  s_bot <- s_top + strip_h
  inset <- if (p <= 60L) 0.12 else 0
  strip <- data.frame(xmin = seq_len(p) - 0.5 + inset, xmax = seq_len(p) + 0.5 - inset,
                      ymin = s_bot - pmax(rel, 0) * strip_h, ymax = s_bot)

  # Rows: runs labelled in the left margin; the right axis marks the first
  # scan of each run (and the last scan) by number or onset time.
  run_centres <- (runs$first_scan + runs$last_scan) / 2
  y_ticks <- unique(c(runs$first_scan, n))
  if (nrow(runs) == 1L) {
    inner <- pretty(c(1, n), n = 5)
    y_ticks <- sort(unique(c(1, inner[inner > 0.08 * n & inner < 0.92 * n], n)))
  }
  if (y_axis == "time" && identical(tl$unit, "s")) {
    onset <- c(0, cumsum(rep(runs$TR, runs$scans)))
    y_lab <- .fd_mmss(onset[y_ticks])
    y_name <- .fd_time_label("global")
  } else {
    y_lab <- format(y_ticks, trim = TRUE)
    y_name <- "Scan"
  }

  fill_name <- if (scale == "column") "Each column / its max abs value" else "Value"
  fill_scale <- if (palette == "grey") {
    ggplot2::scale_fill_gradientn(
      colours = c("#FFFFFF", .fd_ink$text),
      limits = fill_limits %||% range(vals, na.rm = TRUE), name = fill_name,
      oob = .fd_squish, na.value = .fd_ink$band, labels = .fd_hm_tick_labels,
      guide = .fd_hm_bar())
  } else if (is.null(fill_limits) && is.null(fill_midpoint)) {
    .fd_hm_fill_signed(max(abs(vals), na.rm = TRUE), fill_name)
  } else {
    mid <- fill_midpoint %||% 0
    half <- max(abs(vals - mid), na.rm = TRUE)
    .fd_hm_fill_signed(NULL, fill_name, limits = fill_limits %||% (mid + c(-half, half)))
  }

  plt <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = value)) +
    ggplot2::geom_raster(...) +
    fill_scale
  if (any(quiet)) {
    # A veil of the neutral midpoint colour mutes nuisance columns while
    # keeping their zero identical to the rest of the matrix.
    qg <- .fd_hm_groups(quiet)
    qg <- qg[qg$label == "TRUE", , drop = FALSE]
    plt <- plt + ggplot2::annotate("rect", xmin = qg$start - 0.5, xmax = qg$end + 0.5,
                                   ymin = 0.5, ymax = n + 0.5, fill = .fd_div[4],
                                   alpha = 0.45, colour = NA)
  }
  if (length(sub_rules)) {
    plt <- plt + ggplot2::annotate("segment", x = sub_rules, xend = sub_rules,
                                   y = 0.5, yend = n + 0.5, colour = .fd_ink$rule,
                                   linewidth = 0.3)
  }
  if (length(bounds)) {
    plt <- plt +
      ggplot2::annotate("segment", x = bounds, xend = bounds, y = 0.5, yend = s_bot,
                        colour = "#FFFFFF", linewidth = 1.2) +
      ggplot2::annotate("segment", x = bounds, xend = bounds, y = 0.5, yend = s_bot,
                        colour = .fd_ink$muted, linewidth = 0.35)
  }
  if (isTRUE(block_separators) && nrow(runs) > 1L) {
    plt <- plt + ggplot2::geom_hline(yintercept = runs$first_scan[-1] - 0.5,
                                     colour = .fd_ink$muted, linewidth = 0.35)
  }

  # The strip: baseline rule, grey bars, near-flat columns in warning orange
  # (also outlined over the matrix so they are found at any column count).
  plt <- plt +
    ggplot2::annotate("rect", xmin = strip$xmin, xmax = strip$xmax, ymin = strip$ymin,
                      ymax = strip$ymax, fill = .fd_ink$zero, colour = NA)
  if (any(flat)) {
    fx <- which(flat)
    plt <- plt +
      ggplot2::annotate("rect", xmin = fx - 0.5, xmax = fx + 0.5, ymin = s_top,
                        ymax = s_bot, fill = .fd_cat[2], colour = NA) +
      ggplot2::annotate("rect", xmin = fx - 0.5, xmax = fx + 0.5, ymin = 0.5,
                        ymax = n + 0.5, fill = NA, colour = .fd_cat[2],
                        linewidth = if (p <= 40L) 0.7 else 0.45)
  }

  # Spanners: term over a rule; for baseline designs a run tier sits beneath
  # (over multi-column run groups only; single-column groups such as the
  # per-run intercepts are labelled "Run k" on the column axis instead).
  if (is_baseline && nrow(runs) > 1L) {
    run_groups <- sub_groups
    run_no <- sub_part[run_groups$start]
    run_groups$label <- ifelse(run_no == "", "", paste("Run", run_no))
    run_groups$short <- run_no
    run_groups <- run_groups[nzchar(run_groups$label) &
                               run_groups$end > run_groups$start, , drop = FALSE]
    spanners <- c(.fd_hm_spanners(groups, text_pt = 19, rule_pt = 16),
                  .fd_hm_spanners(run_groups, text_pt = 3, size = 8.3, face = "plain",
                                  col = .fd_ink$text2, overflow_pt = 0))
    reserve <- 32
  } else {
    spanners <- .fd_hm_spanners(groups, text_pt = 6, rule_pt = 3)
    reserve <- 18
  }

  if (is.null(subtitle)) {
    subtitle <- .fd_hm_design_subtitle(p, tl, n)
    if (any(flat)) {
      subtitle <- paste0(subtitle, .fd_hm_sep(), "near-flat: ",
                         .fd_hm_name_or_count(ci$full_label[flat], "column"))
    }
  }
  caption <- paste0("Bars under the matrix: each column's peak abs value relative to ",
                    "the largest in its term; orange marks columns below 5%.")

  plt + spanners +
    ggplot2::scale_x_continuous(breaks = x_show, labels = col_labels[x_show],
                                expand = c(0, 0), sec.axis = .fd_hm_reserve_axis()) +
    ggplot2::scale_y_reverse(
      breaks = run_centres, labels = paste("Run", runs$run), expand = c(0, 0),
      limits = c(s_bot, 0.5),
      sec.axis = ggplot2::dup_axis(breaks = y_ticks, labels = y_lab, name = y_name)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    theme_fmridesign() +
    .fd_hm_theme(.fd_hm_x_text(col_labels[x_show], rotate_x_text), reserve) +
    ggplot2::theme(
      axis.text.y.left   = ggplot2::element_text(colour = .fd_ink$text, angle = 90,
                                                 hjust = 0.5, vjust = 0,
                                                 margin = ggplot2::margin(r = 4)),
      axis.text.y.right  = ggplot2::element_text(colour = .fd_ink$muted),
      axis.title.y.right = ggplot2::element_text(colour = .fd_ink$muted, angle = 90,
                                                 size = ggplot2::rel(0.85)),
      legend.position = "bottom",
      legend.justification = "right"
    )
}

#' Design matrix heatmap
#'
#' Draws the design matrix as an image in the style of SPM: one row per scan
#' (time runs downward), one column per regressor. Columns are grouped by
#' model term under a spanner, runs are separated by thin rules and labelled
#' in the left margin, and the right margin marks the first scan of each run.
#'
#' By default each column is divided by its maximum absolute value
#' (`scale = "column"`, stated in the legend) so regressors in different units
#' share one fill scale; zero is always the neutral midpoint of the
#' package's diverging palette. Because rescaling would make a near-flat
#' regressor look healthy, real magnitudes stay visible: a strip of bars under
#' the matrix shows each column's peak absolute value relative to the largest
#' in its term, and columns below 5% of that (or all zero) are marked in
#' orange, outlined over the matrix, and named in the subtitle. With 12 or
#' fewer columns each label also carries the column's raw range. Use
#' `scale = "none"` to plot raw values, or `palette = "grey"` for an SPM-style
#' greyscale.
#'
#' For baseline models, columns are grouped by term (drift, intercept,
#' nuisance) and then run, with a "Run k" sub-spanner over each run's
#' columns, so the block-diagonal structure reads as a staircase; nuisance
#' columns are drawn at reduced opacity so they do not dominate.
#'
#' @param x An `event_model` or `baseline_model`.
#' @param block_separators Logical; draw a rule at each run boundary.
#' @param rotate_x_text Logical; angle the column labels when they would
#'   otherwise overlap. With `FALSE`, labels are always horizontal.
#' @param fill_midpoint Numeric or `NULL`. Centre of the diverging fill
#'   scale; defaults to 0.
#' @param fill_limits Numeric length-2 vector or `NULL`. Fill-scale limits;
#'   by default symmetric about `fill_midpoint` (diverging) or the data range
#'   (grey).
#' @param scale `"column"` (default) divides each column by its maximum
#'   absolute value; `"none"` plots raw values.
#' @param palette `"diverging"` (default; the package's signed palette,
#'   symmetric about zero) or `"grey"` (white at the minimum to ink at the
#'   maximum, as in SPM).
#' @param y_axis Label the right-hand axis by `"scan"` number (default) or
#'   onset `"time"` (min:s, from the sampling frame).
#' @param title,subtitle Plot title and subtitle. The default subtitle states
#'   the number of regressors, runs, and scans, and the TR, and names any
#'   near-flat columns.
#' @param max_labels Maximum number of column labels to print; beyond this
#'   labels are thinned to every k-th column.
#' @param ... Passed to [ggplot2::geom_raster()].
#' @return A ggplot object.
#' @seealso [correlation_map()], [plot_contrasts()], [theme_fmridesign()]
#' @examples
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30, 5, 15, 25, 35),
#'   run = rep(1:2, each = 4),
#'   cond = factor(c("A", "B", "A", "B", "B", "A", "B", "A"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 2)
#' emod <- event_model(onset ~ hrf(cond), data = des, block = ~run,
#'                     sampling_frame = sframe)
#' design_map(emod)
#' design_map(emod, palette = "grey", y_axis = "time")
#' @method design_map event_model
#' @export
design_map.event_model <- function(x,
                                   block_separators = TRUE,
                                   rotate_x_text = TRUE,
                                   fill_midpoint = NULL,
                                   fill_limits = NULL,
                                   scale = c("column", "none"),
                                   palette = c("diverging", "grey"),
                                   y_axis = c("scan", "time"),
                                   title = "Design matrix",
                                   subtitle = NULL,
                                   max_labels = 60,
                                   ...) {
  .fd_hm_design_map(x, block_separators = block_separators,
                    rotate_x_text = rotate_x_text, fill_midpoint = fill_midpoint,
                    fill_limits = fill_limits, scale = scale, palette = palette,
                    y_axis = y_axis, title = title, subtitle = subtitle,
                    max_labels = max_labels, ...)
}

#' @rdname design_map.event_model
#' @examples
#' bmod <- baseline_model(basis = "poly", degree = 3, sframe = sframe)
#' design_map(bmod)
#' @method design_map baseline_model
#' @export
design_map.baseline_model <- function(x,
                                      block_separators = TRUE,
                                      rotate_x_text = TRUE,
                                      fill_midpoint = NULL,
                                      fill_limits = NULL,
                                      scale = c("column", "none"),
                                      palette = c("diverging", "grey"),
                                      y_axis = c("scan", "time"),
                                      title = "Baseline design matrix",
                                      subtitle = NULL,
                                      max_labels = 60,
                                      ...) {
  .fd_hm_design_map(x, block_separators = block_separators,
                    rotate_x_text = rotate_x_text, fill_midpoint = fill_midpoint,
                    fill_limits = fill_limits, scale = scale, palette = palette,
                    y_axis = y_axis, title = title, subtitle = subtitle,
                    max_labels = max_labels, ...)
}

# ---------------------------------------------------------------------------
# correlation_map
# ---------------------------------------------------------------------------

.fd_hm_correlation_map <- function(x,
                                   method = c("pearson", "spearman"),
                                   half_matrix = TRUE,
                                   limits = c("fixed", "data"),
                                   rotate_x_text = TRUE,
                                   annotate = NULL,
                                   flag_threshold = 0.5,
                                   vif_threshold = 5,
                                   title = "Regressor correlations",
                                   subtitle = NULL,
                                   ...) {
  method <- match.arg(method)
  limits <- match.arg(limits)
  DM <- as.matrix(design_matrix(x))
  ci <- .fd_hm_columns(x)
  DM <- DM[, ci$col, drop = FALSE]
  p <- ncol(DM)
  n <- nrow(DM)
  labs_full <- ci$full_label
  run <- .fd_hm_scan_runs(x, n)
  n_runs <- length(unique(run))

  flat <- apply(DM, 2, stats::sd, na.rm = TRUE) < .Machine$double.eps
  R <- suppressWarnings(stats::cor(DM, method = method, use = "pairwise.complete.obs"))
  R[flat, ] <- NA
  R[, flat] <- NA

  # Lower triangle below the diagonal (or every off-diagonal cell when
  # half_matrix = FALSE); the diagonal itself carries each column's VIF.
  ij <- expand.grid(i = seq_len(p), j = seq_len(p))
  ij <- ij[if (isTRUE(half_matrix)) ij$i > ij$j else ij$i != ij$j, , drop = FALSE]
  df <- data.frame(x = ij$j, y = ij$i, r = R[cbind(ij$i, ij$j)])

  low <- R
  low[upper.tri(low, diag = TRUE)] <- NA
  max_r <- suppressWarnings(max(abs(low), na.rm = TRUE))
  limit <- if (limits == "fixed" || !is.finite(max_r) || max_r == 0) 1 else max_r
  n_flag <- sum(abs(low) >= flag_threshold, na.rm = TRUE)

  vif <- if (p <= 150L) .fd_hm_vif(DM, run) else rep(NA_real_, p)
  aliased <- is.infinite(vif)
  high_vif <- aliased | (!is.na(vif) & vif >= vif_threshold)
  vif_lab <- ifelse(aliased, "aliased", ifelse(is.na(vif), "", .fd_hm_num(vif, 2)))
  if (p <= 10L) {
    vif_lab <- ifelse(is.finite(vif), paste("VIF", vif_lab), vif_lab)
  } else if (p > 20L) {
    vif_lab <- rep("", p)
  } else {
    vif_lab[aliased] <- "!"
  }
  diag_df <- data.frame(x = seq_len(p), y = seq_len(p), lab = vif_lab,
                        col = ifelse(high_vif, .fd_cat[2], .fd_ink$muted),
                        high = high_vif)

  if (is.null(subtitle)) {
    subtitle <- if (p < 2L) {
      "Only one regressor: nothing to correlate"
    } else if (!is.finite(max_r)) {
      "No regressor varies: correlations are undefined"
    } else {
      w <- which(abs(low) == max_r, arr.ind = TRUE)[1, ]
      s <- paste0("Max abs r = ", formatC(max_r, format = "f", digits = 2), ": ",
                  labs_full[w[[2]]], " vs ", labs_full[w[[1]]], .fd_hm_sep(),
                  n_flag, if (n_flag == 1L) " pair" else " pairs", " at abs r ",
                  .fd_hm_ge(), " ", format(flag_threshold))
      if (any(high_vif)) {
        s <- paste0(s, "\nVIF ", .fd_hm_ge(), " ", format(vif_threshold),
                    " or aliased: ",
                    .fd_hm_name_or_count(labs_full[high_vif], "column"))
      }
      .fd_hm_ascii(s)
    }
  }
  caption <- paste0(
    if (method == "spearman") "Spearman rank" else "Pearson",
    " r across all ", format(n, big.mark = ","), " scans",
    if (n_runs > 1L) " (runs concatenated)" else "",
    ".\nDiagonal: variance inflation factor of each column, with run means removed",
    if (n_runs > 1L) " (per-run intercepts)" else "", ".")

  x_idx <- y_idx <- seq_len(p)
  groups <- .fd_hm_groups(ci$term, ci$term_label)
  bounds <- .fd_hm_boundaries(groups)

  annotate <- annotate %||% (p <= 20L)
  digits <- if (p <= 10L) 2L else 1L
  df$lab <- if (isTRUE(annotate)) .fd_hm_fmt_r(df$r, digits) else ""
  df$tcol <- .fd_hm_text_col(df$r / limit)
  text_size <- if (p <= 8L) 3.4 else if (p <= 14L) 2.9 else 2.4
  flagged <- df[!is.na(df$r) & abs(df$r) >= flag_threshold, , drop = FALSE]

  # Group rules inside the triangle. A vertical rule starts one row below
  # the diagonal, so it never runs alongside a diagonal (VIF) cell.
  rules <- NULL
  if (length(bounds)) {
    if (isTRUE(half_matrix)) {
      v <- data.frame(x = bounds, xend = bounds, y = bounds + 1, yend = p + 0.5)
      v <- v[v$y < v$yend, , drop = FALSE]
      rules <- rbind(v, data.frame(x = 0.5, xend = bounds, y = bounds, yend = bounds))
    } else {
      rules <- rbind(data.frame(x = bounds, xend = bounds, y = 0.5, yend = p + 0.5),
                     data.frame(x = 0.5, xend = p + 0.5, y = bounds, yend = bounds))
    }
  }

  legend_name <- if (limit == 1) {
    if (method == "spearman") "Spearman r" else "r"
  } else {
    paste0("r (", .fd_hm_pm(), "max abs r)")
  }
  plt <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_tile(ggplot2::aes(fill = r), colour = "#FFFFFF",
                       linewidth = if (p <= 30L) 0.5 else 0.1, ...) +
    .fd_hm_fill_signed(limit, legend_name,
                       breaks = if (limit == 1) c(-1, -0.5, 0, 0.5, 1)
                                else ggplot2::waiver())
  if (!is.null(rules) && nrow(rules)) {
    plt <- plt + ggplot2::geom_segment(
      data = rules, ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE, colour = .fd_ink$muted, linewidth = 0.4)
  }
  inset <- 0.06
  lw_flag <- if (p <= 30L) 0.7 else 0.35
  if (nrow(flagged)) {
    plt <- plt + ggplot2::geom_rect(
      data = flagged,
      ggplot2::aes(xmin = x - 0.5 + inset, xmax = x + 0.5 - inset,
                   ymin = y - 0.5 + inset, ymax = y + 0.5 - inset),
      inherit.aes = FALSE, fill = NA, colour = .fd_ink$text, linewidth = lw_flag)
  }
  if (any(diag_df$high)) {
    plt <- plt + ggplot2::geom_rect(
      data = diag_df[diag_df$high, , drop = FALSE],
      ggplot2::aes(xmin = x - 0.5 + inset, xmax = x + 0.5 - inset,
                   ymin = y - 0.5 + inset, ymax = y + 0.5 - inset),
      inherit.aes = FALSE, fill = NA, colour = .fd_cat[2], linewidth = lw_flag)
  }
  if (any(nzchar(df$lab))) {
    plt <- plt + ggplot2::geom_text(ggplot2::aes(label = lab, colour = tcol),
                                    size = text_size, show.legend = FALSE)
  }
  if (any(nzchar(diag_df$lab))) {
    plt <- plt + ggplot2::geom_text(
      data = diag_df[nzchar(diag_df$lab), , drop = FALSE],
      ggplot2::aes(x = x, y = y, label = lab, colour = col), inherit.aes = FALSE,
      size = text_size * 0.8, show.legend = FALSE)
  }
  plt <- plt + ggplot2::scale_colour_identity()

  shown <- .fd_hm_thin(p, 60)
  cell_in <- 0.6
  plt <- plt + .fd_hm_spanners(groups, text_pt = 6, rule_pt = 3) +
    ggplot2::scale_x_continuous(
      breaks = shown, labels = labs_full[shown], expand = c(0, 0),
      limits = c(0.5, p + 0.5), sec.axis = .fd_hm_reserve_axis()
    ) +
    ggplot2::scale_y_reverse(breaks = shown, labels = labs_full[shown],
                             expand = c(0, 0), limits = c(p + 0.5, 0.5)) +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    theme_fmridesign() +
    .fd_hm_theme(.fd_hm_x_text(labs_full[shown], rotate_x_text,
                               panel_in = min(4.5, p * cell_in))) +
    if (p <= 10L) {
      # Small capped panels leave no room inside; put the legend underneath.
      ggplot2::theme(legend.position = "bottom", legend.justification = "right")
    } else {
      ggplot2::theme(
        legend.position = "inside",
        legend.position.inside = c(0.99, 0.97),
        legend.justification = c(1, 1),
        legend.direction = "horizontal",
        legend.title.position = "top"
      )
    }
  # Cap the cell size so a few regressors do not become one huge pale square.
  if (p * cell_in <= 5) {
    plt <- plt + .fd_hm_panel_size(p * cell_in, p * cell_in)
  }
  plt
}

#' Regressor correlation heatmap
#'
#' Shows the pairwise correlations between design-matrix columns as the lower
#' triangle of a matrix. The fill is the package's diverging palette, fixed
#' at \[-1, 1\] by default with a neutral zero; for up to 20 columns each cell
#' also prints r (two decimals for up to 10 columns). Cells with abs(r) at or
#' above `flag_threshold` are outlined and counted in the subtitle, which
#' also names the most strongly correlated pair.
#'
#' Pairwise correlations miss multicollinearity, so the diagonal shows each
#' column's variance inflation factor, VIF_j = 1 / (1 - R2_j), from
#' regressing column j on all other columns after removing each run's mean
#' (per-run intercepts). VIFs at or above `vif_threshold` are drawn in a
#' warning colour, and a column that is an exact linear combination of
#' others is marked "aliased".
#'
#' Correlations are computed over all scans of the full design matrix, with
#' runs concatenated (as stated in the caption). Columns keep design order and
#' are grouped by term, so the map lines up with [design_map()].
#'
#' @param x An `event_model` or `baseline_model`.
#' @param method Correlation method, `"pearson"` (default) or `"spearman"`.
#'   VIFs are always based on linear regression.
#' @param half_matrix Logical; if `TRUE` (default) draw only the lower
#'   triangle. With `FALSE`, both triangles are drawn.
#' @param limits `"fixed"` (default) spans the fill scale over \[-1, 1\];
#'   `"data"` makes it symmetric about 0 up to the largest abs(r), and says
#'   so in the legend title.
#' @param absolute_limits Deprecated logical kept for compatibility; `FALSE`
#'   is equivalent to `limits = "data"`.
#' @param rotate_x_text Logical; angle column labels when they would overlap.
#' @param annotate Logical or `NULL`; print r in each cell. `NULL` (default)
#'   annotates when there are at most 20 columns.
#' @param flag_threshold Cells with abs(r) at or above this value are
#'   outlined.
#' @param vif_threshold VIFs at or above this value are flagged.
#' @param title,subtitle Plot title and subtitle.
#' @param ... Passed to [ggplot2::geom_tile()].
#' @return A ggplot object.
#' @seealso [design_map()], [check_collinearity()]
#' @examples
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30),
#'   run = 1,
#'   cond = factor(c("A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#' emod <- event_model(onset ~ hrf(cond), data = des, block = ~run,
#'                     sampling_frame = sframe)
#' correlation_map(emod)
#' @method correlation_map event_model
#' @export
correlation_map.event_model <- function(x,
                                        rotate_x_text = TRUE,
                                        method = c("pearson", "spearman"),
                                        half_matrix = TRUE,
                                        limits = c("fixed", "data"),
                                        absolute_limits = NULL,
                                        annotate = NULL,
                                        flag_threshold = 0.5,
                                        vif_threshold = 5,
                                        title = "Regressor correlations",
                                        subtitle = NULL,
                                        ...) {
  limits <- if (isFALSE(absolute_limits)) "data" else match.arg(limits)
  .fd_hm_correlation_map(x, method = method, half_matrix = half_matrix,
                         limits = limits, rotate_x_text = rotate_x_text,
                         annotate = annotate, flag_threshold = flag_threshold,
                         vif_threshold = vif_threshold,
                         title = title, subtitle = subtitle, ...)
}

#' @rdname correlation_map.event_model
#' @examples
#' bmod <- baseline_model(basis = "poly", degree = 2,
#'                        sframe = fmrihrf::sampling_frame(c(40, 40), TR = 1))
#' correlation_map(bmod)
#' @method correlation_map baseline_model
#' @export
correlation_map.baseline_model <- function(x,
                                           method = c("pearson", "spearman"),
                                           half_matrix = TRUE,
                                           limits = c("fixed", "data"),
                                           absolute_limits = NULL,
                                           rotate_x_text = TRUE,
                                           annotate = NULL,
                                           flag_threshold = 0.5,
                                           vif_threshold = 5,
                                           title = "Baseline regressor correlations",
                                           subtitle = NULL,
                                           ...) {
  limits <- if (isFALSE(absolute_limits)) "data" else match.arg(limits)
  .fd_hm_correlation_map(x, method = method, half_matrix = half_matrix,
                         limits = limits, rotate_x_text = rotate_x_text,
                         annotate = annotate, flag_threshold = flag_threshold,
                         vif_threshold = vif_threshold,
                         title = title, subtitle = subtitle, ...)
}

# ---------------------------------------------------------------------------
# plot_contrasts
# ---------------------------------------------------------------------------

# Flatten contrast_weights() into one row per contrast (or F-contrast
# component), aligned to the design-matrix columns.
.fd_hm_contrast_rows <- function(x) {
  dm_names <- colnames(design_matrix(x))
  cws <- contrast_weights(x)
  rows <- list()
  for (nm in names(cws)) {
    cw <- cws[[nm]]
    if (is.null(cw) || is.null(cw$offset_weights)) next
    W <- as.matrix(cw$offset_weights)
    if (!is.null(rownames(W)) && all(rownames(W) %in% dm_names)) {
      full <- matrix(0, length(dm_names), ncol(W))
      full[match(rownames(W), dm_names), ] <- W
      W <- full
    }
    if (nrow(W) != length(dm_names)) next
    for (k in seq_len(ncol(W))) {
      rows[[length(rows) + 1L]] <- list(
        key = nm, name = cw$name %||% sub("^.*#", "", nm), k = k, K = ncol(W),
        f = inherits(cw, "Fcontrast") || ncol(W) > 1L, w = unname(W[, k])
      )
    }
  }
  rows
}

#' Contrast weight heatmap
#'
#' Shows every contrast defined on an `event_model` as a row of weights over
#' all design-matrix columns, in design order and grouped by term, so it
#' lines up column-for-column with [design_map()]. Weights use the package's
#' diverging palette, symmetric about zero up to the largest absolute weight
#' across all contrasts (stated in the legend), and every weight is printed:
#' zero weights sit at the neutral midpoint with a faint "0", so exclusion
#' reads as an explicit weight rather than a missing cell.
#'
#' Two columns in the right margin check each row. "sum w" is the sum of the
#' weights; non-zero sums are shown in a warning colour. The SE multiplier
#' is sqrt(c' (X'X)^+ c), with X the design matrix plus one intercept per
#' run: the standard error of the contrast estimate per unit of noise
#' standard deviation, given this design (lower is better). Contrasts that
#' are not estimable from the design are marked "not est.". F-contrasts
#' appear as one row per component, numbered under the contrast name, each
#' with its own values.
#'
#' @param x An `event_model` with contrasts defined in its terms.
#' @param absolute_limits Logical; if `TRUE`, fix the fill scale at \[-1, 1\].
#'   Otherwise it is symmetric about 0 up to the largest abs(weight).
#' @param rotate_x_text Logical; angle column labels when they would overlap.
#' @param scale_mode `"auto"` or `"diverging"` (both diverging and symmetric
#'   about 0), or `"one_sided"` (neutral-to-warm magnitude scale from 0, for
#'   non-negative weights).
#' @param coord_fixed Logical; if `TRUE`, keep cells square. The default
#'   (`FALSE`) uses shallow rows of fixed height.
#' @param annotate Logical or `NULL`; print weights in the cells. `NULL`
#'   (default) prints them when there are at most 40 columns.
#' @param title,subtitle Plot title and subtitle.
#' @param ... Passed to [ggplot2::geom_tile()].
#' @return A ggplot object.
#' @seealso [contrast_weights()], [design_map()]
#' @examples
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30, 40, 50),
#'   run = 1,
#'   cond = factor(c("A", "B", "C", "A", "B", "C"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 60, TR = 1)
#' cset <- contrast_set(
#'   A_vs_B = pair_contrast(~ cond == "A", ~ cond == "B", name = "A_vs_B"),
#'   B_vs_C = pair_contrast(~ cond == "B", ~ cond == "C", name = "B_vs_C")
#' )
#' emod <- event_model(onset ~ hrf(cond, contrasts = cset),
#'                     data = des, block = ~run, sampling_frame = sframe)
#' plot_contrasts(emod)
#' @method plot_contrasts event_model
#' @export
plot_contrasts.event_model <- function(x,
                                       absolute_limits = FALSE,
                                       rotate_x_text = TRUE,
                                       scale_mode = c("auto", "diverging", "one_sided"),
                                       coord_fixed = FALSE,
                                       annotate = NULL,
                                       title = "Contrast weights",
                                       subtitle = NULL,
                                       ...) {
  scale_mode <- match.arg(scale_mode)
  rows <- .fd_hm_contrast_rows(x)
  if (length(rows) == 0L) {
    stop("No contrasts found in this event_model.", call. = FALSE)
  }
  DM <- as.matrix(design_matrix(x))
  ci <- .fd_colinfo(x)
  p <- nrow(ci)
  n_rows <- length(rows)

  # Row labels: contrast name, with component numbers for F-contrasts. Names
  # shared by contrasts on different terms get their term prefix back.
  keys <- vapply(rows, `[[`, "", "key")
  short <- vapply(rows, `[[`, "", "name")
  first <- !duplicated(keys)
  clash <- short %in% short[first][duplicated(short[first])]
  short[clash] <- sub("#", ": ", keys[clash], fixed = TRUE)
  is_f <- vapply(rows, `[[`, logical(1), "f")
  row_lab <- vapply(seq_len(n_rows), function(i) {
    r <- rows[[i]]
    if (r$K == 1L) short[i] else if (r$k == 1L) paste0(short[i], "   1") else as.character(r$k)
  }, "")

  W <- do.call(rbind, lapply(rows, `[[`, "w"))
  W[abs(W) < 1e-12] <- 0
  sums <- rowSums(W)
  empty <- rowSums(W != 0) == 0
  balanced <- abs(sums) < 1e-8 & !empty
  sum_lab <- ifelse(abs(sums) < 1e-8, "0", .fd_hm_num(sums, 3))
  sum_lab[empty] <- "all 0"
  sum_col <- ifelse(balanced, .fd_ink$muted, .fd_cat[2])

  se <- .fd_hm_contrast_se(DM, .fd_hm_scan_runs(x, nrow(DM)), t(W))
  se_lab <- ifelse(is.na(se), "not est.", .fd_hm_num(se, 2))
  se_lab[empty] <- ""
  se_col <- ifelse(is.na(se), .fd_cat[2], .fd_ink$text2)

  df <- data.frame(x = rep(seq_len(p), each = n_rows),
                   y = rep(seq_len(n_rows), times = p),
                   w = as.vector(W))
  wmax <- max(abs(df$w))
  if (!is.finite(wmax) || wmax == 0) wmax <- 1
  one_sided <- scale_mode == "one_sided"
  limit <- if (isTRUE(absolute_limits)) 1 else if (one_sided) max(df$w, 0) else wmax
  if (!is.finite(limit) || limit <= 0) limit <- 1

  annotate <- annotate %||% (p <= 40L)
  df$lab <- if (isTRUE(annotate)) .fd_hm_fmt_w(df$w) else ""
  df$tcol <- ifelse(df$w == 0, .fd_ink$zero, .fd_hm_text_col(df$w / limit, 0.6))
  text_size <- if (p <= 10L) 3.2 else if (p <= 20L) 2.7 else 2.2

  groups <- .fd_hm_groups(ci$term, ci$term_label)
  bounds <- .fd_hm_boundaries(groups)
  contrast_bounds <- .fd_hm_boundaries(.fd_hm_groups(keys))
  col_labels <- ci$full_label
  x_show <- .fd_hm_thin(p, 60)

  pm <- .fd_hm_pm()
  legend_name <- if (one_sided) {
    if (isTRUE(absolute_limits)) "Weight (fixed 0 to 1)" else "Weight (0 to max w)"
  } else if (isTRUE(absolute_limits)) {
    paste0("Weight (fixed ", pm, "1)")
  } else {
    paste0("Weight (", pm, "max abs w)")
  }
  fill_scale <- if (one_sided) {
    .fd_scale_magnitude(limit = limit, name = legend_name, guide = .fd_hm_bar(),
                        labels = .fd_hm_tick_labels)
  } else {
    .fd_hm_fill_signed(limit, legend_name)
  }

  plt <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_tile(ggplot2::aes(fill = w), colour = "#FFFFFF", linewidth = 0.6, ...) +
    fill_scale
  if (any(nzchar(df$lab))) {
    plt <- plt + ggplot2::geom_text(data = df[nzchar(df$lab), , drop = FALSE],
                                    ggplot2::aes(label = lab, colour = tcol),
                                    size = text_size, show.legend = FALSE) +
      ggplot2::scale_colour_identity()
  }
  if (length(bounds)) {
    plt <- plt + ggplot2::geom_vline(xintercept = bounds, colour = "#FFFFFF",
                                     linewidth = 2)
  }
  if (any(is_f) && length(contrast_bounds)) {
    plt <- plt + ggplot2::geom_hline(yintercept = contrast_bounds,
                                     colour = .fd_ink$muted, linewidth = 0.4)
  }

  # Right-margin check columns: sum of weights, then the SE multiplier.
  se_head <- .fd_hm_ascii(paste0("SE ", .fd_u("{times}"), intToUtf8(0x03C3L)))
  char_pt <- 6.2
  w_sum <- char_pt * max(nchar(c(sum_lab, "sum w"))) + 16
  w_se <- char_pt * max(nchar(c(se_lab, se_head)))
  margin_layers <- c(
    list(.fd_hm_margin_text("sum w", NA, .fd_ink$text, size = 9.4, face = "bold",
                            top = TRUE),
         .fd_hm_margin_text(se_head, NA, .fd_ink$text, x_pt = 10 + w_sum, size = 9.4,
                            face = "bold", top = TRUE)),
    lapply(seq_len(n_rows), function(i) {
      .fd_hm_margin_text(sum_lab[i], i, sum_col[i], size = 9)
    }),
    lapply(seq_len(n_rows), function(i) {
      .fd_hm_margin_text(se_lab[i], i, se_col[i], x_pt = 10 + w_sum, size = 9)
    })
  )

  if (is.null(subtitle)) {
    n_t <- sum(first & !is_f)
    subtitle <- paste0(.fd_hm_plural(length(unique(keys)), "contrast"), " over ",
                       .fd_hm_plural(p, "regressor"))
    if (any(is_f)) {
      subtitle <- paste0(subtitle, .fd_hm_sep(),
                         .fd_hm_plural(length(unique(keys[is_f])), "F-contrast"))
    }
    if (n_t > 0L) {
      subtitle <- paste0(subtitle, .fd_hm_sep(), sum(balanced[first & !is_f]),
                         " of ", n_t, " sum to zero")
    }
    n_empty <- length(unique(keys[empty]))
    if (n_empty > 0L) {
      subtitle <- paste0(subtitle, .fd_hm_sep(), n_empty,
                         if (n_empty == 1L) " has" else " have", " no non-zero weights")
    }
    n_ne <- length(unique(keys[is.na(se) & !empty]))
    if (n_ne > 0L) {
      subtitle <- paste0(subtitle, .fd_hm_sep(), n_ne, " not estimable")
    }
  }
  caption <- paste0(se_head, ": standard error of the contrast per unit noise SD, ",
                    "given this design (with run intercepts). Lower is better.")

  plt <- plt + .fd_hm_spanners(groups, text_pt = 6, rule_pt = 3) + margin_layers +
    ggplot2::scale_x_continuous(breaks = x_show, labels = col_labels[x_show],
                                expand = c(0, 0), sec.axis = .fd_hm_reserve_axis()) +
    ggplot2::scale_y_reverse(breaks = seq_len(n_rows), labels = row_lab,
                             expand = c(0, 0)) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    theme_fmridesign() +
    .fd_hm_theme(.fd_hm_x_text(col_labels[x_show], rotate_x_text)) +
    ggplot2::theme(
      axis.text.y.left = ggplot2::element_text(colour = .fd_ink$text, hjust = 1),
      legend.position = "bottom",
      legend.justification = "right",
      plot.margin = ggplot2::margin(10, 10 + w_sum + w_se + 12, 8, 10)
    )
  if (isTRUE(coord_fixed)) {
    plt <- plt + ggplot2::coord_fixed(clip = "off")
  } else {
    # Shallow rows of fixed height instead of large squares.
    plt <- plt + ggplot2::coord_cartesian(clip = "off") +
      .fd_hm_panel_size(height_in = n_rows * 0.38,
                        aspect = min(1, n_rows * 0.38 / 6))
  }
  plt
}

utils::globalVariables(c("y", "value", "r", "w", "lab", "tcol", "xend", "yend", "col",
                         "xmin", "xmax", "ymin", "ymax"))
