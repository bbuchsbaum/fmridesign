#' Shared visual system for fmridesign plots
#'
#' Every plotting method in the package draws from the same theme, palettes,
#' and column metadata so that a regressor keeps its colour and its label
#' across `plot()`, `design_map()`, `correlation_map()` and `plot_contrasts()`.
#'
#' @name fmridesign-plotting
#' @keywords internal
#' @importFrom stats cor
NULL

# ---------------------------------------------------------------------------
# Palettes
# ---------------------------------------------------------------------------

# Categorical order validated for colour-vision deficiency (adjacent-pair
# CVD separation >= 8 dE in OKLab) and a normal-vision floor >= 15. Hues are
# assigned in this fixed order and never cycled.
.fd_cat <- c("#0072B2", "#D55E00", "#009E73", "#C28A00",
             "#7B5CB8", "#4FA3D9", "#B04A7A")

# Diverging ramp: cool pole, neutral warm-grey midpoint, warm pole.
.fd_div <- c("#1D4F7A", "#5D8FBF", "#B9D0E6", "#F7F6F4",
             "#EDBBA6", "#C8664A", "#8A2A1C")

# Ink and surface tokens.
.fd_ink <- list(
  text   = "#1C232E",
  text2  = "#4A5465",
  muted  = "#7D8697",
  rule   = "#DADDE3",
  grid   = "#ECEEF1",
  band   = "#F4F5F7",
  zero   = "#B8BEC8"
)

#' Colour palettes used by fmridesign plots
#'
#' `fmridesign_palette()` returns the package's categorical palette (a
#' colour-vision-deficiency-checked ordering) or its diverging ramp, so custom
#' plots can match the built-in ones.
#'
#' @param type One of `"categorical"` or `"diverging"`.
#' @param n Number of colours. For `"categorical"`, at most 7 distinct hues are
#'   available; more than that is an error because recycled hues are
#'   indistinguishable. For `"diverging"`, the ramp is interpolated to `n`.
#' @return A character vector of hex colours.
#' @examples
#' fmridesign_palette("categorical", 4)
#' fmridesign_palette("diverging", 11)
#' @export
fmridesign_palette <- function(type = c("categorical", "diverging"), n = NULL) {
  type <- match.arg(type)
  if (type == "categorical") {
    if (is.null(n)) return(.fd_cat)
    if (n > length(.fd_cat)) {
      stop("The categorical palette has ", length(.fd_cat),
           " distinct hues; ", n, " were requested. ",
           "Group series or use small multiples instead.", call. = FALSE)
    }
    return(.fd_cat[seq_len(n)])
  }
  if (is.null(n)) return(.fd_div)
  grDevices::colorRampPalette(.fd_div, space = "Lab")(n)
}

# Tints of a base colour, from full strength towards white; used to separate
# basis functions within one condition while keeping its hue.
.fd_tints <- function(col, n) {
  if (n <= 1L) return(col)
  ramp <- grDevices::colorRampPalette(c(col, "#FFFFFF"), space = "Lab")(n + 2L)
  ramp[seq_len(n)]
}

# Assign colours to a vector of group keys in first-appearance order. Beyond
# the categorical palette, fall back to a single neutral-to-blue ramp so the
# plot stays honest (no recycled hues) and relies on direct labels.
.fd_colour_map <- function(keys) {
  keys <- unique(as.character(keys))
  n <- length(keys)
  cols <- if (n <= length(.fd_cat)) {
    .fd_cat[seq_len(n)]
  } else {
    grDevices::colorRampPalette(c("#9DB7D5", "#0072B2", "#12324F"),
                                space = "Lab")(n)
  }
  stats::setNames(cols, keys)
}

# ---------------------------------------------------------------------------
# Theme
# ---------------------------------------------------------------------------

#' ggplot2 theme for fmridesign plots
#'
#' A restrained theme: left-aligned titles, recessive grid, no panel border,
#' and small uppercase-free strip labels. All plotting methods in the package
#' use it; add it to your own plots to match.
#'
#' @param base_size Base font size in points.
#' @param base_family Base font family.
#' @return A ggplot2 theme object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'     ggplot2::geom_point() +
#'     theme_fmridesign()
#' }
#' @export
theme_fmridesign <- function(base_size = 11, base_family = "") {
  ink <- .fd_ink
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text              = ggplot2::element_text(colour = ink$text),
      plot.title        = ggplot2::element_text(face = "bold", size = base_size * 1.25,
                                                hjust = 0, margin = ggplot2::margin(b = 3)),
      plot.subtitle     = ggplot2::element_text(colour = ink$text2, size = base_size * 0.95,
                                                hjust = 0, margin = ggplot2::margin(b = 8)),
      plot.caption      = ggplot2::element_text(colour = ink$muted, size = base_size * 0.8,
                                                hjust = 0, margin = ggplot2::margin(t = 8)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.margin       = ggplot2::margin(10, 14, 8, 10),
      axis.title        = ggplot2::element_text(colour = ink$text2, size = base_size * 0.9),
      axis.text         = ggplot2::element_text(colour = ink$muted, size = base_size * 0.82),
      axis.ticks        = ggplot2::element_line(colour = ink$rule, linewidth = 0.3),
      axis.ticks.length = grid::unit(2.5, "pt"),
      panel.grid.major  = ggplot2::element_line(colour = ink$grid, linewidth = 0.3),
      panel.grid.minor  = ggplot2::element_blank(),
      panel.spacing     = grid::unit(6, "pt"),
      strip.text        = ggplot2::element_text(colour = ink$text, face = "bold",
                                                size = base_size * 0.85, hjust = 0,
                                                margin = ggplot2::margin(2, 0, 2, 0)),
      legend.position   = "top",
      legend.justification = "left",
      legend.title      = ggplot2::element_text(colour = ink$text2, size = base_size * 0.85),
      legend.text       = ggplot2::element_text(colour = ink$text, size = base_size * 0.82),
      legend.key.height = grid::unit(0.8, "lines"),
      legend.margin     = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing = grid::unit(4, "pt")
    )
}

# ---------------------------------------------------------------------------
# Column metadata for plotting
# ---------------------------------------------------------------------------

# Turn an internal condition tag such as "task.face_load.high" into
# "face x high", using the term's variable names to find level boundaries.
.fd_condition_label <- function(cond, vars) {
  if (is.na(cond) || !nzchar(cond)) return(cond)
  if (grepl("^\\.trial_factor", cond)) {
    return(paste0("trial ", sub("^.*\\.\\.\\.?", "", cond)))
  }
  vars <- vars[nzchar(vars)]
  if (length(vars) == 0L) return(cond)
  esc <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", vars[order(-nchar(vars))])
  pat <- paste0("(^|_)(", paste(esc, collapse = "|"), ")\\.")
  m <- gregexpr(pat, cond, perl = TRUE)[[1]]
  if (m[1] == -1L) return(cond)
  ends <- m + attr(m, "match.length")
  starts_next <- c(m[-1], nchar(cond) + 1L)
  levels <- substring(cond, ends, starts_next - 1L)
  paste(levels, collapse = .fd_u(" {times} "))
}

# Build a tidy description of each design-matrix column: which term it belongs
# to, a human-readable label, basis index, run, and role. Built from the
# `col_metadata` attached by the model builders so it follows the actual data
# layout rather than re-parsing column names.
.fd_colinfo <- function(x) {
  DM <- design_matrix(x)
  cm <- tryCatch(as.data.frame(design_colmap(x)), error = function(e) NULL)
  n <- ncol(DM)
  if (is.null(cm) || nrow(cm) != n) {
    cm <- data.frame(col = seq_len(n), name = colnames(DM), term_tag = "design",
                     condition = colnames(DM), run = NA_integer_, role = NA_character_,
                     basis_ix = NA_integer_, basis_total = NA_integer_,
                     basis_label = NA_character_, stringsAsFactors = FALSE)
  }
  cm$name <- colnames(DM)
  term_vars <- .fd_term_vars(x)

  lab <- vapply(seq_len(n), function(i) {
    tt <- cm$term_tag[i]
    cond <- cm$condition[i]
    if (inherits(x, "baseline_model")) return(.fd_baseline_label(cm[i, ]))
    if (is.na(cond)) return(cm$name[i])
    .fd_condition_label(cond, term_vars[[tt]] %||% character(0))
  }, character(1))

  if (inherits(x, "baseline_model")) {
    # Nuisance columns (`nuis_<name>_block_<run>`): design_colmap() supplies
    # the run and role; label them with the user's own column names.
    nz <- cm$term_tag %in% "nuisance"
    if (any(nz)) {
      cm$role[nz] <- "nuisance"
      src <- tryCatch(terms(x)[["nuisance"]]$source_colnames, error = function(e) NULL)
      if (length(src) == sum(nz)) {
        lab[nz] <- src
      } else {
        lab[nz] <- sub("^nuis_(.*)_block_[0-9]+$", "\\1", cm$name[nz])
      }
    }
  }

  term <- cm$term_tag
  term[is.na(term)] <- "design"
  term_lab <- vapply(term, function(tt) {
    v <- term_vars[[tt]]
    if (length(v) > 1L) paste(v, collapse = .fd_u(" {times} ")) else if (length(v) == 1L) v else tt
  }, character(1))

  basis_ix <- cm$basis_ix
  basis_label <- cm$basis_label
  cond_key <- lab

  out <- data.frame(
    col         = seq_len(n),
    name        = cm$name,
    term        = term,
    term_label  = unname(term_lab),
    label       = lab,
    cond_key    = cond_key,
    basis_ix    = basis_ix,
    basis_label = basis_label,
    run         = cm$run,
    role        = cm$role,
    stringsAsFactors = FALSE
  )
  # Full label: add basis component (event models) or run (baseline models)
  # so every column label is unique.
  has_basis <- !is.na(out$basis_ix) & !inherits(x, "baseline_model")
  out$full_label <- out$label
  out$full_label[has_basis] <- paste0(out$label[has_basis], .fd_u(" {dot} "),
                                      .fd_short_basis(out$basis_label[has_basis],
                                                      out$basis_ix[has_basis]))
  has_run <- !is.na(out$run) & inherits(x, "baseline_model")
  out$full_label[has_run] <- paste0(out$label[has_run], .fd_u(" {dot} run "), out$run[has_run])
  # Disambiguate identical labels coming from different terms.
  dup <- duplicated(out$full_label) | duplicated(out$full_label, fromLast = TRUE)
  out$full_label[dup] <- paste0(out$term_label[dup], ": ", out$full_label[dup])
  out$full_label <- make.unique(out$full_label, sep = " #")
  out
}

.fd_short_basis <- function(basis_label, basis_ix) {
  lab <- basis_label
  lab[is.na(lab)] <- sprintf("b%d", basis_ix[is.na(lab)])
  lab <- sub("^component_0*", "b", lab)
  lab <- sub("^lag_0*", "lag ", lab)
  lab <- sub("^derivative$", "temporal deriv.", lab)
  lab <- sub("^dispersion$", "dispersion deriv.", lab)
  lab
}

.fd_baseline_label <- function(row) {
  tt <- row$term_tag
  if (identical(tt, "drift")) {
    return(sprintf("drift %s", sub("^component_0*", "", row$basis_label %||% "")))
  }
  if (identical(tt, "block")) return("intercept")
  sub("_[0-9]+$", "", row$name)
}

# Variables that make up each event term, keyed by term tag.
.fd_term_vars <- function(x) {
  if (!inherits(x, "event_model")) return(list())
  tms <- x$terms
  if (is.null(tms)) return(list())
  cm <- attr(design_matrix(x), "col_metadata")
  tags <- names(attr(design_matrix(x), "col_indices")) %||% names(tms)
  out <- lapply(tms, function(t) {
    ev <- t$events %||% list()
    v <- names(ev)
    v[!grepl("^\\.trial_factor", v)]
  })
  if (length(tags) == length(out)) names(out) <- tags
  out
}

# ---------------------------------------------------------------------------
# Time / run helpers
# ---------------------------------------------------------------------------

# Per-scan run ids and times plus a run table for bands and labels.
.fd_timeline <- function(sframe, n_rows, block_x = c("global", "run")) {
  block_x <- match.arg(block_x)
  if (is.null(sframe)) {
    runs <- data.frame(run = 1L, start = 0.5, end = n_rows + 0.5, scans = n_rows,
                       TR = 1, first_scan = 1L, last_scan = n_rows)
    return(list(run = rep(1L, n_rows), time = seq_len(n_rows), runs = runs,
                unit = "scan", TR = 1))
  }
  bl <- fmrihrf::blocklens(sframe)
  TR <- sframe$TR
  if (length(TR) == 1L) TR <- rep(TR, length(bl))
  run <- rep(seq_along(bl), bl)
  time <- fmrihrf::samples(sframe, global = (block_x == "global"))
  dur <- bl * TR
  if (block_x == "global") {
    end <- cumsum(dur); start <- c(0, utils::head(end, -1L))
  } else {
    start <- rep(0, length(bl)); end <- dur
  }
  runs <- data.frame(run = seq_along(bl), start = start, end = end, scans = bl,
                     TR = TR, first_scan = c(1L, utils::head(cumsum(bl), -1L) + 1L),
                     last_scan = cumsum(bl))
  if (length(run) != n_rows) {
    return(.fd_timeline(NULL, n_rows))
  }
  list(run = run, time = time, runs = runs, unit = "s", TR = TR)
}

# Format seconds as m:ss for axis labels.
.fd_mmss <- function(x) {
  x <- round(x)
  out <- sprintf("%d:%02d", x %/% 60, x %% 60)
  out[is.na(x)] <- NA_character_
  out
}

# ---------------------------------------------------------------------------
# Family conventions (iteration 2): one run idiom, one time label, one signed
# palette, one magnitude palette.
# ---------------------------------------------------------------------------

# Run idiom for time-series plots: hairline rules at run boundaries (drawn by
# the caller as geom_vline so tests can find them) and "Run k" labels on the
# top axis. No shading: shading only alternate runs made unshaded runs read as
# "no run".
.fd_run_layers <- function(runs, ...) list()

# Secondary (top) axis carrying "Run k" labels at run midpoints.
.fd_run_sec_axis <- function(runs) {
  if (is.null(runs) || nrow(runs) < 2L) return(ggplot2::waiver())
  ggplot2::sec_axis(~ ., breaks = (runs$start + runs$end) / 2,
                    labels = paste("Run", runs$run))
}

# Theme elements for the top run axis, shared by every time-series plot.
.fd_run_axis_theme <- function(base_size = 11) {
  ggplot2::theme(
    axis.text.x.top  = ggplot2::element_text(colour = .fd_ink$text2, face = "plain",
                                             size = base_size * 0.82),
    axis.ticks.x.top = ggplot2::element_blank()
  )
}

# One wording for time axes across the family.
.fd_time_label <- function(block_x = "global") {
  if (identical(block_x, "run")) "Time within run (min:s)" else "Time (min:s)"
}

# Signed quantities: diverging ramp, symmetric about zero.
.fd_scale_signed <- function(aesthetic = "fill", limit = 1, name = NULL, ...) {
  limit <- if (is.finite(limit) && limit > 0) limit else 1
  ggplot2::scale_fill_gradientn(colours = .fd_div, limits = c(-limit, limit),
                                oob = .fd_squish, name = name, aesthetics = aesthetic, ...)
}

# Unsigned magnitudes: neutral (zero) to the warm pole of the diverging ramp,
# so "more positive" looks the same in every plot.
.fd_mag <- c("#FFFFFF", "#EDBBA6", "#C8664A", "#8A2A1C", "#4A140B")
.fd_scale_magnitude <- function(aesthetic = "fill", limit = 1, name = NULL, ...) {
  limit <- if (is.finite(limit) && limit > 0) limit else 1
  ggplot2::scale_fill_gradientn(colours = .fd_mag, limits = c(0, limit),
                                oob = .fd_squish, name = name, aesthetics = aesthetic, ...)
}

# Clamp out-of-range values to the scale limits (scales::squish without the
# dependency).
.fd_squish <- function(x, range = c(0, 1), only.finite = TRUE) {
  pmin(pmax(x, range[1]), range[2])
}

# Typographic glyphs, built from code points so sources stay ASCII.
.fd_glyphs <- c(times = 0xD7, dot = 0xB7, minus = 0x2212, ndash = 0x2013,
                Sigma = 0x3A3, check = 0x2713)
.fd_u <- function(s) {
  for (nm in names(.fd_glyphs)) {
    s <- gsub(paste0("{", nm, "}"), intToUtf8(.fd_glyphs[[nm]]), s, fixed = TRUE)
  }
  s
}

# Compact numeric formatting for annotations: 1.62 -> "1.6", 0.034 -> "0.034".
.fd_num <- function(x, digits = 2) {
  out <- trimws(formatC(signif(x, digits), format = "fg", digits = digits))
  out <- sub("\\.$", "", out)
  gsub("-", .fd_u("{minus}"), out, fixed = TRUE)
}

# Colours for event-model display rows that expose factorial structure: for a
# term crossing two or more factors, hue follows the first factor's level and
# lightness the remaining factors' combination. Other terms take the next
# free hues. Returns a named vector keyed by row key, or NULL when the
# structure would need more hues than the palette has.
.fd_structured_colours <- function(ci, row_key) {
  rows <- unique(row_key)
  first <- ci[match(rows, row_key), , drop = FALSE]
  hue_key <- character(length(rows))
  shade_ix <- integer(length(rows))
  shade_n <- integer(length(rows))
  sep <- .fd_u(" {times} ")
  for (tt in unique(first$term)) {
    ii <- which(first$term == tt)
    parts <- strsplit(first$label[ii], sep, fixed = TRUE)
    nf <- unique(lengths(parts))
    if (length(nf) == 1L && nf >= 2L) {
      lead <- vapply(parts, `[`, character(1), 1L)
      rest <- vapply(parts, function(p) paste(p[-1L], collapse = sep), character(1))
      rest_levels <- unique(rest)
      hue_key[ii] <- paste(tt, lead)
      shade_ix[ii] <- match(rest, rest_levels)
      shade_n[ii] <- length(rest_levels)
    } else {
      hue_key[ii] <- paste(tt, first$label[ii])
      shade_ix[ii] <- 1L
      shade_n[ii] <- 1L
    }
  }
  hues <- unique(hue_key)
  if (length(hues) > length(.fd_cat)) return(NULL)
  base <- stats::setNames(.fd_cat[seq_along(hues)], hues)
  cols <- vapply(seq_along(rows), function(i) {
    b <- base[[hue_key[i]]]
    if (shade_n[i] <= 1L) return(b)
    # Darker = later level (e.g. high load), lighter = earlier level.
    ramp <- grDevices::colorRampPalette(c(.fd_tints(b, 3L)[2L], b, .fd_shade(b)),
                                        space = "Lab")(shade_n[i])
    ramp[shade_ix[i]]
  }, character(1))
  stats::setNames(cols, rows)
}

# Darken a colour towards the text ink.
.fd_shade <- function(col, amount = 0.45) {
  grDevices::colorRampPalette(c(col, .fd_ink$text), space = "Lab")(100)[round(amount * 100)]
}

utils::globalVariables(c("start", "end"))
