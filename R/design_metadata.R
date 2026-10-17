## First-class column metadata for design matrices. The schema lives in
## `.empty_col_metadata()` and is the single source of truth for the columns
## returned by `design_colmap()` and `design_meta()`.

#' Fast, validated constructor for fixed-schema metadata tibbles
#'
#' `tibble::tibble()` is convenient but its NSE / name-repair / glue machinery
#' dominates design-matrix build time when called once per term (profiling shows
#' ~15% of `event_model()`). This helper reproduces exactly the parts of
#' `tibble::tibble()` we rely on for the metadata schema -- length-1 column
#' recycling and a `tbl_df` result -- while skipping the overhead by delegating
#' to the low-level `tibble::new_tibble()`.
#'
#' Unlike raw `new_tibble()` (which in tibble 3.x neither recycles nor validates
#' column lengths), this recycles every length-1 column to `n` and errors on any
#' other length mismatch, so a forgotten recycle can never silently produce a
#' structurally corrupt tibble.
#'
#' @param cols Named list of columns (atomic vectors).
#' @param n Target number of rows.
#' @return A `tbl_df` with `n` rows.
#' @keywords internal
#' @noRd
.new_meta_tibble <- function(cols, n) {
  cols <- lapply(cols, function(v) {
    lv <- length(v)
    if (lv != n && lv == 1L) {
      v <- rep(v, n)
    } else if (lv != n) {
      stop(sprintf("metadata column length %d incompatible with nrow %d", lv, n),
           call. = FALSE)
    }
    # Metadata columns are plain atomic vectors. Strip any stray attributes
    # (e.g. a leftover `orig_names` that could ride in on a scalar value) so the
    # output is fully deterministic and independent of incidental input attrs.
    if (is.atomic(v)) attributes(v) <- NULL
    v
  })
  tibble::new_tibble(cols, nrow = n)
}

#' @keywords internal
#' @noRd
.empty_col_metadata <- function() {
  .new_meta_tibble(list(
    col              = integer(0),
    name             = character(0),
    term_tag         = character(0),
    term_index       = integer(0),
    condition        = character(0),
    run              = integer(0),
    role             = character(0),
    model_source     = character(0),
    basis_name       = character(0),
    basis_ix         = integer(0),
    basis_total      = integer(0),
    basis_label      = character(0),
    is_block_diagonal = logical(0),
    modulation_type  = character(0),
    modulation_id    = character(0)
  ), n = 0L)
}

#' Determine modulation type and id for a realised event term
#'
#' Inspects the term's events; if it contains exactly one ParametricBasis event,
#' the basis registry decides the modulation type (`parametric` /
#' `amplitude` / `covariate`) and the id is the variable name. Otherwise
#' falls back to `"amplitude"`.
#'
#' @param term An `event_term` (or covariate_convolved_term).
#' @return Length-1 list with `modulation_type` and `modulation_id`.
#' @keywords internal
#' @noRd
.event_term_modulation <- function(term) {
  if (inherits(term, "covariate_convolved_term")) {
    return(list(
      modulation_type = "covariate",
      modulation_id   = term$varname %||% NA_character_
    ))
  }
  if (inherits(term, "feature_term")) {
    return(list(
      modulation_type = "feature",
      modulation_id   = term$varname %||% NA_character_
    ))
  }

  basis_ev <- Find(function(ev) !is.null(ev$meta$basis), term$events %||% list())
  if (!is.null(basis_ev)) {
    basis <- basis_ev$meta$basis
    entry <- get_basis_entry(class(basis))
    return(list(
      modulation_type = if (!is.null(entry)) entry$modulation else "parametric",
      modulation_id   = basis$argname %||% basis_ev$varname %||% NA_character_
    ))
  }

  list(modulation_type = "amplitude", modulation_id = NA_character_)
}

# Basis families fmridesign names in lower case, as in hrf(basis = ...)
.hrf_basis_families <- c("spmg1", "spmg2", "spmg3", "gamma", "gaussian",
                         "bspline", "tent", "fir", "fourier", "daguerre")

#' Canonical basis name of an HRF
#'
#' The value recorded as `basis_name` in column metadata. fmrihrf marks some
#' bases with a subclass (`"SPMG3_HRF"`, `"FIR_HRF"`, `"BSpline_HRF"`, ...),
#' but its lag / block / normalise decorators return plain `"HRF"` objects,
#' and which bases carry a subclass varies between fmrihrf versions
#' (`HRF_SPMG1` is `"SPMG1_HRF"` only after 0.4.0). Decorators do keep the
#' base name in `attr(hrf, "name")`, with suffixes such as `"_lag(2)"`,
#' `"_block(w=3)"` or `"_norm"`. So the name comes from the subclass when
#' there is one, else from the `"name"` attribute with decorator suffixes
#' removed. Known families are returned in lower case as spelled in
#' `hrf(basis = ...)` (`"spmg1"`, `"spmg3"`, `"fir"`, ...); other names are
#' returned as they are.
#'
#' @param hrf An HRF object, or `NULL`.
#' @return Character scalar; `NA` when no name can be found.
#' @keywords internal
#' @noRd
.hrf_basis_name <- function(hrf) {
  if (is.null(hrf)) return(NA_character_)
  subclass <- setdiff(class(hrf), c("HRF", "function"))
  nm <- if (length(subclass) > 0L) {
    sub("_HRF$", "", subclass[[1L]])
  } else {
    attr(hrf, "name", exact = TRUE)
  }
  if (!is.character(nm) || length(nm) != 1L || is.na(nm) || !nzchar(nm)) {
    return(NA_character_)
  }
  nm <- sub("(_lag\\([^)]*\\)|_block\\([^)]*\\)|_norm)+$", "", nm)
  if (tolower(nm) %in% .hrf_basis_families) tolower(nm) else nm
}

#' Human-friendly labels for basis components
#'
#' Vectorised over both arguments (recycled to a common length).
#'
#' @param basis_name Canonical basis name from `.hrf_basis_name()`
#'   (e.g. `"spmg3"`, `"fir"`).
#' @param basis_ix   1-based component index.
#' @return Character vector of labels; `NA` where either input is `NA`.
#' @keywords internal
#' @noRd
.basis_label <- function(basis_name, basis_ix) {
  if (length(basis_name) == 0L || length(basis_ix) == 0L) return(character(0))
  n <- max(length(basis_name), length(basis_ix))
  basis_name <- rep_len(as.character(basis_name), n)
  basis_ix <- rep_len(as.integer(basis_ix), n)
  out <- sprintf("component_%02d", basis_ix)
  fir <- basis_name %in% "fir"
  out[fir] <- sprintf("lag_%02d", basis_ix[fir] - 1L)
  spmg2 <- basis_name %in% "spmg2"
  out[spmg2] <- c("canonical", "derivative")[pmin(pmax(basis_ix[spmg2], 1L), 2L)]
  spmg3 <- basis_name %in% "spmg3"
  out[spmg3] <- c("canonical", "derivative", "dispersion")[pmin(pmax(basis_ix[spmg3], 1L), 3L)]
  out[is.na(basis_name) | is.na(basis_ix)] <- NA_character_
  out
}

#' Build column metadata for a single convolved event term
#'
#' Called from `convolve.event_term()` once the final column names are known.
#'
#' @param term       The `event_term` being convolved.
#' @param hrf        The HRF object used for convolution.
#' @param base_cnames Character vector of base condition names (without `_b##`).
#' @param nb         Total number of basis functions.
#' @param term_tag   Term tag (may be NULL for Ident-only terms).
#' @param colnames_final Final column names produced by `make_column_names()`.
#' @return A tibble of metadata, one row per column.
#' @keywords internal
#' @noRd
.term_col_metadata <- function(term, hrf, base_cnames, nb,
                               term_tag, colnames_final) {
  n_cond <- length(base_cnames)
  if (n_cond == 0L || nb <= 0L || length(colnames_final) == 0L) {
    return(.empty_col_metadata())
  }

  cond_per_col  <- rep(base_cnames, each = nb)
  basis_per_col <- rep(seq_len(nb), times = n_cond)
  basis_name    <- .hrf_basis_name(hrf)
  modu <- .event_term_modulation(term)

  .make_col_metadata(
    name            = colnames_final,
    condition       = cond_per_col,
    term_tag        = if (is.null(term_tag)) NA_character_ else term_tag,
    basis_name      = basis_name,
    basis_ix        = if (nb > 1L) basis_per_col else NA_integer_,
    basis_total     = if (nb > 1L) as.integer(nb) else NA_integer_,
    basis_label     = if (nb > 1L) .basis_label(basis_name, basis_per_col) else NA_character_,
    modulation_type = modu$modulation_type,
    modulation_id   = modu$modulation_id %||% NA_character_
  )
}

#' Build column metadata for a covariate term
#'
#' Covariate terms bypass `convolve.event_term()`; produce metadata directly
#' from the term's design matrix.
#'
#' @param term A `covariate_convolved_term`.
#' @return A tibble of metadata.
#' @keywords internal
#' @noRd
.covariate_term_col_metadata <- function(term) {
  cn <- colnames(term$design_matrix) %||% character(0)
  if (length(cn) == 0L) return(.empty_col_metadata())
  cond <- term$condition_tags %||% cn
  if (length(cond) != length(cn)) cond <- cn
  .make_col_metadata(
    name            = cn,
    condition       = cond,
    term_tag        = attr(term, "term_tag") %||% term$varname %||% NA_character_,
    modulation_type = "covariate",
    modulation_id   = cond
  )
}

# Construct a per-term metadata tibble matching the canonical schema in
# `.empty_col_metadata()`. Any columns omitted take their schema default.
#' @keywords internal
#' @noRd
.make_col_metadata <- function(name,
                               condition       = name,
                               term_tag        = NA_character_,
                               basis_name      = NA_character_,
                               basis_ix        = NA_integer_,
                               basis_total     = NA_integer_,
                               basis_label     = NA_character_,
                               role            = "task",
                               model_source    = "event",
                               modulation_type = "amplitude",
                               modulation_id   = NA_character_,
                               is_block_diagonal = FALSE) {
  n <- length(name)
  .new_meta_tibble(list(
    col              = seq_len(n),  # replaced with absolute positions by combiner
    name             = name,
    term_tag         = term_tag,
    term_index       = NA_integer_,
    condition        = condition,
    run              = NA_integer_,
    role             = role,
    model_source     = model_source,
    basis_name       = basis_name,
    basis_ix         = basis_ix,
    basis_total      = basis_total,
    basis_label      = basis_label,
    is_block_diagonal = is_block_diagonal,
    modulation_type  = modulation_type,
    modulation_id    = modulation_id
  ), n = n)
}

#' Combine per-term metadata tibbles into a design-wide metadata tibble
#'
#' Stamps `term_index` and the absolute `col` index. Intended for use by the
#' internal event-model design-matrix builder.
#'
#' @param meta_list   Named list of per-term metadata tibbles (or NULL entries).
#' @param term_names  Character vector of term tags aligned with `meta_list`.
#' @param term_indices Optional integer vector giving the original term index
#'   for each entry of `meta_list`. Defaults to `seq_along(meta_list)`.
#' @return A tibble with one row per design matrix column.
#' @keywords internal
#' @noRd
.combine_col_metadata <- function(meta_list, term_names, term_indices = NULL) {
  if (length(meta_list) == 0L) return(.empty_col_metadata())
  if (is.null(term_indices)) term_indices <- seq_along(meta_list)

  parts <- vector("list", length(meta_list))
  cursor <- 0L
  for (i in seq_along(meta_list)) {
    md <- meta_list[[i]]
    if (is.null(md) || nrow(md) == 0L) next
    md$term_index <- as.integer(term_indices[i])
    if (length(term_names) >= i && !is.na(term_names[i])) {
      md$term_tag <- term_names[i]
    }
    md$col <- cursor + seq_len(nrow(md))
    cursor <- cursor + nrow(md)
    parts[[i]] <- md
  }
  parts <- purrr::compact(parts)
  if (length(parts) == 0L) return(.empty_col_metadata())

  # Row-bind the per-term parts. Every part is a `.make_col_metadata()` output
  # and therefore shares the exact fixed schema of `.empty_col_metadata()`, so a
  # name-keyed per-column concatenation reproduces `dplyr::bind_rows()` exactly
  # (types are homogeneous per column) while avoiding its splice/vec_c overhead.
  schema <- names(.empty_col_metadata())
  for (p in parts) {
    if (!identical(names(p), schema)) {
      stop("internal error: metadata part does not match the canonical schema; ",
           "cannot combine.", call. = FALSE)
    }
  }
  total <- sum(vapply(parts, nrow, integer(1)))
  combined <- lapply(schema, function(nm) do.call(c, lapply(parts, `[[`, nm)))
  names(combined) <- schema
  .new_meta_tibble(combined, n = total)
}

#' Accessor: column metadata for a design matrix
#'
#' Returns the `col_metadata` tibble attached to a design matrix produced by
#' `event_model()`. If the attribute is missing (e.g., on a manually
#' constructed matrix) returns `NULL`.
#'
#' @param x A design matrix or any object with a `design_matrix()` method.
#' @return A tibble, or `NULL`.
#' @export
#' @examples
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30),
#'   run = 1,
#'   cond = factor(c("A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#' emod <- event_model(onset ~ hrf(cond), data = des,
#'                     block = ~run, sampling_frame = sframe)
#' design_meta(emod)
design_meta <- function(x) {
  dm <- if (inherits(x, "data.frame") || is.matrix(x)) x else design_matrix(x)
  attr(dm, "col_metadata")
}
