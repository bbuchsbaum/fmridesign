#' @keywords internal
#' @noRd
parse_term <- function(vars, ttype) {
  nvars <- length(vars) # number of variables

  term <- vapply(vars, function(v) {
    parsed <- deparse(v, backtick = TRUE)
    attr(terms(reformulate(parsed)), "term.labels")
  }, character(1))

  label <- sprintf("%s(%s)", ttype, paste(term, collapse = ","))

  list(term = term, label = label)
}

#' Construct a Covariate Term
#'
#' @description
#' Creates a covariate term that is added directly to the fMRI model without being convolved 
#' with a hemodynamic response function (HRF). This is useful for including nuisance variables, 
#' continuous covariates, or any other regressors that should not undergo HRF convolution.
#'
#' @details
#' In fMRI analysis, some predictors should not be convolved with the HRF because they 
#' represent:
#' * Continuous physiological measurements (e.g., heart rate, respiration)
#' * Motion parameters from head movement correction
#' * Scanner drift or other technical artifacts
#' * Behavioral measures that directly correlate with BOLD signal
#' * Global signal or other nuisance variables
#'
#' The covariate term can be combined with standard HRF-convolved event terms in the 
#' same model. For example:
#' ```r
#' model <- event_model(onset ~ hrf(stimulus) + covariate(motion_x, motion_y, data = cov_data), 
#'                     data = events, block = ~ 1, sampling_frame = sframe)
#' ```
#'
#' Final columns follow the package-wide
#' `term_tag_condition_tag` convention. The default term tag is `cov`; `id`
#' or `prefix` replaces it. Thus `covariate(x, y, data = d)` produces `cov_x`
#' and `cov_y`, while `id = "motion"` produces `motion_x` and `motion_y`.
#' Named matrix columns become condition tags; unnamed or duplicated matrix
#' columns use deterministic [feature_suffix()] tags such as `f01` and `f02`.
#'
#' @param ... Covariate expressions. Each expression may evaluate to a numeric
#'   vector, matrix, or data frame. Matrix/data-frame columns are expanded into
#'   separate regressors.
#' @param data A data.frame containing the variables.
#' @param id An optional term identifier. As elsewhere in `fmridesign`, the
#'   identifier becomes the term tag prepended to final design-matrix columns.
#' @param prefix An optional term-tag prefix used when `id` is not supplied.
#' @param subset Optional expression used to subset the covariate data.
#'
#' @return A list containing information about the covariate term with class 
#' 'covariatespec' that can be used within an event_model.
#'
#' @examples
#' # Add motion parameters as covariates
#' motion_data <- data.frame(
#'   x = rnorm(100),  # x translation
#'   y = rnorm(100)   # y translation
#' )
#' cv <- covariate(x, y, data = motion_data, prefix = "motion")
#'
#' # A matrix is expanded using its column names
#' lag_set <- cbind(lag_m2 = rnorm(100), lag_0 = rnorm(100),
#'                  lag_p2 = rnorm(100))
#' lag_data <- data.frame(row = seq_len(100))
#' lag_data$lag_set <- lag_set
#' cv_lags <- covariate(lag_set, data = lag_data, id = "alignment")
#'
#' # Combine with event model
#' sframe <- sampling_frame(blocklens = c(100), TR = 2)
#' # 50 events, strictly increasing onsets per block
#' event_data <- data.frame(
#'   stimulus = factor(rep(c("A", "B"), 25)),
#'   onset = seq(0, by = 4, length.out = 50)
#' )
#' 
#' # Full model with both HRF-convolved events and non-convolved covariates
#' model <- event_model(
#'   onset ~ hrf(stimulus) + covariate(x, y, data = motion_data, id = "motion"),
#'   data = event_data,
#'   block = ~ 1,
#'   sampling_frame = sframe
#' )
#' 
#' @seealso 
#' * [event_model()] for creating complete fMRI models
#' * [hrf()] for creating HRF-convolved event terms
#'
#' @export
covariate <- function(..., data, id = NULL, prefix = NULL, subset = NULL) {
  vars <- as.list(substitute(list(...)))[-1]
  if (length(vars) == 0L) {
    stop("`covariate()` requires at least one covariate expression.", call. = FALSE)
  }

  parsed <- parse_term(vars, "covariate")
  term <- parsed$term
  label <- parsed$label

  # Covariates use the same term-tag precedence as other event-model terms:
  # id, then prefix, then a compact class-specific default. Individual
  # regressor names are condition tags and are composed later by
  # make_column_names().
  effective_id <- if (is.null(id) && is.null(prefix)) "cov" else id
  termname <- make_term_tag(list(
    id = effective_id,
    prefix = prefix,
    vars = list()
  ))

  ret <- list(
    data = data,
    name = termname,
    id = effective_id,
    prefix = prefix,
    varnames = term,
    vars = term,
    label = label,
    subset = rlang::enexpr(subset)
  )

  class(ret) <- c("covariatespec", "hrfspec", "list")
  ret
}

#' Normalize one evaluated covariate expression
#'
#' @param value Evaluated covariate value.
#' @param argname Source expression label.
#' @return A list containing a numeric matrix, condition tags, and provenance.
#' @keywords internal
#' @noRd
.normalize_covariate_value <- function(value, argname) {
  is_vector <- is.atomic(value) && is.null(dim(value)) &&
    (is.numeric(value) || is.logical(value))

  if (is_vector) {
    mat <- matrix(value, ncol = 1L)
    cond_tags <- sanitize(argname, allow_dot = TRUE)
    source_columns <- argname
  } else {
    if (is.data.frame(value)) {
      valid_cols <- vapply(value, function(col) is.numeric(col) || is.logical(col), logical(1))
      if (!all(valid_cols)) {
        stop(sprintf(
          "Covariate '%s' contains non-numeric column(s): %s",
          argname, paste(names(value)[!valid_cols], collapse = ", ")
        ), call. = FALSE)
      }
      value <- as.matrix(value)
    }

    if (!is.matrix(value) || !(is.numeric(value) || is.logical(value))) {
      stop(sprintf(
        "Covariate '%s' must evaluate to a numeric vector, matrix, or data frame.",
        argname
      ), call. = FALSE)
    }
    mat <- value
    source_columns <- colnames(mat)
    unusable_names <- is.null(source_columns) ||
      length(source_columns) != ncol(mat) ||
      anyNA(source_columns) || any(!nzchar(source_columns)) ||
      anyDuplicated(source_columns)
    cond_tags <- if (unusable_names) {
      feature_suffix(seq_len(ncol(mat)), ncol(mat))
    } else {
      unname(.sanitizeName(source_columns))
    }
  }

  if (nrow(mat) == 0L || ncol(mat) == 0L) {
    stop(sprintf("Covariate '%s' must contain at least one row and column.", argname),
         call. = FALSE)
  }
  storage.mode(mat) <- "double"
  colnames(mat) <- cond_tags

  list(
    matrix = mat,
    condition_tags = cond_tags,
    source_arg = rep(argname, ncol(mat)),
    source_column = if (is.null(source_columns)) rep(NA_character_, ncol(mat)) else source_columns,
    source_index = seq_len(ncol(mat))
  )
}

#' @keywords internal
#' @noRd
covariate_term <- function(varname, mat, condition_tags = colnames(mat),
                           source_map = NULL) {
  stopifnot(is.matrix(mat))
  ret <- list(
    varname = varname,
    condition_tags = condition_tags,
    source_map = source_map,
    design_matrix = suppressMessages(tibble::as_tibble(mat))
  )
  class(ret) <- c("covariate_term", "matrix_term", "fmri_term", "list")
  ret
}

#' @method construct covariatespec
#' @rdname construct
#' @param sampling_frame Optional sampling_frame that overrides the one present
#'   in `model_spec`.
#' @export
construct.covariatespec <- function(x, model_spec, sampling_frame = NULL, ...) {
  pieces <- lapply(seq_along(x$vars), function(i) {
    v <- x$vars[[i]]
    expr <- rlang::parse_expr(v)
    value <- rlang::eval_tidy(expr, data = x$data)
    .normalize_covariate_value(value, x$varnames[[i]])
  })

  nrows <- vapply(pieces, function(piece) nrow(piece$matrix), integer(1))
  if (length(unique(nrows)) != 1L) {
    details <- paste0(x$varnames, "=", nrows, collapse = ", ")
    stop(sprintf(
      "Covariate inputs must have the same number of rows; got %s.", details
    ), call. = FALSE)
  }

  mat <- do.call(cbind, lapply(pieces, `[[`, "matrix"))
  condition_tags <- unlist(lapply(pieces, `[[`, "condition_tags"), use.names = FALSE)
  source_map <- tibble::tibble(
    condition = condition_tags,
    source_arg = unlist(lapply(pieces, `[[`, "source_arg"), use.names = FALSE),
    source_column = unlist(lapply(pieces, `[[`, "source_column"), use.names = FALSE),
    source_index = unlist(lapply(pieces, `[[`, "source_index"), use.names = FALSE)
  )

  term_tag <- x$term_tag %||% x$name
  final_names <- make_column_names(term_tag, condition_tags, nb = 1L)
  unique_names <- make.unique(final_names, sep = ".")
  if (!identical(final_names, unique_names)) {
    changed <- which(final_names != unique_names)
    examples <- paste0(
      final_names[changed], " -> ", unique_names[changed], collapse = ", "
    )
    cli::cli_warn(
      paste0(
        "Duplicate covariate column names were automatically resolved: ",
        examples, ". Supply distinct matrix column names or separate term IDs."
      ),
      class = "fmridesign_name_clash"
    )
  }
  colnames(mat) <- unique_names

  cterm <- covariate_term(term_tag, mat, condition_tags, source_map)

  sframe <- if (is.null(sampling_frame)) {
    model_spec$sampling_frame
  } else {
    sampling_frame
  }

  ## Validate that the covariate matrix matches the sampling frame length
  expected_rows <- sum(fmrihrf::blocklens(sframe))
  if (nrow(mat) != expected_rows) {
    stop(sprintf(
      "Covariate term '%s' has %d rows but sampling_frame expects %d",
      x$name, nrow(mat), expected_rows
    ), call. = FALSE)
  }

  ret <- list(
    varname = term_tag,
    condition_tags = condition_tags,
    source_map = source_map,
    spec = x,
    evterm = cterm,
    design_matrix = cterm$design_matrix,
    sampling_frame = sframe,
    id = term_tag
  )

  class(ret) <- c("covariate_convolved_term", "convolved_term", "fmri_term", "list")
  ret
}

#' @export
event_table.covariate_convolved_term <- function(x, ...) {
  cnames <- colnames(x$design_matrix)
  ret <- do.call(cbind, lapply(cnames, function(tname) {
    rep(.sanitizeName(tname), nrow(x$design_matrix))
  }))
  
  colnames(ret) <- cnames
  suppressMessages(tibble::as_tibble(ret,.name_repair="check_unique"))
  
}

#' @export
#' @rdname conditions
conditions.covariate_term <- function(x, drop.empty = TRUE,
                                      expand_basis = FALSE,
                                      style = c("canonical", "display"), ...) {
  match.arg(style)
  x$condition_tags %||% colnames(x$design_matrix)
}

#' @export
#' @rdname conditions
conditions.covariate_convolved_term <- function(x, drop.empty = TRUE,
                                                expand_basis = FALSE,
                                                style = c("canonical", "display"), ...) {
  conditions(x$evterm, drop.empty = drop.empty,
             expand_basis = expand_basis, style = match.arg(style), ...)
}

#' @export
#' @rdname condition_map
condition_map.covariate_term <- function(x, drop.empty = TRUE,
                                         expand_basis = FALSE, ...) {
  tags <- conditions(x, drop.empty = drop.empty, expand_basis = expand_basis, ...)
  tibble::tibble(display = tags, canonical = tags)
}

#' @export
#' @rdname condition_map
condition_map.covariate_convolved_term <- function(x, drop.empty = TRUE,
                                                   expand_basis = FALSE, ...) {
  condition_map(x$evterm, drop.empty = drop.empty,
                expand_basis = expand_basis, ...)
}

#' @export
#' @rdname shortnames
shortnames.covariate_term <- function(x, ...) {
  as.character(conditions(x, ...))
}

#' @export
#' @rdname shortnames
shortnames.covariate_convolved_term <- function(x, ...) {
  shortnames(x$evterm, ...)
}

#' @export
#' @rdname longnames
longnames.covariate_term <- function(x, ...) {
  as.character(conditions(x, ...))
}

#' @export
#' @rdname longnames
longnames.covariate_convolved_term <- function(x, ...) {
  longnames(x$evterm, ...)
}

#' @rdname fmrihrf-reexports
#' @export
nbasis.covariate_convolved_term <- function(x, ...) {
  1L
}
