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
#' @param ... A variable argument set of covariate names.
#' @param data A data.frame containing the variables.
#' @param id An optional identifier for the covariate term.
#' @param prefix An optional prefix to add to the covariate names.
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
covariate <- function(..., data, id=NULL, prefix=NULL, subset=NULL) {
  vars <- as.list(substitute(list(...)))[-1] 
  parsed <- parse_term(vars, "covariate")
  term <- parsed$term
  label <- parsed$label
  
  varnames <- if (!is.null(prefix)) {
    paste0(prefix, "_", term)
  } else {
    term
  }
  
  termname <- paste0(varnames, collapse="::")
  
  if (is.null(id)) {
    id <- termname
  }  

  ret <- list(
    data=data,
    name=termname, ## covariate(x,y), where termname = "x::y"
    id=id, ## covariate(x), id by default is "x::y"
    varnames=varnames, ## list of all variables (e.g. list(x,y))
    vars=term, ## list of unparsed vars
    label=label, ## "covariate(x)" the full expression
    subset=rlang::enexpr(subset))
  
  class(ret) <- c("covariatespec", "hrfspec", "list")
  ret
}

#' @keywords internal
#' @noRd
covariate_term <- function(varname, mat) {
  stopifnot(is.matrix(mat))
  ret <- list(varname=varname, design_matrix=suppressMessages(tibble::as_tibble(mat)))
  class(ret) <- c("covariate_term", "matrix_term", "fmri_term", "list")
  ret
}

#' @method construct covariatespec
#' @rdname construct
#' @param sampling_frame Optional sampling_frame that overrides the one present
#'   in `model_spec`.
#' @export
construct.covariatespec <- function(x, model_spec, sampling_frame = NULL, ...) {
  mat <- do.call(cbind, lapply(x$vars, function(v) {
    expr <- rlang::parse_expr(v)
    rlang::eval_tidy(expr, data = x$data)
  }))
  
  colnames(mat) <- x$varnames

  cterm <- covariate_term(x$name, mat)

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
    varname=x$name,
    spec=x,
    evterm=cterm,
    design_matrix=cterm$design_matrix,
    sampling_frame=sframe,
    id=if(!is.null(x$id)) x$id else x$varname
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

#' @rdname fmrihrf-reexports
#' @export
nbasis.covariate_convolved_term <- function(x,...) {
  ncol(x$design_matrix)
}
