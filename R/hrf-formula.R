#' @importFrom rlang %||%
#' @importFrom assertthat assert_that


#' @keywords internal
#' @noRd
make_hrf <- function(basis, lag, nbasis = NULL, label = NULL) {
  if (!is.numeric(lag) || length(lag) > 1) {
    stop("hrf: 'lag' must be a numeric scalar")
  }

  if (is.character(basis)) {
    basis <- .hrf_from_name(basis, nbasis)
  } else if (is.function(basis) && !inherits(basis, "HRF")) {
    # gen_hrf() wraps a plain function with as_hrf(); name it after the
    # caller's `basis` expression (`label`) rather than its own argument.
    basis <- fmrihrf::gen_hrf(basis, name = label %||% "custom")
  } else if (!inherits(basis, "HRF")) {
    stop("invalid basis function: must be 1) character string indicating hrf type, e.g. 'gamma' 2) a function or 3) an object of class 'HRF': ", basis)
  }

  fmrihrf::gen_hrf(basis, lag = lag)
}

# Label for a plain-function `basis` argument: the symbol it was passed as,
# or "custom" for an inline function.
.basis_fun_label <- function(expr) {
  if (is.symbol(expr)) as.character(expr) else "custom"
}

#' Resolve a basis name to an HRF object
#'
#' Fixed bases are fmrihrf's constants. "bspline" (alias "bs"), "tent", "fir"
#' and "fourier" are basis sets sized by `nbasis` (`NULL`: their standard size, 5 or 12 for
#' "fir"); the fmrihrf constant is reused when it already has that size.
#' (fmrihrf::getHRF() is not used for the fixed bases because it renames
#' them, e.g. "SPMG1" to "spmg1".)
#' @keywords internal
#' @noRd
.hrf_from_name <- function(basis, nbasis) {
  hrf_map <- list(
    "spmg1" = fmrihrf::HRF_SPMG1,
    "spmg2" = fmrihrf::HRF_SPMG2,
    "spmg3" = fmrihrf::HRF_SPMG3,
    "gamma" = fmrihrf::HRF_GAMMA,
    "gaussian" = fmrihrf::HRF_GAUSSIAN,
    "bspline" = fmrihrf::HRF_BSPLINE,
    "fir" = fmrihrf::HRF_FIR
  )
  known <- c(names(hrf_map), "bs", "tent", "fourier")
  if (!basis %in% known) {
    stop("Unknown HRF basis name: ", basis, ". Available options: ",
         paste(known, collapse = ", "))
  }
  if (basis == "bs") basis <- "bspline"

  base <- hrf_map[[basis]]
  if (!basis %in% c("bspline", "tent", "fir", "fourier")) {
    return(base)
  }
  nbasis <- nbasis %||% if (is.null(base)) 5L else fmrihrf::nbasis(base)
  if (!is.null(base) && nbasis == fmrihrf::nbasis(base)) {
    return(base)
  }

  span <- attr(base, "span") %||% 24
  if (basis == "fir") {
    # FIR: one indicator per span/nbasis-second bin (like HRF_FIR)
    return(fmrihrf::hrf_fir_generator(nbasis = nbasis, span = span))
  }
  if (basis == "tent") {
    return(fmrihrf::hrf_tent_generator(nbasis = nbasis, span = span))
  }
  fn <- switch(basis,
    bspline = {
      degree <- attr(base, "params")$degree %||% 3
      function(t) fmrihrf::hrf_bspline(t, span = span, N = nbasis, degree = degree)
    },
    fourier = function(t) fmrihrf::hrf_fourier(t, span = span, nbasis = nbasis)
  )
  fmrihrf::as_hrf(fn, name = basis, nbasis = nbasis, span = span)
}

#' hemodynamic regressor specification function for model formulas.
#'
#' This function is to be used in formulas for fitting functions, e.g. onsets ~ hrf(fac1,fac2) ...
#' It captures the variables/expressions provided and packages them with HRF/contrast
#' information into an `hrfspec` object, which is then processed by `event_model`.
#'
#' @param ... One or more variable names (bare or character) or expressions involving variables
#'            present in the `data` argument of `event_model`.
#' @param basis the impulse response function or the name of a pre-supplied function,
#'        one of: "spmg1", "spmg2", "spmg3", "gamma", "gaussian", "bspline" (or "bs"),
#'        "tent", "fir", "fourier" (the last four are sized by `nbasis`).
#'        Can also be an `HRF` object or a plain function of time.
#' @param hrf_fun optional per-onset HRF generator. Can be:
#'        \itemize{
#'          \item A function that takes a data frame of event data (with columns `onset`, `duration`,
#'                `blockid`, plus any term variables) and returns either a single HRF object
#'                (recycled to all events) or a list of HRF objects (one per event).
#'          \item A formula (e.g., `~hrf_column`) referencing a column in the event data that
#'                contains a list of pre-built HRF objects.
#'        }
#'        When `hrf_fun` is specified, the `basis` argument is ignored. The generator function
#'        is called AFTER any subsetting via `subset=`, ensuring correct alignment between
#'        HRFs and events. All returned HRFs must have the same `nbasis` for design matrix consistency.
#' @param onsets optional onsets override. If missing, onsets will be taken from the LHS of the main model formula.
#' @param durations optional durations override. If missing, durations argument from `event_model` is used.
#' @param prefix a character string that is prepended to the variable names and used to identify the term.
#'               Can be used to disambiguate two \code{hrf} terms with the same variable(s) but different onsets or basis functions.
#' @param subset an expression indicating the subset of 'onsets' to keep.
#' @param precision sampling precision in seconds.
#' @param nbasis number of basis functions -- only used for hemodynamic response functions (e.g. bspline) that take a variable number of bases.
#'        If omitted, such a basis set uses its standard size (12 bins for "fir", 5 functions otherwise).
#' @param contrasts one or more \code{contrast_spec} objects created with the \code{contrast}, `pair_contrast` etc. functions.
#'                  Must be NULL, a single contrast spec, or a *named* list of contrast specs.
#' @param id a unique \code{character} identifier used to refer to term, otherwise will be determined from variable names.
#' @param name Optional human-readable name for the term.
#' @param lag a temporal offset in seconds which is added to onset before convolution
#' @param summate whether impulse amplitudes sum up when duration is greater than 0.
#' @param normalize logical; if TRUE, each convolved regressor column is peak-normalized
#'        so that `max(abs(column)) == 1`. This is useful when events have different durations,
#'        since longer events produce taller peaks after convolution. Normalizing ensures that
#'        beta estimates reflect condition differences rather than duration-dependent amplitude
#'        scaling, which is important for analyses like MVPA or RSA. Default FALSE.
#' @examples
#' form1 <- onsets ~ hrf(condition, basis="spmg1")
#'
#' form2 <- onsets ~ hrf(condition) + hrf(RT)
#'
#' form3 <- onsets ~ hrf(condition, RT, basis="spmg3")
#'
#' library(rlang)
#' con1 <- pair_contrast(~ condition == "A", ~ condition == "B", name="AvB")
#' form4 <- onsets ~ hrf(condition, Poly(RT, 2), contrasts=con1)
#'
#' form5 <- onsets ~ hrf(condition, hrf_fun = boxcar_hrf_gen())
#'
#' form6 <- onsets ~ hrf(condition,
#'                       hrf_fun = weighted_hrf_gen("sub_times", "sub_weights"))
#'
#' @export
#' @return an \code{hrfspec} instance
hrf <- function(..., basis="spmg1", hrf_fun=NULL, onsets=NULL, durations=NULL, prefix=NULL, subset=NULL, precision=.3,
                nbasis=1, contrasts=NULL, id=NULL, name=NULL, lag=0, summate=TRUE, normalize=FALSE) {

  vars <- rlang::enquos(...) # Capture variables/expressions as quosures
  subset_quo <- if (missing(subset)) NULL else rlang::enquo(subset)

  # --- Handle special named arguments within ... ---
  var_names <- names(vars)
  # Remove any quosures whose name matches hrf formals (these are control args, not vars)
  hrf_formals <- names(formals(hrf))

  # Check if hrf_fun was passed via ... (happens when called from eval() on a quoted expression)
  # In this case, hrf_fun formal is NULL but there's a quosure named "hrf_fun" in vars
  if (is.null(hrf_fun) && "hrf_fun" %in% var_names) {
    hrf_fun_idx <- which(var_names == "hrf_fun")
    hrf_fun_quo <- vars[[hrf_fun_idx]]
    # Evaluate the quosure to get the actual function/formula
    hrf_fun <- rlang::eval_tidy(hrf_fun_quo)
  }

  var_indices <- ! (var_names %in% hrf_formals)
  vars <- vars[var_indices]

  if (length(vars) == 0) {
      stop("`hrf` must have at least one variable or expression specified in `...`")
  }
  
  # --- Determine Term ID/Name (Prioritize id, then name, then auto) --- 
  final_id <- id # Prioritize explicit id
  if (is.null(final_id) && !is.null(name)) { # Use name if id is missing
      final_id <- name
  }
  # final_id remains NULL if neither id nor name provided, hrfspec will generate default
  
  # --- Check contrasts argument --- 
  if (!is.null(contrasts)) {
    # Accept a single contrast_spec directly (even though it inherits from list)
    if (inherits(contrasts, "contrast_spec")) {
      contrasts <- stats::setNames(list(contrasts), contrasts$name %||% "contrast1")
    } else if (is.list(contrasts)) {
      # A contrast_set is already a list of contrast_specs; a bare list is checked
      if (!inherits(contrasts, "contrast_set") &&
          !all(vapply(contrasts, inherits, logical(1), "contrast_spec"))) {
        stop("If `contrasts=` is a list, all elements must be contrast_spec objects.\n",
             "  Hint: Use functions like contrast(), pair_contrast(), contrast_set() to define contrasts.", 
             call. = FALSE)
      }
      # Ensure names, falling back to each spec's name and then "contrast<i>"
      if (is.null(names(contrasts)) || any(names(contrasts) == "")) {
        cnames <- vapply(contrasts, function(cs) cs$name %||% "", character(1))
        blanks <- which(cnames == "")
        cnames[blanks] <- paste0("contrast", blanks)
        if (any(duplicated(cnames))) {
          stop("If `contrasts=` is a list, it must be a named list with unique names.", call. = FALSE)
        }
        names(contrasts) <- cnames
      }
    } else {
      stop("`contrasts=` argument must be a contrast_spec, a contrast_set, or a list of contrast_spec objects.\n",
           "  Detected type: ", class(contrasts)[1], "\n",
           "  Hint: Use functions like contrast(), pair_contrast(), contrast_set() to define contrasts.",
           call. = FALSE)
    }
  }
  # -----------------------------

  # --- Handle hrf_fun parameter ---
  if (!is.null(hrf_fun)) {
    if (!rlang::is_formula(hrf_fun) && !is.function(hrf_fun)) {
      stop("`hrf_fun` must be a function or formula (e.g., ~hrf_column)", call. = FALSE)
    }

    # Warn if both basis (non-default) and hrf_fun specified
    if (!missing(basis) && !identical(basis, "spmg1")) {
      warning("Both `basis` and `hrf_fun` specified; `hrf_fun` takes precedence.", call. = FALSE)
    }
  }
  # -----------------------------

  # An omitted nbasis lets basis sets ("fir", "bspline", ...) use their own size
  basis_obj <- make_hrf(basis, lag, nbasis = if (missing(nbasis)) NULL else nbasis,
                        label = .basis_fun_label(substitute(basis)))

  # Call the internal constructor, passing quosures directly
  ret <- hrfspec(
    vars = vars, # Pass list of quosures captured by enquos
    basis = basis_obj,
    hrf_fun = hrf_fun,  # per-onset HRF generator (function or formula)
    onsets = onsets,
    durations = durations,
    prefix = prefix,
    subset = subset_quo, # Capture subset with its calling environment
    precision = precision,
    contrasts = contrasts, ## Pass validated list of contrast specs
    summate = summate,
    normalize = normalize,
    id = final_id # Pass the determined ID
    )

  ret
}


#' Internal constructor for hrfspec objects
#' 
#' Creates the hrfspec list structure. Called by `hrf()`.
#' Generates termname and label from the input variables/expressions.
#'
#' @param vars List of quosures representing variables/expressions.
#' @param label Optional label for the term (if NULL, generated automatically).
#' @param basis An `HRF` object.
#' @param ... Other arguments passed from `hrf()` (onsets, durations, prefix, subset, etc.)
#' @return An `hrfspec` instance (list with class `hrfspec`).
#' @noRd
#' @keywords internal
hrfspec <- function(vars, label=NULL, basis=fmrihrf::HRF_SPMG1, ...) {

  assert_that(inherits(basis, "HRF"))
  
  # Generate varnames and termname from quosures
  var_labels <- sapply(vars, rlang::as_label)
  # Simple heuristic for names: use symbol directly, otherwise make.names on label
  varnames <- sapply(vars, function(q) {
       expr <- rlang::quo_get_expr(q)
       if (rlang::is_symbol(expr)) as.character(expr) else make.names(rlang::as_label(expr))
  })
  
  termname <- paste0(varnames, collapse=":")
  
  # Generate label if not provided
  if (is.null(label)) {
      label <- paste0("hrf(", paste0(var_labels, collapse=","), ")")
  }
  
  # Capture other arguments passed via ... from hrf()
  other_args <- list(...)
  
  ret <- list(
    name = termname,
    label = label,
    id = other_args$id,
    vars = vars,
    varnames = varnames,
    hrf = basis,
    hrf_fun = other_args$hrf_fun,  # per-onset HRF generator (function or formula)
    onsets = other_args$onsets,
    durations = other_args$durations,
    prefix = other_args$prefix,
    subset = other_args$subset, # Should be an expression
    precision = other_args$precision %||% 0.3,
    contrasts = other_args$contrasts, # Already validated list or NULL
    summate = other_args$summate %||% TRUE,
    normalize = other_args$normalize %||% FALSE
    # data_env is NOT stored here, added later during evaluation
  )
  
  # If user supplied an explicit id (or via name= alias), use it as the primary term name
  if (!is.null(other_args$id)) {
      ret$name <- other_args$id
  }
  
  class(ret) <- c("hrfspec", "list")
  ret
}



#' Get number of basis functions from hrfspec
#'
#' @param x An hrfspec object
#' @param ... Additional arguments (unused)
#' @return The number of basis functions
#' @examples
#' # Create hrfspec with canonical HRF (1 basis function)
#' spec1 <- hrf(condition, basis = "spmg1")
#' nbasis(spec1)
#'
#' # Create hrfspec with derivative HRF (2 basis functions)
#' spec2 <- hrf(condition, basis = "spmg2")
#' nbasis(spec2)
#'
#' # Create hrfspec with FIR basis (custom number of basis functions)
#' spec3 <- hrf(condition, basis = "fir", nbasis = 10)
#' nbasis(spec3)
#' @export
nbasis.hrfspec <- function(x, ...) {
  fmrihrf::nbasis(x$hrf)
}

#' Retrieve contrast specifications from an hrfspec
#'
#' @param x An `hrfspec` object.
#' @param ... Unused.
#' @return The list of contrast specifications attached to the hrfspec, or `NULL`.
#' @export
#' @examples
#' condition <- factor(c("A", "B"))
#' cset <- contrast_set(
#'   diff = column_contrast(pattern_A = "condition.A", pattern_B = "condition.B", name = "diff")
#' )
#' spec <- hrf(condition, contrasts = cset)
#' contrasts(spec)
contrasts.hrfspec <- function(x, ...) {
  x$contrasts
}

#' @export
construct.hrfspec <- function(x, model_spec, ...) {
  et <- construct_event_term(x, model_spec)

  # Set the term_tag attribute on the event_term before returning
  # This ensures that when convolution happens later, the correct column names are generated
  term_tag <- x$id %||% x$name
  attr(et, "term_tag") <- term_tag

  # DON'T convolve here - let build_event_model_design_matrix handle convolution
  # Just return the event_term with hrfspec attached
  # The hrfspec is already attached in construct_event_term

  # Propagate hrf_fun for per-onset HRF generation during convolution
  if (!is.null(x$hrf_fun)) {
    attr(et, "hrf_fun") <- x$hrf_fun
  }

  # Handle add_sum flag if present in the hrfspec (set by trialwise)
  # Store this as an attribute for later processing during convolution
  if (isTRUE(x$add_sum)) {
    attr(et, "add_sum") <- TRUE
    attr(et, "add_sum_label") <- x$id %||% x$name
  }

  # Return the event_term directly, not a convolved_term
  et
}

#' trialwise
#'
#' Generate one regressor per trial (plus an optional grand-mean column)
#' by delegating everything to `hrf()`.
#'
#' Use it **only on the RHS** of an event-model formula:
#'
#'     onset ~ trialwise(basis = "spmg1", add_sum = TRUE)
#'
#' @param basis,lag,nbasis Passed straight to `hrf()`.
#' @param durations Optional durations override (passed to `hrf()`). If NULL, uses
#'        the durations argument from `event_model`.
#' @param add_sum If TRUE, append a column that is the average of all
#'                trialwise columns (useful as a conventional main effect).
#' @param label Term label / prefix for the generated columns.
#' @param normalize logical; if TRUE, each trialwise regressor column is peak-normalized
#'        so that `max(abs(column)) == 1`. When events have different durations, longer events
#'        produce taller peaks after convolution. Normalizing equalizes amplitudes so that beta
#'        estimates are comparable across trials regardless of duration. Particularly useful for
#'        MVPA and RSA analyses. Default FALSE.
#' @return An `hrfspec` term to be used on the RHS of an event-model formula.
#' @examples
#' # Create example trial data for beta-series analysis
#' trial_data <- data.frame(
#'   onset = c(2, 8, 14, 20, 26),
#'   run = c(1, 1, 1, 1, 1)
#' )
#'
#' # Create sampling frame (30 TRs, TR=2s)
#' sframe <- fmrihrf::sampling_frame(blocklens = 30, TR = 2)
#'
#' # Basic trialwise model - creates one regressor per trial
#' emod_trials <- event_model(onset ~ trialwise(),
#'                           data = trial_data,
#'                           block = ~run,
#'                           sampling_frame = sframe)
#' print(emod_trials)
#'
#' # Trialwise with different basis and grand mean
#' emod_trials_mean <- event_model(onset ~ trialwise(basis = "spmg2", add_sum = TRUE),
#'                                data = trial_data,
#'                                block = ~run,
#'                                sampling_frame = sframe)
#' print(emod_trials_mean)
#' @export
trialwise <- function(basis   = "spmg1",
                      lag     = 0,
                      nbasis  = 1,
                      add_sum = FALSE,
                      label   = "trial",
                      durations = NULL,
                      normalize = FALSE) {

  # Create an expression that will evaluate .trial_factor(length(onsets)) 
  # when the onsets variable is available at evaluation time
  trial_expr <- rlang::expr(.trial_factor(length(onsets)))

  # Name a plain-function basis here, where its expression is still visible
  if (is.function(basis) && !inherits(basis, "HRF")) {
    basis <- fmrihrf::gen_hrf(basis, name = .basis_fun_label(substitute(basis)))
  }

  term <- hrf(!!trial_expr,  # Use !! to inject the expression
              basis = basis,
              lag   = lag,
              nbasis = if (missing(nbasis)) NULL else nbasis,
              durations = durations,
              normalize = normalize,
              id     = label) # Use id argument for naming

  term$add_sum <- isTRUE(add_sum) # flag for construct() to act upon
  term
}

#' Internal helper for generating trial factors
#'
#' @param n Length of the factor to generate.
#' @return A factor of length `n` with zero-padded sequential levels.
#' @keywords internal
#' @noRd
.trial_factor <- function(n) {
  pad <- nchar(as.character(n))
  factor(sprintf(paste0("%0", pad, "d"), seq_len(n)))
}


# --- HRF Generator Functions for per-onset HRF specification ---

#' Create duration-based boxcar HRF generator
#'
#' Creates a generator function for use with the `hrf_fun` parameter in `hrf()`.
#' The generator produces boxcar HRFs where each event's duration determines
#' the boxcar width.
#'
#' @param normalize Logical; whether to normalize the boxcar HRF. Default TRUE.
#' @param min_duration Numeric; minimum duration to use (prevents zero-width boxcars). Default 0.1.
#'
#' @return A function that takes an event data frame and returns a list of HRF objects.
#'
#' @examples
#' \donttest{
#' trial_data <- data.frame(
#'   onset = c(0, 10, 25),
#'   duration = c(2, 5, 3),
#'   condition = c("A", "B", "A"),
#'   run = 1
#' )
#' sf <- fmrihrf::sampling_frame(blocklens = 50, TR = 2)
#'
#' emod <- event_model(
#'   onset ~ hrf(condition, hrf_fun = boxcar_hrf_gen()),
#'   data = trial_data, block = ~run, sampling_frame = sf, durations = trial_data$duration
#' )
#' print(emod)
#' }
#'
#' @seealso [weighted_hrf_gen()] for weighted impulse HRFs
#' @export
boxcar_hrf_gen <- function(normalize = TRUE, min_duration = 0.1) {
  function(d) {
    lapply(d$duration, function(dur) {
      fmrihrf::hrf_boxcar(width = max(dur, min_duration), normalize = normalize)
    })
  }
}


#' Create duration-aware normalized HRF generator
#'
#' Generator for use with the `hrf_fun` parameter in `hrf()`. Creates per-onset
#' HRFs by convolving a base HRF with a boxcar of each event's duration, then
#' normalizing to peak = 1. This preserves the temporal shape of the
#' duration-modulated response while equalizing amplitude across events.
#'
#' @param base Base HRF object. Default `fmrihrf::HRF_SPMG1`.
#' @param min_duration Minimum duration (prevents zero-width events). Default 0.
#' @return A function for use with `hrf_fun` in `hrf()`.
#'
#' @examples
#' # Events with variable durations
#' trial_data <- data.frame(
#'   onset = c(0, 10, 25),
#'   duration = c(2, 5, 3),
#'   condition = c("A", "B", "A"),
#'   run = 1
#' )
#' sf <- fmrihrf::sampling_frame(blocklens = 50, TR = 2)
#'
#' emod <- event_model(
#'   onset ~ hrf(condition, hrf_fun = duration_hrf_gen()),
#'   data = trial_data, block = ~run, sampling_frame = sf,
#'   durations = trial_data$duration
#' )
#' print(emod)
#'
#' @seealso [boxcar_hrf_gen()] for duration-based boxcar HRFs
#' @export
duration_hrf_gen <- function(base = fmrihrf::HRF_SPMG1, min_duration = 0) {
  function(d) {
    lapply(d$duration, function(dur) {
      dur <- max(dur, min_duration)
      if (dur == 0) return(base)
      fmrihrf::block_hrf(base, width = dur, normalize = TRUE)
    })
  }
}


#' Create weighted HRF generator from list columns
#'
#' Creates a generator function for use with the `hrf_fun` parameter in `hrf()`.
#' The generator produces weighted impulse HRFs from columns containing lists
#' of sub-event times and weights.
#'
#' @param times_col Character; name of the column containing sub-event times (relative or absolute).
#' @param weights_col Character; name of the column containing sub-event weights.
#' @param relative Logical; if TRUE, times are relative to event onset; if FALSE, times are absolute
#'   and will be converted to relative by subtracting the onset. Default FALSE (absolute times).
#' @param method Character; interpolation method for `hrf_weighted()`. Default "constant".
#' @param normalize Logical; whether to normalize the weighted HRF. Default FALSE.
#'
#' @return A function that takes an event data frame and returns a list of HRF objects.
#'
#' @examples
#' \donttest{
#' trial_data <- data.frame(
#'   onset = c(0, 20),
#'   sub_times = I(list(c(0, 1, 2), c(0, 3, 6))),
#'   sub_weights = I(list(c(0.2, 0.5, 0.3), c(0.1, 0.6, 0.3))),
#'   run = 1
#' )
#' sf <- fmrihrf::sampling_frame(blocklens = 50, TR = 2)
#'
#' emod <- event_model(
#'   onset ~ hrf(onset, hrf_fun = weighted_hrf_gen("sub_times", "sub_weights", relative = TRUE)),
#'   data = trial_data, block = ~run, sampling_frame = sf
#' )
#' print(emod)
#' }
#'
#' @seealso [boxcar_hrf_gen()] for duration-based boxcar HRFs
#' @export
weighted_hrf_gen <- function(times_col = "sub_times", weights_col = "sub_weights",
                              relative = FALSE, method = "constant", normalize = FALSE) {
  function(d) {
    for (col in c(times_col, weights_col)) {
      if (!col %in% names(d)) {
        stop(sprintf("weighted_hrf_gen: column '%s' not found in event data", col), call. = FALSE)
      }
    }

    Map(function(times, weights, onset) {
      # Convert absolute times to relative if needed
      rel_times <- if (relative) times else times - onset
      fmrihrf::hrf_weighted(times = rel_times, weights = weights,
                            method = method, normalize = normalize)
    }, d[[times_col]], d[[weights_col]], d$onset)
  }
}
