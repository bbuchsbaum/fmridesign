#' Extension Registry for External HRF Specifications
#'
#' This file provides an extension mechanism for packages to register
#' their own HRF specification types with fmridesign.
#'

# Create a private environment to store registered extensions
#' Internal registry environment for external HRF specs
#'
#' Holds registration data for external HRF specification classes.
#' @keywords internal
#' @return An environment used internally as a registry.
.fmridesign_extensions <- new.env(parent = emptyenv())

#' Register an External HRF Specification Type
#'
#' Register a new HRF specification class that can be used in event models.
#' This allows external packages to extend fmridesign with their own HRF types.
#'
#' @param spec_class Character string naming the class to register
#' @param package Character string naming the package providing the class
#' @param convolved_class Optional character string naming the associated convolved term class
#' @param requires_external_processing Logical indicating if this spec should be skipped
#'   during standard convolution (e.g., for AFNI terms that are processed externally)
#' @param formula_functions Optional character vector of function names that should
#'   be recognised in formulas and mapped to this HRF specification class.
#'
#' @return Invisible NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # In an external package's .onLoad function:
#' register_hrfspec_extension(
#'   spec_class = "afni_hrfspec",
#'   package = "afnireg",
#'   convolved_class = "afni_hrf_convolved_term",
#'   requires_external_processing = TRUE,
#'   formula_functions = "afni_hrf"
#' )
#' }
register_hrfspec_extension <- function(spec_class, 
                                      package, 
                                      convolved_class = NULL,
                                      requires_external_processing = FALSE,
                                      formula_functions = NULL) {
  if (!is.character(spec_class) || length(spec_class) != 1) {
    stop("spec_class must be a single character string", call. = FALSE)
  }
  if (!is.character(package) || length(package) != 1) {
    stop("package must be a single character string", call. = FALSE)
  }
  if (!is.null(formula_functions) && !is.character(formula_functions)) {
    stop("formula_functions must be NULL or a character vector", call. = FALSE)
  }
  
  # Store the registration info
  .fmridesign_extensions[[spec_class]] <- list(
    spec_class = spec_class,
    package = package,
    convolved_class = convolved_class,
    requires_external_processing = requires_external_processing,
    formula_functions = formula_functions,
    registered_at = Sys.time()
  )
  
  invisible(NULL)
}

#' Check if a Class is a Registered External HRF Specification
#'
#' @param x An object or character string class name
#' @return Logical indicating if the class is registered as an external HRF spec
#' @export
#' @examples
#' register_hrfspec_extension(
#'   spec_class = "demo_hrfspec",
#'   package = "demoPkg"
#' )
#' is_external_hrfspec("demo_hrfspec")
is_external_hrfspec <- function(x) {
  if (is.character(x)) {
    classes <- x
  } else {
    classes <- class(x)
  }

  any(classes %in% names(.fmridesign_extensions))
}

#' Get Information About a Registered External HRF Specification
#'
#' @param spec_class Character string naming the class
#' @return A list with registration information, or NULL if not registered
#' @export
#' @examples
#' register_hrfspec_extension(
#'   spec_class = "demo_hrfspec",
#'   package = "demoPkg",
#'   requires_external_processing = TRUE,
#'   formula_functions = "demo_hrf"
#' )
#' get_external_hrfspec_info("demo_hrfspec")
get_external_hrfspec_info <- function(spec_class) {
  .fmridesign_extensions[[spec_class]]
}

#' List All Registered External HRF Specifications
#'
#' @return A character vector of registered class names
#' @export
#' @examples
#' register_hrfspec_extension(
#'   spec_class = "demo_hrfspec",
#'   package = "demoPkg"
#' )
#' list_external_hrfspecs()
list_external_hrfspecs <- function() {
  names(.fmridesign_extensions)
}

#' Check if an Object Requires External Processing
#'
#' Determines if an HRF specification or convolved term should be
#' handled by external tools rather than R's standard convolution.
#'
#' @param x An object to check
#' @return Logical indicating if external processing is required
#' @export
#' @examples
#' register_hrfspec_extension(
#'   spec_class = "demo_hrfspec",
#'   package = "demoPkg",
#'   requires_external_processing = TRUE
#' )
#' requires_external_processing("demo_hrfspec")
requires_external_processing <- function(x) {
  # Check if it's a registered external spec
  if (is_external_hrfspec(x)) {
    # Get the info for the first matching class
    for (cls in class(x)) {
      info <- get_external_hrfspec_info(cls)
      if (!is.null(info)) {
        return(info$requires_external_processing)
      }
    }
  }

  # Check for convolved terms from external packages
  # This handles classes like "afni_hrf_convolved_term"
  if (inherits(x, "convolved_term")) {
    # Check if the underlying hrfspec requires external processing
    hrfspec <- attr(x, "hrfspec")
    if (!is.null(hrfspec) && is_external_hrfspec(hrfspec)) {
      return(requires_external_processing(hrfspec))
    }
  }

  FALSE
}

#' Get the HRF Function Name for External Specifications
#'
#' Returns the function name(s) that should be recognized in formulas
#' for a given external HRF specification class.
#'
#' @param spec_class Character string naming the class
#' @return Character vector of function names, or NULL if not registered
#' @export
#' @examples
#' register_hrfspec_extension(
#'   spec_class = "demo_hrfspec",
#'   package = "demoPkg",
#'   formula_functions = c("demo_hrf", "demo_trialwise")
#' )
#' get_external_hrfspec_functions("demo_hrfspec")
get_external_hrfspec_functions <- function(spec_class) {
  info <- get_external_hrfspec_info(spec_class)
  if (is.null(info)) {
    return(NULL)
  }

  funcs <- info$formula_functions
  if (!is.null(funcs)) {
    return(funcs)
  }

  # Fallback for legacy registrations created before formula function
  # names were recorded in the registry.
  switch(spec_class,
    "afni_hrfspec" = c("afni_hrf"),
    "afni_trialwise_hrfspec" = c("afni_trialwise"),
    NULL
  )
}

#' Get All External HRF Function Names
#'
#' Returns all function names that should be recognized in formulas
#' from registered external packages.
#'
#' @return Character vector of function names
#' @export
#' @examples
#' register_hrfspec_extension(
#'   spec_class = "demo_hrfspec",
#'   package = "demoPkg",
#'   formula_functions = "demo_hrf"
#' )
#' get_all_external_hrf_functions()
get_all_external_hrf_functions <- function() {
  spec_classes <- list_external_hrfspecs()
  if (length(spec_classes) == 0) {
    return(character(0))
  }

  funcs <- unlist(lapply(spec_classes, get_external_hrfspec_functions))
  unique(funcs[!is.null(funcs)])
}
