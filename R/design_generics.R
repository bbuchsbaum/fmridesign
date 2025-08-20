#' Generic functions for fmridesign package
#' 
#' This file contains the generic functions used throughout the fmridesign package.
#' These generics define the interface for working with event models, baseline models,
#' and related design components.

#' Construct an event model
#' 
#' This function creates an event-based fMRI regression model, represented as a data structure.
#' 
#' @param formula_or_list The model specification, typically a `formula`.
#' @param data A data frame containing the experimental design.
#' @param block A formula specifying the block structure of the design.
#' @param sampling_frame A sampling frame defining the temporal and block structure.
#' @param drop_empty Logical value indicating whether to drop empty factor levels.
#' @param durations A numeric vector specifying the duration (in seconds) of each event.
#' @param ... Additional arguments to be passed to methods.
#' 
#' @export
event_model <- function(formula_or_list, data, block, sampling_frame, drop_empty=TRUE, durations=0, ...) { 
  UseMethod("event_model") 
}

# baseline_model is defined as a regular function in baseline_model.R
# not as a generic with methods

#' Extract cells from a design object
#' 
#' @param x The object to extract cells from.
#' @param drop.empty Logical indicating whether to drop empty cells (default: TRUE).
#' @param ... Additional arguments (e.g., exclude_basis for convolved_term method).
#' @export
cells <- function(x, drop.empty = TRUE, ...) UseMethod("cells")

#' Extract conditions from a design object
#' 
#' @param x The object to extract conditions from.
#' @param drop.empty Logical whether to drop conditions with no events (default: TRUE).
#' @param expand_basis Logical whether to expand basis functions (default: FALSE).
#' @param ... Additional arguments.
#' @export
conditions <- function(x, drop.empty = TRUE, expand_basis = FALSE, ...) UseMethod("conditions")

#' Convolve events with a hemodynamic response function
#' 
#' @param x The events to convolve.
#' @param hrf The hemodynamic response function.
#' @param sampling_frame The sampling frame.
#' @param drop.empty Logical indicating whether to drop columns with all zeros.
#' @param summate Logical indicating whether to sum convolved signals.
#' @param precision Numeric specifying the temporal precision for convolution.
#' @param ... Additional arguments.
#' @export
convolve <- function(x, hrf, sampling_frame, drop.empty = TRUE, summate = TRUE, precision = .1, ...) UseMethod("convolve")

#' Extract or construct a design matrix
#' 
#' @param x The object to extract design matrix from.
#' @param blockid Block ID(s) to extract (for baseline_term method).
#' @param allrows Whether to return all rows (for baseline_term method).
#' @param drop.empty Whether to drop empty columns (for event_term method).
#' @param ... Additional arguments.
#' @export
design_matrix <- function(x, ...) { UseMethod("design_matrix") }

#' Extract elements from an object
#' 
#' @param x The object to extract elements from.
#' @param what Character string specifying what to extract: "values" for numeric/actual values, 
#'   or "labels" for descriptive labels/names.
#' @param transformed Logical indicating whether to return transformed values. Default is TRUE.
#' @param ... Additional arguments.
#' @export
elements <- function(x, ...) UseMethod("elements")

# Note: onsets, durations, and blockids generics are re-exported from fmrihrf
# See R/fmrihrf-reexports.R for documentation


#' Extract term matrices
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @export
term_matrices <- function(x, ...) UseMethod("term_matrices")

#' Extract event terms
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @export
event_terms <- function(x, ...) UseMethod("event_terms")

#' Extract baseline terms
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @export
baseline_terms <- function(x, ...) UseMethod("baseline_terms")


#' Extract event table
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @export
event_table <- function(x, ...) UseMethod("event_table")

#' Check if categorical
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @export
is_categorical <- function(x, ...) UseMethod("is_categorical")

#' Check if continuous
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @export
is_continuous <- function(x, ...) UseMethod("is_continuous")

#' Extract longnames
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @export
longnames <- function(x, ...) UseMethod("longnames")

#' Extract shortnames
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @export
shortnames <- function(x, ...) UseMethod("shortnames")

#' Extract columns
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @export
columns <- function(x, ...) UseMethod("columns")

#' Extract term indices
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @export
term_indices <- function(x, ...) UseMethod("term_indices")

# baseline_term is defined as a regular function in baseline_model.R
# not as a generic

#' Construct method
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @export
construct <- function(x, ...) UseMethod("construct")

#' Split by block
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @export
split_by_block <- function(x, ...) UseMethod("split_by_block")

#' Split onsets
#' 
#' @param x The object.
#' @param sframe The sampling frame object containing timing information.
#' @param global Whether onsets are in global time units (across all runs).
#' @param blocksplit Whether to split onsets by blocks.
#' @param ... Additional arguments.
#' @export
split_onsets <- function(x, sframe, global = FALSE, blocksplit = FALSE, ...) UseMethod("split_onsets")

#' Extract term names
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @export
term_names <- function(x, ...) UseMethod("term_names")

#' Extract contrasts
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @export
contrasts <- function(x, ...) UseMethod("contrasts")

#' Compute correlation map
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @export
correlation_map <- function(x, ...) UseMethod("correlation_map")

#' Compute design map
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @export
design_map <- function(x, ...) UseMethod("design_map")

#' Compute F-contrasts
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @export
Fcontrasts <- function(x, ...) UseMethod("Fcontrasts")

#' Compute contrast weights
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @export
contrast_weights <- function(x, ...) UseMethod("contrast_weights")

#' Extract regressors
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @export
regressors <- function(x, ...) UseMethod("regressors")


#' Extract parent terms
#' 
#' @param x The object.
#' @keywords internal
#' @noRd
parent_terms <- function(x) UseMethod("parent_terms")