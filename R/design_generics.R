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
#' @return An `event_model` object describing the task design.
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
#' @return A data.frame/tibble of cells (categorical combinations) relevant to `x`.
#' @examples
#' sframe <- fmrihrf::sampling_frame(blocklens = 6, TR = 1)
#' bmod <- baseline_model(sframe = sframe)
#' head(cells(bmod))
#' @export
cells <- function(x, drop.empty = TRUE, ...) UseMethod("cells")

#' Extract conditions from a design object
#' 
#' @param x The object to extract conditions from.
#' @param drop.empty Logical whether to drop conditions with no events (default: TRUE).
#' @param expand_basis Logical whether to expand basis functions (default: FALSE).
#' @param ... Additional arguments.
#' @return A character vector of condition names.
#' @examples
#' term <- event_term(
#'   list(condition = factor(c("A", "B", "A"))),
#'   onsets = c(0, 10, 20),
#'   blockids = c(1, 1, 1)
#' )
#' conditions(term)
#' conditions(term, expand_basis = TRUE)
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
#' @param normalize Logical; if TRUE, each convolved regressor column is peak-normalized. Default FALSE.
#' @param ... Additional arguments.
#' @return A matrix-like (often tibble) of convolved regressors.
#' @export
convolve <- function(x, hrf, sampling_frame, drop.empty = TRUE, summate = TRUE, precision = .1, normalize = FALSE, ...) UseMethod("convolve")

#' Extract or construct a design matrix
#' 
#' @param x The object to extract design matrix from.
#' @param blockid Block ID(s) to extract (for baseline_term method).
#' @param allrows Whether to return all rows (for baseline_term method).
#' @param drop.empty Whether to drop empty columns (for event_term method).
#' @param ... Additional arguments.
#' @return A matrix-like object (often tibble) with rows = scans, cols = regressors.
#' @export
design_matrix <- function(x, ...) { UseMethod("design_matrix") }

#' Extract elements from an object
#'
#' @param x The object to extract elements from.
#' @param what Character string specifying what to extract: "values" for numeric/actual values,
#'   or "labels" for descriptive labels/names.
#' @param transformed Logical indicating whether to return transformed values. Default is TRUE.
#' @param ... Additional arguments.
#' @return Requested elements; structure depends on method (e.g., numeric values or labels).
#' @examples
#' # Create an event term with mixed categorical and continuous events
#' term <- event_term(
#'   list(
#'     condition = factor(c("A", "B", "A", "B")),
#'     intensity = c(1.2, 0.8, 1.5, 0.9)
#'   ),
#'   onsets = c(0, 10, 20, 30),
#'   blockids = c(1, 1, 1, 1)
#' )
#'
#' # Extract values (actual numeric/factor codes)
#' elements(term, what = "values")
#'
#' # Extract labels (descriptive names/levels)
#' elements(term, what = "labels")
#' @export
elements <- function(x, ...) UseMethod("elements")

# Note: onsets, durations, and blockids generics are re-exported from fmrihrf
# See R/fmrihrf-reexports.R for documentation


#' Extract term matrices
#'
#' @param x The object.
#' @param ... Additional arguments.
#' @return A list of matrices/tibbles, one per term.
#' @examples
#' # Create a simple experimental design with event model
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30),
#'   run = 1,
#'   cond = factor(c("A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#' emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
#'
#' # Extract term matrices - returns list with one matrix per term
#' term_mats <- term_matrices(emod)
#' names(term_mats)     # Shows term names
#' ncol(term_mats[[1]]) # Number of columns for first term
#'
#' # Create baseline model and extract its term matrices
#' bmod <- baseline_model(sframe = sframe)
#' baseline_mats <- term_matrices(bmod)
#' names(baseline_mats) # Shows baseline term names
#' @export
term_matrices <- function(x, ...) UseMethod("term_matrices")

#' Extract event terms
#'
#' @param x The object.
#' @param ... Additional arguments.
#' @return A named list of event term objects.
#' @examples
#' # Create a simple experimental design
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30),
#'   run = 1,
#'   cond = factor(c("A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#' emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
#'
#' # Extract event terms (named list of event term objects)
#' terms_list <- event_terms(emod)
#' names(terms_list)
#' @export
event_terms <- function(x, ...) UseMethod("event_terms")

#' Extract baseline terms
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @return A named list of baseline term objects.
#' @examples
#' sframe <- fmrihrf::sampling_frame(blocklens = 6, TR = 1)
#' bmod <- baseline_model(sframe = sframe)
#' baseline_terms(bmod)
#' @export
baseline_terms <- function(x, ...) UseMethod("baseline_terms")


#' Extract event table
#'
#' @param x The object.
#' @param ... Additional arguments.
#' @return A data.frame/tibble of event rows.
#' @examples
#' term <- event_term(
#'   list(condition = factor(c("A", "B", "A"))),
#'   onsets = c(0, 10, 20),
#'   blockids = c(1, 1, 1)
#' )
#' event_table(term)
#' @export
event_table <- function(x, ...) UseMethod("event_table")

#' Retrieve per-event condition assignments
#'
#' @param x The object of interest.
#' @param drop.empty Logical; drop unused levels when `TRUE`.
#' @param ... Additional arguments passed to methods.
#' @return Typically a factor aligned with the events of `x`.
#' @examples
#' term <- event_term(
#'   list(condition = factor(c("A", "B", "A"))),
#'   onsets = c(0, 10, 20),
#'   blockids = c(1, 1, 1)
#' )
#' event_conditions(term)
#' @export
event_conditions <- function(x, drop.empty = FALSE, ...) UseMethod("event_conditions")

#' Retrieve canonical event information
#'
#' @param x The object to summarise.
#' @param drop.empty Logical; whether to drop empty conditions in the resulting factor.
#' @param ... Additional arguments passed to methods.
#' @return A data.frame (or tibble) with onset, duration, block, and condition columns.
#' @examples
#' # Create an event term with condition factor
#' term <- event_term(
#'   list(condition = factor(c("A", "B", "A"))),
#'   onsets = c(0, 10, 20),
#'   blockids = c(1, 1, 1)
#' )
#'
#' # Extract canonical event information
#' evt_info <- events(term)
#' print(evt_info)
#' @export
events <- function(x, drop.empty = FALSE, ...) UseMethod("events")

#' Check if categorical
#'
#' @param x The object.
#' @param ... Additional arguments.
#' @return Logical scalar indicating whether `x` is categorical.
#' @examples
#' # Create a categorical event from factor data
#' cat_event <- event_factor(
#'   factor(c("faces", "houses", "faces", "houses")),
#'   name = "condition",
#'   onsets = c(0, 10, 20, 30),
#'   blockids = rep(1, 4)
#' )
#' is_categorical(cat_event)  # Returns TRUE
#'
#' # Create a continuous event from numeric data
#' cont_event <- event_variable(
#'   c(1.2, 0.8, 1.5, 0.9),
#'   name = "reaction_time",
#'   onsets = c(0, 10, 20, 30),
#'   blockids = rep(1, 4)
#' )
#' is_categorical(cont_event)  # Returns FALSE
#'
#' # Event term with mixed types is considered categorical
#' mixed_term <- event_term(
#'   list(condition = factor(c("A", "B", "A", "B")),
#'        modulator = c(1.1, 0.9, 1.2, 0.8)),
#'   onsets = c(0, 10, 20, 30),
#'   blockids = rep(1, 4)
#' )
#' is_categorical(mixed_term)  # Returns TRUE
#' @export
is_categorical <- function(x, ...) UseMethod("is_categorical")

#' Check if continuous
#'
#' @param x The object.
#' @param ... Additional arguments.
#' @return Logical scalar indicating whether `x` is continuous.
#' @examples
#' # Create a continuous event from numeric vector
#' cont_event <- event_variable(
#'   c(1.2, 0.8, 1.5, 0.9),
#'   name = "reaction_time",
#'   onsets = c(0, 10, 20, 30),
#'   blockids = rep(1, 4)
#' )
#' is_continuous(cont_event)  # Returns TRUE
#'
#' # Create a continuous event from matrix
#' mat_event <- event_matrix(
#'   matrix(c(1.1, 0.9, 1.2, 0.8, 2.1, 1.9, 2.2, 1.8), nrow = 4),
#'   name = "coordinates",
#'   onsets = c(0, 10, 20, 30),
#'   blockids = rep(1, 4)
#' )
#' is_continuous(mat_event)  # Returns TRUE
#'
#' # Categorical event is not continuous
#' cat_event <- event_factor(
#'   factor(c("faces", "houses", "faces", "houses")),
#'   name = "condition",
#'   onsets = c(0, 10, 20, 30),
#'   blockids = rep(1, 4)
#' )
#' is_continuous(cat_event)  # Returns FALSE
#'
#' # Event term with all continuous events
#' cont_term <- event_term(
#'   list(rt = c(1.1, 0.9, 1.2, 0.8),
#'        accuracy = c(0.95, 0.87, 0.92, 0.88)),
#'   onsets = c(0, 10, 20, 30),
#'   blockids = rep(1, 4)
#' )
#' is_continuous(cont_term)  # Returns TRUE
#' @export
is_continuous <- function(x, ...) UseMethod("is_continuous")

#' Extract longnames
#'
#' @param x The object.
#' @param ... Additional arguments.
#' @return Character vector of long (fully qualified) names.
#' @examples
#' # Create a simple event term with one condition factor
#' term <- event_term(
#'   list(condition = factor(c("A", "B", "A"))),
#'   onsets = c(0, 10, 20),
#'   blockids = c(1, 1, 1)
#' )
#' longnames(term)  # Returns: "condition.A" "condition.B"
#'
#' # Create event term with multiple factors
#' term2 <- event_term(
#'   list(
#'     category = factor(c("face", "scene", "face")),
#'     attention = factor(c("attend", "attend", "ignore"))
#'   ),
#'   onsets = c(0, 10, 20),
#'   blockids = c(1, 1, 1)
#' )
#' longnames(term2)
#' # Returns: "category.face_attention.attend"
#' #          "category.scene_attention.attend"
#' #          "category.face_attention.ignore"
#' @export
longnames <- function(x, ...) UseMethod("longnames")

#' Extract shortnames
#'
#' @param x The object.
#' @param ... Additional arguments.
#' @return Character vector of short names.
#' @examples
#' # Create a simple event term with one condition factor
#' term <- event_term(
#'   list(condition = factor(c("A", "B", "A"))),
#'   onsets = c(0, 10, 20),
#'   blockids = c(1, 1, 1)
#' )
#' shortnames(term)  # Returns: "A" "B"
#'
#' # Create event term with multiple factors
#' term2 <- event_term(
#'   list(
#'     category = factor(c("face", "scene", "face")),
#'     attention = factor(c("attend", "attend", "ignore"))
#'   ),
#'   onsets = c(0, 10, 20),
#'   blockids = c(1, 1, 1)
#' )
#' shortnames(term2)  # Returns: "face:attend" "scene:attend" "face:ignore"
#' @export
shortnames <- function(x, ...) UseMethod("shortnames")

#' Extract columns
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @return Character vector of column names produced by the object.
#' @examples
#' bs_basis <- BSpline(seq(0, 1, length.out = 5), degree = 3)
#' columns(bs_basis)
#' @export
columns <- function(x, ...) UseMethod("columns")

#' Extract term indices
#'
#' @param x The object.
#' @param ... Additional arguments.
#' @return Integer vector or list mapping term(s) to column indices.
#' @examples
#' # Create a sampling frame and event model
#' sf <- fmrihrf::sampling_frame(blocklens = c(100, 100), TR = 2)
#' events <- data.frame(
#'   onset = c(10, 30, 50, 70),
#'   condition = c("A", "B", "A", "B"),
#'   block = c(1, 1, 2, 2)
#' )
#' model <- event_model(onset ~ hrf(condition), events, ~ block, sf)
#'
#' # Get design matrix and extract term indices
#' dm <- design_matrix(model)
#' indices <- term_indices(dm)
#' print(indices)
#'
#' # Access indices for specific term
#' condition_indices <- indices[["condition"]]
#' print(condition_indices)
#' @export
term_indices <- function(x, ...) UseMethod("term_indices")

#' @rdname term_indices
#' @export
term_indices.default <- function(x, ...) {
  col_indices <- attr(x, "col_indices")
  if (is.null(col_indices)) {
    stop("Object does not have 'col_indices' attribute")
  }
  col_indices
}

# baseline_term is defined as a regular function in baseline_model.R
# not as a generic

#' Construct method
#'
#' @param x The object.
#' @param model_spec A model specification object (used by some methods).
#'   For baselinespec: typically a sampling_frame or list containing one.
#'   For hrfspec/covariatespec: contains data and other model information.
#' @param sampling_frame A sampling_frame object (used by covariatespec method).
#' @param ... Additional arguments.
#' @return A constructed object; return type depends on method.
#' @examples
#' sframe <- fmrihrf::sampling_frame(blocklens = 5, TR = 1)
#' drift_spec <- baseline(degree = 2, basis = "poly")
#' construct(drift_spec, sframe)
#' @export
construct <- function(x, ...) UseMethod("construct")

#' Split by block
#'
#' @param x The object.
#' @param ... Additional arguments.
#' @return A list split by block/run.
#' @export
#' @examples
#' \dontrun{
#' # Create experimental design with multiple runs
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30, 5, 15, 25, 35),
#'   run = c(1, 1, 1, 1, 2, 2, 2, 2),
#'   cond = factor(c("A", "B", "A", "B", "A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = c(40, 40), TR = 1)
#'
#' # Create an event model
#' emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
#'
#' # Example usage (when methods are implemented):
#' # block_list <- split_by_block(emod)
#' # length(block_list)  # Should return 2 (for 2 runs)
#' }
split_by_block <- function(x, ...) UseMethod("split_by_block")

#' Split onsets
#'
#' @param x The object.
#' @param sframe The sampling frame object containing timing information.
#' @param global Whether onsets are in global time units (across all runs).
#' @param blocksplit Whether to split onsets by blocks.
#' @param ... Additional arguments.
#' @return A list of onset vectors, one per block (unless `global=TRUE`).
#' @examples
#' # Create an event term with mixed conditions across blocks
#' conditions <- factor(c("A", "B", "A", "B", "A", "B"))
#' onsets <- c(5, 15, 25, 105, 115, 125)  # Events in blocks 1 and 2
#' blockids <- c(1, 1, 1, 2, 2, 2)
#'
#' term <- event_term(
#'   list(condition = conditions),
#'   onsets = onsets,
#'   blockids = blockids
#' )
#'
#' # Create sampling frame for two blocks of 50 TRs each
#' sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)
#'
#' # Split onsets by condition (default behavior)
#' onset_list <- split_onsets(term, sframe)
#' names(onset_list)  # Shows condition names
#' onset_list$condition.A  # Onsets for condition A
#'
#' # Split with global timing (onsets relative to start of experiment)
#' global_onsets <- split_onsets(term, sframe, global = TRUE)
#'
#' # Split by both condition and block
#' block_split <- split_onsets(term, sframe, blocksplit = TRUE)
#' @export
split_onsets <- function(x, sframe, global = FALSE, blocksplit = FALSE, ...) UseMethod("split_onsets")

#' Extract term names
#'
#' @param x The object.
#' @param ... Additional arguments.
#' @return Character vector of term names.
#' @examples
#' # Create sample event data
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30),
#'   run = 1,
#'   cond = factor(c("A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#'
#' # Event model example
#' emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
#' term_names(emod)  # Returns "cond"
#'
#' # Baseline model example
#' bmod <- baseline_model(basis = "poly", degree = 3, sframe = sframe)
#' term_names(bmod)  # Returns c("constant", "baseline_poly_3")
#' @export
term_names <- function(x, ...) UseMethod("term_names")

#' Extract contrasts
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @return A named list of contrast specifications.
#' @examples
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30),
#'   run = 1,
#'   cond = factor(c("A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#' cset <- contrast_set(
#'   diff = column_contrast(pattern_A = "cond.A", pattern_B = "cond.B", name = "diff")
#' )
#' emod <- event_model(onset ~ hrf(cond, contrasts = cset),
#'                     data = des, block = ~run, sampling_frame = sframe)
#' contrasts(emod)
#' @export
contrasts <- function(x, ...) UseMethod("contrasts")

#' Compute correlation map
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @return A ggplot2 object visualizing regressor correlations.
#' @examples
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30),
#'   run = 1,
#'   cond = factor(c("A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#' emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
#' correlation_map(emod)
#' @export
correlation_map <- function(x, ...) UseMethod("correlation_map")

#' Compute design map
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @return A ggplot2 object visualizing the design matrix.
#' @examples
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30),
#'   run = 1,
#'   cond = factor(c("A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#' emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
#' design_map(emod)
#' @export
design_map <- function(x, ...) UseMethod("design_map")

#' Residualize Data Against a Design
#'
#' Projects the data onto the orthogonal complement of the design, returning
#' residuals from an OLS fit. Specialized for `event_model`, `baseline_model`,
#' or a raw numeric design matrix.
#'
#' @param x A design object (`event_model`, `baseline_model`) or a numeric matrix (design matrix X).
#' @param data A numeric vector/matrix/data.frame of observations Y with rows matching `nrow(design_matrix(x))`.
#' @param cols Optional integer or character vector selecting columns of the design to project out.
#' @param ... Additional arguments passed to methods.
#' @return Residuals with the same dimensions as `data`.
#' @examples
#' # Simple example with a raw design matrix
#' X <- cbind(1, 1:5)
#' Y <- cbind(1:5, 2:6)
#' R <- residualize(X, Y)
#' dim(R)
#' @export
residualize <- function(x, data, cols = NULL, ...) UseMethod("residualize")

#' Column Metadata Map
#'
#' Returns a tibble with one row per column of the design matrix and
#' structured metadata (term, condition, basis, role, run, etc.).
#'
#' @param x An object containing or producing a design matrix (e.g., event_model, baseline_model).
#' @param ... Additional arguments passed to methods.
#' @return A tibble with per-column metadata.
#' @examples
#' # Create a simple event model
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30),
#'   run = 1,
#'   cond = factor(c("A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#' emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
#'
#' # Extract column metadata
#' colmap <- design_colmap(emod)
#' head(colmap)
#'
#' # Query specific columns
#' colmap[colmap$condition == "A", ]
#' @export
design_colmap <- function(x, ...) UseMethod("design_colmap")

#' Compute F-contrasts
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @return A named list of matrices with F-contrast weights.
#' @examples
#' des <- data.frame(
#'   onset = c(0, 10, 20, 30),
#'   run = 1,
#'   cond = factor(c("A", "B", "A", "B"))
#' )
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
#' emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
#' names(Fcontrasts(emod))
#' @export
Fcontrasts <- function(x, ...) UseMethod("Fcontrasts")

#' Compute contrast weights
#' 
#' @param x The object.
#' @param ... Additional arguments.
#' @return A named list of contrast weight objects or matrices.
#' @export
contrast_weights <- function(x, ...) UseMethod("contrast_weights")

#' Extract regressors
#'
#' @param x The object.
#' @param ... Additional arguments.
#' @return Character vector of regressor names for `x`.
#' @examples
#' # Create an event term with two conditions
#' term <- event_term(
#'   list(condition = factor(c("A", "B", "A", "B"))),
#'   onsets = c(0, 10, 20, 30),
#'   blockids = c(1, 1, 1, 1)
#' )
#'
#' # Create a sampling frame for timing information
#' sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 2)
#'
#' # Extract regressors convolved with canonical HRF
#' reg <- regressors(term, hrf = fmrihrf::HRF_SPMG1, sampling_frame = sframe)
#' names(reg)  # Shows regressor names: "condition.A" "condition.B"
#' @export
regressors <- function(x, ...) UseMethod("regressors")


#' Extract parent terms
#' 
#' @param x The object.
#' @keywords internal
#' @noRd
parent_terms <- function(x) UseMethod("parent_terms")
