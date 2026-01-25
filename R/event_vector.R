###############################################################################
# EVENT_VECTOR.R
#
# This file contains helper routines for cleaning names, checking ordering,
# constructing event model terms (and various event types), extracting design
# matrices, computing contrasts, and printing event-related objects for fMRI.
#
###############################################################################

## ============================================================================
## Section 1: Helper Functions (Moved to R/utils-internal.R)
## ============================================================================

# Removed .sanitizeName
# Removed is.increasing
# Removed is.strictly.increasing
# Removed .checkEVArgs

## ============================================================================
## Section 2: Event Term Construction
## ============================================================================

#' Create an event model term from a named list of variables.
#'
#' Generates an `event_term` object which represents the combination of one 
#' or more event sequences (e.g., a factor crossed with a numeric modulator).
#' It takes a list of variables (factors, numeric vectors, matrices, basis objects)
#' along with shared onsets, block IDs, and durations.
#' It uses the `EV` factory internally to create standardized `event` objects for each variable.
#'
#' @param evlist A named list of variables (factors, numeric, matrices, ParametricBasis objects).
#'        The names are used as variable identifiers within the term.
#' @param onsets Numeric vector of onset times (in seconds).
#' @param blockids Numeric vector of block numbers (non-decreasing integers).
#' @param durations Numeric vector of event durations (seconds, default is 0). 
#'        Can be scalar (recycled) or vector matching length of `onsets`.
#' @param subset Optional logical vector indicating which events to retain (applied before processing).
#'
#' @return A list object with class `c("event_term", "event_seq")`. Contains:
#'   \item{varname}{Concatenated variable names from `evlist`.} 
#'   \item{events}{A named list of the processed `event` objects.} 
#'   \item{subset}{The `subset` vector used.} 
#'   \item{event_table}{A tibble representing the combinations of descriptive levels/names 
#'                    for each event in the term, constructed using `elements(..., values=FALSE)`.} 
#'   \item{onsets}{Numeric vector of onsets (after processing/subsetting).} 
#'   \item{blockids}{Numeric vector of block IDs (after processing/subsetting).} 
#'   \item{durations}{Numeric vector of durations (after processing/subsetting).} 
#'
#' @examples 
#' x1 <- factor(rep(letters[1:3], 10))
#' x2 <- factor(rep(1:3, each = 10))
#' onsets <- seq(1, 100, length.out = 30)
#' blockids <- rep(1:3, each = 10)
#' 
#' eterm <- event_term(list(Condition = x1, Group = x2),
#'                     onsets = onsets,
#'                     blockids = blockids)
#' print(eterm)
#' head(event_table(eterm))
#' levels(eterm)
#' head(design_matrix(eterm))
#'
#' @export
#' @import assertthat
#' @importFrom tibble as_tibble tibble
#' @examples
#' term <- event_term(
#'   list(condition = factor(c("A", "B", "A"))),
#'   onsets = c(0, 10, 20),
#'   blockids = c(1, 1, 1)
#' )
#' head(design_matrix(term))
event_term <- function(evlist, onsets, blockids, durations = 0, subset = NULL) {
  
  # Convert blockids to numeric if they are factors.
  if (is.factor(blockids)) {
    blockids <- as.numeric(as.character(blockids))
  }
  
  # Ensure blockids are non-decreasing.
  # Use base R equivalent of is.increasing
  assert_that(!is.unsorted(blockids), msg = "'blockids' must consist of non-decreasing integers")
  
  # NOTE: subset handling is now primarily inside event(), 
  # but event_term might still need its own subset logic if it uses it before calling EV?
  # Currently, it passes subset down to EV.
  if (is.null(subset)) { 
    subset <- rep(TRUE, length(onsets)) 
  }
  
  # Duration recycling is handled inside event() via .checkEVArgs
  # if (length(durations) == 1) {
  #   durations <- rep(durations, length(onsets))
  # }
  
  vnames <- names(evlist)
  onlen <- length(onsets)
  
  # Basic check on input lengths before calling EV factory
  getlen <- function(v) {
    if (inherits(v, "event")) length(v$onsets)
    else if (is.matrix(v)) nrow(v)
    else if (inherits(v, "ParametricBasis")) nrow(v$y) # Check basis matrix dim
    else length(v)
  }
  for(i in seq_along(evlist)) {
      assert_that(getlen(evlist[[i]]) == onlen, 
                  msg=sprintf("Length mismatch between onsets (%d) and variable '%s' (%d)", 
                              onlen, vnames[i], getlen(evlist[[i]])))
  }
  
  # Create event objects by dispatching to the appropriate public wrapper
  evs <- lapply(seq_along(evlist), function(i) {
    vals <- evlist[[i]]
    vname_i <- vnames[i]
    
    # Type checking and dispatching (replaces EV factory logic)
    if (inherits(vals, "event")) {
        # If it's already an event object, just use it as-is
        # TODO: Consider whether to re-apply subset/durations/etc from current call
        vals
    } else if (inherits(vals, "ParametricBasis")) {
        event_basis(basis = vals, name = vname_i, onsets = onsets, blockids = blockids, durations = durations, subset = subset)
    } else if (is.factor(vals) || is.character(vals)) {
        event_factor(fac = vals, name = vname_i, onsets = onsets, blockids = blockids, durations = durations, subset = subset)
    } else if (is.matrix(vals)) {
        # Handle single-column matrices as vectors for event_variable?
        # No, event_matrix handles matrices directly now.
        event_matrix(mat = vals, name = vname_i, onsets = onsets, blockids = blockids, durations = durations, subset = subset)
    } else if (is.numeric(vals) && (is.vector(vals) || length(dim(vals)) <= 1)) {
         # Check specifically for numeric vectors (or 1D arrays)
        event_variable(vec = vals, name = vname_i, onsets = onsets, blockids = blockids, durations = durations, subset = subset)
    } else {
        stop(sprintf("Unsupported value type '%s' for variable '%s' in event_term", class(vals)[1], vname_i))
    }
  })
  
  names(evs) <- sapply(evs, function(ev) ev$varname)
  pterms <- unlist(lapply(evs, function(ev) ev$varname))
  
  # Check if any event object creation resulted in zero events after subsetting
  if (length(evs) > 0 && length(evs[[1]]$onsets) == 0) {
      term_label <- paste(vnames, collapse = ":")
      n_in <- length(onsets)
      n_keep <- if (is.null(subset)) n_in else sum(isTRUE(subset))
      subset_note <- if (!is.null(subset)) paste0(" (kept ", n_keep, "/", n_in, ")") else ""
      msg <- sprintf("Event term '%s' has zero realized events after processing%s.", term_label, subset_note)

      warn_enabled <- getOption("fmridesign.warn_zero_events", TRUE)
      if (isTRUE(warn_enabled)) {
        cli::cli_warn(msg, class = "fmridesign_zero_events")
      } else {
        cli::cli_inform(msg, class = "fmridesign_zero_events")
      }
      # Proceed with empty structures; downstream code will handle gracefully
  }
  
  # Rebuild event_table based on the *actual* content of the event objects
  # event() now handles the internal structure, so we use elements()
  # This relies on elements.event(..., values=FALSE) returning the appropriate factor levels/names
  
  # Get descriptive elements (levels/names) for each event
  descriptive_elements <- elements(evs[[1]], values = FALSE) # Need a way to call elements on list 'evs'? 
  # No, elements.event_term works on the term object after it's built.
  # Let's reconstruct the table *after* building the initial list. 

  varname <- paste(sapply(evs, function(x) x$varname), collapse = ":")
  
  # Create the list structure first
  ret <- list(varname = varname, 
              events = evs, 
              subset = subset, # Keep original subset for reference?
              # Placeholder for event_table, rebuild below
              event_table = NULL, 
              # Use onsets/blockids/durations from the *first* processed event object
              # Assumes they are consistent across all events after processing (checked by event())
              onsets = if(length(evs) > 0) evs[[1]]$onsets else numeric(0), 
              blockids = if(length(evs) > 0) evs[[1]]$blockids else numeric(0), 
              durations = if(length(evs) > 0) evs[[1]]$durations else numeric(0))
  
  class(ret) <- c("event_term", "event_seq")
  
  # Now build the event_table using elements()
  # Get descriptive elements (levels/names) for the term
  descriptive_elements_list <- elements(ret, what = "labels") # Explicitly request labels
  # Combine into a tibble directly from the list
  etab <- try(tibble::as_tibble(descriptive_elements_list), silent = TRUE)
  if (inherits(etab, "try-error")) {
      warning("Failed to create event_table for term: ", varname)
      etab <- tibble::tibble()
  }
  ret$event_table <- etab

  # Persist per-event condition metadata for quick retrieval later
  cond_meta <- .compute_event_term_condition_metadata(ret)
  ret$condition_levels <- cond_meta$levels
  ret$condition_ids <- cond_meta$ids
  
  ret
}

#' @export
event_table.event_term <- function(x, ...) x$event_table

## ============================================================================
## Section 2.5: Event Condition Metadata Helpers
## ============================================================================

#' @keywords internal
#' @noRd
.compute_event_term_condition_metadata <- function(x) {
  n_events <- length(x$onsets)

  categorical_events <- x$events[vapply(x$events, is_categorical, logical(1))]

  # Default metadata for intercept-only / continuous-only terms
  if (length(categorical_events) == 0L) {
    levels_out <- "condition"
    ids_out <- if (n_events > 0L) rep.int(1L, n_events) else integer(0)
    return(list(levels = levels_out, ids = as.integer(ids_out)))
  }

  # Prepare level tokens for each categorical event component
  level_tokens <- lapply(categorical_events, function(ev) {
    levs <- levels(ev)
    level_token(ev$varname, levs)
  })

  # Generate canonical ordering of condition levels via expand.grid
  cond_grid <- if (length(level_tokens) == 1L) {
    data.frame(level_tokens[[1L]], stringsAsFactors = FALSE)
  } else {
    do.call(expand.grid, c(level_tokens, list(stringsAsFactors = FALSE)))
  }

  if (nrow(cond_grid) == 0L) {
    cond_levels <- character(0)
  } else if (is.null(dim(cond_grid))) {
    cond_levels <- make_cond_tag(cond_grid)
  } else {
    cond_levels <- apply(cond_grid, 1, make_cond_tag)
  }

  if (n_events == 0L) {
    return(list(levels = cond_levels, ids = integer(0)))
  }

  # Map each event instance to the corresponding condition token combination
  per_event_tokens <- lapply(seq_along(categorical_events), function(idx) {
    ev <- categorical_events[[idx]]
    tok <- level_tokens[[idx]]
    labels <- elements(ev, what = "labels")
    if (length(labels) == 0L) {
      rep(NA_character_, n_events)
    } else {
      ids <- as.integer(labels)
      vals <- tok[ids]
      vals[is.na(ids)] <- NA_character_
      vals
    }
  })

  tokens_matrix <- do.call(cbind, per_event_tokens)
  if (!is.null(tokens_matrix) && !is.matrix(tokens_matrix)) {
    tokens_matrix <- matrix(tokens_matrix, ncol = 1L)
  }

  event_labels <- if (is.null(tokens_matrix) || nrow(tokens_matrix) == 0L) {
    character(0)
  } else {
    apply(tokens_matrix, 1, function(row) {
      row <- row[!is.na(row)]
      if (length(row) == 0L) NA_character_ else make_cond_tag(row)
    })
  }

  ids_out <- match(event_labels, cond_levels)
  list(levels = cond_levels, ids = as.integer(ids_out))
}

#' @keywords internal
#' @noRd
.event_term_condition_metadata <- function(x) {
  levels <- x$condition_levels
  ids <- x$condition_ids
  if (is.null(levels) || is.null(ids)) {
    .compute_event_term_condition_metadata(x)
  } else {
    list(levels = levels, ids = as.integer(ids))
  }
}

## ============================================================================
## Section 3: EV Factory and Event Constructors (REMOVE EV Factory)
## ============================================================================

# Removed EV factory function. Logic is now inlined in event_term.

# Removed event_factor, event_variable, event_matrix, event_basis wrappers.
# They are now located in R/event-classes.R

## ============================================================================
## Section 4: Levels and Formula Methods
## ============================================================================

#' @method formula event_term
#' @export
formula.event_term <- function(x, ...) {
  # NOTE: This uses parent_terms.event_term which still exists below
  #       It might need adjustment if parent_terms logic changes.
  as.formula(paste("~ (", paste(parent_terms(x), collapse = ":"), "-1)"))
}

#' @noRd
.vector_of_labels <- function(ev) {
  # Helper to generate the vector of condition labels for a single event.
  # Uses levels.event() for the actual levels/column names.
  lvls <- levels(ev) # Get levels/colnames from levels.event
  
  if (is_continuous(ev) && length(lvls) > 1) {
    # Continuous multi-column (matrix/basis): Use index 1:ncol
    vapply(seq_along(lvls), 
           \(k) .label_component(ev, k), 
           character(1))
  } else if (is_categorical(ev)) {
    # Categorical: Use actual levels
    vapply(lvls, \(lvl) .label_component(ev, lvl), character(1))
  } else {
    # Single continuous variable (vector): Just the variable name
    .label_component(ev)
  }
}

## ============================================================================
## Section 5: Cells Extraction
## ============================================================================

#' @method cells event_term
#' @rdname cells
#' @export
cells.event_term <- function(x, drop.empty = TRUE, ...) {
  ## ----------------------------------------------------------------
  ## 0. fast cache ---------------------------------------------------
  # Use fixed name as levels rarely change post-construction
  cache_attr_name <- "..cells" 
  if (!is.null(cached <- attr(x, cache_attr_name))) {
    cnt <- attr(cached, "count")
    # Need to handle potential NULL count if cache is invalid
    if(is.null(cnt)) {
        warning("Invalid cache detected for cells.event_term, recomputing.")
    } else {
        return(if (drop.empty) cached[cnt > 0, , drop = FALSE] else cached)
    }
  }

  ## ----------------------------------------------------------------
  ## 1. categorical events only -------------------------------------
  # Use Filter and Negate for conciseness
  cats <- Filter(Negate(is_continuous), x$events)

  if (length(cats) == 0) {                  # no factors => one big cell
    # Use a more descriptive name if needed, maybe based on varname
    # Consistent with cells.event: use the (first/only) varname
    var_name_cont <- if (length(x$events) > 0) x$events[[1]]$varname else "all_events"
    out <- tibble::tibble(!!var_name_cont := var_name_cont) # Use varname for column
    count_val <- length(x$onsets)
    attr(out, "count") <- count_val
    # Assign name to the count attribute
    names(attr(out, "count")) <- var_name_cont 
    
    attr(x, cache_attr_name) <- out # Cache the result
    return(out)
  }

  ## ----------------------------------------------------------------
  ## 2. observed combination counts ---------------------------------
  # Reconstruct factors from internal representation
  obs_list <- lapply(cats, \(ev) {
      # Add checks for valid meta$levels and value structure
      if (is.null(ev$meta$levels) || !is.matrix(ev$value) || ncol(ev$value) != 1) {
           stop(paste("Invalid internal structure for categorical event:", ev$varname), call.=FALSE)
      }
      factor(ev$value[, 1L], levels = seq_along(ev$meta$levels), labels = ev$meta$levels)
  })
  # Create data frame of observed factor combinations
  obs <- do.call(data.frame, c(obs_list, stringsAsFactors = FALSE))
  
  # Use table() for efficient contingency counting (if few factors)
  # For many factors, alternative might be needed 
  # tbl <- as.data.frame.matrix(table(obs)) # This creates wide format, not needed directly
  
  # Generate the grid of all possible level combinations
  levels_list <- lapply(cats, levels)
  grid <- expand.grid(levels_list, stringsAsFactors = FALSE) # Use FALSE then convert relevant columns
  # Ensure column names match original variable names
  colnames(grid) <- names(cats)
  # Convert grid columns to factors matching the levels in obs_list
  for(i in seq_along(grid)){
      grid[[i]] <- factor(grid[[i]], levels=levels(obs_list[[i]]))
  }

  ## match() much faster than join for simple cases ------------------
  # Create unique string keys for observed and grid rows
  # Use sep that's unlikely to appear in levels
  key_obs  <- do.call(paste, c(obs, sep = "\001")) 
  key_grid <- do.call(paste, c(grid, sep = "\001"))
  # Count occurrences by matching observed keys to grid keys
  count    <- tabulate(match(key_obs, key_grid), nbins = nrow(grid))

  ## ----------------------------------------------------------------
  ## 3. assemble result ---------------------------------------------
  out <- tibble::as_tibble(grid) # Convert final grid to tibble

  # --- Add names to the count vector ---
  # Generate names for the counts based on the grid rows (factor level combinations)
  if (nrow(grid) > 0) {
      # Use a separator consistent with how interactions might be named elsewhere
      cell_names <- apply(grid, 1, paste, collapse = ":")
      # Ensure counts has names before attaching
      if (length(count) == length(cell_names)) {
         names(count) <- cell_names
      } else {
         # This shouldn't happen if logic above is correct, but add a warning
         warning("Mismatch between number of cells and counts calculated in cells.event_term")
      }
  } # Else count is likely integer(0) and doesn't need names
  # --- End adding names ---

  attr(out, "count") <- count # Attach the now named count vector

  attr(x, cache_attr_name) <- out # Cache the result (with named counts)

  # Filter based on drop.empty
  if (drop.empty) {
      keep_idx <- count > 0
      filtered_out <- out[keep_idx, , drop = FALSE]
      # Ensure the count attribute on the filtered output is also correct
      attr(filtered_out, "count") <- count[keep_idx]
      filtered_out
  } else {
       out
  }
}

#' @noRd
.event_set <- function(x, exclude_basis = FALSE) {
  evtab <- event_table(x)
  
  evset <- if (fmrihrf::nbasis(x) > 1 & !exclude_basis) {
    ncond <- fmrihrf::nbasis(x)
    # Construct a zero-padded string for basis labels.
    zstr <- paste0(rep("0", ceiling(log10(ncond + 1e-6))), collapse = "")
    
          evlist <- c(list(factor(paste("basis", zstr, 1:fmrihrf::nbasis(x), sep = ""))), cells(x$evterm))
    names(evlist) <- c("basis", parent_terms(x$evterm))
    evlist <- lapply(evlist, levels)
    ret <- expand.grid(evlist, stringsAsFactors = TRUE)
    ret[c(2:length(ret), 1)]
  } else {
    cells(x$evterm)
  }
}

#' @export
#' @rdname cells
cells.covariate_convolved_term <- function(x, ...) {
  unique(event_table(x))
}

#' @export
#' @importFrom stringr str_trim
cells.convolved_term <- function(x, ...) {
  exclude_basis <- list(...)$exclude_basis
  if (is.null(exclude_basis)) exclude_basis <- FALSE
  
  evtab <- event_table(x)
  evset <- .event_set(x, exclude_basis = exclude_basis)
  
  strels <- apply(apply(evtab, 2, stringr::str_trim), 1, paste, collapse = ":")
  
  strlevs <- if (nrow(evset) > 1) {
    apply(apply(evset, 2, stringr::str_trim), 1, paste, collapse = ":")
  } else {
    as.character(evset[1, 1])
  }
  
  attr(evset, "rownames") <- strlevs
  
  counts <- if (exclude_basis) {
    rep(attr(cells(x$evterm), "count"), each = 1)
  } else {
          rep(attr(cells(x$evterm), "count"), each = fmrihrf::nbasis(x))
  }
  
  ret <- evset[counts > 0, , drop = FALSE]
  attr(ret, "count") <- counts[counts > 0]
  ret
}

## ============================================================================
## Section 6: Conditions and Parent Terms
## ============================================================================

#' @method conditions event_term
#' @rdname conditions
#' @export
#' @importFrom tibble tibble
#' @importFrom rlang :=
conditions.event_term <- function(x, drop.empty = TRUE, expand_basis = FALSE, ...) {
  
  # --- Caching --- 
  opts_key <- paste(drop.empty, expand_basis, sep="|") # Keep drop.empty in key for now
  cached_val <- attr(x, "..conds")
  cached_opts <- attr(x, "..conds_opts")
  
  # --- RE-ENABLE CACHE --- 
  if (!is.null(cached_val) && !is.null(cached_opts) && identical(cached_opts, opts_key)) {
    return(cached_val)
  }
  # message("--- conditions.event_term: Cache bypassed/missed, recalculating ---") # Keep commented out
  # --- END RE-ENABLE ---
  
  # --- Shortcut for single continuous event with one column --- 
  if (length(x$events) == 1 && is_continuous(x$events[[1]])) {
    cols <- try(columns(x$events[[1]]), silent=TRUE)
    if (!inherits(cols, "try-error") && length(cols) == 1) {
        base_cond_tags <- cols 
        if (expand_basis) {
            hrfspec <- attr(x, "hrfspec")
            nb <- if (!is.null(hrfspec) && !is.null(hrfspec$hrf)) fmrihrf::nbasis(hrfspec$hrf) else 1L
            final_cond_tags <- add_basis(base_cond_tags, nb)
        } else {
            final_cond_tags <- base_cond_tags
        }
        attr(x, "..conds") <- final_cond_tags
        attr(x, "..conds_opts") <- opts_key
        return(final_cond_tags)
    }
  }
  
  # --- Generate Tokens for Each Component --- 
  comp_tokens_list <- lapply(x$events, function(ev) {
      if (is_categorical(ev)) {
          levs <- levels(ev) 
          # Handle case where factor might have no levels after subsetting? levels() should return character(0)
          if (length(levs) == 0) return(character(0))
          level_token(ev$varname, levs)
      } else {
          columns(ev) 
      }
  })
  
  # Filter out components that returned empty tokens (e.g., factors with no levels)
  comp_tokens_list <- Filter(function(tk) length(tk) > 0, comp_tokens_list)
  
  if (length(comp_tokens_list) == 0) { # If ALL components became empty
       final_out <- character(0)
       attr(x, "..conds") <- final_out
       attr(x, "..conds_opts") <- opts_key
       return(final_out)
  }
  
  # --- Combine Tokens using expand.grid and make_cond_tag --- 
  names(comp_tokens_list) <- names(Filter(function(tk) length(tk) > 0, x$events)) # Match names to filtered tokens
  full_grid <- expand.grid(comp_tokens_list, stringsAsFactors = FALSE)
  base_cond_tags_all <- apply(full_grid, 1, make_cond_tag)
  
  # --- REMOVED drop.empty LOGIC BLOCK --- 
  # The logic relying on cells() was flawed for mixed continuous/categorical terms.
  # Dropping based on actual matrix rank deficiency is handled by design_matrix() / model.matrix().
  base_cond_tags_final <- base_cond_tags_all
  
  # --- Handle expand_basis --- 
  if (expand_basis) {
      hrfspec <- attr(x, "hrfspec")
      nb <- if (!is.null(hrfspec) && !is.null(hrfspec$hrf)) fmrihrf::nbasis(hrfspec$hrf) else 1L
      final_cond_tags <- add_basis(base_cond_tags_final, nb)
  } else {
      final_cond_tags <- base_cond_tags_final
  }
  
  # --- Cache and Return --- 
  final_out <- as.vector(final_cond_tags)
  attr(x, "..conds") <- final_out
  attr(x, "..conds_opts") <- opts_key
  
  return(final_out)
}

#' @method shortnames event_term
#' @export
shortnames.event_term <- function(x, drop.empty = TRUE, ...) {
  # Get the cells (combinations of factor levels)
  term_cells <- cells(x, drop.empty = drop.empty)
  
  if (nrow(term_cells) == 0) {
    return(character(0))
  }
  
  # For each row, create shortname by joining with ":"
  shortnames_vec <- apply(term_cells, 1, function(row) {
    paste(row, collapse = ":")
  })
  
  # Return as character vector
  as.character(shortnames_vec)
}

#' @method longnames event_term
#' @export
longnames.event_term <- function(x, drop.empty = TRUE, expand_basis = FALSE, ...) {
  # Use the conditions method which already implements the new naming scheme
  conditions(x, drop.empty = drop.empty, expand_basis = expand_basis, ...)
}

#' @noRd
parent_terms.event_term <- function(x) unlist(lapply(x$events, function(ev) ev$varname))

## ============================================================================
## Section 7: Continuous/Categorical Checks and Elements Extraction
## ============================================================================

#' @export
is_continuous.event_term <- function(x, ...) all(sapply(x$events, function(x) is_continuous(x)))

#' @export
is_categorical.event_term <- function(x, ...) !is_continuous(x)

#' @method elements event_term
#' @rdname elements
#' @export
elements.event_term <- function(x, what = c("values", "labels"), ...) {
  # Ensure 'what' is determined correctly, respecting 'values' if passed via ...
  dots <- list(...)
  if (!missing(what)) {
      what <- match.arg(what)
  } else if (!is.null(dots$values)) {
      what <- if (dots$values) "values" else "labels"
  } else {
      what <- "values" # Default if neither 'what' nor 'values' is specified
  }
  
  # Pass the determined 'what' argument and any other arguments down
  # lapply will now create a named list directly as elements.event returns vectors/matrices
  els <- lapply(x$events, elements, what = what, ...)
  
  # Sanitize names of the resulting list (which should already be named by lapply)
  # If lapply didn't preserve names from x$events, this is needed.
  names(els) <- vapply(names(x$events), .sanitizeName, character(1))
  els
}

## ============================================================================
## Section 7.5: Event Conditions and Tables
## ============================================================================

#' @export
event_conditions.event_term <- function(x, drop.empty = FALSE, ...) {
  meta <- .event_term_condition_metadata(x)
  lev <- meta$levels %||% character(0)
  ids <- meta$ids

  if (!drop.empty) {
    vals <- if (length(ids) == 0L) character(0) else lev[ids]
    return(factor(vals, levels = lev))
  }

  present <- unique(ids[!is.na(ids)])
  if (length(present) == 0L) {
    return(factor(rep(NA_character_, length(ids)), levels = character(0)))
  }
  present <- sort(present)
  lev_drop <- lev[present]
  map <- match(ids, present)
  factor(if (length(map) == 0L) character(0) else lev_drop[map], levels = lev_drop)
}

#' @export
event_conditions.convolved_term <- function(x, drop.empty = FALSE, ...) {
  if (!inherits(x$evterm, "event_term")) {
    stop("event_conditions is only defined for event-based convolved_terms.", call. = FALSE)
  }
  event_conditions(x$evterm, drop.empty = drop.empty, ...)
}

#' @export
events.event_term <- function(x, drop.empty = FALSE, ...) {
  ons <- as.numeric(onsets(x))
  du <- durations(x)
  if (is.null(du)) {
    du <- rep(0, length(ons))
  } else {
    du <- as.numeric(du)
    if (length(du) != length(ons)) {
      du <- rep_len(du, length(ons))
    }
  }
  bl <- blockids(x)
  if (is.null(bl)) {
    bl <- rep(1L, length(ons))
  } else {
    bl <- as.integer(bl)
    if (length(bl) != length(ons)) {
      bl <- rep_len(bl, length(ons))
    }
  }

  data.frame(
    onset = ons,
    duration = du,
    block = bl,
    condition = event_conditions(x, drop.empty = drop.empty),
    stringsAsFactors = FALSE
  )
}

#' @export
events.convolved_term <- function(x, drop.empty = FALSE, ...) {
  if (!inherits(x$evterm, "event_term")) {
    stop("events is only defined for event-based convolved_terms.", call. = FALSE)
  }
  events(x$evterm, drop.empty = drop.empty, ...)
}

#' Group column indices by condition for a term/basis pair
#'
#' @param term An `event_term`.
#' @param basis An `HRF` object.
#' @param sampling_frame Unused; present for future compatibility.
#' @return A named list mapping condition tags to integer indices.
#' @keywords internal
#' @examples
#' \dontrun{
#' term <- event_term(
#'   list(condition = factor(c("A", "B"))),
#'   onsets = c(0, 5),
#'   blockids = c(1, 1)
#' )
#' column_groups_by_condition(term, fmrihrf::HRF_SPMG1, NULL)
#' }
column_groups_by_condition <- function(term, basis, sampling_frame) {
  nb <- fmrihrf::nbasis(basis)
  base_levels <- as.character(conditions(term, expand_basis = FALSE, drop.empty = FALSE))
  if (length(base_levels) == 0L || nb <= 0L) {
    return(list())
  }
  split(seq_len(length(base_levels) * nb), rep(base_levels, each = nb))
}

## ============================================================================
## Section 8: Onsets and Block IDs
## ============================================================================

#' @export
onsets.convolved_term <- function(x, ...) {
  onsets(x$evterm, ...)
}

#' @export
onsets.event_term <- function(x, ...) {
  x$onsets
}

#' @export
blockids.event_term <- function(x, ...) {
  x$blockids
}

#' @export
blockids.convolved_term <- function(x, ...) {
  fmrihrf::blockids(x$evterm)
}

#' @export
durations.event_term <- function(x, ...) {
  x$durations
}

#' @export
durations.convolved_term <- function(x, ...) {
  durations(x$evterm, ...)
}



## ============================================================================
## Section 9: Splitting Onsets
## ============================================================================

#' @method split_onsets event_term
#' @rdname split_onsets
#' @export
split_onsets.event_term <- function(x, sframe, global = FALSE, blocksplit = FALSE, ...) {
  # Get categorical events.
  facs <- x$events[!sapply(x$events, is_continuous)]
  
  if (length(facs) == 0) {
    ons <- if (global) {
      fmrihrf::global_onsets(sframe, onsets(x), fmrihrf::blockids(x))
    } else {
      onsets(x)
    }
    return(list(split(ons, fmrihrf::blockids(x))))
  }
  
  # For categorical events, construct a crossed factor.
  facs <- lapply(facs, function(fac) unlist(elements(fac)))
  
  f <- function(...) {
    interaction(..., drop = TRUE, sep = ":")
  }
  
  cfac <- try(do.call(f, facs))
  # If error, a more informative error message might be warranted.
  
  ret <- if (global) {
    split(fmrihrf::global_onsets(sframe, onsets(x), fmrihrf::blockids(x)), cfac)
  } else {
    split(onsets(x), cfac)
  }
  
  if (blocksplit) {
    bsplit <- split(fmrihrf::blockids(x), cfac)
    ret <- lapply(seq_along(ret), function(i) {
      split(ret[[i]], bsplit[[i]])
    })
  }
  
  names(ret) <- longnames(x)
  ret
}

## ============================================================================
## Section 10: Convolution and Regressor Generation
## ============================================================================

## --- Helper functions for per-onset HRF generation (hrf_fun) ---

#' Build event data frame for HRF generator function
#'
#' Creates a data frame containing event information that can be passed to
#' an hrf_fun generator. The data frame includes onset, duration, blockid,
#' and any term variables extracted from the event_term. Also includes
#' columns from the original data (if attached) to support list columns
#' and other complex data types.
#'
#' @param event_term An event_term object.
#' @return A data frame with columns: onset, duration, blockid, plus term variables.
#' @keywords internal
#' @noRd
build_event_data_for_generator <- function(event_term) {
  n_events <- length(event_term$onsets)

  # Start with timing info
 event_data <- data.frame(
    onset = event_term$onsets,
    duration = event_term$durations,
    blockid = event_term$blockids,
    stringsAsFactors = FALSE
  )

  # Add term variables from the events list
  for (ev_name in names(event_term$events)) {
    ev <- event_term$events[[ev_name]]
    if (!is.null(ev$meta$levels)) {
      # Factor: reconstruct from integer codes
      event_data[[ev_name]] <- factor(
        ev$value[, 1],
        levels = seq_along(ev$meta$levels),
        labels = ev$meta$levels
      )
    } else if (ncol(ev$value) == 1) {
      # Single-column continuous
      event_data[[ev_name]] <- ev$value[, 1]
    } else {
      # Multi-column: add each column with suffix
      for (j in seq_len(ncol(ev$value))) {
        event_data[[paste0(ev_name, "_", j)]] <- ev$value[, j]
      }
    }
  }

  # Merge in columns from original data (if attached)
  # This allows access to list columns and other complex types not in event_term$events
  original_data <- attr(event_term, "original_data")
  if (!is.null(original_data)) {
    for (col_name in names(original_data)) {
      # Only add columns not already in event_data
      if (!(col_name %in% names(event_data))) {
        event_data[[col_name]] <- original_data[[col_name]]
      }
    }
  }

  event_data
}

#' Evaluate formula-style hrf_fun
#'
#' Extracts an HRF list from a column in event_data referenced by a formula.
#'
#' @param hrf_formula A formula like ~hrf_column.
#' @param event_data Data frame of event information.
#' @param original_data Optional: the original data passed to event_model (for column lookup).
#' @return A list of HRF objects.
#' @keywords internal
#' @noRd
evaluate_hrf_formula <- function(hrf_formula, event_data, original_data = NULL) {
  # Extract column name from formula RHS
  col_name <- as.character(rlang::f_rhs(hrf_formula))

  # Try event_data first
  if (col_name %in% names(event_data)) {
    hrf_col <- event_data[[col_name]]
  } else if (!is.null(original_data) && col_name %in% names(original_data)) {
    # Fall back to original data (with subsetting handled externally)
    hrf_col <- original_data[[col_name]]
  } else {
    stop(sprintf("hrf_fun formula references column '%s' not found in event data. Available columns: %s",
                 col_name, paste(names(event_data), collapse = ", ")), call. = FALSE)
  }

  # Validate it's a list
  if (!is.list(hrf_col)) {
    stop(sprintf("Column '%s' must contain a list of HRF objects", col_name), call. = FALSE)
  }

  hrf_col
}

#' Validate HRF generator result
#'
#' Validates that the result of an hrf_fun generator is a valid list of HRF objects.
#' Handles single HRF (recycles to all events), validates length, checks all elements
#' are HRF objects, and verifies consistent nbasis.
#'
#' @param result The result from an hrf_fun generator.
#' @param n_events Expected number of events.
#' @param term_tag Optional term tag for error messages.
#' @return A validated list of HRF objects with length equal to n_events.
#' @keywords internal
#' @noRd
validate_hrf_list <- function(result, n_events, term_tag = NULL) {
  term_label <- term_tag %||% "unknown"

  # Handle single HRF (recycle to all events)
  if (inherits(result, "HRF")) {
    return(rep(list(result), n_events))
  }

  # Must be a list at this point
 if (!is.list(result)) {
    stop(sprintf("hrf_fun for term '%s' must return an HRF object or list of HRF objects",
                 term_label), call. = FALSE)
  }

  # Validate length
  if (length(result) == 1) {
    result <- rep(result, n_events)
  } else if (length(result) != n_events) {
    stop(sprintf("hrf_fun for term '%s' returned %d HRFs but %d events exist",
                 term_label, length(result), n_events), call. = FALSE)
  }

  # Validate each element is an HRF
  for (i in seq_along(result)) {
    if (!inherits(result[[i]], "HRF")) {
      stop(sprintf("Element %d of hrf_fun result for term '%s' is not an HRF object (class: %s)",
                   i, term_label, class(result[[i]])[1]), call. = FALSE)
    }
  }

  # Validate consistent nbasis
  nbasis_vals <- vapply(result, fmrihrf::nbasis, integer(1))
  if (length(unique(nbasis_vals)) > 1) {
    stop(sprintf("All HRFs from hrf_fun for term '%s' must have the same nbasis. Got: %s",
                 term_label, paste(unique(nbasis_vals), collapse = ", ")), call. = FALSE)
  }

  result
}

## --- End of hrf_fun helper functions ---

#' Convolve HRF with Design Matrix.
#'
#' Convolves a HRF with a design matrix (one column per condition) to produce a
#' list of regressors.
#'
#' @param hrf A function representing the HRF (used when hrf_list is NULL).
#' @param dmat Design matrix (with named columns).
#' @param globons Numeric vector of global onsets.
#' @param durations Numeric vector of event durations.
#' @param summate Logical; if TRUE, summate the convolved HRF (default: TRUE).
#' @param hrf_list Optional list of HRF objects, one per event (row in dmat).
#'        When provided, allows per-onset HRF specification. The list is subsetted
#'        to match non-zero amplitude events for each condition.
#'
#' @return A list of regressors (one for each column).
#' @export
#' @examples
#' hrf <- fmrihrf::HRF_SPMG1
#' dmat <- data.frame(A = c(1, 0, 1), B = c(0, 1, 0))
#' globons <- c(0, 10, 20)
#' durations <- rep(0, 3)
#' regs <- convolve_design(hrf, dmat, globons, durations)
#' length(regs)
convolve_design <- function(hrf, dmat, globons, durations, summate = TRUE, hrf_list = NULL) {
  cond.names <- names(dmat)

  # Check if we have per-onset HRFs
  hrf_is_list <- !is.null(hrf_list)

  # Remove rows with NA values
  if (any(is.na(dmat)) || any(is.na(globons))) {
    keep <- apply(dmat, 1, function(vals) all(!is.na(vals)))
    keep[is.na(globons)] <- FALSE
    dmat <- dmat[keep, , drop = FALSE]
    durations <- durations[keep]
    globons <- globons[keep]
    # Also filter HRF list if present
    if (hrf_is_list) {
      hrf_list <- hrf_list[keep]
    }
  }

  reglist <- purrr::map(1:ncol(dmat), function(i) {
    amp <- dmat[, i][[1]]
    nonzero <- which(amp != 0)
    if (length(nonzero) == 0) {
      # Empty regressor - use first HRF from list or single HRF for span info
      ref_hrf <- if (hrf_is_list && length(hrf_list) > 0) hrf_list[[1]] else hrf
      fmrihrf::regressor(onsets = numeric(0), hrf = ref_hrf, amplitude = 0)
    } else {
      # Subset HRF list if applicable (for non-zero amplitude events only)
      hrf_for_reg <- if (hrf_is_list) hrf_list[nonzero] else hrf

      fmrihrf::regressor(
        onsets = globons[nonzero],
        hrf = hrf_for_reg,  # Single HRF or list of HRFs
        amplitude = amp[nonzero],
        duration = durations[nonzero],
        summate = summate
      )
    }
  })

  reglist
}

#' Extract regressors for an event term
#'
#' Convolve the event-term design matrix with an HRF and return the
#' resulting regressors.
#'
#' @rdname regressors
#' @param hrf HRF function
#' @param sampling_frame sampling_frame object
#' @param summate Logical; sum HRF responses
#' @param drop.empty Logical; drop empty conditions
#' @export
regressors.event_term <- function(x, hrf, sampling_frame, summate = FALSE, drop.empty = TRUE, ...) {
  globons <- fmrihrf::global_onsets(sampling_frame, x$onsets, x$blockids)
  durations <- x$durations
  blockids <- x$blockids
  nimages <- sum(fmrihrf::blocklens(sampling_frame))
  cnames <- conditions(x)
  dmat <- design_matrix(x, drop.empty)
  ncond <- ncol(dmat)
  
  reg <- convolve_design(hrf, dmat, globons, durations, summate = summate)
  names(reg) <- colnames(dmat)
  reg
}

#' @method convolve event_term
#' @rdname convolve
#' @export
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>% group_by select do ungroup
#' @examples
#' term <- event_term(
#'   list(condition = factor(c("A", "B", "A"))),
#'   onsets = c(0, 5, 10),
#'   blockids = c(1, 1, 1)
#' )
#' sf <- fmrihrf::sampling_frame(blocklens = 20, TR = 1)
#' conv <- convolve(term, fmrihrf::HRF_SPMG1, sf)
#' names(conv)
convolve.event_term <- function(x, hrf, sampling_frame, drop.empty = TRUE,
                                summate = TRUE, precision = 0.3, ...) {
  # Check for term_tag attribute (should have been added in realise_event_terms)
  term_tag <- attr(x, "term_tag")
  # --- REMOVED FALLBACK LOGIC FOR term_tag ---
  # If term_tag is NULL (e.g., for Ident()-only terms), make_column_names will handle it
  # by not prepending a term_tag, resulting in direct variable names.

  # --- Check for hrf_fun (per-onset HRF generator) ---
  hrfspec <- attr(x, "hrfspec")
  hrf_fun <- attr(x, "hrf_fun") %||% (if (!is.null(hrfspec)) hrfspec$hrf_fun else NULL)

  # --- Generate per-onset HRF list if hrf_fun specified ---
  hrf_list <- NULL
  if (!is.null(hrf_fun) && length(x$onsets) > 0) {
    # Build event data frame for generator
    event_data <- build_event_data_for_generator(x)

    if (rlang::is_formula(hrf_fun)) {
      # Formula syntax: extract column from event_data or original_data
      original_data <- attr(x, "original_data")
      hrf_list <- evaluate_hrf_formula(hrf_fun, event_data, original_data)
    } else if (is.function(hrf_fun)) {
      # Function syntax: call with event_data
      hrf_list <- tryCatch(
        hrf_fun(event_data),
        error = function(e) {
          stop(sprintf("hrf_fun failed for term '%s': %s",
                       term_tag %||% x$varname, e$message), call. = FALSE)
        }
      )
    }

    # Validate the result
    hrf_list <- validate_hrf_list(hrf_list, nrow(event_data), term_tag)

    # Update hrf for nbasis calculation (use first HRF from list)
    hrf <- hrf_list[[1]]
  }

  # --- Basic Setup ---
  globons <- fmrihrf::global_onsets(sampling_frame, x$onsets, x$blockids)
  durations <- x$durations
  blockids <- x$blockids
  nimages <- sum(fmrihrf::blocklens(sampling_frame))
  
  # --- Get Unconvolved Design Matrix (dmat) --- 
  # design_matrix handles dropping empty/rank-deficient columns based on drop.empty
  dmat <- design_matrix(x, drop.empty = drop.empty)
  
  # --- Get Base Condition Names from the *actual* matrix columns --- 
  # This ensures names match the columns being convolved
  base_cnames <- colnames(dmat)
  
  # Check if dmat became empty after dropping
  if (ncol(dmat) == 0 || nrow(dmat) == 0) {
      warning(sprintf("Design matrix for term '%s' became empty after dropping. Convolution will result in an empty matrix.", term_tag), call.=FALSE)
      # Proceed to generate names for an empty matrix
      base_cnames <- character(0) # Use empty names
  }
  
  # --- Convolution per Block --- 
  block_ids <- unique(blockids)
  # Precompute global sample times once to avoid buggy blockids handling
  global_samples <- fmrihrf::samples(sampling_frame, global = TRUE)
  sample_blockids <- fmrihrf::blockids(sampling_frame)
  cmat_list <- lapply(block_ids, function(bid) {
    idx <- which(blockids == bid)
    # Ensure we subset the correct dmat based on drop.empty consistency
    dblock <- dmat[idx, , drop = FALSE] 
    globons_block <- globons[idx]
    durations_block <- durations[idx]
    
    # Skip block if dblock is empty (e.g., no events for this term in this block)
    if(nrow(dblock) == 0 || ncol(dblock) == 0) return(NULL)

    # Subset hrf_list for this block if applicable
    hrf_list_block <- if (!is.null(hrf_list)) hrf_list[idx] else NULL

    reg <- convolve_design(hrf, dblock, globons_block, durations_block, summate = summate, hrf_list = hrf_list_block)
    
    # FIXED METHOD: Evaluate against all global samples but extract only block-specific rows
    # This preserves temporal accuracy while maintaining correct dimensions
    sam_all <- fmrihrf::samples(sampling_frame, global = TRUE)
    
    # Evaluate regressors against ALL samples (preserves temporal accuracy)
    full_block_mat <- do.call(cbind, lapply(reg, function(r) fmrihrf::evaluate(r, sam_all, precision = precision)))
    
    # Extract only the rows for this specific block (maintains correct dimensions)
    block_lengths <- fmrihrf::blocklens(sampling_frame)
    if (as.integer(bid) == 1) {
      block_start <- 1
      block_end <- block_lengths[1]
    } else {
      block_start <- sum(block_lengths[1:(as.integer(bid)-1)]) + 1
      block_end <- sum(block_lengths[1:as.integer(bid)])
    }
    block_mat <- full_block_mat[block_start:block_end, , drop = FALSE]
    block_mat
  })
  
  # Remove NULLs (from blocks with no events/cols) and rbind
  cmat_list <- Filter(Negate(is.null), cmat_list)
  
  # --- Generate Final Column Names --- 
  nb <- fmrihrf::nbasis(hrf)
  # Use the base_cnames derived directly from the dmat that was convolved
  cn <- make_column_names(term_tag, base_cnames, nb)
  
  # Handle case where convolution results in an empty matrix
  if (length(cmat_list) == 0) {
      warning(sprintf("Convolution resulted in an empty matrix for term '%s\'.\n  Returning tibble with correct names but 0 rows.", term_tag), call.=FALSE)
      # Return empty tibble with correct names and 0 rows
      return(tibble::as_tibble(matrix(numeric(0), nrow=0, ncol=length(cn)), 
                               .name_repair="minimal", .names_minimal = cn))
  }
  cmat <- do.call(rbind, cmat_list)
  
  # Handle add_sum flag if present (set by trialwise)
  if (isTRUE(attr(x, "add_sum"))) {
    if (ncol(cmat) > 0) { # Ensure there are columns to average
      mean_col <- matrix(rowMeans(cmat, na.rm = TRUE), ncol = 1)
      mean_col_name <- make.names(paste0(attr(x, "add_sum_label") %||% term_tag, "_mean"))
      colnames(mean_col) <- mean_col_name
      cmat <- cbind(cmat, mean_col)
      # Update column names to include the new mean column
      cn <- c(cn, mean_col_name)
    } else {
      warning(sprintf("Cannot add sum column for term '%s': no base columns generated.", term_tag))
    }
  }
  
  # Assign names, checking for length consistency
  if (length(cn) == ncol(cmat)) {
    colnames(cmat) <- cn
  } else {
      warning(sprintf("Final column name count (%d) mismatch with convolved matrix columns (%d) for term '%s'. Using generic names.", 
                      length(cn), ncol(cmat), term_tag), call. = FALSE)
      colnames(cmat) <- make.names(paste0("col_", seq_len(ncol(cmat))), unique=TRUE)
  }
  
  # --- Optional Debug Validation --- 
  if (getOption("fmrireg.debug", FALSE)) {
     fn <- get0("is_valid_heading", mode = "function")
     if (!is.null(fn)){
        stopifnot(all(fn(colnames(cmat))))
     } else {
        warning("fmrireg.debug=TRUE: is_valid_heading helper not found for validation.")
     }
  }
  
  # --- Return Result --- 
  suppressMessages(tibble::as_tibble(cmat, .name_repair = "minimal"))
}

## ============================================================================
## Section 11: F-Contrast Computation
## ============================================================================

#' @export
Fcontrasts.event_term <- function(x, max_inter = 4L, ...) {

  ## --- helpers -------------------------------------------------------------
  .is_cat <- function(ev) !is_continuous(ev)
  .Dmat   <- function(n) {
      if (n < 2) stop("Need at least 2 levels for contrasts.")
      con <- stats::contr.sum(n)
      colnames(con) <- paste0("c", 1:(n - 1))
      con
  }
  .Cvec   <- function(n) matrix(1, nrow = n, ncol = 1)

  ## --- preparation ---------------------------------------------------------
  evs_cat <- Filter(.is_cat, x$events)
  if (!length(evs_cat)) stop("No categorical variables found in term '", x$varname, "' for Fcontrasts.", call.=FALSE)

  C <- lapply(evs_cat, function(ev) .Cvec(length(levels(ev))))
  D <- lapply(evs_cat, function(ev) .Dmat(length(levels(ev))))
  names(C) <- names(D) <- names(evs_cat)

  # --- Get expected row names in Kronecker order ---------------------------
  cat_levels_list <- lapply(evs_cat, levels)
  # --- build Cartesian product of levels (Kronecker order) ------------------ 
  lvl_grid        <- do.call(expand.grid, cat_levels_list)
  cat_cond_names  <- apply(lvl_grid, 1, paste, collapse = ":")
  expected_rows   <- nrow(lvl_grid)
  # Try to align with the canonical condition tags used in design_matrix()
  cond_tags <- try(conditions(x, drop.empty = FALSE, expand_basis = FALSE), silent = TRUE)
  if (!inherits(cond_tags, "try-error") && length(cond_tags) == expected_rows) {
    cat_cond_names <- cond_tags
  }
  # ---------------------------------------------------------------------- 

  ## --- Compute main effects matrices (without rownames yet) ---------------
  main <- Map(function(i) {
      mat_list <- C
      mat_list[[i]] <- D[[i]] 
      Reduce(kronecker, mat_list)
  }, seq_along(D)) |> 
    stats::setNames(names(evs_cat))

  ## --- Compute interaction effects matrices (without rownames yet) -------
  final_contrasts_list <- if (length(D) > 1 && length(D) <= max_inter) {
      inter <- unlist(lapply(2:length(D), function(k) {
          combn(length(D), k, simplify = FALSE, FUN = function(ix) {
              mat_list <- C
              mat_list[ix] <- D[ix] 
              M <- Reduce(kronecker, mat_list)
              attr(M, "name") <- paste(names(evs_cat)[ix], collapse=":")
              M
          })
      }), recursive = FALSE)
      names(inter) <- vapply(inter, attr, "", "name")
      c(main, inter)
  } else {
      main
  }
  
  ## --- Assign correct categorical rownames to all matrices -------
  final_contrasts_named <- lapply(seq_along(final_contrasts_list), function(i) {
       M <- final_contrasts_list[[i]]
       mat_name <- names(final_contrasts_list)[i]
       if (!is.matrix(M)) { 
            warning(paste("Skipping rownames for invalid matrix in Fcontrasts list element:", mat_name))
            return(M)
       }
       # Compare nrow(M) to expected rows from CATEGORICAL grid/interaction
       if (nrow(M) == expected_rows) {
            # Use dimnames[[1]] <- assignment (should be correct now)
            dimnames(M)[[1]] <- cat_cond_names
       } else {
           # This warning should be less likely now, but keep for safety
           warning(paste("Dimension mismatch for contrast '", mat_name, "': expected ", 
                         expected_rows, " rows (from categorical interaction), but matrix has ", nrow(M), ". Rownames not assigned."))
       }
       M # Return matrix (modified in place)
  })
  
  names(final_contrasts_named) <- names(final_contrasts_list)
  
  final_contrasts_named
}

#' Retrieve contrast definitions for an event term
#'
#' This accessor returns the list of contrast specifications attached to the
#' term's originating hrfspec, if any.
#'
#' @param x An `event_term` object.
#' @param ... Unused.
#' @return A list of contrast specifications or `NULL` when none are defined.
#' @export
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
#' contrasts(terms(emod)[[1]])
contrasts.event_term <- function(x, ...) {
  hrfspec <- attr(x, "hrfspec")
  if (is.null(hrfspec)) return(NULL)
  hrfspec$contrasts
}

## ============================================================================
## Section 12: Design Matrix Construction
## ============================================================================

#' @method design_matrix event_term
#' @rdname design_matrix
#' @export
#' @importFrom tibble as_tibble
design_matrix.event_term <- function(x, drop.empty = TRUE, ...) {

  # --- Special case: term contains only one continuous event --- 
  # This includes single numeric variables and multi-column basis functions.
  # Bypass model.matrix and return the value matrix directly.
  if (is_continuous(x) && length(x$events) == 1) {
    ev      <- x$events[[1]]
    # Directly use the value matrix (N x K)
    out_mat <- ev$value 
    # Ensure it's a data frame before naming
    out_df <- as.data.frame(out_mat) # Convert matrix to data frame
    
    # Use columns() to get the base condition tags for naming intermediate matrix
    # These tags represent the *parts* of the final name (e.g., "01", "02" for Poly)
    # Do NOT sanitize with make.names here, as these are intermediate component names.
    intermediate_cond_tags <- try(columns(ev), silent = TRUE)

    if (inherits(intermediate_cond_tags, "try-error") || length(intermediate_cond_tags) != ncol(out_df)) {
      warning(sprintf("Failed to get valid condition tags via columns() or column count mismatch for event term '%s' (varname: '%s'). Using generic V# names for intermediate matrix.", 
                      x$varname %||% "UnnamedTerm", ev$varname %||% "UnnamedEventVar"), call. = FALSE)
      # Fallback to generic, sanitized names if columns() fails or gives wrong number
      cnames <- make.names(paste0("V", seq_len(ncol(out_df))), unique = TRUE) 
    } else {
      # Use the raw condition tags (e.g., "01", "02") directly as intermediate names.
      # These are not necessarily valid full R names yet but are components.
      cnames <- intermediate_cond_tags 
    }
    
    names(out_df) <- cnames
    # Return as a tibble
    return(tibble::as_tibble(out_df, .name_repair = "minimal"))
  }
  
  ## ----------------------------------------------------------------
  ## 1. Build the "data" data-frame that model.matrix() needs
  ## ----------------------------------------------------------------
  #   * For categorical events -> factor column (1 per event)
  #   * For continuous events  -> numeric column(s) (one per matrix column)
  
  # Helper to extract appropriate column(s) from an event object
  build_cols <- function(ev) {
    if (is_categorical(ev)) {
      # Categorical: return data frame with factor column named ev$varname
      fac <- factor(ev$value[, 1L],
                    levels = seq_along(ev$meta$levels),
                    labels = ev$meta$levels)
      df_out <- data.frame(fac, check.names=FALSE)
      colnames(df_out) <- ev$varname 
      df_out
    } else {
      # Continuous/Basis: return data frame with a single matrix column named ev$varname
      mat_col <- ev$value # This is the N x K matrix
      df_out <- data.frame(I(mat_col)) # Use I() to store matrix in one column
      colnames(df_out) <- ev$varname # Name the column containing the matrix
      df_out
    }
  }
  
  # Combine columns from all events into a single data frame
  # cbind should now handle the mix of factor columns and matrix columns
  df_list <- lapply(x$events, build_cols)
  df <- try(do.call(cbind, df_list), silent=TRUE)
  if (inherits(df, "try-error")) {
      stop("Failed to construct data frame for model.matrix from event term: ", x$varname, 
           "\n  Original error: ", attr(df, "condition")$message)
  }
  
  # === DEBUG PRINT ===
  #message(sprintf("Term: %s, nrow(df): %d, length(x$onsets): %d", x$varname, nrow(df), length(x$onsets)))
  # === END DEBUG ===
  
  if (nrow(df) != length(x$onsets)) {
      stop("Internal error: Row mismatch when building data frame for event term: ", x$varname)
  }

  ## ----------------------------------------------------------------
  ## 2. model.matrix() with the term's formula
  ## ----------------------------------------------------------------
  # Get the formula (e.g., ~ Condition:Modulator - 1)
  form <- formula(x)
  
  # Special case: Check for single-level factors which cause model.matrix to fail
  # If any factor column has only one level, handle it specially
  has_single_level_factor <- FALSE
  for (col_name in colnames(df)) {
    col_data <- df[[col_name]]
    if (is.factor(col_data) && nlevels(col_data) <= 1) {
      has_single_level_factor <- TRUE
      break
    }
  }
  
  if (has_single_level_factor) {
    # For single-level factors, create a simple matrix of ones
    # This represents the constant effect of that single level
    n_rows <- nrow(df)
    mm <- matrix(1, nrow = n_rows, ncol = 1)
    # Use the condition name from conditions() for naming
    expected_colnames_raw <- conditions(x, drop.empty = FALSE)
    if (length(expected_colnames_raw) > 0) {
      colnames(mm) <- make.names(expected_colnames_raw[1], unique = TRUE)
    } else {
      colnames(mm) <- make.names(x$varname, unique = TRUE)
    }
  } else {
    # Normal case: use model.matrix
    # Ensure the data frame column names match what the formula expects
    # (formula uses original names, df uses sanitized/numbered names)
    # model.matrix should handle this via the data=df argument.
    
    mm <- try(model.matrix(form, data = df), silent=TRUE)
    if (inherits(mm, "try-error")) {
         stop("Failed to create model matrix for event term: ", x$varname, 
              "\n  Formula was: ", deparse(form),
              "\n  Data frame columns: ", paste(colnames(df), collapse=", "),
              "\n  Original error: ", attr(mm, "condition")$message)
    }
    
    # --- SET COLNAMES using conditions() as the single source of truth --- 
    # Get potentially *all* condition names first (before drop.empty)
    expected_colnames_raw <- conditions(x, drop.empty = FALSE)
    
    # Sanitize the raw names using make.names
    expected_colnames <- make.names(expected_colnames_raw, unique = TRUE)
    
    # Basic check: Does the number of columns match?
    # model.matrix might produce fewer columns if rank-deficient, 
    # but conditions() should produce the full set based on expand.grid.
    # This mismatch needs careful handling. For now, assume model.matrix is correct
    # in terms of *which* columns are estimable, and use conditions() to name them.
    # If ncol(mm) < length(expected_colnames), it implies model.matrix dropped some.
    
    # TODO: How to robustly map expected_colnames to the columns present in mm?
    # This is tricky. model.matrix column names (e.g., ConditionB:Modulator1)
    # don't directly map to conditions() output (e.g., Condition[B]:Modulator[1]).
    # For now, we ASSUME the order is the same and the number of columns matches 
    # *if the design is full rank*. If not, this naming will be wrong.
    # A more robust solution might involve parsing model.matrix colnames or attributes.
    # Let's proceed with the direct assignment as per the reviewer's suggestion, 
    # but acknowledge this fragility.
    if (ncol(mm) != length(expected_colnames)) {
        warning(sprintf("Column count mismatch for '%s': model.matrix (%d) vs conditions (%d). Naming may be incorrect due to rank deficiency.",
                        x$varname, ncol(mm), length(expected_colnames_raw)), call. = FALSE)
        # Attempt to name the existing columns anyway, hoping the order matches
        # This might fail if length(expected_colnames) is shorter, though unlikely.
        colnames(mm) <- expected_colnames[1:ncol(mm)] 
    } else {
        colnames(mm) <- expected_colnames
    }
  }
  
  ## ----------------------------------------------------------------
  ## 3. Drop empty columns if requested (optional)
  ## ----------------------------------------------------------------
  # model.matrix might return fewer columns than expected if interactions 
  # lead to rank deficiency. drop.empty applies to the *output* matrix.
  if (isTRUE(drop.empty)) {
      # Check for intercept columns and constant columns
      # Intercept columns are named "(Intercept)" 
      # Constant columns have zero variance but non-zero values (e.g., all ones)
      is_intercept <- (colnames(mm) == "(Intercept)")
      
      # Calculate variance and check for all-zero columns
      col_vars <- apply(mm, 2, var, na.rm = TRUE)
      col_all_zero <- colSums(abs(mm), na.rm = TRUE) == 0
      
      # A column should be kept if:
      # 1. It's an intercept column, OR
      # 2. It has non-zero variance (not constant), OR  
      # 3. It's a constant non-zero column (zero variance but not all zeros)
      is_constant_nonzero <- (col_vars < 1e-8 | is.na(col_vars)) & !col_all_zero
      keep_cols <- which(is_intercept | col_vars > 1e-8 | is.na(col_vars) | is_constant_nonzero)
      
      if (length(keep_cols) < ncol(mm)){
          # message("Dropping empty columns: ", paste(colnames(mm)[! (1:ncol(mm)) %in% keep_cols], collapse=", "))
          mm <- mm[, sort(keep_cols), drop = FALSE]
      }
  }
  
  # Optional: Handle NAs by zero-filling (historical behavior)
  # mm[is.na(mm)] <- 0 

  tibble::as_tibble(mm, .name_repair = "check_unique")
}

## ============================================================================
## Section 13: Print Methods
## ============================================================================

#' Print fmri_term objects.
#'
#' @param x An fmri_term object.
#' @param ... Additional arguments.
#' @export
#' @rdname print
print.fmri_term <- function(x, ...) {
  cat("fmri_term: ", class(x)[[1]], "\n")
  cat("  Term Name: ", x$varname, "\n")
  cat("  Num Rows: ", nrow(design_matrix(x)), "\n")
  cat("  Num Columns: ", ncol(design_matrix(x)), "\n")
}

#' Print convolved_term objects.
#'
#' @param x A convolved_term object.
#' @param ... Additional arguments.
#' @export
#' @rdname print
print.convolved_term <- function(x, ...) {
  cat("fmri_term: ", class(x)[[1]], "\n")
  cat("  Term Name: ", x$varname, "\n")
  cat("  Formula:  ", as.character(formula(x$evterm)), "\n")
  cat("  Num Events: ", nrow(x$evterm$event_table), "\n")
  cat("  Num Rows: ", nrow(design_matrix(x)), "\n")
  cat("  Num Columns: ", ncol(design_matrix(x)), "\n")
  cat("  Conditions: ", conditions(x), "\n")
  cat("  Term Types: ", paste(purrr::map_chr(x$evterm$events, ~ class(.)[[1]])), "\n")
}


#' Print event_term objects
#'
#' Provides a concise summary of an event_term object using cli.
#'
#' @param x An event_term object.
#' @param ... Additional arguments (unused).
#' @import cli
#' @export
#' @rdname print
print.event_term <- function(x, ...) {
  nevents <- length(x$onsets)
  nvars <- length(x$events)
  
  cli::cli_h1("Event Term: {.field {x$varname}}")
  
  cli::cli_div(theme = list(span.info = list(color = "blue")))
  cli::cli_text("{.info * Number of Events:} {nevents}")
  cli::cli_text("{.info * Variables:} {paste(names(x$events), collapse = ", ")}")

  cli::cli_h2("Variable Types")
  if (nvars > 0) {
    for (name in names(x$events)) {
      # Use is_continuous generic method
      type <- if (is_continuous(x$events[[name]])) "Continuous" else "Categorical"
      # Use {.field {name}} for safer interpolation of the variable name
      cli::cli_text("{.info  * {.field {name}}:} {type}")
    }
  } else {
     cli::cli_text(" (No variables in term)")
  }
  
  if (nevents > 0) {
    cli::cli_h2("Timing")
    onset_range <- range(x$onsets, na.rm = TRUE)
    dur_range <- range(x$durations, na.rm = TRUE)
    # Evaluate sprintf outside cli::cli_text to avoid interpolation issues
    onset_range_str <- sprintf("%.2f - %.2f sec", onset_range[1], onset_range[2])
    dur_range_str <- sprintf("%.2f - %.2f sec", dur_range[1], dur_range[2])
    cli::cli_text("{.info * Onset Range:} {onset_range_str}")
    cli::cli_text("{.info * Duration Range:} {dur_range_str}")
    
    cli::cli_h2("Blocks")
    blocks_table <- table(x$blockids)
    nblocks <- length(blocks_table)
    cli::cli_text("{.info * Number of Blocks:} {nblocks}")
    # Truncate long block lists
    max_show_blocks <- 10
    blocks_display <- if(nblocks > max_show_blocks) {
                          paste(c(names(blocks_table)[1:max_show_blocks], "..."), collapse = ", ")
                      } else {
                          paste(names(blocks_table), collapse = ", ")
                      }
    cli::cli_text("{.info * Block IDs:} {blocks_display}")
    events_per_block_display <- if(nblocks > max_show_blocks) {
                                     paste(c(blocks_table[1:max_show_blocks], "..."), collapse = ", ")
                                 } else {
                                     paste(blocks_table, collapse = ", ")
                                 }
    cli::cli_text("{.info * Events per Block:} {events_per_block_display}")
  } else {
      cli::cli_alert_info("Event term is empty.")
  }
  cli::cli_end()
  
  invisible(x)
}

## ============================================================================
## Section 14: End of File
###############################################################################
# End of event_vector.R
