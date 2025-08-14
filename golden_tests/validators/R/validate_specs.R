#!/usr/bin/env Rscript

#' Golden Test Validator for R Implementation
#' 
#' This script validates the R implementation against golden test specifications.
#' It parses XML test files, executes R code, and verifies numeric outputs.

library(xml2)
library(tibble)
library(dplyr)
library(purrr)

# Validation functions --------------------------------------------------------

#' Parse a golden test XML file
#' @param xml_path Path to the XML test specification
#' @return List containing test metadata, inputs, expected outputs, and R code
parse_golden_test <- function(xml_path) {
  doc <- read_xml(xml_path)
  
  # Handle namespace - use the default namespace
  ns <- xml_ns(doc)
  
  # Extract metadata
  metadata <- list(
    id = xml_text(xml_find_first(doc, "//d1:metadata/d1:id", ns)),
    version = xml_text(xml_find_first(doc, "//d1:metadata/d1:version", ns)),
    description = xml_text(xml_find_first(doc, "//d1:metadata/d1:description", ns))
  )
  
  # Extract R implementation
  r_code <- xml_text(xml_find_first(doc, "//d1:implementations/d1:R", ns))
  
  # Extract expected outputs
  expected <- list(
    dimensions = list(
      rows = as.integer(xml_text(xml_find_first(doc, "//d1:expected_outputs/d1:design_matrix/d1:dimensions/d1:rows", ns))),
      columns = as.integer(xml_text(xml_find_first(doc, "//d1:expected_outputs/d1:design_matrix/d1:dimensions/d1:columns", ns)))
    ),
    column_names = xml_text(xml_find_all(doc, "//d1:expected_outputs/d1:design_matrix/d1:column_names/d1:name", ns)),
    checks = xml_find_all(doc, "//d1:expected_outputs/d1:design_matrix/d1:numeric_checks/d1:check", ns) %>%
      map(~ parse_numeric_check(.x, ns))
  )
  
  list(
    metadata = metadata,
    r_code = r_code,
    expected = expected
  )
}

#' Parse a numeric check from XML
parse_numeric_check <- function(check_node, ns) {
  type <- xml_text(xml_find_first(check_node, "d1:type", ns))
  expected <- as.numeric(xml_text(xml_find_first(check_node, "d1:expected", ns)))
  tolerance <- as.numeric(xml_text(xml_find_first(check_node, "d1:tolerance", ns)))
  description <- xml_text(xml_find_first(check_node, "d1:description", ns))
  
  # Parse location
  location <- list()
  col_node <- xml_find_first(check_node, "d1:location/d1:column", ns)
  if (length(col_node) > 0) {
    location$column <- as.integer(xml_text(col_node)) + 1  # Convert to 1-based R indexing
  }
  
  row_node <- xml_find_first(check_node, "d1:location/d1:row", ns)
  if (length(row_node) > 0) {
    location$row <- as.integer(xml_text(row_node)) + 1  # Convert to 1-based R indexing
  }
  
  row_range_node <- xml_find_first(check_node, "d1:location/d1:row_range", ns)
  if (length(row_range_node) > 0) {
    range_vals <- as.integer(strsplit(xml_text(row_range_node), ",")[[1]])
    location$row_range <- range_vals + 1  # Convert to 1-based R indexing
  }
  
  col_range_node <- xml_find_first(check_node, "d1:location/d1:column_range", ns)
  if (length(col_range_node) > 0) {
    range_vals <- as.integer(strsplit(xml_text(col_range_node), ",")[[1]])
    location$column_range <- range_vals  # Store as 0-based, will convert in perform_numeric_check
  }
  
  # Parse column1 and column2 for correlation checks
  col1_node <- xml_find_first(check_node, "d1:location/d1:column1", ns)
  if (length(col1_node) > 0) {
    location$column1 <- as.integer(xml_text(col1_node)) + 1  # Convert to 1-based R indexing
  }
  
  col2_node <- xml_find_first(check_node, "d1:location/d1:column2", ns)
  if (length(col2_node) > 0) {
    location$column2 <- as.integer(xml_text(col2_node)) + 1  # Convert to 1-based R indexing
  }
  
  # Parse full_matrix for matrix-wide operations
  full_matrix_node <- xml_find_first(check_node, "d1:location/d1:full_matrix", ns)
  if (length(full_matrix_node) > 0) {
    location$full_matrix <- xml_text(full_matrix_node) == "true"
  }
  
  # Parse block for block-specific operations
  block_node <- xml_find_first(check_node, "d1:location/d1:block", ns)
  if (length(block_node) > 0) {
    location$block <- as.integer(xml_text(block_node))
  }
  
  list(
    type = type,
    expected = expected,
    tolerance = tolerance,
    description = description,
    location = location
  )
}

#' Execute a numeric check on a matrix
perform_numeric_check <- function(dm_matrix, check) {
  actual <- switch(check$type,
    exact_value = dm_matrix[check$location$row, check$location$column],
    
    peak_value = {
      if (!is.null(check$location$row_range)) {
        rows <- check$location$row_range[1]:check$location$row_range[2]
        max(abs(dm_matrix[rows, check$location$column]))
      } else {
        max(abs(dm_matrix[, check$location$column]))
      }
    },
    
    column_sum = sum(dm_matrix[, check$location$column]),
    
    row_sum = {
      if (!is.null(check$location$column_range)) {
        cols <- (check$location$column_range[1] + 1):(check$location$column_range[2] + 1)  # Convert 0-based to 1-based
        sum(dm_matrix[check$location$row, cols])  # Row is already 1-based from parsing
      } else {
        sum(dm_matrix[check$location$row, ])  # Row is already 1-based from parsing
      }
    },
    
    mean = mean(dm_matrix[, check$location$column]),
    
    std_dev = sd(dm_matrix[, check$location$column]),
    
    min = min(dm_matrix[, check$location$column]),
    
    max = max(dm_matrix[, check$location$column]),
    
    correlation = {
      if (!is.null(check$location$column1) && !is.null(check$location$column2)) {
        cor(dm_matrix[, check$location$column1], dm_matrix[, check$location$column2])
      } else {
        stop("correlation check requires both column1 and column2")
      }
    },
    
    matrix_rank = {
      if (!is.null(check$location$full_matrix) && check$location$full_matrix) {
        qr(dm_matrix)$rank
      } else {
        stop("matrix_rank check requires full_matrix=true")
      }
    },
    
    condition_number = {
      if (!is.null(check$location$full_matrix) && check$location$full_matrix) {
        svd_result <- svd(dm_matrix)
        max(svd_result$d) / min(svd_result$d[svd_result$d > 1e-10])
      } else {
        stop("condition_number check requires full_matrix=true")
      }
    },
    
    peak_time = {
      col_data <- dm_matrix[, check$location$column]
      peak_index <- which.max(abs(col_data)) - 1  # Convert to 0-based indexing
      peak_index * 2.0  # Convert to time assuming TR=2.0s
    },
    
    temporal_difference = {
      if (!is.null(check$location$column1) && !is.null(check$location$column2)) {
        col1_peak <- (which.max(abs(dm_matrix[, check$location$column1])) - 1) * 2.0
        col2_peak <- (which.max(abs(dm_matrix[, check$location$column2])) - 1) * 2.0
        abs(col2_peak - col1_peak)  # Already in time units
      } else {
        stop("temporal_difference check requires both column1 and column2")
      }
    },
    
    undershoot_ratio = {
      col_data <- dm_matrix[, check$location$column]
      min_val <- min(col_data)
      max_val <- max(col_data)
      min_val / max_val
    },
    
    block_independence = {
      if (!is.null(check$location$column) && !is.null(check$location$row_range)) {
        rows <- check$location$row_range[1]:check$location$row_range[2]
        mean(abs(dm_matrix[rows, check$location$column]))
      } else {
        stop("block_independence check requires column and row_range")
      }
    },
    
    min_singular_value = {
      if (!is.null(check$location$full_matrix) && check$location$full_matrix) {
        svd_result <- svd(dm_matrix)
        min(svd_result$d[svd_result$d > 1e-10])
      } else {
        stop("min_singular_value check requires full_matrix=true")
      }
    },
    
    stop("Unknown check type: ", check$type)
  )
  
  passed <- abs(actual - check$expected) <= check$tolerance
  
  list(
    type = check$type,
    description = check$description,
    expected = check$expected,
    actual = actual,
    tolerance = check$tolerance,
    passed = passed,
    diff = abs(actual - check$expected)
  )
}

#' Validate a single golden test
validate_golden_test <- function(xml_path) {
  cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
  cat("Validating:", basename(xml_path), "\n")
  cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")
  
  # Parse test specification
  test_spec <- parse_golden_test(xml_path)
  cat("Test ID:", test_spec$metadata$id, "\n")
  cat("Description:", test_spec$metadata$description, "\n\n")
  
  # Create a new environment for test execution
  test_env <- new.env(parent = globalenv())
  
  # Execute R code
  cat("Executing R implementation...\n")
  tryCatch({
    eval(parse(text = test_spec$r_code), envir = test_env)
  }, error = function(e) {
    cat("ERROR executing R code:\n", conditionMessage(e), "\n")
    return(list(success = FALSE, error = conditionMessage(e)))
  })
  
  # Check if dm_matrix exists
  if (!exists("dm_matrix", envir = test_env)) {
    cat("ERROR: dm_matrix not found after executing R code\n")
    return(list(success = FALSE, error = "dm_matrix not created"))
  }
  
  dm_matrix <- get("dm_matrix", envir = test_env)
  
  # Validate dimensions
  cat("\nValidating dimensions...\n")
  actual_dims <- dim(dm_matrix)
  expected_dims <- c(test_spec$expected$dimensions$rows, test_spec$expected$dimensions$columns)
  
  if (!all(actual_dims == expected_dims)) {
    cat(sprintf("  FAIL: Expected %dx%d, got %dx%d\n", 
                expected_dims[1], expected_dims[2],
                actual_dims[1], actual_dims[2]))
    return(list(success = FALSE, error = "Dimension mismatch"))
  } else {
    cat(sprintf("  PASS: Dimensions correct (%dx%d)\n", actual_dims[1], actual_dims[2]))
  }
  
  # Perform numeric checks
  cat("\nPerforming numeric checks...\n")
  results <- map(test_spec$expected$checks, ~ perform_numeric_check(dm_matrix, .x))
  
  # Display results
  for (result in results) {
    status <- if (result$passed) "PASS" else "FAIL"
    cat(sprintf("  %s: %s\n", status, result$description))
    cat(sprintf("       Expected: %.6f ± %.6f, Actual: %.6f (diff: %.6f)\n",
                result$expected, result$tolerance, result$actual, result$diff))
  }
  
  # Summary
  n_passed <- sum(map_lgl(results, ~ .x$passed))
  n_total <- length(results)
  all_passed <- n_passed == n_total
  
  cat(sprintf("\nSummary: %d/%d checks passed\n", n_passed, n_total))
  
  list(
    success = all_passed,
    test_id = test_spec$metadata$id,
    n_passed = n_passed,
    n_total = n_total,
    results = results
  )
}

#' Validate all golden tests in a directory
validate_all_tests <- function(spec_dir = "../../specs") {
  # Find all XML files
  xml_files <- list.files(spec_dir, pattern = "\\.xml$", 
                          recursive = TRUE, full.names = TRUE)
  
  cat("Found", length(xml_files), "test specifications\n\n")
  
  # Validate each test
  all_results <- map(xml_files, validate_golden_test)
  
  # Summary
  cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
  cat("OVERALL SUMMARY\n")
  cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")
  
  success_count <- sum(map_lgl(all_results, ~ .x$success))
  total_count <- length(all_results)
  
  cat(sprintf("\nTests passed: %d/%d\n", success_count, total_count))
  
  # List failed tests
  failed_tests <- all_results[!map_lgl(all_results, ~ .x$success)]
  if (length(failed_tests) > 0) {
    cat("\nFailed tests:\n")
    for (test in failed_tests) {
      cat(sprintf("  - %s (%d/%d checks passed)\n", 
                  test$test_id, test$n_passed, test$n_total))
    }
  }
  
  invisible(all_results)
}

# Main execution --------------------------------------------------------------

if (sys.nframe() == 0) {
  # Script is being run directly
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0) {
    # Validate all tests
    results <- validate_all_tests()
  } else {
    # Validate specific test
    results <- validate_golden_test(args[1])
  }
}