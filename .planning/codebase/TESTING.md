# Testing Patterns

**Analysis Date:** 2025-01-25

## Test Framework

**Runner:**
- `testthat` (version not specified, but Roxygen 7.3.3 implies recent testthat)
- Config: `tests/testthat.R` loads and runs tests
- Edition: testthat edition 3 (via `testthat::local_edition(3)`)

**Assertion Library:**
- `testthat` expectations (primary): `expect_equal()`, `expect_true()`, `expect_s3_class()`, etc.
- `assertthat` for runtime validation in source code (not test assertions)

**Run Commands:**
```bash
devtools::test()                          # Run all tests
testthat::test_file("tests/testthat/test_event_model.R")  # Run specific file
devtools::check()                         # Runs tests as part of package check
covr::package_coverage()                  # Generate coverage report
```

## Test File Organization

**Location:**
- Co-located in `tests/testthat/` directory (separate from source)
- File pattern: `test_{feature}.R` or `test-{feature}.R` (both naming styles used)

**Naming:**
- `test_event_model.R`: Main event model tests (962 lines)
- `test_baseline.R`: Baseline model tests (127 lines)
- `test_event_vector.R`: Event vector construction (928 lines)
- `test_contrast.R`: Contrast specification and validation (898 lines)
- `test_hrf.R`: HRF integration tests (490 lines)
- `test_hrf_generator.R`: HRF generator tests (353 lines)
- `test-convolution-fixes.R`: Regression tests (325 lines)
- `test-naming-utils.R`: Naming convention tests (166 lines)
- Helper files: `helper-naming.R` (13 lines)

**Test File Count:** 15 test files totaling ~4,481 lines
**Test Count:** ~182 individual test assertions/test_that blocks

## Test Structure

**Suite Organization:**
```r
library(testthat)
testthat::local_edition(3)  # Set testthat edition

# Optional: Load helper functions
source("helper-naming.R")

# Tests grouped by functionality
test_that("event_model creates correct object structure", {
  # Setup
  test_data <- create_test_data()

  # Execute
  model <- event_model(formula_or_list = form, data = events, block = ~block)

  # Assert
  expect_s3_class(model, "event_model")
  expect_named(model, c("terms", "design_matrix", ...))
})
```

**Patterns:**

**Setup Pattern:**
- Helper function: `create_test_data()` in `test_event_model.R` generates consistent test fixtures
- Parameters: seed (123), n_events (10), n_blocks (2)
- Returns list with `$events` (data.frame) and `$sf` (sampling_frame)

```r
create_test_data <- function(seed = 123, n_events = 10, n_blocks = 2) {
  set.seed(seed)
  block_len <- ceiling(n_events / n_blocks)
  blocklens <- rep(block_len, n_blocks)
  sf <- fmrihrf::sampling_frame(blocklens = blocklens, TR = TR)

  events <- data.frame(
    x = rnorm(n_events),
    condition = factor(rep(letters[1:2], length.out=n_events)),
    onset = ...
  )
  list(events=events, sf=sf)
}
```

**Teardown Pattern:**
- Implicit via garbage collection (no explicit cleanup needed for R tests)
- `on.exit()` used for resource cleanup in helper functions like `capture_cli_print()`

```r
capture_cli_print <- function(x) {
  old_opts <- options(cli.dynamic = FALSE)
  on.exit(options(old_opts), add = TRUE)  # Restore on exit

  tmp <- tempfile()
  con <- file(tmp, open = "wt")
  sink(con)
  on.exit({sink(); close(con); unlink(tmp)}, add = TRUE)

  print(x)
}
```

**Assertion Pattern:**
- Multiple assertions per test_that block (10-20 typical)
- Classes validated first: `expect_s3_class(model, "event_model")`
- Structure validated: `expect_named()`, `expect_type()`
- Attributes checked: `expect_equal(nrow(model$design_matrix), expected_rows)`

## Mocking

**Framework:** None detected (base R only)

**Patterns:**
- Direct instantiation of dependencies (no mocking library used)
- `fmrihrf::sampling_frame()` created directly in test setup
- Data frames constructed inline for test events

**What to Mock:**
- External HRF computations: Pass pre-computed `fmrihrf::sampling_frame()` objects
- Random data: Use `set.seed()` for reproducibility, not mocks

**What NOT to Mock:**
- Core model objects (`event_term`, `event_model`, `baseline_model`)
- Design matrix construction (test with actual data)
- Validation logic (should be tested as-is)

**Example without mocking:**
```r
test_that("event_model accepts both formula and list interfaces", {
  # Create real sampling frame and data
  sf <- fmrihrf::sampling_frame(blocklens = 100, TR = 2)
  events <- data.frame(onset = c(0, 10, 20), cond = factor(c("A", "B", "A")))

  # Test with formula
  m1 <- event_model(onset ~ hrf(cond), data = events, block = ~block, sampling_frame = sf)
  expect_s3_class(m1, "event_model")

  # Test with list
  spec <- list(hrf(cond))
  m2 <- event_model(spec, data = events, block = events$block, sampling_frame = sf)
  expect_s3_class(m2, "event_model")
})
```

## Fixtures and Factories

**Test Data:**
```r
# Minimal event data factory
create_test_data <- function(seed = 123, n_events = 10, n_blocks = 2) {
  set.seed(seed)
  data.frame(
    onset = sample(0:100, n_events),
    run = rep(1:n_blocks, length.out = n_events),
    condition = factor(rep(c("A", "B"), length.out = n_events))
  )
}

# Sampling frame factory
sf <- fmrihrf::sampling_frame(blocklens = c(100, 100), TR = 2)
```

**Location:**
- Helper functions in `tests/testthat/test_event_model.R` (not centralized)
- Some tests inline data generation
- `helper-naming.R` provides validation helpers like `is_valid_heading()`

**Example Fixture:**
```r
# From helper-naming.R
is_valid_heading <- function(x) {
  # Validates design matrix column names follow expected pattern
  grepl("^[A-Za-z\\.][A-Za-z0-9\\._#]*$", x)
}

# Used in naming tests to validate generated column headers
test_that("naming produces valid column headers", {
  expect_true(all(is_valid_heading(colnames(dm))))
})
```

## Coverage

**Requirements:** Not explicitly enforced (no coverage thresholds in config)

**View Coverage:**
```bash
covr::package_coverage()              # Print coverage summary
covr::report()                        # Open HTML coverage report
```

**Coverage Tools Available:**
- Listed in `Suggests:` in DESCRIPTION: `covr`
- Not integrated into CI/CD (no GitHub Actions workflow detected)

**Coverage Status:** Not explicitly tracked in repository

## Test Types

**Unit Tests:**
- Scope: Individual function behavior
- Approach: Direct function calls with controlled inputs
- Examples: `test_event_model.R`, `test_baseline.R`
- Pattern: Test creation, validation, design matrix generation for each model type

```r
test_that("event_model creates correct design matrix dimensions", {
  test_data <- create_test_data()
  model <- event_model(onset ~ hrf(condition), data = test_data$events, ...)

  # Assert on specific properties
  expect_equal(nrow(model$design_matrix), sum(fmrihrf::blocklens(sf)))
  expect_true(!is.null(attr(model$design_matrix, "term_spans")))
})
```

**Integration Tests:**
- Scope: Multiple components working together
- Approach: Build models, generate contrasts, validate against design matrices
- Examples: `test_contrast.R` (contrasts + event models), `test-convolution-fixes.R` (HRF + convolution + naming)
- Pattern: Multi-step workflows with intermediate validations

```r
test_that("can build contrasts from convolved terms", {
  sframe <- fmrihrf::sampling_frame(blocklens=rep(436/2, 2), TR=2)
  emodel <- event_model(onset ~ hrf(condition), data=facedes, block=~run, sampling_frame=sframe)

  con <- pair_contrast(~ condition=="A", ~ condition=="B", name="AvB")
  weights <- contrast_weights(con, terms(emodel)[[1]])

  expect_equal(as.vector(weights$weights), c(1, -1, 0, 0, 0))
})
```

**E2E Tests:**
- Framework: Not explicitly used
- Note: Integration tests serve as de facto end-to-end tests

## Common Patterns

**Async Testing:**
- Not applicable (R is single-threaded for test execution)
- `parallel=FALSE` is default in event_model; tests don't exercise parallel code
- Options set: `options(mc.cores=2)` in some test files for reproducibility

**Error Testing:**
```r
test_that("event_model rejects invalid inputs", {
  test_data <- create_test_data()

  # Test missing required parameter
  expect_error(
    event_model(onset ~ hrf(condition), data = test_data$events),  # missing sampling_frame
    "sampling_frame"
  )

  # Test invalid formula
  expect_error(
    event_model(~ hrf(condition), data = test_data$events, ...)  # missing LHS
  )
})
```

**Class/Inheritance Testing:**
```r
test_that("event_model has correct class hierarchy", {
  model <- event_model(onset ~ hrf(condition), data = events, block = ~block, sampling_frame = sf)

  expect_s3_class(model, "event_model")        # Direct class
  expect_s3_class(model, "list")               # Inherits from list
  expect_s3_class(model$terms[[1]], "event_term")
  expect_s3_class(model$design_matrix, "tbl_df")
})
```

**Attribute Testing:**
```r
test_that("design matrix has required attributes", {
  dm <- model$design_matrix

  term_spans <- attr(dm, "term_spans")
  col_indices <- attr(dm, "col_indices")

  expect_true(!is.null(term_spans))
  expect_true(!is.null(col_indices))
  expect_equal(length(term_spans), length(model$terms))
  expect_type(col_indices, "list")
})
```

**Dimension Testing:**
```r
test_that("design matrix dimensions are correct", {
  # Rows: should match total scans in sampling frame
  expect_equal(
    nrow(design_matrix(model)),
    sum(fmrihrf::blocklens(sf))
  )

  # Columns: should match sum of term contributions
  expect_equal(
    ncol(design_matrix(model)),
    sum(sapply(model$terms, function(t) ncol(design_matrix(t))))
  )
})
```

**Data Frame Testing:**
```r
test_that("event_table is properly structured", {
  eterm <- event_term(
    list(condition = factor(c("A", "B", "A")), intensity = c(1, 0.5, 1.2)),
    onsets = c(0, 10, 20),
    blockids = c(1, 1, 1)
  )

  et <- event_table(eterm)
  expect_s3_class(et, "tbl_df")
  expect_equal(nrow(et), 3)
  expect_true("condition" %in% colnames(et))
})
```

---

*Testing analysis: 2025-01-25*
