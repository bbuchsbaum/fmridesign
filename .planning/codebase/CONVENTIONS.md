# Coding Conventions

**Analysis Date:** 2025-01-25

## Naming Patterns

**Files:**
- Kebab-case for test files: `test_event_model.R`, `test-naming-utils.R`
- Snake_case for source files: `event_model.R`, `baseline_model.R`, `naming-utils.R`
- Helper utilities prefixed with underscore: `utils-internal.R`, `fmrihrf-imports.R`
- Test helper files: `helper-naming.R`

**Functions:**
- Snake_case for public and internal functions: `event_model()`, `baseline_model()`, `design_matrix()`
- Leading underscore for internal helpers: `.checkEVArgs()`, `.sanitizeName()`, `.label_component()`
- Generic functions use lowercase: `convolve()`, `cells()`, `conditions()`
- Roxygen tag convention: `@keywords internal`, `@noRd` for unexported internals

**Variables:**
- Snake_case for variables: `blocklens`, `sampling_frame`, `event_term`, `term_tags`
- Single letters for loop indices: `i`, `j`, `n`, `x`
- Descriptive names for parameters: `onsets`, `blockids`, `durations`, `drop_empty`

**Types & Classes:**
- S3 object classes in quotes: `c("event_model", "list")`, `c("event_term", "event_seq")`
- Class inheritance chain preserved in class attribute
- Method naming: `{generic}.{class}` pattern for S3 methods (e.g., `design_matrix.event_term`)

## Code Style

**Formatting:**
- No explicit linter configured (no `.lintr`, `.editorconfig`, or `styler` files detected)
- Standard R code style observed throughout codebase
- Line length: Generally 80-100 characters, flexible
- Indentation: 2-4 spaces (appears to be 2 primary)
- Comments use `#` with space after: `# This is a comment`

**Roxygen Documentation:**
- roxygen version: 7.3.3 with markdown enabled
- All exported functions use `@export` tag
- Documentation structure: title, description (`@details`), `@param`, `@return`, `@examples`
- Examples marked with `\dontrun{}` for long-running or interactive code
- Importation declarations: `@import assertthat`, `@importFrom dplyr mutate`, etc.
- Internal functions marked: `@keywords internal @noRd`

**Linting & Style Enforcement:**
- No automatic style enforcement detected
- Pattern suggests manual adherence to R conventions
- Manual code organization with section comments (e.g., `## ============================================================================`)

## Import Organization

**Order:**
1. Roxygen documentation imports (`@import`, `@importFrom`)
2. Namespace documentation: `NULL` statements with roxygen tags
3. Internal utility imports in file headers
4. Package dependencies from `DESCRIPTION` Imports section

**Path Aliases:**
- No path aliases detected
- Uses fully qualified package names: `fmrihrf::sampling_frame()`, `dplyr::mutate()`
- Some re-exports: `fmrihrf-reexports.R` provides wrapped access to `fmrihrf` functions

**Package Dependencies (from DESCRIPTION):**
```
Core: fmrihrf, assertthat, rlang, stringr, Matrix, splines
Data: dplyr, tidyr, purrr, tibble
Visualization: ggplot2, plotly
Utilities: utils, cli, stats
```

## Error Handling

**Patterns:**
- Explicit validation using `assertthat::assert_that()` with custom `msg=` parameter
- Direct `stop()` calls for control flow errors with `sprintf()` for message formatting
- `warning()` calls with `call.=FALSE` to suppress function name in message
- Try-catch pattern: `try({...}, silent=TRUE)` for optional dependencies

**Common checks:**
```r
# Length validation with custom message
assertthat::assert_that(length(x) == n, msg = sprintf("Length mismatch: got %d, expected %d", length(x), n))

# Conditional errors
if (!is.null(subset)) {
  assertthat::assert_that(length(subset) == n_initial, msg = sprintf("subset length mismatch"))
}

# NA and sorting validation
assertthat::assert_that(all(!is.na(onsets)), msg = sprintf("NA in onsets (%s)", vname))
assertthat::assert_that(!is.unsorted(blockids), msg = sprintf("blockids not non-decreasing"))
```

**Error message style:**
- Errors formatted with `sprintf()` for interpolation
- Messages include context (variable names, expected vs actual values)
- `call.=FALSE` used to suppress R's default function name prepend
- Validation happens early in functions (fail-fast pattern)

## Logging

**Framework:** Base R `message()` and `warning()` functions (no logging framework)

**Patterns:**
- `warning()` for non-fatal issues that user should know about
- `message()` for informational output (e.g., term generation defaults)
- Progress bars supported via `progress=TRUE` parameter in `event_model()`
- No debug-level logging implemented

**Examples:**
```r
warning("No terms provided to build design matrix.", call. = FALSE)
message(sprintf("Generated default list names for %d terms with NULL term_tags", length(na_indices)))
```

## Comments

**When to Comment:**
- Section headers with long comment blocks (40+ char separator lines): `## ============================================================================`
- Complex algorithm explanations: "Cumulative sum of columns, starting from 0"
- Non-obvious design decisions: "CRITICAL FIX: Replace sequences of one or more dots..."
- Inline comments for special cases or TODO items

**JSDoc/TSDoc:**
- Uses Roxygen2 format: `#' @param x Description`, `#' @return Description`
- All public functions fully documented
- Examples provided in `@examples` section
- Internal functions use `@keywords internal @noRd` instead of full docs

**TODO Pattern:**
- `TODO:` format appears in documentation: `TODO: Review/refactor/integrate .checkEVArgs itself later (Ticket EV-4)`
- Comments mark known limitations or future work

## Function Design

**Size:**
- Most functions 30-100 lines
- Large functions broken into logical sections with comment headers
- `event_vector.R` is 2000+ lines (utility collection rather than single function)

**Parameters:**
- Descriptive names for all parameters
- Default values for optional parameters: `durations = 0`, `drop_empty = TRUE`
- `...` used to capture additional arguments passed to methods
- Boolean parameters prefixed with verb: `drop_empty`, `parallel`, `progress`

**Return Values:**
- Explicit return statements: `return(list(...))`
- Functions return S3 objects with class attributes
- Attributes attached to returned objects: `attr(dm, "term_spans")`, `attr(dm, "col_indices")`
- NULL returns for void-like operations marked with inline comment

**Generic Functions:**
- Core operations defined as generics: `design_matrix()`, `convolve()`, `conditions()`
- Generic defined in `design_generics.R`: uses `UseMethod()` pattern
- Methods registered automatically via class matching (S3 convention)

## Module Design

**Exports:**
- Selective export strategy: not all functions exported
- Public API functions marked `@export` in Roxygen
- Internal utilities: `@keywords internal @noRd`
- Re-exported functions from `fmrihrf`: `fmrihrf-reexports.R`

**Barrel Files:**
- No explicit barrel files observed
- `NAMESPACE` auto-generated from Roxygen declarations
- Package collation order specified in `DESCRIPTION` file for load ordering

**File Organization (by function):**
- Core models: `event_model.R`, `baseline_model.R`
- Helpers for models: `event_model_helpers.R`
- Generic function definitions: `design_generics.R`
- Event construction: `event_vector.R`, `event-classes.R`
- Validation: `validate.R`
- Naming utilities: `naming-utils.R`, `utils-internal.R`
- External integrations: `fmrihrf-imports.R`, `fmrihrf-reexports.R`

---

*Convention analysis: 2025-01-25*
