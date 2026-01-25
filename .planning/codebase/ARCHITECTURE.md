# Architecture

**Analysis Date:** 2025-01-25

## Pattern Overview

**Overall:** Functional domain-driven design with S3 object system and generic function dispatch.

**Key Characteristics:**
- Multi-stage pipeline for event model construction (parse → realise → build)
- Block-structured design matrices respecting fMRI run/block organization
- S3 class hierarchy for representing events, terms, and models
- Formula interface leveraging R's formula syntax for intuitive model specification
- Extensive use of S3 generics for polymorphic operations across object types

## Layers

**Entry Point / User Interface:**
- Purpose: Primary API functions for user interaction
- Location: `R/event_model.R`, `R/baseline_model.R`, `R/hrf-formula.R`
- Contains: `event_model()`, `baseline_model()`, `hrf()`, `trialwise()`, `covariate()` functions
- Depends on: All downstream layers
- Used by: Direct user code and scripts

**Formula Processing:**
- Purpose: Parse and interpret R formula syntax, extract variables, evaluate HRF specifications
- Location: `R/event_model_helpers.R`, `R/hrf-formula.R`
- Contains: `parse_event_formula()`, `find_and_eval_hrf_calls()`, `make_hrf()`, `realise_event_terms()`
- Depends on: Event classes layer
- Used by: Event model construction pipeline

**Event System:**
- Purpose: Represent individual events and event sequences
- Location: `R/event-classes.R`, `R/event_vector.R`
- Contains: `event()` (internal factory), `event_term()`, `event_factor()`, `event_variable()`, `event_matrix()`
- Depends on: Validation layer, utilities
- Used by: Formula processing, term construction

**Design Matrix Construction:**
- Purpose: Convert event specifications into numerical design matrices
- Location: `R/event_model.R` (build_event_model_design_matrix), `R/event_vector.R`, `R/baseline_model.R`
- Contains: HRF convolution, block-diagonal assembly, column indexing
- Depends on: fmrihrf (external HRF functions), Matrix (sparse structures)
- Used by: Event model and baseline model assembly

**Generics & Accessors:**
- Purpose: Define generic functions for consistent interface across object types
- Location: `R/design_generics.R`
- Contains: `design_matrix()`, `conditions()`, `event_terms()`, `baseline_terms()`, `convolve()`, `elements()`, etc.
- Depends on: None (defines interface)
- Used by: All object types implement these generics

**Contrast System:**
- Purpose: Specify and manage contrast specifications for hypothesis testing
- Location: `R/contrast.R`
- Contains: Contrast definition, legacy pattern translation, condition filtering
- Depends on: Term and design matrix infrastructure
- Used by: HRF specifications, event models

**Baseline Models:**
- Purpose: Construct nuisance regressors for baseline drift correction
- Location: `R/baseline_model.R`
- Contains: Polynomial drift, B-splines, natural splines, block-wise intercepts, nuisance regressors
- Depends on: Design generics, splines, Matrix
- Used by: Standalone baseline construction

**Basis Functions:**
- Purpose: Represent parametric basis transformations (polynomial, scaling, B-splines)
- Location: `R/basis.R`
- Contains: `Poly()`, `Scale()`, `BSpline()`, `sub_basis()`, basis prediction methods
- Depends on: None (standalone)
- Used by: Event specification, HRF functions

**Naming & Validation:**
- Purpose: Column naming consistency, input validation, error checking
- Location: `R/naming-utils.R`, `R/validate.R`, `R/utils-internal.R`
- Contains: Name sanitization, clash resolution, argument validation, helper utilities
- Depends on: stringr, assertthat, rlang
- Used by: All layers for consistent naming and validation

**Visualization:**
- Purpose: Plot design matrices and correlations
- Location: `R/design_map.R`
- Contains: ggplot-based visualization of design matrices, correlation maps
- Depends on: ggplot2, plotly
- Used by: Design inspection and diagnostics

## Data Flow

**Event Model Construction (Main Pipeline):**

1. User calls `event_model(formula, data, block, sampling_frame, ...)`
2. `parse_event_model()` extracts:
   - Onset variable from formula LHS
   - Block assignments from data
   - HRF specifications via `find_and_eval_hrf_calls()` on formula RHS
3. `realise_event_terms()` processes each HRF spec:
   - Evaluates variables from data
   - Constructs `event_term` objects via `event_term()` factory
   - Each `event_term` contains multiple `event` objects for variables in the term
4. `build_event_model_design_matrix()`:
   - Convolves each term's events with specified HRF
   - Assembles block-diagonal matrix (one block per run)
   - Creates column index metadata for term-to-column mapping
5. Returns assembled `event_model` object with `design_matrix`, `terms`, `sampling_frame`, `contrasts`

**Event Specification Flow:**

1. `hrf(condition, basis="spmg1")` creates `hrfspec` object encoding:
   - Variables to cross
   - HRF basis (character, HRF object, or function)
   - Optional contrasts, lag, nbasis
2. During `realise_event_terms()`:
   - Extract values of specified variables from data
   - Create `event_term` via `event_term(list(...), onsets, blockids, durations)`
3. `event_term()` factory:
   - Maps each variable to `event()` objects using `EV()` internal dispatch
   - Factors become categorical events, numerics become continuous
   - Builds `event_table` with row-wise categorical combinations

**Design Matrix Generation:**

1. For each `event_term`:
   - Extract event onsets, durations, blockids
   - Convolve with HRF using `convolve.event_term()`
   - Creates block-wise matrices, one per run/block
2. Block-diagonal assembly:
   - Stack all block matrices vertically
   - Track column indices per term
   - Add metadata attributes (col_indices, term_spans)
3. Return as tibble with columns = regressors, rows = scans

**State Management:**

- **Sampling Frame**: Central timing object from fmrihrf defining TR, block lengths, total scans
- **Block Structure**: Events tagged with blockid (non-decreasing integer), preserved through all stages
- **Column Metadata**: `col_indices` attribute maps term names to column ranges in design matrix
- **Immutability**: All operations create new objects; original data unchanged

## Key Abstractions

**event_model (S3 class):**
- Purpose: Complete fMRI design specification with event-based regressors
- Files: `R/event_model.R`
- Pattern: List-like S3 object containing `terms`, `design_matrix`, `sampling_frame`, `contrasts`, `model_spec`
- Properties: Preserves formula and data context for inspection and reconstruction

**baseline_model (S3 class):**
- Purpose: Drift correction and nuisance regressor specification
- Files: `R/baseline_model.R`
- Pattern: List-like object containing drift specification and constructed matrices
- Methods: Polynomial, B-spline, natural spline bases with optional block-wise intercepts

**event_term (S3 class):**
- Purpose: Single term in model formula with one or more crossed variables
- Files: `R/event_vector.R`
- Pattern: Contains list of `event` objects (one per variable), plus metadata
- Properties: Tracks event timing, variable combinations, and event table

**event (internal S3 class):**
- Purpose: Single event variable with standardized storage
- Files: `R/event-classes.R`
- Pattern: Stores normalized value matrix (numeric), metadata, blockids, onsets, durations
- Used internally by event_term and convolve operations

**hrfspec (S3 class):**
- Purpose: Specification for HRF term before realization
- Files: `R/hrf-formula.R`
- Pattern: Captures variable references, HRF basis choice, contrasts, lag
- Lifecycle: Created by `hrf()`, evaluated during `parse_event_formula()`

**convolved_term (S3 class):**
- Purpose: Event term after convolution with HRF
- Files: `R/event_vector.R`
- Pattern: Design matrix with basis columns per condition
- Properties: Inherits from event_term, adds convolved regressors

## Entry Points

**event_model():**
- Location: `R/event_model.R` (main dispatcher), `R/design_generics.R` (generic)
- Triggers: Direct user call with formula or list specification
- Responsibilities:
  - Parse formula into onsets and HRF specifications
  - Realise terms into event objects
  - Build combined design matrix
  - Validate block structure

**baseline_model():**
- Location: `R/baseline_model.R`
- Triggers: Direct user call specifying drift basis and optional nuisance terms
- Responsibilities:
  - Construct polynomial/spline basis matrices
  - Handle block-wise intercepts
  - Assemble block-diagonal structure

**hrf():**
- Location: `R/hrf-formula.R`
- Triggers: Called within event_model formula (RHS)
- Responsibilities:
  - Package variable references and HRF basis into hrfspec
  - Validate basis specification
  - Store contrasts and HRF parameters

## Error Handling

**Strategy:** Fail fast with informative messages. Validation at each layer.

**Patterns:**

- **Onset Validation**: `assert_that(!is.unsorted(blockids))` ensures block structure
- **Dimension Matching**: Check event lengths match onsets in `event()` factory
- **HRF Validity**: Verify basis type in `make_hrf()`, reject unknown bases
- **Formula Parsing**: Wrap `eval_tidy()` calls with try/catch to report evaluation failures
- **Design Matrix Assembly**: Verify column counts match before stacking blocks

**Key Error Messages:**
- "blockids must consist of non-decreasing integers"
- "onset variable is not numeric"
- "Unknown HRF basis name"
- "Matrices must have same number of columns for block-diagonal assembly"

## Cross-Cutting Concerns

**Logging:** No formal logging; uses `cli` package for progress bars during term realisation (optional `progress` argument).

**Validation:**
- Input validation via `assertthat::assert_that()` with clear messages
- Helper `.checkEVArgs()` normalizes event arguments (recycling, subsetting)
- Type checks for formula components, HRF basis, block structure

**Authentication:** Not applicable (not a web service).

**Column Naming:**
- Pattern: `term_tag_condition_tag_b##`
- Clash resolution: Auto-generated tags from variable names, `#N` suffix for clashes
- Sanitization: Dots → underscores in tags
- Central implementation: `naming-utils.R`

**Block Handling:**
- All operations preserve block structure
- Design matrices are block-diagonal
- Baseline and event terms aligned by block ID
- Sampling frame defines total blocks and lengths

---

*Architecture analysis: 2025-01-25*
