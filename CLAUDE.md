# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Overview

`fmridesign` is an R package for constructing design matrices for
functional magnetic resonance imaging (fMRI) analyses. It provides tools
for creating event models with flexible hemodynamic response functions
(HRFs) and baseline models for nuisance regression.

## Key Development Commands

### Build and Check

``` r
# Build the package
devtools::build()

# Check the package (includes running tests)
devtools::check()

# Install the package locally
devtools::install()

# Load the package for development
devtools::load_all()
```

### Testing

``` r
# Run all tests
devtools::test()

# Run a specific test file
testthat::test_file("tests/testthat/test-event_model.R")

# Run tests with coverage
covr::package_coverage()
```

### Documentation

``` r
# Generate documentation from roxygen2 comments
devtools::document()

# Build vignettes
devtools::build_vignettes()

# Check examples in documentation
devtools::run_examples()
```

## Architecture

### Core Components

1.  **Event Models** (`event_model.R`)
    - Main entry point:
      [`event_model()`](https://bbuchsbaum.github.io/fmridesign/reference/event_model.md)
      function
    - Handles formula-based (`onset ~ hrf(condition)`) and list-based
      interfaces
    - Uses three-stage pipeline: parse → realise → build design matrix
    - Column naming: `term_tag_condition_tag_b##` (e.g.,
      `condition_A_b01`)
2.  **Baseline Models** (`baseline_model.R`)
    - Main entry point:
      [`baseline_model()`](https://bbuchsbaum.github.io/fmridesign/reference/baseline_model.md)
      function
    - Handles drift correction (polynomial, B-splines, natural splines)
    - Supports block-wise intercepts and nuisance regressors
    - Uses block-diagonal matrix construction
3.  **HRF Integration** (`fmrihrf-imports.R`, `hrf-formula.R`)
    - Depends on `fmrihrf` package for hemodynamic response functions
    - Supports multiple HRF bases (canonical, SPM derivatives, FIR,
      etc.)
    - Formula interface uses
      [`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md)
      function to specify terms
4.  **Design Matrix Construction**
    - Both event and baseline models produce design matrices via
      [`design_matrix()`](https://bbuchsbaum.github.io/fmridesign/reference/design_matrix.md)
      generic
    - Matrices are block-structured for multi-run experiments
    - Column indices tracked for term-wise operations

### Key Design Patterns

- **S3 Object System**: Uses S3 classes throughout (`event_model`,
  `baseline_model`, `convolved_term`, etc.)
- **Generic Functions**:
  [`design_matrix()`](https://bbuchsbaum.github.io/fmridesign/reference/design_matrix.md),
  [`conditions()`](https://bbuchsbaum.github.io/fmridesign/reference/conditions.md),
  [`terms()`](https://rdrr.io/r/stats/terms.html),
  [`construct()`](https://bbuchsbaum.github.io/fmridesign/reference/construct.md)
- **Formula Interface**: Leverages R’s formula syntax for model
  specification
- **Tidy Data**: Returns tibbles for design matrices, uses tidyverse
  functions internally

### Dependencies

- Core: `fmrihrf` (for HRF functions), `Matrix` (sparse matrices),
  `splines`
- Tidyverse: `dplyr`, `tidyr`, `purrr`, `tibble`
- Validation: `assertthat`, `rlang`
- Visualization: `ggplot2`, `plotly`

## Important Considerations

1.  **Block Structure**: fMRI data is organized in blocks/runs. Most
    operations respect this structure.

2.  **Sampling Frame**: The `sampling_frame` object (from `fmrihrf`)
    defines timing information and is required for all models.

3.  **Column Naming**: The package uses a consistent naming scheme for
    design matrix columns to avoid clashes and maintain
    interpretability.

4.  **Contrast Specifications**: Contrasts can be defined within HRF
    terms and are propagated through to the full design matrix.
