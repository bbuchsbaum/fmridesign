# External Integrations

**Analysis Date:** 2025-01-25

## APIs & External Services

**No external APIs or cloud services:**
- fmridesign is a standalone statistical package with no external API integrations
- No remote data fetching, network calls, or cloud service dependencies
- All computation is local and self-contained

## Data Storage

**Databases:**
- None. Package works with in-memory R objects only.

**File Storage:**
- Local filesystem only
- Input: reads event data from R data frames, CSV files via user-provided code
- Output: generates design matrices as R matrices/tibbles, no automatic file saving
- Configuration: vignettes and example data in `inst/` and `vignettes/` directories

**Caching:**
- None. All computations are performed on-demand, no persistent caching layer

## Authentication & Identity

**Auth Provider:**
- Not applicable. Package has no authentication requirements.

## Monitoring & Observability

**Error Tracking:**
- Not configured. Errors are handled via `assertthat` assertions and standard R error mechanisms.
- Local error logging via `cli` package for progress reporting and validation messages

**Logs:**
- Console output only via `cli::cli_*()` functions
- Progress bars during long operations: `cli::cli_progress_bar()` family
- No file-based logging configured

## CI/CD & Deployment

**Hosting:**
- GitHub repository: https://github.com/bbuchsbaum/fmridesign
- Package website (pkgdown): https://bbuchsbaum.github.io/fmridesign/
- Deployed via GitHub Pages from `gh-pages` branch

**CI Pipeline:**
- GitHub Actions workflow: `.github/workflows/pkgdown.yaml`
  - Triggers: Push to main/master, pull requests, releases
  - Tasks: R dependency setup, pkgdown site building, deployment to GitHub Pages
  - Permissions: write access to repository contents

- R-hub validation: `.github/workflows/rhub.yaml`
  - Manual workflow dispatch for cross-platform R-hub testing
  - Platforms: linux, windows, macos (configurable)
  - Validates against multiple R versions and OS configurations

**Package Distribution:**
- CRAN-ready (uses GPL >= 2 license, follows CRAN standards)
- Installation via GitHub: `devtools::install_github("bbuchsbaum/fmridesign")`

## Environment Configuration

**Required env vars:**
- None. Package requires no environment variables for core functionality.

**Optional env vars:**
- `GITHUB_PAT` - Used by pkgdown CI workflow for GitHub API access (auto-set by Actions)
- `RHUB_TOKEN` - Used by R-hub validation workflow (optional for local testing)

**Secrets location:**
- GitHub Secrets (managed via repository settings)
- `GITHUB_TOKEN` - Automatically provided by GitHub Actions
- `RHUB_TOKEN` - Optional, configured per repository for R-hub validation

## Webhooks & Callbacks

**Incoming:**
- None. Package has no webhook endpoints.

## Dependencies on Ecosystem Packages

**Related fmri Analysis Packages (not bundled):**
- `fmrihrf` (imported) - Hemodynamic response functions
- `fmrireg` - Downstream package for fMRI analysis and model fitting
- `fmridataset` - fMRI dataset structures (mentioned in documentation)
- `albersdown` - pkgdown theme for documentation website

**R Ecosystem:**
- All dependencies are from CRAN or GitHub
- No system-level native dependencies required
- Pure R implementation, no C/Fortran bindings

## Data Interchange

**Input Formats:**
- R data frames
- CSV files (via user-provided read functions)
- Raw vectors/matrices from user code

**Output Formats:**
- R matrices (dense and sparse via `Matrix` package)
- tibbles (modern R data frames)
- S3 objects: `event_model`, `baseline_model`, `contrast`, etc.

## Website & Documentation Infrastructure

**Documentation Platform:**
- pkgdown (v2+) for R package documentation site generation
- Template: `albersdown` custom Bootstrap 5 theme
- CSS: `vignettes/albers.css` + local `vignettes/albers.js`
- Deployment: GitHub Pages via Actions workflow

**Documentation Content:**
- Roxygen2 comments in source code (auto-generated NAMESPACE and man files)
- Vignettes: Markdown/Rmd articles in `vignettes/` directory
- Reference documentation: Auto-generated from roxygen2 comments
- Articles: Curated list in `_pkgdown.yml` navbar

## Testing Infrastructure

**Test Data:**
- Fixture file: `tests/testthat/events_testdat.txt`
- Helper functions: `tests/testthat/helper-naming.R`
- Framework: testthat for unit testing
- Configuration: `tests/testthat.R` loads library and runs test_check()

## Build & Release Process

**Package Checking:**
- Standard CRAN check via `devtools::check()`
- Runs R CMD check with full test suite
- Validates across platforms via R-hub

**Vignette Building:**
- VignetteBuilder: knitr
- Source: `vignettes/*.Rmd` files
- Built into `doc/` directory during package build

**Source Code:**
- Collate order defined in DESCRIPTION for proper loading: baseline_model.R → baseline_model_helpers.R → ... → sampling_frame.R

---

*Integration audit: 2025-01-25*
