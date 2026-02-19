## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

* NOTE: "Suggests or Enhances not in mainstream repositories: albersdown"
  - Addressed by removing `albersdown` from `Suggests`.
  - `albersdown` is retained only for pkgdown website styling
    (`Config/Needs/website`) and is not required for package checks.
  - Vignette setup no longer calls `albersdown`; a standard `ggplot2` theme
    is used instead.

* NOTE: Possibly misspelled words: HRF, hemodynamic, trialwise
  - These are standard terms in the functional MRI analysis domain.

## Test environments

* local macOS (aarch64-apple-darwin20), R 4.5.1
* win-builder R-release (4.5.2)
* win-builder R-devel (2026-02-08 r89382)
