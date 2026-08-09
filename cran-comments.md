## Resubmission

This is a resubmission of fmridesign, previously submitted as version 0.5.0.
Version 0.6.0 retains the changes requested during the previous review and adds
construction-time validation for nuisance regressors, consistent interaction
contrast keys, warnings for degenerate parametric modulators, and compatibility
handling for spurious HRF-metadata warnings from `fmrihrf` 0.3.0.

The previous reviewer-requested changes remain in place:

* Method references are included in the DESCRIPTION with DOI links.
* Exported functions have runnable examples; internal-only functions do not
  expose examples as user-facing API.
* There are no `\dontrun{}` examples. Longer examples use `\donttest{}`.
* Vignettes restore any session options they change.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

The same source tarball produced this result under both R 4.6.1 and R-devel.

## Test environments

* Ubuntu 24.04.4 LTS (aarch64-unknown-linux-gnu), R 4.6.1
* Ubuntu 24.04.4 LTS (aarch64-unknown-linux-gnu), R-devel
  (2026-06-21 r90185)
* local macOS Sonoma 14.3 (aarch64-apple-darwin20), R 4.5.1

All checks used the CRAN release of `fmrihrf` (0.3.0). The macOS check produced
one additional local-tooling note because its installed HTML Tidy is too old
for HTML-manual validation; that validation passed on both Ubuntu checks.

The source tarball is 1.9 MB and its installed `doc` directory is 2.5 MB.
