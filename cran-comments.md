## Resubmission

This is a resubmission of fmridesign, previously submitted as versions 0.5.0
and 0.6.0. Version 0.7.0 keeps the changes requested in earlier reviews:

* Method references are given in the DESCRIPTION with DOI links.
* Exported functions have runnable examples; internal-only functions do not
  expose examples as user-facing API.
* There are no `\dontrun{}` examples. Longer examples use `\donttest{}`.
* Vignettes restore any session options they change.

Changes since 0.6.0 that concern CRAN policy:

* The package no longer uses any unexported function of 'fmrihrf': design
  columns are evaluated through the public `fmrihrf::regressor()` /
  `fmrihrf::evaluate()` API, and HRF generators are called directly rather
  than looked up in the 'fmrihrf' namespace.
* fmridesign no longer registers a `print()` method for 'fmrihrf''s
  `sampling_frame` class, so loading it no longer overwrites a method of
  another package. Generics shared with 'fmrihrf' (`onsets()`, `durations()`,
  `blockids()`, `nbasis()`) are re-exported rather than redefined, so
  attaching fmridesign does not mask them.
* The `Remotes` field is removed; the package is built and checked against the
  CRAN release of 'fmrihrf' (0.4.0).

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* Ubuntu 24.04.5 LTS (x86_64-pc-linux-gnu), R 4.6.2, `--as-cran`

All checks used the CRAN release of 'fmrihrf' (0.4.0).

## Reverse dependencies

There are no reverse dependencies on CRAN (new package).
