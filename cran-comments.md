## Release preparation

This candidate has not been submitted. It requires 'fmrihrf' (>= 0.5.0),
which has been submitted to CRAN and is awaiting publication. Advance checks
use that exact submitted source archive (SHA-256:
`de38381a4a744cf25e19310a0ed06ce858447a1e2dde93e06ad1fa2b45178d8f`).
Before submitting fmridesign, repeat checks with dependencies installed
cleanly from CRAN once fmrihrf 0.5.0 is available, and update these notes.

## Resubmission

This is prepared as a resubmission of fmridesign, previously submitted as
versions 0.5.0 and 0.6.0. Version 0.7.0 keeps the changes requested in earlier
reviews:

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
* The `Remotes` field is removed. The minimum 'fmrihrf' version is 0.5.0,
  which provides the event-duration API used by `feature()`. The four
  duration integration/validation tests no longer skip for older versions.

## R CMD check results

0 errors | 0 warnings | 3 notes

* New submission.
* The local environment could not verify the current time.
* HTML manual validation was skipped because macOS supplies an obsolete
  HTML Tidy. The PDF manual check passed.

Examples, including `--run-donttest`, tests, vignette rebuilding, and the
PDF manual passed. A first check also found the Git worktree pointer file
in the source archive; `.Rbuildignore` now excludes it and the final check
confirms that the package has no hidden-file note.

## Test environment

* macOS 27.2 (aarch64-apple-darwin20), R 4.5.1, `R CMD check --as-cran`
* Exact submitted fmrihrf 0.5.0 source archive, installed in an isolated library

## Additional checks

* Full test suite: 3,641 passing assertions, no failures. All four event-duration
  tests ran. One existing test is skipped because AFNI export moved out of
  fmridesign.
* Installation with the older local fmrihrf 0.4.0.9000 was rejected, confirming
  that the 0.5.0 dependency floor is enforced.
* Package coverage: 92.98%, above the repository's 90% threshold.
* Local pkgdown site build completed; no deployment was made.

## Reverse dependencies

There are no reverse dependencies on CRAN (new package).
