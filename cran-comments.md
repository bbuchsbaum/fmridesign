## Release preparation

This candidate has not been submitted. The required 'fmrihrf' 0.5.0 source
was published on CRAN on 2026-11-11 at 16:50:02 UTC:
<https://cran.r-project.org/package=fmrihrf>.
The post-publication checks use a fresh library installed only from CRAN,
including fmrihrf 0.5.0 installed from source. The published source archive
has SHA-256:
`3a65c79fc2e0c91697f1dbbc9a6b12d07ec6dfd19677226547eb9567aa2f9e10`.
Cross-platform CI and current builder checks remain to be run before
submission; update these notes with their results.

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
* Contrast tests use their existing self-contained synthetic fixture instead
  of silently loading an undeclared local fmrireg installation and its methods.

## R CMD check results

Post-publication check against the fresh CRAN library:

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
* Published CRAN fmrihrf 0.5.0, installed from source in a fresh library
* 96 packages freshly installed from CRAN; every non-base dependency namespace
  loaded during the full test suite was verified to come from that library

## Additional checks

* Full test suite: 3,641 passing assertions, no failures. All four event-duration
  tests ran. One existing test is skipped because AFNI export moved out of
  fmridesign.
* Installation with the older local fmrihrf 0.4.0.9000 was rejected, confirming
  that the 0.5.0 dependency floor is enforced.
* Package coverage: 92.98%, above the repository's 90% threshold.
* Local pkgdown site build completed during advance preparation; no deployment
  was made.

## Reverse dependencies

There are no reverse dependencies on CRAN (new package).
