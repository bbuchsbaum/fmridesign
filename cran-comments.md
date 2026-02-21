## Resubmission

This is a resubmission addressing reviewer feedback from 2026-02-11.

Changes made:

* Added method references (Friston et al. 1995, Lindquist 2008) with
  `<doi:...>` links to the DESCRIPTION Description field.

* Removed `\dontrun{}` from all exported function examples (`sanitize`,
  `basis_suffix`, `feature_suffix`, `split_by_block`). Examples now run
  directly. `register_hrfspec_extension` uses `\donttest{}`.
  The two remaining `\dontrun{}` are on internal (`@keywords internal`)
  functions only.

* Replaced commented-out code in examples with runnable code
  (`event_model`, `split_by_block`).

* Vignettes now save and restore `options()` via
  `old_opts <- options(...)` / `options(old_opts)`.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

* NOTE: Possibly misspelled words: HRF, hemodynamic, trialwise
  - These are standard terms in the functional MRI analysis domain.

## Test environments

* local macOS (aarch64-apple-darwin20), R 4.5.1
* win-builder R-release (4.5.2)
* win-builder R-devel (2026-02-08 r89382)
