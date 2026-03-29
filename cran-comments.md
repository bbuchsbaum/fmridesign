## Resubmission

This is a resubmission addressing reviewer feedback from 2026-02-11.

Changes made:

* Added method references (Friston et al. 1995, Lindquist 2008) with
  `<doi:...>` links to the DESCRIPTION Description field.

* Exported functions that had examples but were not exported
  (`sanitize`, `basis_suffix`, `feature_suffix`, `split_by_block`).
  Removed examples from internal-only functions
  (`column_groups_by_condition`, `translate_legacy_pattern`).

* Removed all `\dontrun{}`. Three longer-running examples use
  `\donttest{}` instead (`register_hrfspec_extension`,
  `boxcar_hrf_gen`, `weighted_hrf_gen`).

* Replaced commented-out code in examples with runnable code
  (`event_model`, `hrf`, `split_by_block`).

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
