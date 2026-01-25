---
phase: 01-documentation-quality
plan: 01
subsystem: documentation
tags: [roxygen2, CRAN, spell-check, examples, documentation-quality]

requires: []
provides: [complete-documentation, spell-check-wordlist, runnable-examples]
affects: [01-02]

tech-stack:
  added: []
  patterns: []

key-files:
  created:
    - inst/WORDLIST
  modified:
    - R/residualize.R
    - R/validate.R
    - R/hrf-formula.R
    - man/residualize-methods.Rd
    - man/validate_contrasts.Rd
    - man/check_collinearity.Rd
    - man/boxcar_hrf_gen.Rd
    - man/weighted_hrf_gen.Rd

decisions:
  - id: DOC-001
    title: "WORDLIST includes Greek letters used in vignettes"
    context: "Vignettes use β and ε symbols in mathematical notation"
    decision: "Add Greek letters to WORDLIST rather than changing vignette content"
    rationale: "Mathematical notation is more readable with actual symbols than escaped versions"
    date: 2026-01-25
  - id: DOC-002
    title: "Keep \\dontrun{} for internal/external-dependent functions"
    context: "Some functions cannot run without external package context or are internal utilities"
    decision: "Retain \\dontrun{} only for register_hrfspec_extension and translate_legacy_pattern"
    rationale: "register_hrfspec_extension requires external package, translate_legacy_pattern is @keywords internal"
    date: 2026-01-25

metrics:
  duration: "5 minutes"
  tasks: 5
  commits: 3
  files_modified: 9
  completed: 2026-01-25
---

# Phase 01 Plan 01: Documentation Quality Summary

**One-liner:** Fixed all CRAN documentation requirements - spell check, complete @param/@return docs, and runnable examples

## What Was Built

This plan resolved all documentation quality issues identified in the research phase to prepare for CRAN submission:

1. **Spelling whitelist (inst/WORDLIST)**: Created comprehensive technical term whitelist with 70 terms including fMRI domain vocabulary (HRF, SPMG, fmrihrf), R ecosystem terms (tibble, ggplot, pkgdown), and Greek letters (β, ε) used in mathematical notation

2. **Complete method documentation (R/residualize.R)**: Added full roxygen2 documentation to residualize S3 methods including @param for all parameters, @return specification, and two runnable examples demonstrating usage with matrices and event_model objects

3. **Runnable examples**: Converted 4 \dontrun{} blocks to fully executable examples by adding necessary setup code (sampling_frame creation, event_model construction) for:
   - validate_contrasts()
   - check_collinearity()
   - boxcar_hrf_gen()
   - weighted_hrf_gen()

## Verification Results

All CRAN documentation checks pass:

```
✓ spelling::spell_check_package() - 0 errors
✓ urlchecker::url_check() - All URLs valid
✓ tools::checkDocFiles() - No missing documentation
✓ devtools::run_examples() - All examples run successfully
✓ Only 2 \dontrun{} remain (register_hrfspec_extension, translate_legacy_pattern) - both justified
```

## Tasks Completed

| Task | Name | Commit | Files |
|------|------|--------|-------|
| 1 | Create inst/WORDLIST for technical terms | b12f9d0 | inst/WORDLIST |
| 2 | Add @return and @examples to residualize methods | 576c6ab | R/residualize.R, man/residualize-methods.Rd |
| 3 | Convert \dontrun{} to runnable examples | 659c4a2 | R/validate.R, R/hrf-formula.R, man/*.Rd (4 files) |
| 4 | Verify @param documentation completeness | (verification only) | - |
| 5 | Verify URLs in documentation | (verification only) | - |

## Technical Details

### WORDLIST Coverage

The whitelist includes:
- **fMRI domain**: AFNI, HRF, HRFs, SPMG, fmrihrf, hemodynamic, convolve, etc.
- **R ecosystem**: tibble, ggplot, pkgdown, dplyr concepts
- **Package-specific**: hrfspec, baselinespec, covariatespec, trialwise, etc.
- **Mathematical**: β (beta), ε (epsilon) for regression notation

### Example Pattern

All converted examples follow this pattern:
```r
# 1. Create minimal event data (onset, run, condition)
# 2. Create sampling_frame with fmrihrf::sampling_frame()
# 3. Create event_model with onset ~ hrf()
# 4. Call the function being demonstrated
```

This pattern ensures examples are:
- Self-contained (no external dependencies)
- Fast (small datasets, few scans)
- Illustrative (show realistic usage)

### Remaining \dontrun{} Justification

- **register_hrfspec_extension**: Requires external package to extend, cannot run in isolation
- **translate_legacy_pattern**: Internal function (@keywords internal), example is illustrative only

## Deviations from Plan

None - plan executed exactly as written.

## Impact

### Immediate

- ✓ Package passes all CRAN documentation checks
- ✓ All exported functions have complete, verified documentation
- ✓ Users can run examples to understand function usage
- ✓ spell_check_package() runs cleanly in CI/CD

### Future

- **CRAN submission readiness**: Documentation requirements fully satisfied (REQ-DOC-01 through REQ-DOC-04)
- **Maintainability**: WORDLIST prevents false-positive spelling errors in future development
- **User experience**: Runnable examples improve discoverability and understanding

## Next Phase Readiness

### Blockers

None.

### Concerns

None.

### Prerequisites for Next Plan

The next plan (01-02) will likely address remaining CRAN requirements such as:
- Package size optimization (currently 9.8 MB, near 10 MB limit)
- NEWS.md file creation
- DESCRIPTION metadata refinement

All documentation is now in order and will not block subsequent work.

## Key Learnings

1. **Greek letters in WORDLIST**: The spelling package accepts Unicode characters in WORDLIST, making it straightforward to whitelist mathematical notation symbols

2. **@rdname for S3 method docs**: Using @rdname to link S3 method implementations to a central documentation block is cleaner than duplicating @param docs across methods

3. **Minimal example pattern**: The fmrihrf::sampling_frame() + event_model() pattern provides a reusable template for creating self-contained examples throughout the package

4. **tools::checkDocFiles() completeness**: This function catches undocumented parameters that might slip past manual review - valuable for pre-CRAN validation

## Files Modified

### Created
- `inst/WORDLIST` - Technical term whitelist for spell checker (70 terms)

### Modified
- `R/residualize.R` - Added complete documentation block with @param, @return, @examples, and @rdname links
- `R/validate.R` - Converted \dontrun{} to runnable examples for validate_contrasts() and check_collinearity()
- `R/hrf-formula.R` - Converted \dontrun{} to runnable examples for boxcar_hrf_gen() and weighted_hrf_gen()
- `man/*.Rd` - Regenerated documentation files (5 files updated)

## Success Criteria Met

All 11 success criteria from the plan are satisfied:

1. ✓ inst/WORDLIST exists with 70 technical terms
2. ✓ urlchecker::url_check() returns no broken URLs
3. ✓ spelling::spell_check_package() returns zero errors
4. ✓ Every exported function has complete @param documentation (no warnings)
5. ✓ residualize-methods.Rd has @return and @examples sections
6. ✓ validate_contrasts() example runs without error
7. ✓ check_collinearity() example runs without error
8. ✓ boxcar_hrf_gen() example runs without error
9. ✓ weighted_hrf_gen() example runs without error
10. ✓ Only 2 \dontrun{} remain (register_hrfspec_extension, translate_legacy_pattern)
11. ✓ devtools::run_examples() completes successfully

## Commits

- b12f9d0: docs(01-01): create inst/WORDLIST for technical terms
- 576c6ab: docs(01-01): add @return and @examples to residualize methods
- 659c4a2: docs(01-01): convert \dontrun{} to runnable examples
