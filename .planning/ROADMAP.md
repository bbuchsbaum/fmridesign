# Roadmap: fmridesign CRAN Submission

## Overview

Prepare fmridesign v0.5.0 for CRAN submission as v1.0.0. The package functionality is complete and R CMD check passes locally. This milestone focuses on documentation completeness, namespace hygiene, multi-platform validation, and submission mechanics. Five phases progress from quick documentation fixes through platform testing to final submission.

## Milestone 1: CRAN Submission v1.0.0

## Phases

**Phase Numbering:**
- Integer phases (1, 2, 3): Planned milestone work
- Decimal phases (2.1, 2.2): Urgent insertions (marked with INSERTED)

- [x] **Phase 1: Documentation Quality** - Fix spelling, URLs, @return tags, and examples ✓
- [ ] **Phase 2: Namespace & Metadata** - Audit exports, internals, and DESCRIPTION compliance
- [ ] **Phase 3: Local Validation** - Achieve clean R CMD check --as-cran locally
- [ ] **Phase 4: Platform Testing** - Validate on Windows, macOS, and R-devel
- [ ] **Phase 5: Submission** - Prepare NEWS.md, cran-comments.md, and submit

## Phase Details

### Phase 1: Documentation Quality
**Goal**: All exported functions have complete, error-free documentation
**Depends on**: Nothing (first phase)
**Requirements**: REQ-DOC-01, REQ-DOC-02, REQ-DOC-03, REQ-DOC-04, REQ-DOC-05, REQ-DOC-06
**Success Criteria** (what must be TRUE):
  1. `spelling::spell_check_package()` returns zero errors (or all in WORDLIST)
  2. `urlchecker::url_check()` returns no broken or redirected URLs
  3. Every exported function has @return documentation (checkhelper passes)
  4. Every exported function has complete @param documentation (no warnings)
  5. All @examples run without error via `devtools::run_examples()`
  6. \dontrun{} used only for examples requiring external resources or user interaction
**Plans**: 1 plan

Plans:
- [x] 01-01-PLAN.md - Fix spelling (WORDLIST), @return/@examples for residualize, convert \dontrun{} to runnable ✓

### Phase 2: Namespace & Metadata
**Goal**: Only user-facing functions exported, DESCRIPTION fully CRAN-compliant
**Depends on**: Phase 1
**Requirements**: REQ-NS-01, REQ-NS-02, REQ-NS-03, REQ-NS-04, REQ-DESC-01, REQ-DESC-02, REQ-DESC-03, REQ-DESC-04, REQ-DESC-05
**Success Criteria** (what must be TRUE):
  1. NAMESPACE contains only user-facing functions (no internal helpers exported)
  2. All internal helpers have @keywords internal in roxygen
  3. No ::: calls to unexported functions in package code
  4. All S3 methods properly exported (S3method() in NAMESPACE)
  5. DESCRIPTION Title is title case without "package" word
  6. DESCRIPTION Description is complete sentences with proper punctuation
  7. Authors@R includes 'cph' (copyright holder) role
  8. Maintainer email is valid and active
**Plans**: 1 plan

Plans:
- [ ] 02-01-PLAN.md - Unexport internal utilities, add 'cph' role, verify NAMESPACE compliance

### Phase 3: Local Validation
**Goal**: R CMD check --as-cran passes with 0 errors, 0 warnings, minimal notes
**Depends on**: Phase 2
**Requirements**: REQ-CHK-01, REQ-CHK-02, REQ-CHK-03, REQ-SIZE-01
**Success Criteria** (what must be TRUE):
  1. `devtools::check(args = c('--as-cran'))` returns 0 ERRORs
  2. `devtools::check(args = c('--as-cran'))` returns 0 WARNINGs
  3. Only "new submission" NOTE acceptable (all others fixed)
  4. Source tarball from `devtools::build()` is under 10 MB
**Plans**: TBD

Plans:
- [ ] 03-01: Run R CMD check --as-cran and fix issues

### Phase 4: Platform Testing
**Goal**: Package passes checks on Windows, macOS, and R-devel
**Depends on**: Phase 3
**Requirements**: REQ-CHK-04, REQ-CHK-05, REQ-CHK-06, REQ-TEST-01, REQ-TEST-02, REQ-TEST-03
**Success Criteria** (what must be TRUE):
  1. win-builder (R-release and R-devel) returns 0 errors, 0 warnings
  2. mac-builder returns 0 errors, 0 warnings
  3. rhub v2 R-devel check passes
  4. `covr::package_coverage()` runs and reports coverage percentage
  5. Test coverage assessed for edge cases (documented gaps acceptable)
  6. All tests pass consistently via `devtools::test()`
**Plans**: TBD

Plans:
- [ ] 04-01: Submit to win-builder and mac-builder
- [ ] 04-02: Run rhub v2 and assess coverage

### Phase 5: Submission
**Goal**: Package submitted to CRAN with complete submission materials
**Depends on**: Phase 4
**Requirements**: REQ-QUAL-01, REQ-QUAL-02, REQ-QUAL-03
**Success Criteria** (what must be TRUE):
  1. NEWS.md exists with changelog for v1.0.0
  2. cran-comments.md prepared with test results and submission notes
  3. GitHub Actions R-CMD-check workflow active and passing
  4. Package submitted via `devtools::release()` or web form
**Plans**: TBD

Plans:
- [ ] 05-01: Prepare submission materials and submit

## Progress

**Execution Order:**
Phases execute in numeric order: 1 -> 2 -> 3 -> 4 -> 5

| Phase | Plans Complete | Status | Completed |
|-------|----------------|--------|-----------|
| 1. Documentation Quality | 1/1 | ✓ Complete | 2026-01-25 |
| 2. Namespace & Metadata | 0/1 | Planned | - |
| 3. Local Validation | 0/1 | Not started | - |
| 4. Platform Testing | 0/2 | Not started | - |
| 5. Submission | 0/1 | Not started | - |
