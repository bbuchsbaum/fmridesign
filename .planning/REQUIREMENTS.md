# CRAN Submission Requirements

**Version:** 0.5.0 -> 1.0.0
**Target:** CRAN initial submission

---

## Mandatory (CRAN Rejection Without)

### Documentation

| ID | Requirement | Validation |
|----|-------------|------------|
| REQ-DOC-01 | All exported functions have @return documentation | `checkhelper::check_missing_return()` |
| REQ-DOC-02 | All exported functions have complete @param documentation | `devtools::check()` |
| REQ-DOC-03 | All exported functions have runnable @examples | `devtools::run_examples()` |
| REQ-DOC-04 | \dontrun{} only used for truly non-runnable examples | Manual audit |
| REQ-DOC-05 | Spelling errors fixed | `spelling::spell_check_package()` |
| REQ-DOC-06 | All URLs valid and reachable | `urlchecker::url_check()` |

### Namespace

| ID | Requirement | Validation |
|----|-------------|------------|
| REQ-NS-01 | Only user-facing functions exported | Audit NAMESPACE |
| REQ-NS-02 | Internal helpers marked with @keywords internal | Manual review |
| REQ-NS-03 | No ::: calls to unexported functions | `grep ':::' R/*.R` |
| REQ-NS-04 | All S3 methods properly exported | `devtools::check()` |

### R CMD check

| ID | Requirement | Validation |
|----|-------------|------------|
| REQ-CHK-01 | 0 ERRORs on R CMD check --as-cran | `devtools::check()` |
| REQ-CHK-02 | 0 WARNINGs on R CMD check --as-cran | `devtools::check()` |
| REQ-CHK-03 | Minimal NOTEs (only "new submission" acceptable) | `devtools::check()` |
| REQ-CHK-04 | Passes on Windows (win-builder) | win-builder.r-project.org |
| REQ-CHK-05 | Passes on macOS (mac-builder) | mac.r-project.org/macbuilder |
| REQ-CHK-06 | Passes on R-devel | rhub v2 |

### DESCRIPTION

| ID | Requirement | Validation |
|----|-------------|------------|
| REQ-DESC-01 | Title in title case, no "package" word | Manual review |
| REQ-DESC-02 | Description complete sentences, proper formatting | Manual review |
| REQ-DESC-03 | Authors@R includes 'cph' (copyright holder) role | Check DESCRIPTION |
| REQ-DESC-04 | Valid maintainer email | Verify active |
| REQ-DESC-05 | All Imports available on CRAN | Check fmrihrf status |

### Package Size

| ID | Requirement | Validation |
|----|-------------|------------|
| REQ-SIZE-01 | Source tarball < 10 MB | `devtools::build()` + check size |

---

## Strongly Recommended

### Testing

| ID | Requirement | Validation |
|----|-------------|------------|
| REQ-TEST-01 | Test coverage assessed with covr | `covr::package_coverage()` |
| REQ-TEST-02 | Edge cases and error paths tested | Manual audit |
| REQ-TEST-03 | All tests pass consistently | `devtools::test()` |

### Quality

| ID | Requirement | Validation |
|----|-------------|------------|
| REQ-QUAL-01 | NEWS.md exists with changelog | Check file exists |
| REQ-QUAL-02 | cran-comments.md prepared for submission | Template ready |
| REQ-QUAL-03 | GitHub Actions R-CMD-check workflow active | .github/workflows/ |

---

## Nice to Have (Post-Submission)

| ID | Requirement | Validation |
|----|-------------|------------|
| REQ-OPT-01 | inst/CITATION file | Optional |
| REQ-OPT-02 | Code style with lintr/styler | `lintr::lint_package()` |
| REQ-OPT-03 | inst/WORDLIST for spelling exceptions | After spell check |

---

## Known Issues (fmridesign-specific)

From research:

1. **Package size:** 9.8 MB - near 10 MB limit (REQ-SIZE-01)
2. ~~**\dontrun{} usage:** 6 instances need review (REQ-DOC-04)~~ ✓ Fixed - only 2 justified remain
3. **Missing NEWS.md:** Needs creation (REQ-QUAL-01)
4. **Authors@R:** May need 'cph' role added (REQ-DESC-03)

---

## Traceability

| Requirement | Phase | Status |
|-------------|-------|--------|
| REQ-DOC-01 | Phase 1 | ✓ Complete |
| REQ-DOC-02 | Phase 1 | ✓ Complete |
| REQ-DOC-03 | Phase 1 | ✓ Complete |
| REQ-DOC-04 | Phase 1 | ✓ Complete |
| REQ-DOC-05 | Phase 1 | ✓ Complete |
| REQ-DOC-06 | Phase 1 | ✓ Complete |
| REQ-NS-01 | Phase 2 | Pending |
| REQ-NS-02 | Phase 2 | Pending |
| REQ-NS-03 | Phase 2 | Pending |
| REQ-NS-04 | Phase 2 | Pending |
| REQ-DESC-01 | Phase 2 | Pending |
| REQ-DESC-02 | Phase 2 | Pending |
| REQ-DESC-03 | Phase 2 | Pending |
| REQ-DESC-04 | Phase 2 | Pending |
| REQ-DESC-05 | Phase 2 | Pending |
| REQ-CHK-01 | Phase 3 | Pending |
| REQ-CHK-02 | Phase 3 | Pending |
| REQ-CHK-03 | Phase 3 | Pending |
| REQ-SIZE-01 | Phase 3 | Pending |
| REQ-CHK-04 | Phase 4 | Pending |
| REQ-CHK-05 | Phase 4 | Pending |
| REQ-CHK-06 | Phase 4 | Pending |
| REQ-TEST-01 | Phase 4 | Pending |
| REQ-TEST-02 | Phase 4 | Pending |
| REQ-TEST-03 | Phase 4 | Pending |
| REQ-QUAL-01 | Phase 5 | Pending |
| REQ-QUAL-02 | Phase 5 | Pending |
| REQ-QUAL-03 | Phase 5 | Pending |

---

*Generated: 2025-01-25 from research synthesis*
*Traceability added: 2025-01-25*
