# CRAN Submission Requirements: Feature Landscape

**Domain:** CRAN R package submission
**Package:** fmridesign (brownfield - preparing for initial CRAN submission)
**Researched:** 2026-01-25
**Confidence:** HIGH (based on official CRAN documentation and policies)

## Overview

This document categorizes CRAN submission requirements into three tiers: Mandatory (rejection without these), Strongly Recommended (likely issues if missing), and Nice to Have (improves package quality). This research is specific to **initial CRAN submissions**, which face more scrutiny than updates.

---

## Mandatory Requirements

These features MUST be present or your package WILL be rejected by CRAN.

### 1. R CMD check: Clean on All Platforms

| Requirement | Details | Validation |
|-------------|---------|------------|
| **No ERRORs** | Package must pass `R CMD check --as-cran` with zero errors | `devtools::check()` |
| **No WARNINGs** | Package must pass with zero warnings | `devtools::check()` |
| **Minimal NOTEs** | Eliminate all NOTEs except unavoidable ones (new submission NOTE, non-ASCII data with proper encoding) | `devtools::check()` |
| **Platform coverage** | Must work on at least 2 major platforms (Windows, macOS, Linux) | Win-builder, R-hub, GitHub Actions |
| **R version compatibility** | Test with current release, R-patched, and R-devel | `rhub::check_for_cran()` |

**Why mandatory:** CRAN will only accept packages that pass R CMD check --as-cran on their machines with no warnings, errors or significant notes.

**Sources:**
- [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html)
- [CRAN Submission Checklist](https://cran.r-project.org/web/packages/submission_checklist.html)
- [R CMD check requirements](https://r-pkgs.org/R-CMD-check.html)

---

### 2. DESCRIPTION File: Complete and Correct

| Field | Requirement | Format |
|-------|-------------|--------|
| **Package** | Valid package name (letters, numbers, periods only; start with letter) | `fmridesign` |
| **Version** | Semantic versioning (e.g., 0.5.0) | `x.y.z` |
| **Title** | Informative title in title case; no redundant "R package" or period at end | < 65 characters |
| **Description** | Detailed, informative description for potential users; function names with parentheses `foo()`; external software/packages in single quotes | Multiple sentences |
| **Authors@R** | Complete author information with roles (at least one 'cre' and 'cph') | `person()` format |
| **Maintainer email** | Valid, monitored email address; person, not mailing list; no filtering | Active email |
| **License** | Valid CRAN license (GPL-2+, MIT, etc.) | Standard format |
| **Imports** | All packages used in code that aren't base R | Alphabetical |
| **Suggests** | Packages for tests, vignettes, examples | Alphabetical |

**Why mandatory:** Incomplete or improperly formatted DESCRIPTION will cause immediate rejection.

**Common issues for fmridesign:**
- Current Description is good but ensure it remains < 5 MB when built
- Authors@R needs copyright holder ('cph' role) if not already present
- GPL (>= 2) is acceptable; no LICENSE file needed for standard licenses

**Sources:**
- [DESCRIPTION File Issues - CRAN Cookbook](https://contributor.r-project.org/cran-cookbook/description_issues.html)
- [R Packages: DESCRIPTION](https://r-pkgs.org/description.html)

---

### 3. Documentation: Complete for All Exported Functions

| Requirement | Details | Tag |
|-------------|---------|-----|
| **All exports documented** | Every exported function MUST have complete documentation | `#' @export` requires docs |
| **@param for all arguments** | Every function parameter documented | `@param arg_name Description` |
| **@return/@returns** | Every function MUST document return value, even if NULL/invisible | `@return Description of output` |
| **@examples** | Every exported function MUST have runnable examples | `@examples` |
| **Example execution** | Examples must run without error in < 5 seconds per .Rd file | `\dontrun{}` sparingly |
| **@title** | Clear, concise title | Auto-generated from first line OK |
| **@description** | Informative description | Usually first paragraph |

**Why mandatory:** CRAN requires all exported functions to have documented return values and examples for initial submissions. Missing documentation causes rejection.

**Known gaps for fmridesign:**
- "incomplete @examples" - HIGH PRIORITY
- "some @param/@return missing" - HIGH PRIORITY
- All must be fixed before submission

**Example guidelines:**
- Use `\dontrun{}` ONLY when example truly cannot execute (missing API keys, external software)
- Use `\donttest{}` for slow examples (> 5 seconds) that work but shouldn't run on CRAN
- Prefer `if (interactive())` for interactive-only code
- Unwrap examples if they execute in < 5 seconds

**Sources:**
- [Documenting functions - roxygen2](https://roxygen2.r-lib.org/articles/rd.html)
- [Function documentation - R Packages](https://r-pkgs.org/man.html)
- [Code examples policy - R-hub blog](https://blog.r-hub.io/2020/01/27/examples/)

---

### 4. Namespace: Proper Exports and Imports

| Requirement | Details | Roxygen Tag |
|-------------|---------|-------------|
| **Export only user-facing functions** | Internal/helper functions should NOT be exported | `@noRd` or `@keywords internal` |
| **Export all S3 methods** | All S3 methods must be exported/registered, even for internal generics | `@export` on methods |
| **Import used functions** | Explicitly import all non-base functions used | `@importFrom pkg fun` |
| **No ::: in code** | Don't call unexported functions from other packages; not allowed by CRAN | Remove all `:::` |
| **Clean NAMESPACE** | Auto-generated by roxygen2; don't hand-edit | `devtools::document()` |

**Why mandatory:** Improper namespace management causes check failures and is grounds for rejection.

**Known gaps for fmridesign:**
- "internal functions may be exported" - MUST AUDIT
- Review all `@export` tags; remove from internal helpers
- Use `@keywords internal` for documented internal functions

**Sources:**
- [Managing imports and exports - roxygen2](https://roxygen2.r-lib.org/articles/namespace.html)
- [Internal functions in R packages - R-hub](https://blog.r-hub.io/2019/12/12/internal-functions/)

---

### 5. Package Size and Content Limits

| Limit | Maximum | Mitigation |
|-------|---------|------------|
| **Source tarball** | 10 MB (if possible) | Compress data, minimize vignettes |
| **Data** | 5 MB total | Use `tools::resaveRdaFiles()`; consider external data package |
| **Documentation** | 5 MB total | Minimize examples output, compress images |
| **Individual vignettes** | Reasonable size | Use articles for large graphics-heavy docs |

**Why mandatory:** Exceeding size limits causes rejection. CRAN infrastructure has constraints.

**For fmridesign:**
- LazyData: false is appropriate (package has no data)
- Vignettes likely OK given current structure
- Monitor built package size with `devtools::build()`

**Sources:**
- [Size and Limitations - Coatless Professor](https://blog.thecoatlessprofessor.com/programming/r/size-and-limitations-of-packages-on-cran/)
- [CRAN Repository Policy - Size](https://cran.r-project.org/web/packages/policies.html)

---

### 6. Dependencies: Available and Appropriate

| Requirement | Details |
|-------------|---------|
| **Imports from standard repos** | All Imports/Depends must be on CRAN or Bioconductor | Check `fmrihrf` is on CRAN |
| **Suggests optional** | Suggests packages not required to install but should be available | OK if not on CRAN |
| **Version constraints** | Only specify minimum versions when necessary; test compatibility | Use `>= x.y.z` sparingly |
| **No circular dependencies** | Package A cannot depend on B if B depends on A | Check with `tools::package_dependencies()` |

**Why mandatory:** Packages depending on unavailable or non-standard repositories will be rejected.

**Critical for fmridesign:**
- **VERIFY `fmrihrf` (>= 0.1.0) IS ON CRAN** - if not, this is a blocker
- If `fmrihrf` is not on CRAN, must either:
  1. Submit `fmrihrf` to CRAN first, OR
  2. Vendor the required code, OR
  3. Defer CRAN submission until dependency is available

**Sources:**
- [Dependencies in Practice - R Packages](https://r-pkgs.org/dependencies-in-practice.html)
- [CRAN Repository Policy - Dependencies](https://cran.r-project.org/web/packages/policies.html)

---

### 7. Code Quality Standards

| Requirement | Details | Detection |
|-------------|---------|-----------|
| **No user home writes** | Must not write to user directories (home, package dir, getwd()) without permission | R CMD check NOTE |
| **No warn option tampering** | Cannot set `options(warn = -1)` or negative warn values | R CMD check ERROR |
| **Max 2 cores** | Examples and tests can use max 2 CPU cores | `parallel::detectCores()` check |
| **No stack checking disable** | Cannot disable R's stack-checking mechanism | CRAN policy violation |
| **Graceful internet failures** | Network-dependent code must fail gracefully with informative messages | Check with offline tests |

**Why mandatory:** These are explicit CRAN policy violations that cause rejection.

**For fmridesign:**
- Review for any file I/O in examples
- Check if any parallel code exists (likely not relevant)
- Ensure no network dependencies in core functionality

**Sources:**
- [Code Issues - CRAN Cookbook](https://contributor.r-project.org/cran-cookbook/code_issues.html)
- [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html)

---

## Strongly Recommended Requirements

These features are not absolute requirements but their absence will likely cause issues, delays, or requests for changes.

### 8. Comprehensive Testing

| Feature | Recommendation | Tool |
|---------|----------------|------|
| **Unit tests** | Cover core functionality; aim for > 80% coverage | `testthat`, `covr` |
| **Example testing** | All examples should run and demonstrate functionality | `devtools::run_examples()` |
| **Edge cases** | Test error conditions, boundary cases | `testthat::expect_error()` |
| **Platform testing** | Test on Windows, macOS, Linux before submission | GitHub Actions, R-hub |

**Why strongly recommended:** While not strictly mandatory, CRAN reviewers often request more tests if coverage is sparse or if they discover bugs during review.

**For fmridesign:**
- testthat is in Suggests - good
- Should run `covr::package_coverage()` to check current coverage
- Add tests for any uncovered exported functions

**Sources:**
- [Testing - R Packages](https://r-pkgs.org/testing-basics.html)
- [R CMD check](https://r-pkgs.org/R-CMD-check.html)

---

### 9. Vignettes and Long-Form Documentation

| Feature | Recommendation | Tool |
|---------|----------------|------|
| **Getting started vignette** | At least one vignette showing package workflow | `usethis::use_vignette()` |
| **Build successfully** | Vignettes must build without error | `devtools::build_vignettes()` |
| **Reasonable size** | Keep vignette output small; use articles for heavy content | `usethis::use_article()` |
| **Informative content** | Show real-world usage, not just function listings | User perspective |

**Why strongly recommended:** Vignettes significantly improve package usability. While not mandatory, CRAN reviewers may question packages without any vignettes, especially for complex functionality.

**For fmridesign:**
- knitr, rmarkdown in Suggests - infrastructure ready
- Complex functionality (event models, HRF integration) benefits from vignettes
- Check existing vignettes build cleanly

**Sources:**
- [Vignettes - R Packages](https://r-pkgs.org/vignettes.html)
- [R Markdown: Package Vignettes](https://bookdown.org/yihui/rmarkdown/r-package-vignette.html)

---

### 10. NEWS and Version Documentation

| Feature | Recommendation |
|---------|----------------|
| **NEWS.md** | Document changes between versions |
| **Version increments** | Follow semantic versioning; increment for each submission |
| **Deprecation notices** | Warn users before removing functionality |

**Why strongly recommended:** Helps users understand package evolution. CRAN reviewers appreciate clear version documentation.

**For fmridesign:**
- Create NEWS.md with `usethis::use_news_md()`
- Document changes in 0.5.0 and plan for 0.5.1 (or 1.0.0 if ready)

**Sources:**
- [NEWS and versioning - R Packages](https://r-pkgs.org/other-markdown.html#sec-news)

---

### 11. CRAN Comments File

| Feature | Recommendation |
|---------|----------------|
| **cran-comments.md** | Document submission, testing, check results |
| **Address NOTES** | Explain unavoidable NOTEs (new submission, etc.) |
| **Testing evidence** | List platforms tested on |

**Why strongly recommended:** Makes CRAN reviewers' jobs easier. Shows diligence.

**For fmridesign:**
- Create with `usethis::use_cran_comments()`
- Template:
  ```markdown
  ## Test environments
  * local: macOS, R 4.x.x
  * win-builder: R-devel, R-release
  * R-hub: ubuntu, windows, macos

  ## R CMD check results
  0 errors | 0 warnings | 1 note

  * This is a new release.
  ```

**Sources:**
- [CRAN comments - usethis](https://usethis.r-lib.org/reference/use_cran_comments.html)
- [Releasing to CRAN - R Packages](https://r-pkgs.org/release.html)

---

### 12. Spell Check

| Feature | Recommendation | Tool |
|---------|----------------|------|
| **Documentation spelling** | No typos in DESCRIPTION, Rd files, vignettes | `devtools::spell_check()` |
| **WORDLIST** | Maintain custom dictionary for technical terms | `inst/WORDLIST` |

**Why strongly recommended:** CRAN reviewers often flag spelling errors. Shows attention to detail.

**For fmridesign:**
- Run `spelling::spell_check_package()`
- Add domain terms (fMRI, HRF, etc.) to WORDLIST

**Sources:**
- [Spell checking - devtools](https://devtools.r-lib.org/reference/spell_check.html)

---

## Nice to Have

These features improve package quality but are not required for CRAN acceptance.

### 13. Package Website with pkgdown

| Feature | Benefit |
|---------|---------|
| **pkgdown site** | Professional documentation website |
| **GitHub Pages hosting** | Free hosting for package docs |
| **Search functionality** | Easier to find functions |
| **Articles** | Supplementary documentation without CRAN size limits |

**For fmridesign:**
- URL field suggests pkgdown already configured
- Config/Needs/website field indicates custom theme
- This is already in place - excellent

**Sources:**
- [pkgdown](https://pkgdown.r-lib.org/)

---

### 14. Continuous Integration

| Feature | Benefit |
|---------|---------|
| **GitHub Actions** | Automated R CMD check on multiple platforms |
| **Test coverage tracking** | Monitor code coverage over time |
| **Automatic pkgdown deployment** | Auto-update website on push |

**For fmridesign:**
- Set up with `usethis::use_github_action_check_standard()`
- Add coverage badge with `usethis::use_github_action("test-coverage")`

**Sources:**
- [GitHub Actions for R](https://github.com/r-lib/actions)

---

### 15. Additional Documentation Polish

| Feature | Benefit |
|---------|---------|
| **README badges** | Show build status, coverage, CRAN version |
| **CODE_OF_CONDUCT** | Community guidelines |
| **CONTRIBUTING** | Guide for contributors |
| **Detailed README** | Installation, quick start, examples |

**For fmridesign:**
- README likely exists (check completeness)
- Add badges for professional appearance
- Consider contributing guidelines if open to PRs

**Sources:**
- [Other markdown files - R Packages](https://r-pkgs.org/other-markdown.html)

---

## CRAN Submission Checklist: Priority Order

Based on known gaps for fmridesign, here's the recommended work order:

### Phase 1: Blockers (Must Fix Before Submission)

1. **Verify fmrihrf availability** - Check if dependency is on CRAN
2. **Complete @examples** - Add examples to all exported functions
3. **Complete @param/@return** - Document all parameters and return values
4. **Audit exports** - Remove @export from internal functions; use @keywords internal
5. **R CMD check --as-cran** - Achieve 0 errors, 0 warnings, minimal notes

### Phase 2: Quality Improvements (Strongly Recommended)

6. **Test coverage** - Ensure > 80% coverage, especially for exported functions
7. **Platform testing** - Win-builder, R-hub, GitHub Actions
8. **Spell check** - Fix typos, create WORDLIST
9. **NEWS.md** - Document version history
10. **cran-comments.md** - Prepare submission documentation

### Phase 3: Polish (Nice to Have)

11. **README polish** - Ensure complete with examples
12. **CI/CD setup** - GitHub Actions for automated checks
13. **Additional vignettes** - If needed for complex workflows
14. **Code of conduct / Contributing** - Community guidelines

---

## Common First-Submission Rejection Reasons

Based on community experience, these are the most frequent reasons packages are rejected on first submission:

1. **Incomplete documentation** - Missing @return or @examples (HIGH PRIORITY for fmridesign)
2. **DESCRIPTION errors** - Grammar, missing copyright holder, redundant phrasing
3. **Examples issues** - Too slow (> 5 sec), incorrect use of \dontrun{}
4. **Unavailable dependencies** - Imports not on CRAN/Bioconductor
5. **Platform failures** - Works on one platform but fails on others
6. **Size violations** - Data or documentation > 5 MB
7. **Policy violations** - Writing to user directories, disabling warnings

**Sources:**
- [CRAN incoming dashboard](https://r-hub.github.io/cransays/articles/dashboard.html)
- [Common rejection reasons - R Packages](https://r-pkgs.org/release.html)
- [Preparing for CRAN - ThinkR](https://github.com/ThinkR-open/prepare-for-cran)

---

## Timeline Considerations

| Milestone | Timing |
|-----------|--------|
| **Fix blockers** | 1-2 weeks |
| **Test on multiple platforms** | 3-5 days |
| **Quality improvements** | 1 week |
| **Submit to CRAN** | After all mandatory requirements met |
| **CRAN review** | 1-14 days (typically 3-7 for new submissions) |
| **Respond to feedback** | Within 2 weeks |

**Submission frequency:** After initial submission, updates should be spaced 30+ days apart unless responding to CRAN team feedback.

---

## Confidence Assessment

| Area | Confidence | Basis |
|------|------------|-------|
| Mandatory requirements | HIGH | Official CRAN policy, submission checklist, repository policy |
| Documentation requirements | HIGH | Verified through multiple official sources and community consensus |
| Examples policy | HIGH | Official CRAN cookbook, roxygen2 documentation |
| Size limits | HIGH | CRAN repository policy |
| Code standards | HIGH | CRAN cookbook, repository policy |
| Strong recommendations | MEDIUM | Community best practices, not explicitly required but commonly requested |
| Nice to have | MEDIUM | Ecosystem standards, improve quality but not required for acceptance |

All findings verified through official CRAN documentation, official R Packages book, and consistent community guidance.

---

## Sources

This research is based on authoritative sources from the CRAN team and R community:

**Official CRAN Documentation:**
- [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html)
- [CRAN Submission Checklist](https://cran.r-project.org/web/packages/submission_checklist.html)
- [DESCRIPTION File Issues - CRAN Cookbook](https://contributor.r-project.org/cran-cookbook/description_issues.html)
- [Code Issues - CRAN Cookbook](https://contributor.r-project.org/cran-cookbook/code_issues.html)
- [General Issues - CRAN Cookbook](http://contributor.r-project.org/cran-cookbook/general_issues.html)

**R Packages Book (Hadley Wickham & Jenny Bryan):**
- [R CMD check](https://r-pkgs.org/R-CMD-check.html)
- [DESCRIPTION](https://r-pkgs.org/description.html)
- [Function documentation](https://r-pkgs.org/man.html)
- [Dependencies in Practice](https://r-pkgs.org/dependencies-in-practice.html)
- [Releasing to CRAN](https://r-pkgs.org/release.html)

**roxygen2 Documentation:**
- [Documenting functions](https://roxygen2.r-lib.org/articles/rd.html)
- [Managing imports and exports](https://roxygen2.r-lib.org/articles/namespace.html)

**R-hub Blog:**
- [Code examples in R package manuals](https://blog.r-hub.io/2020/01/27/examples/)
- [Internal functions in R packages](https://blog.r-hub.io/2019/12/12/internal-functions/)

**Community Resources:**
- [Preparing for CRAN - ThinkR](https://github.com/ThinkR-open/prepare-for-cran)
- [Size and Limitations - Coatless Professor](https://blog.thecoatlessprofessor.com/programming/r/size-and-limitations-of-packages-on-cran/)
- [CRAN incoming dashboard](https://r-hub.github.io/cransays/articles/dashboard.html)

---

## Next Steps for fmridesign

Based on this research, the immediate action items are:

1. **CRITICAL: Verify fmrihrf is on CRAN** - This is a hard blocker
2. **Complete all @examples** - Add runnable examples to every exported function
3. **Complete all @param and @return** - Ensure no documentation gaps
4. **Audit namespace exports** - Review which functions should be internal
5. **Run R CMD check --as-cran** - Iterate until 0 errors, 0 warnings, minimal notes
6. **Test on Win-builder** - Ensure Windows compatibility
7. **Prepare submission materials** - NEWS.md, cran-comments.md, spell check

After these are complete, the package will be ready for CRAN submission.
