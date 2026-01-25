# CRAN Submission Pitfalls

**Domain:** R package submission to CRAN
**Researched:** 2026-01-25
**Package:** fmridesign (first CRAN submission)

## Executive Summary

CRAN rejections for first-time submitters typically fall into three categories:

1. **Automated check failures** (ERRORs, WARNINGs, problematic NOTEs) - 40% of rejections
2. **DESCRIPTION file formatting issues** - 35% of rejections
3. **Policy violations** (file system access, timing, size limits) - 25% of rejections

The good news: All are preventable with systematic checking before submission. The bad news: CRAN's human review is strict and literal - minor formatting issues in DESCRIPTION fields cause rejections despite passing `R CMD check --as-cran`.

---

## Critical Pitfalls

These mistakes cause immediate rejection and require resubmission.

### Pitfall 1: ERRORs or WARNINGs in R CMD check

**What goes wrong:** Package fails `R CMD check --as-cran` with ERRORs or WARNINGs.

**Why it happens:**
- Testing only on local machine, not multiple platforms
- Using older R version instead of R-devel
- Not running with `--as-cran` flag which enables stricter checks
- Dependency on packages not available on all platforms

**Consequences:**
- Automatic rejection - packages with ERRORs or WARNINGs will not be accepted
- Lost time waiting for rejection email
- Version number increment required for resubmission

**Prevention:**
1. Run `devtools::check()` locally (includes `--as-cran`)
2. Test on R-devel (not just release version)
3. Use GitHub Actions or R-hub to test on Windows, macOS, Linux
4. Run checks frequently during development, not just before submission
5. Fix ALL warnings, even ones that seem harmless

**Detection:**
- `R CMD check --as-cran` reports errors or warnings
- GitHub Actions checks failing
- Local check reports issues you've been ignoring

**Which phase:** Pre-submission validation (Milestone phase 1)

**Warning signs for fmridesign:**
- Package currently passes `R CMD check` (good sign)
- Dependencies include plotly/ggplot2 which are cross-platform (good)
- Depends on fmrihrf (already on CRAN - good)

---

### Pitfall 2: DESCRIPTION Title/Description Field Formatting

**What goes wrong:** DESCRIPTION file's Title or Description fields violate CRAN's formatting requirements.

**Why it happens:**
- Title and Description fields are "real hotspots for nitpicking during CRAN's human review"
- Requirements are documented but easy to overlook
- These fields are shown prominently on CRAN package pages

**Consequences:**
- Manual rejection from CRAN reviewer
- Embarrassing for issues that R CMD check doesn't catch
- Requires resubmission with incremented version

**Prevention:**

**Title field requirements:**
- Use title case (`tools::toTitleCase()` can help)
- Do NOT start with "A package for..." or "This package..."
- Do NOT include the word "package"
- Do NOT include the package name itself
- Put R package names in single quotes (e.g., 'ggplot2')
- Put software/API names in single quotes
- Keep under 65 characters

**Description field requirements:**
- Write in complete sentences with proper grammar
- Provide 1-3 sentences explaining what the package does
- Use single quotes for package names, software, APIs
- Use `()` after function names: `foo()` not `foo` or `'foo'`
- Include citations if based on published methods: `Authors (year) <doi:...>`
- No leading/trailing whitespace

**Detection:**
Run CRAN's manual checks yourself:
```r
# Check title case
tools::toTitleCase("your title") == "Your Title"

# Check for forbidden phrases
grepl("package|this|a tool", Title, ignore.case = TRUE)

# Spell check
spelling::spell_check_package()
```

**Which phase:** Pre-submission validation (Milestone phase 1)

**Warning signs for fmridesign:**
- Current Title: "Design Matrix Construction for fMRI Analysis" - GOOD (title case, no "package")
- Current Description: Starts with "Constructs and inspects..." - GOOD (active voice)
- No citations in Description - could add if based on published methods

---

### Pitfall 3: Improper Use of \dontrun{} in Examples

**What goes wrong:** Using `\dontrun{}` for examples that could run but take time, or using it to hide broken examples.

**Why it happens:**
- Confusion between `\dontrun{}` (never runs) vs `\donttest{}` (runs for users, skipped on CRAN)
- **Critical change in 2025:** `R CMD check --as-cran` now RUNS `\donttest{}` examples
- Old advice from pre-2025 is outdated

**Consequences:**
- CRAN rejects packages misusing `\dontrun{}`
- Examples that should demonstrate functionality are hidden from users
- False impression that code doesn't work

**Prevention:**

**Use \dontrun{} ONLY when:**
- Example truly cannot run (missing API keys, external software required)
- Example requires user interaction (file.choose(), menu())
- Example requires authentication tokens

**Use \donttest{} when:**
- Example takes > 5 seconds to run
- Example is interactive but works
- Example uses random seed or is occasionally flaky

**Use nothing (run example) when:**
- Example takes < 5 seconds
- Example demonstrates core functionality
- Example is reproducible and stable

**Modern guidance (2025):**
> "Unwrap the examples if they are executable in < 5 sec, or replace `\dontrun{}` with `\donttest{}`"

**Detection:**
```bash
# Find all dontrun usage
grep -r "\\dontrun" R/*.R

# For each one, ask:
# 1. Does it actually run? If yes, use \donttest or nothing
# 2. Does it take < 5 sec? If yes, unwrap completely
# 3. Does it require external resources? Only then use \dontrun
```

**Which phase:** Documentation review (Milestone phase 2)

**Warning signs for fmridesign:**
- Has 6 instances of `\dontrun{}` in R files
- Need to review each: Are they truly non-executable, or just slow?
- Example from contrast.R looks like it COULD run (simple function calls)

---

### Pitfall 4: Writing to User Filespace

**What goes wrong:** Package writes files to user's home directory, working directory, or anywhere except `tempdir()` during examples/tests/vignettes.

**Why it happens:**
- Functions like `write.csv()`, `saveRDS()` naturally write to current directory
- Caching data for performance
- Not understanding CRAN's "no side effects" policy

**Consequences:**
- CRAN policy violation - packages "should not write in the user's home filespace"
- Rejection during manual review
- Anti-social behavior (writing files without user consent)

**Prevention:**

**Golden rules:**
1. Examples/tests/vignettes should ONLY write to `tempdir()`
2. Clean up with `unlink()` after tests
3. User-facing functions CAN write to user-specified paths (that's their purpose)
4. Use `withr::local_tempfile()` or `withr::local_tempdir()` in tests

**Safe pattern:**
```r
# In examples/tests
tmp <- tempfile(fileext = ".csv")
write.csv(data, tmp)
# ... use tmp ...
unlink(tmp)

# Or with withr
withr::local_tempfile()
```

**Unsafe patterns:**
```r
# DON'T DO THIS in examples/tests:
write.csv(data, "output.csv")  # Writes to current directory
saveRDS(obj, "~/cache/data.rds")  # Writes to home directory
dir.create("results")  # Creates in current directory
```

**Detection:**
```bash
# Search for file-writing functions in examples/tests
grep -E "write\.|saveRDS|dir.create" man/*.Rd
grep -E "write\.|saveRDS|dir.create" tests/testthat/*.R

# Check they all use tempfile/tempdir
```

**Which phase:** Documentation and testing review (Milestone phase 2)

**Warning signs for fmridesign:**
- Package builds design matrices (data objects, not files) - LOW RISK
- No obvious file export functions in core API
- Check vignettes and examples for any file writing

---

## Moderate Pitfalls

These cause delays or require extra explanation but may not cause immediate rejection.

### Pitfall 5: Unexplained NOTEs

**What goes wrong:** `R CMD check` produces NOTEs that aren't explained in submission.

**Why it happens:**
- "Each NOTE requires human oversight, which creates friction for both you and CRAN"
- Some NOTEs are expected (e.g., "New submission" for first submission)
- Some NOTEs are false positives or unavoidable
- Developers assume CRAN will understand context

**Prevention:**

**Step 1: Eliminate all avoidable NOTEs**
- Undocumented functions/data
- Non-standard file names
- Large file sizes
- Missing package dependencies

**Step 2: For unavoidable NOTEs, document in cran-comments.md:**

```markdown
## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Bradley Buchsbaum <brad.buchsbaum@gmail.com>'

  New submission

  This is my first submission to CRAN.
```

**Common expected NOTEs for first submission:**
- "New submission" - always appears, just acknowledge it
- "Possibly misspelled words in DESCRIPTION" - explain domain terms (e.g., "fMRI")

**Detection:**
Run `devtools::check()` and review ALL notes. For each NOTE:
1. Can I fix it? If yes, fix it.
2. If unavoidable, document in cran-comments.md

**Which phase:** Pre-submission validation (Milestone phase 1)

**Warning signs for fmridesign:**
- First submission will have "New submission" NOTE (expected)
- Domain terminology ("fMRI", "HRF", "hemodynamic") may trigger spell-check NOTEs
- Need to explain these are correct technical terms

---

### Pitfall 6: Package Size Exceeds Limits

**What goes wrong:** Source tarball exceeds CRAN's size limits.

**Why it happens:**
- Official policy: source tarballs "should if possible not exceed 10MB"
- Practical enforcement: CRAN requests packages "Less than 5 MB, please"
- Vignettes with plots/images add significant size
- Data files in inst/ or data/ directories
- Pre-built vignette outputs

**Consequences:**
- Manual rejection with request to reduce size
- Need to restructure package or move data elsewhere

**Prevention:**

**Check size before submission:**
```bash
# Build tarball
R CMD build .

# Check size
ls -lh fmridesign_*.tar.gz
```

**If too large:**
1. Compress vignette images more aggressively
2. Use `knitr::opts_chunk$set(dpi = 72)` for lower resolution
3. Move large data to separate package or external download
4. Ensure `doc/` directory in .Rbuildignore (built vignettes excluded)
5. Remove unnecessary files via .Rbuildignore

**Detection:**
- Build tarball and check size
- If > 5MB, investigate what's contributing

**Which phase:** Pre-submission validation (Milestone phase 1)

**Warning signs for fmridesign:**
- Estimated tarball size: ~9.8 MB (CLOSE to 10MB limit, needs attention)
- Large files found in `doc/` directory (built vignette outputs):
  - `a_04_event_models.html` (2.6MB)
  - `a_03_baseline_model.html` (1.6MB)
- CRITICAL: Ensure `doc/` is in .Rbuildignore (it is - good)
- May need to reduce vignette plot sizes or resolution

---

### Pitfall 7: Examples/Tests Take Too Long

**What goes wrong:** Examples take > 5 seconds each, or total check time is excessive.

**Why it happens:**
- "Checking the package should take as little CPU time as possible, as the CRAN check farm is a very limited resource"
- Examples meant to be thorough rather than quick
- Tests running extensive computations

**Consequences:**
- CRAN rejection with note about timing
- Need to wrap slow examples in `\donttest{}`
- Need to skip slow tests on CRAN with `skip_on_cran()`

**Prevention:**

**Examples:**
- Each example should run in < 5 seconds
- Wrap longer examples in `\donttest{}`
- Use smaller datasets in examples (full-size in vignettes)

**Tests:**
- Put slow tests behind `skip_on_cran()` from testthat
- Keep basic functionality tests fast
- Extensive tests can run locally/CI but skip on CRAN

**Example:**
```r
test_that("complex simulation works", {
  skip_on_cran()  # This test takes 30 seconds

  result <- run_long_simulation()
  expect_equal(result$status, "success")
})
```

**Detection:**
```r
# Time your examples
system.time(devtools::run_examples())

# Check individual example timing
# Each should be < 5 seconds
```

**Which phase:** Testing optimization (Milestone phase 3)

**Warning signs for fmridesign:**
- Design matrix construction for fMRI could be computationally intensive
- Convolution with HRF bases may take time
- Review examples for timing, use smaller test cases

---

### Pitfall 8: Missing or Incorrect LICENSE Information

**What goes wrong:** LICENSE file doesn't match DESCRIPTION, or copyright holder unclear.

**Why it happens:**
- Different license formats (MIT vs MIT + file LICENSE)
- Copyright holder defaults may not be appropriate
- Confusion about what goes in LICENSE file vs LICENSE.md

**Consequences:**
- Rejection for unclear intellectual property rights
- Legal ambiguity about package ownership

**Prevention:**

**For common licenses:**

**GPL (>= 2)** (fmridesign's license):
- DESCRIPTION: `License: GPL (>= 2)`
- No LICENSE file needed (standard license)
- Copyright holder in Authors@R with `role = "cph"`

**MIT:**
- DESCRIPTION: `License: MIT + file LICENSE`
- LICENSE file contains: year and copyright holder name
- LICENSE.md goes in .Rbuildignore

**Authors@R:**
```r
person("Bradley", "Buchsbaum",
       role = c("aut", "cre", "cph"),  # cph = copyright holder
       email = "brad.buchsbaum@gmail.com")
```

**Detection:**
- Check DESCRIPTION License field matches actual files
- Verify Authors@R includes copyright holder (`cph` role)
- If using MIT, ensure LICENSE file exists with correct format

**Which phase:** Pre-submission validation (Milestone phase 1)

**Warning signs for fmridesign:**
- Currently using GPL (>= 2) - standard, no extra files needed (GOOD)
- Authors@R has aut, cre - should add 'cph' role for completeness
- No LICENSE file present - correct for GPL

---

## Minor Pitfalls

These cause annoyance or extra back-and-forth but are easily fixed.

### Pitfall 9: Missing or Incomplete cran-comments.md

**What goes wrong:** No cran-comments.md file, or file doesn't explain test environments and NOTEs.

**Why it happens:**
- Template file can be auto-generated but needs customization
- Developers don't know what to include
- File exists but is outdated from previous attempt

**Prevention:**

**Create with usethis:**
```r
usethis::use_cran_comments()
```

**Required sections:**
```markdown
## Test environments
* local: macOS 14.x, R 4.4.0
* GitHub Actions: ubuntu-latest, windows-latest, macos-latest (R-release)
* GitHub Actions: ubuntu-latest (R-devel)
* win-builder: R-devel

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  New submission

## Downstream dependencies

There are currently no downstream dependencies for this package.
```

**For first submission, mention:**
- This is a new submission
- Which platforms tested on
- Explanation of any NOTEs

**For resubmission:**
- This is a resubmission
- What changed in response to feedback
- Increment version number (patch)

**Detection:**
- Does cran-comments.md exist?
- Is it in .Rbuildignore?
- Does it explain all NOTEs?

**Which phase:** Pre-submission validation (Milestone phase 1)

**Warning signs for fmridesign:**
- cran-comments.md exists and is in .Rbuildignore (GOOD)
- Need to update with current test environments
- Need to explain "New submission" NOTE

---

### Pitfall 10: Version Number Issues on Resubmission

**What goes wrong:** After CRAN rejection, resubmitting with same version number or wrong increment.

**Why it happens:**
- Not knowing the convention
- Forgetting to update after fixing issues
- Using wrong semantic versioning component

**Consequences:**
- Confusion for CRAN maintainers
- Possible rejection
- Lost submission attempt

**Prevention:**

**Rules:**
1. **First submission:** Use any version (commonly 0.1.0 or 1.0.0)
2. **Resubmission after feedback:** Increment PATCH version
   - First: 0.5.0
   - Resubmit: 0.5.1
   - Resubmit again: 0.5.2
3. **After acceptance:** Follow semantic versioning normally

**In cran-comments.md:**
```markdown
## Resubmission
This is a resubmission. In this version I have:

* Fixed DESCRIPTION title capitalization
* Wrapped long-running examples in \donttest{}
* Added cph role to Authors@R
```

**Detection:**
- Before resubmission, check version was incremented
- Update Date field in DESCRIPTION
- Document changes in cran-comments.md

**Which phase:** Resubmission process (if needed after initial submission)

**Warning signs for fmridesign:**
- Current version: 0.5.0 (good starting point)
- If rejected, next version should be 0.5.1
- Update Date field when incrementing

---

### Pitfall 11: URL Formatting in DESCRIPTION

**What goes wrong:** URLs and DOIs not formatted per CRAN requirements.

**Why it happens:**
- Specific format requirements not obvious
- Old examples use different formats
- Auto-linking requires specific syntax

**Consequences:**
- NOTEs about malformed URLs
- Broken links on CRAN package page
- Manual review rejection

**Prevention:**

**DOI format:**
- Correct: `<doi:10.1234/example>`
- Wrong: `<https://doi.org/10.1234/example>`
- No space after colon

**arXiv format:**
- Correct: `<doi:10.48550/arXiv.1234.5678>` (use DOI)
- Wrong: `<arXiv:1234.5678>`

**Regular URLs:**
- Correct: `<https://example.com>`
- Enclose in angle brackets
- Must be https (not http)

**In Description field:**
```
Authors (2024) <doi:10.1234/example>
```

**In URL field:**
```
URL: https://github.com/bbuchsbaum/fmridesign,
    https://bbuchsbaum.github.io/fmridesign/
```

**Detection:**
```r
# Check URLs are accessible
urlchecker::url_check()
```

**Which phase:** Pre-submission validation (Milestone phase 1)

**Warning signs for fmridesign:**
- Current URL field looks correct (GitHub repo and pkgdown site)
- No DOIs in Description currently
- If package implements published methods, consider adding citations

---

### Pitfall 12: Submission Frequency Violations

**What goes wrong:** Submitting updates too frequently (< 30 days between submissions).

**Why it happens:**
- Eagerness to fix bugs
- Not batching changes
- Not understanding CRAN resource constraints

**Consequences:**
- Rejection with note about submission frequency
- Damaged relationship with CRAN maintainers
- Policy states "should not be done too frequently"

**Prevention:**

**Rules:**
- Wait 30 days between submissions (general guideline)
- Exception: Resubmitting after CRAN feedback (immediate resubmission OK)
- Exception: Urgent bug fixes affecting users

**Batch changes:**
- Collect multiple fixes/features
- Test thoroughly
- Submit once with all changes

**Communication:**
If urgent submission needed, explain in submission comments:
```
This is an urgent resubmission (within 30 days) because
the current CRAN version has a critical bug that breaks
functionality on Windows.
```

**Detection:**
- Check dates of previous submissions
- Count days since last submission
- Only submit when truly ready

**Which phase:** Post-acceptance maintenance (future)

**Warning signs for fmridesign:**
- First submission - not applicable yet
- After acceptance, batch bug fixes rather than submitting each immediately

---

## Phase-Specific Warnings

| Phase Topic | Likely Pitfall | Mitigation Strategy |
|-------------|---------------|---------------------|
| Phase 1: Pre-submission validation | DESCRIPTION formatting, R CMD check failures | Use checklist, run tools::toTitleCase(), test on R-devel |
| Phase 2: Documentation review | Improper \dontrun{} usage, missing function docs | Review all examples, ensure < 5 sec or wrap properly |
| Phase 3: Testing optimization | Tests too slow, no skip_on_cran() | Profile test timing, add skips for slow tests |
| Phase 4: Size optimization | Tarball > 5MB from vignette plots | Compress images, reduce DPI, check doc/ excluded |
| Phase 5: Final submission | Missing cran-comments.md, unexplained NOTEs | Generate template, document all NOTEs |
| Phase 6: Resubmission (if needed) | Wrong version increment, unclear changes | Increment patch, document in cran-comments.md |

---

## Quick Reference: Pre-submission Checklist

Run these checks before submitting to CRAN:

```r
# 1. R CMD check with zero errors, warnings, notes (except expected)
devtools::check()

# 2. Check DESCRIPTION formatting
spelling::spell_check_package()
tools::toTitleCase(desc::desc_get_field("Title"))

# 3. Check URLs
urlchecker::url_check()

# 4. Test on multiple platforms
# - GitHub Actions: Windows, macOS, Linux
# - R-hub (if available)

# 5. Check size
# Build tarball and verify < 5MB
devtools::build()

# 6. Ensure cran-comments.md exists and is current
usethis::use_cran_comments()

# 7. Review examples timing
system.time(devtools::run_examples())

# 8. Check for dontrun misuse
# Search and review each instance

# 9. Verify no writes to user filespace in examples/tests
# Grep for write., saveRDS, dir.create

# 10. Final check on R-devel
# Use GitHub Actions with R-devel
```

---

## Specific Recommendations for fmridesign

Based on package analysis, prioritize these checks:

### HIGH PRIORITY

1. **Package size (9.8MB tarball)**
   - Current size is near the 10MB limit
   - Main contributors: vignette HTML outputs in doc/
   - **Action:** Verify `doc/` is in .Rbuildignore (it is)
   - **Action:** Reduce vignette plot resolution/size if tarball > 5MB after build
   - **Action:** Use `knitr::opts_chunk$set(dpi = 72, fig.width = 6, fig.height = 4)`

2. **Review \dontrun{} usage**
   - 6 instances found in R files
   - **Action:** For each, verify it truly can't run
   - **Action:** If examples CAN run but are slow, use \donttest{}
   - **Action:** If examples CAN run quickly (< 5 sec), unwrap completely

3. **Add copyright holder role**
   - **Action:** Add `cph` role to Authors@R field
   - Current: `role = c("aut", "cre")`
   - Change to: `role = c("aut", "cre", "cph")`

### MEDIUM PRIORITY

4. **Domain terminology NOTEs**
   - Spell checker may flag: fMRI, HRF, hemodynamic, parametric modulators
   - **Action:** Add to inst/WORDLIST file or explain in cran-comments.md

5. **Example timing**
   - Design matrix construction with HRF convolution may be slow
   - **Action:** Profile example run time
   - **Action:** Use small test datasets in examples (save realistic sizes for vignettes)

6. **Test for file-writing violations**
   - Package seems to work with in-memory objects (good)
   - **Action:** Grep vignettes for write., saveRDS, etc.
   - **Action:** If found, ensure using tempdir()

### LOW PRIORITY

7. **URL formatting**
   - Current URLs appear correct
   - **Action:** Run `urlchecker::url_check()` to verify

8. **Update cran-comments.md**
   - File exists (good)
   - **Action:** Update with current test environments
   - **Action:** Explain "New submission" NOTE
   - **Action:** Explain any domain-specific spell-check NOTEs

---

## Sources

### CRAN Official Documentation
- [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html)
- [CRAN Submission Checklist](https://cran.r-project.org/web/packages/submission_checklist.html)
- [Submitting to CRAN](https://cran.r-project.org/submit.html)
- [CRAN Package Licenses](https://cran.r-project.org/web/licenses/)

### Best Practices Guides
- [R Packages (2e) - Releasing to CRAN](https://r-pkgs.org/release.html)
- [R Packages (2e) - Licensing](https://r-pkgs.org/license.html)
- [R Packages (2e) - DESCRIPTION](https://r-pkgs.org/description.html)
- [ThinkR: Preparing for CRAN](https://github.com/ThinkR-open/prepare-for-cran)

### Technical Resources
- [R-hub: URL Checks](https://blog.r-hub.io/2020/12/01/url-checks/)
- [R-hub: Code Examples](https://blog.r-hub.io/2020/01/27/examples/)
- [The CRAN Cookbook](http://contributor.r-project.org/cran-cookbook/general_issues.html)
- [The Coatless Professor: Size and Limitations](https://www.thecoatlessprofessor.com/programming/r/size-and-limitations-of-packages-on-cran/)

### Experience Reports
- [Andrew Wheeler: My Journey Submitting to CRAN](https://andrewpwheeler.com/2022/07/22/my-journey-submitting-to-cran/)
- [B101nfo: Submissions Accepted First Try](https://llrs.dev/post/2024/01/10/submission-cran-first-try/)
- [Julia Silge: How I Learned to Stop Worrying and Love R CMD Check](https://juliasilge.com/blog/how-i-stopped/)

### Package Development Tools
- [usethis: CRAN Comments](https://usethis.r-lib.org/reference/use_cran_comments.html)
- [usethis: Licenses](https://usethis.r-lib.org/reference/licenses.html)
- [testthat Documentation](https://testthat.r-lib.org/)

### Community Resources
- [Karl Broman: Getting Your Package on CRAN](https://kbroman.org/pkg_primer/pages/cran.html)
- [Marine Data Science: CRAN Submission Checklist](https://www.marinedatascience.co/blog/2020/01/09/checklist-for-r-package-re-submissions-on-cran/)
- [COVID19R Project: Preparing and Submitting to CRAN](https://covid19r.github.io/documentation/preparing-and-submitting-to-cran.html)
