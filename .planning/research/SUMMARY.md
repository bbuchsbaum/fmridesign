# Research Summary: CRAN Submission Tooling for fmridesign

**Domain:** R package CRAN submission workflow
**Researched:** 2025-01-25
**Overall confidence:** HIGH

## Executive Summary

The 2025 R package CRAN submission ecosystem has matured around the **r-lib tooling suite** (devtools, usethis, rcmdcheck) with significant infrastructure changes in 2024-2025. The most notable change is **rhub v2's complete rewrite** to use GitHub Actions instead of dedicated servers, requiring packages to be on GitHub but providing faster, more integrated testing.

The modern CRAN submission workflow emphasizes:
1. **Automated validation** - GitHub Actions running multi-platform checks on every push
2. **Comprehensive pre-submission testing** - Multiple specialized tools (goodpractice, checkhelper, spelling, urlchecker) catch issues before submission
3. **Official platform testing** - win-builder and mac-builder remain essential for Windows/macOS validation
4. **Documentation quality** - Spelling, URL validation, and pkgdown websites are now expected standards

For fmridesign (brownfield package at v0.5.0), the stack is largely **already in place** - the package uses devtools, roxygen2, testthat, pkgdown, and has rhub v2 configured. The roadmap should focus on **systematic validation** using the tools rather than setup.

## Key Findings

**Stack:** r-lib ecosystem (devtools 2.4.6+, usethis 3.2.1+, rcmdcheck) + rhub v2 (GitHub Actions-based) + official services (win-builder, mac-builder) + quality tools (goodpractice, checkhelper, spelling, urlchecker, lintr, styler, covr)

**Architecture:** Tool ecosystem is layered:
- **Core layer:** devtools (orchestrator) delegates to specialized packages (rcmdcheck, pkgload, pkgbuild, roxygen2, testthat)
- **Validation layer:** goodpractice bundles multiple checkers; checkhelper adds CRAN-specific checks
- **Platform layer:** rhub v2 (GitHub Actions), win-builder (Windows), mac-builder (macOS)
- **Quality layer:** spelling, urlchecker, lintr, styler, covr operate independently
- **CI/CD layer:** r-lib/actions provides pre-built GitHub Actions workflows

**Critical change:** rhub v2 (March 2025) is a complete rewrite using GitHub Actions. Old rhub v1 functions are deprecated. fmridesign already has rhub v2 configured (`.github/workflows/rhub.yaml` exists).

## Implications for Roadmap

The research reveals that CRAN submission is less about "what tools to adopt" (fmridesign already has the core stack) and more about **systematic execution of validation workflow**. The roadmap should structure phases around validation categories rather than tool installation.

Suggested phase structure:

### Phase 1: Core Documentation Quality
**Focus:** Fix issues that will definitely cause CRAN rejection
- **Tools:** spelling, urlchecker, checkhelper
- **Why first:** These are binary pass/fail issues (broken URLs, missing @return tags, spelling errors)
- **Rationale:** Quick wins that eliminate known rejection causes
- **Estimated complexity:** Low - run tools, fix findings

### Phase 2: R CMD check --as-cran Clean Pass
**Focus:** Achieve 0 ERRORS, 0 WARNINGS, minimal NOTEs on local system
- **Tools:** devtools::check(args = c('--as-cran')), goodpractice
- **Why second:** Cannot submit until this passes
- **Rationale:** This is the gatekeeper; everything else is preparatory or supplementary
- **Estimated complexity:** Medium - may reveal code issues requiring fixes

### Phase 3: Multi-Platform Validation
**Focus:** Verify package works on Windows, macOS, multiple R versions
- **Tools:** rhub v2, win-builder, mac-builder, GitHub Actions check-standard
- **Why third:** Can only test after local checks pass
- **Rationale:** CRAN tests on multiple platforms; must verify compatibility
- **Estimated complexity:** Medium - may reveal platform-specific issues

### Phase 4: Code Quality Improvements (Optional)
**Focus:** Improve code style, coverage, quality metrics
- **Tools:** lintr, styler, covr
- **Why optional:** Not required for CRAN, but improves package quality
- **Rationale:** Better code quality reduces post-submission issues and maintenance burden
- **Estimated complexity:** Low to Medium - mostly automated fixes

### Phase 5: Submission Preparation
**Focus:** Version bumping, NEWS updates, cran-comments.md, final checks
- **Tools:** usethis::use_release_issue(), devtools::release()
- **Why last:** Only do this when everything else is clean
- **Rationale:** Creates submission checklist and handles submission mechanics
- **Estimated complexity:** Low - mostly documentation

### Phase 6: Post-Submission Monitoring (Future)
**Focus:** Monitor CRAN check results after submission
- **Tools:** foghorn
- **Why future:** Only relevant after submission accepted
- **Rationale:** Track package status across CRAN platforms
- **Estimated complexity:** Very Low - passive monitoring

## Phase Ordering Rationale

**Sequential dependencies:**
- Phase 1 findings won't affect Phase 2, but fixing them first prevents distraction
- Phase 2 must pass before Phase 3 is meaningful (no point testing broken code on other platforms)
- Phase 4 can run in parallel with Phase 3 but is optional
- Phase 5 requires all previous phases complete
- Phase 6 follows submission

**Risk-based ordering:**
- Front-load high-rejection-risk issues (documentation, URLs)
- Test local environment before multi-platform (faster feedback loop)
- Optional quality improvements don't block submission

**Effort distribution:**
- Phase 1: 1-2 days (mostly mechanical fixes)
- Phase 2: 2-5 days (depends on issues found)
- Phase 3: 1-2 days (mostly waiting for builds)
- Phase 4: 1-3 days (optional, code quality dependent)
- Phase 5: 1 day (documentation and submission)

## Research Flags for Phases

| Phase | Research Likelihood | Reason |
|-------|-------------------|---------|
| Phase 1 | LOW | Tools are straightforward; findings are clear |
| Phase 2 | MEDIUM | May encounter package-specific R CMD check issues requiring investigation |
| Phase 3 | MEDIUM | Platform-specific issues (especially Windows/macOS compiled code) may need research |
| Phase 4 | LOW | lintr/styler/covr are well-documented |
| Phase 5 | LOW | Submission process is well-documented |

**Likely deep-dive topics:**
- If Phase 2 reveals NOTEs that aren't auto-fixable (research: "how to resolve [specific NOTE]")
- If Phase 3 reveals platform-specific failures (research: platform-specific dependencies, compilation flags)
- If package has compiled code issues on macOS arm64 (research: R package C++ arm64 compatibility)

## Confidence Assessment

| Area | Confidence | Notes |
|------|------------|-------|
| Core Tools | HIGH | All tools verified via WebSearch + official documentation cross-reference; devtools/usethis/rcmdcheck are stable, official r-lib tools |
| Platform Testing | HIGH | win-builder and mac-builder are official CRAN services; rhub v2 documented in official blog; fmridesign already has rhub.yaml |
| Validation Tools | HIGH | goodpractice (rOpenSci), spelling (rOpenSci), urlchecker (r-lib) widely used and documented |
| GitHub Actions | HIGH | r-lib/actions is official; workflows well-documented; standard for R package CI/CD |
| Tool Versions | HIGH | Verified via WebSearch results showing recent CRAN releases (devtools Oct 2025, lintr Nov 2025, rhub March 2025) |
| Workflow | MEDIUM | Based on R Packages book and community practices; not official CRAN guidance but widely followed |

**Confidence reasoning:**
- HIGH confidence for tools: Verified via official CRAN pages, r-lib documentation, recent package release dates
- MEDIUM confidence for workflow: Best practices synthesized from multiple sources (R Packages book, community blogs) rather than single authoritative source
- No LOW confidence areas: All findings cross-verified with multiple sources

## Gaps to Address

### Successfully Resolved
- rhub v2 architecture (confirmed: GitHub Actions-based, v1 deprecated)
- Tool versions and currency (verified: all tools actively maintained in 2025)
- Platform testing options (confirmed: win-builder, mac-builder, rhub v2 all available)

### Remaining Gaps (for phase-specific research)
1. **Package-specific R CMD check issues** - Cannot predict what NOTEs/WARNINGS will appear without running check; Phase 2 will reveal these
2. **Platform-specific compilation** - fmridesign has no compiled code (verified: no src/ directory expected), but depends on packages that might (fmrihrf); Phase 3 may reveal issues
3. **CRAN submission communication** - Response strategies for CRAN maintainer feedback not researched (out of scope for tooling research)

### Not Investigated (intentionally out of scope)
- CRAN policy compliance beyond technical checks (e.g., appropriate package scope, license compliance) - assumed brownfield package already complies
- Post-acceptance maintenance workflow - not relevant for initial submission
- Alternative submission platforms (Bioconductor, R-universe) - focused on CRAN per request

## Current Package Status (Observed)

Based on repository inspection:

**Already in place:**
- devtools ecosystem (DESCRIPTION shows testthat, knitr, rmarkdown, covr in Suggests)
- roxygen2 (RoxygenNote: 7.3.3 in DESCRIPTION)
- pkgdown (URL shows GitHub Pages site; `.github/workflows/pkgdown.yaml` exists)
- rhub v2 (`.github/workflows/rhub.yaml` exists)
- Testing infrastructure (testthat in Suggests)

**Not observed (may need addition):**
- inst/WORDLIST (no file found - spelling package may not be configured)
- .lintr configuration (not checked, but package may not use lintr)
- GitHub Actions R-CMD-check workflow (rhub.yaml exists but not standard check-standard)

**Implications:**
- Infrastructure setup is largely complete
- Roadmap focuses on **execution** (run checks, fix issues) not **setup** (install tools, configure workflows)
- Phase 1 may include one-time setup tasks (use_spell_check, use_github_action("check-standard"))

## Ready for Roadmap

Research is comprehensive and ready to inform roadmap creation. Key recommendations:

1. **Structure roadmap by validation category** (documentation, local checks, platform checks) not by tool
2. **Assume brownfield context** - tools mostly present, focus on systematic execution
3. **Front-load quick wins** - documentation quality fixes before deep R CMD check debugging
4. **Plan for unknowns** - Phase 2 and 3 likely to reveal issues requiring ad-hoc research
5. **Keep Phase 4 optional** - code quality improvements are valuable but not CRAN blockers

The tooling landscape is stable, well-documented, and mature. Success depends on systematic execution more than tool selection.
