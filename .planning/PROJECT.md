# fmridesign CRAN Submission

## What This Is

An R package for constructing design matrices for fMRI analyses. Provides formula-based event models with flexible HRF specifications, baseline models for drift correction, and contrast systems for hypothesis testing. Currently at v0.5.0, targeting CRAN submission.

## Core Value

Design matrices that correctly encode experimental structure — if the matrix is wrong, all downstream analyses are wrong.

## Requirements

### Validated

<!-- Shipped and confirmed working. -->

- ✓ Event model construction via formula interface (`onset ~ hrf(condition)`) — existing
- ✓ Baseline model construction (polynomial, B-splines, natural splines, block intercepts) — existing
- ✓ HRF integration via fmrihrf (multiple basis types, per-onset HRFs) — existing
- ✓ Contrast specification system (pair contrasts, pattern matching) — existing
- ✓ Block-structured design matrices respecting fMRI run organization — existing
- ✓ S3 class system for events, terms, and models — existing
- ✓ Design matrix visualization — existing
- ✓ R CMD check passes with 0 errors, 0 warnings, 0 notes — existing

### Active

<!-- Current scope. Building toward these. -->

- [ ] All exported functions have complete @param documentation
- [ ] All exported functions have @return documentation
- [ ] All exported functions have runnable @examples
- [ ] Internal functions properly marked (@keywords internal, not exported)
- [ ] Exports audit complete (only user-facing functions exported)
- [ ] Test coverage assessed with covr
- [ ] Test coverage improved for edge cases and error paths
- [ ] Documentation quality polished for clarity
- [ ] CRAN submission checklist complete

### Out of Scope

- New features — this milestone is about CRAN readiness, not functionality
- Breaking API changes — maintain backward compatibility
- Performance optimization — not blocking submission
- Additional vignettes — existing documentation sufficient for v1

## Context

**Brownfield project:** Mature R package with substantial functionality. Core design matrix construction works and is tested. The work is polish and compliance, not implementation.

**Dependency situation:** Primary dependency `fmrihrf` is already on CRAN. No blockers from dependencies.

**Codebase state:** See `.planning/codebase/` for detailed analysis. Key concerns from CONCERNS.md include some fragile areas in rank-deficiency handling and column naming, but these are documented and don't block CRAN submission.

**CRAN requirements:** Must pass R CMD check on multiple platforms, have complete documentation for all exports, runnable examples, and follow CRAN policies.

## Constraints

- **R version**: R >= 4.1.0 (already specified in DESCRIPTION)
- **Dependencies**: All dependencies must be on CRAN (satisfied)
- **Check results**: Must pass R CMD check with no errors, warnings, or notes
- **Examples**: All @examples must run without error in < 5 seconds each
- **License**: GPL (>= 2) — already set, compatible with dependencies

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| Focus on documentation over new features | CRAN submission is the goal; features can come in future versions | — Pending |
| Keep existing API unchanged | Avoid breaking changes for existing users | — Pending |

---
*Last updated: 2026-01-25 after initialization*
