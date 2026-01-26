# Phase 3: Local Validation - Context

**Gathered:** 2026-01-25
**Status:** Ready for planning

<domain>
## Phase Boundary

Achieve a clean `R CMD check --as-cran` locally with 0 errors, 0 warnings, and minimal notes. Source tarball from `devtools::build()` must be under 10 MB. This phase does NOT include cross-platform testing (Phase 4) or submission materials (Phase 5).

</domain>

<decisions>
## Implementation Decisions

### Package size strategy
- If tarball exceeds 10 MB, trim data files first (compress or remove bundled datasets)
- Claude assesses which datasets are essential vs. trimmable
- Only compress .rda/.RData to xz if tarball actually exceeds 10 MB — no preemptive compression
- If data trimming insufficient, aggressively exclude non-essential files via .Rbuildignore (no CRAN size exception request)

### NOTE tolerance
- Fix ALL NOTEs — aim for cleanest possible submission (only "new submission" NOTE acceptable)
- For "no visible binding" NOTEs from tidy eval / NSE: suppress with `utils::globalVariables()` (standard CRAN-accepted pattern)
- Tidyverse dependency NOTEs (dplyr, tidyr, purrr, tibble) are accepted — these are core to the package design; document in cran-comments.md
- Add all non-standard top-level files/directories to .Rbuildignore (e.g., .planning/, Rplots.pdf)

### Fix approach for unknowns
- Fix issues immediately as they surface (don't defer)
- Run check fully, batch all fixes, then re-check once to verify
- If issue traces to upstream dependency (e.g., fmrihrf): flag as blocker, do not work around
- Minor API changes (adding defaults, deprecating args) OK without review; significant signature changes need approval
- Claude triages based on severity and effort

### Example/test runtime
- Slow examples (>5 seconds): wrap in `\donttest{}` — CRAN-accepted, still documented
- Slow tests: use `testthat::skip_on_cran()` rather than optimizing test data
- All vignette chunks run during check — no pre-computed output
- Test dependencies not in Suggests: add to Suggests (don't skip_if_not_installed)

### Claude's Discretion
- Specific .Rbuildignore entries beyond the obvious ones
- Order of fix operations within a batch
- Whether a given NOTE is "easy enough" to fix vs. requiring documentation
- Exact globalVariables() list composition

</decisions>

<specifics>
## Specific Ideas

No specific requirements — open to standard approaches. The key constraint is the 9.8 MB tarball size already near the 10 MB limit (from STATE.md), so size awareness should be front-of-mind during validation.

</specifics>

<deferred>
## Deferred Ideas

None — discussion stayed within phase scope

</deferred>

---

*Phase: 03-local-validation*
*Context gathered: 2026-01-25*
