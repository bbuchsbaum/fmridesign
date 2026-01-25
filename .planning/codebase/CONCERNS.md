# Codebase Concerns

**Analysis Date:** 2026-01-25

## Tech Debt

**Event Vector Convolution Naming Fragility:**
- Issue: Column naming from `model.matrix()` does not directly map to `conditions()` output when design is rank-deficient. Workaround assumes column order matches but may produce incorrect names.
- Files: `R/event_vector.R` (lines 1499-1515), `R/event_model_helpers.R` (lines 330-356)
- Impact: When categorical factors have rank deficiency (one level dropped), design matrix columns may be incorrectly labeled, causing subsequent contrast specifications to target wrong columns.
- Fix approach: Parse `model.matrix()` colnames directly or track rank-deficient dimensions through model building pipeline. Document current limitations and add validation checks to detect naming mismatches.

**Character Variables Not Converted to Factors in HRF Formula:**
- Issue: TODO comment at `R/hrf-formula.R` line 76 indicates character variables should be coerced to factors but this is not implemented.
- Files: `R/hrf-formula.R` (line 76)
- Impact: Character columns in event data passed to `hrf()` may not be treated as categorical predictors, leading to unexpected design matrix structure.
- Fix approach: Add `as.factor()` conversion in formula parsing or provide explicit guidance in documentation about pre-processing requirements.

**Incomplete Parallel Processing Implementation:**
- Issue: Parallel processing flag exists but warns and falls back to sequential `lapply()` every time.
- Files: `R/event_model_helpers.R` (lines 271-276)
- Impact: Users cannot benefit from parallel computation even with `parallel=TRUE` parameter set.
- Fix approach: Either complete parallel implementation using `future.apply` or remove the non-functional parameter.

**Unimplemented Event Model Check (Ticket EM-19):**
- Issue: TODO comment references missing validation check.
- Files: `R/event_model_helpers.R` (line 213)
- Impact: Unknown validation gap - ticket EM-19 should clarify what check was planned.
- Fix approach: Resolve ticket EM-19 to determine required validation and implement.

**Event Classes Validation TODO (Ticket EV-4):**
- Issue: `.checkEVArgs()` helper is marked for future refactoring/integration but is currently ad-hoc.
- Files: `R/event-classes.R` (line 58)
- Impact: Input validation is scattered across helper functions, making it harder to audit and maintain consistent validation rules.
- Fix approach: Refactor validation into centralized, well-documented validation pipeline.

**Column Name Mapping Uncertainty:**
- Issue: Comment at `R/event_vector.R` lines 1499-1506 explicitly acknowledges fragility in mapping `model.matrix()` columns to expected column names when design is rank-deficient. Workaround assumes column order matches.
- Files: `R/event_vector.R` (lines 1499-1515)
- Impact: Silent column naming errors possible when factors have missing levels or when design matrix rank < expected columns.
- Fix approach: Implement robust column mapping that parses `model.matrix()` attributes or tracks rank reduction explicitly.

## Known Bugs

**Empty Design Matrix After Dropping Columns:**
- Symptoms: When all columns of a design matrix are dropped (e.g., after removing single-level factors), subsequent convolution produces warnings about empty matrices.
- Files: `R/event_vector.R` (lines 1143-1147, 1199)
- Trigger: Event term with single-level categorical factor or all-zero indicator columns.
- Workaround: Ensure input factors have multiple levels before creating event terms. Use `droplevels()` on input data.

**DEBUG Print Statement Left in Code:**
- Symptoms: Commented debug print statement remains in production code.
- Files: `R/event_vector.R` (lines 1434-1436)
- Trigger: Viewing source code or analyzing for debug code.
- Workaround: None needed for functionality, but should be removed in next cleanup.

## Error Handling Issues

**Silent Failures with Graceful Degradation:**
- Issue: Many operations use `try()` blocks and catch errors, converting them to warnings. This allows pipeline to proceed but may produce incorrect results downstream.
- Files: `R/event_vector.R` (lines 189, 350, 423), `R/contrast.R` (multiple locations), `R/baseline_model.R` (lines 697, 888)
- Impact: Errors in intermediate steps (e.g., failed condition name retrieval) generate warnings but continue silently, potentially producing misleading output without user awareness.
- Recommendations: Use stricter error handling for critical operations. Reserve warnings for non-critical issues. Add function return status flags.

**Contrast Dimension Mismatch Handling:**
- Issue: When contrast weights dimensions don't match design matrix, validation skips the contrast and warns rather than failing fast.
- Files: `R/validate.R` (lines 135-141)
- Impact: Users may not realize a contrast they specified wasn't applied, leading to incorrect statistical results.
- Recommendations: Make dimension mismatches errors rather than warnings. Add contrast validation test suite.

## Performance Bottlenecks

**Single do.call(cbind) for All Terms:**
- Problem: All term design matrices are combined in one `do.call(cbind, term_matrices_filtered)` operation. For large numbers of terms, this creates a single large object.
- Files: `R/event_model_helpers.R` (line 316)
- Cause: Non-incremental column binding. Memory overhead proportional to final matrix size.
- Improvement path: For very large designs (many terms, long runs), consider streaming approach or memory-conscious building. Profile with realistic datasets.

**Repeated Attribute Assignments:**
- Problem: Attributes are assigned one at a time using multiple `attr()` calls rather than using `attributes()<-` for bulk assignment.
- Files: `R/event_model_helpers.R` (lines 287-289, 374-380)
- Cause: Multiple S3 object traversals and assignments.
- Improvement path: Batch attribute assignment for efficiency when setting multiple attributes on same object.

**Iteration in event_vector Processing:**
- Problem: Heavy use of `lapply()` and nested loops with `tryCatch()` for block-wise convolution.
- Files: `R/event_vector.R` (70+ iteration patterns counted)
- Cause: Block structure in fMRI requires block-wise convolution but current implementation may not be optimized for large numbers of blocks.
- Improvement path: Consider vectorized operations or compiled backend for convolution-heavy code.

## Fragile Areas

**HRF Function Interface:**
- Files: `R/event_vector.R` (lines 1030, 1098), `R/hrf-formula.R` (lines 40-75)
- Why fragile: Support for both single HRF and list of HRFs (per-block), with multiple fallback paths. Distinction between `hrf_fun` attribute and `hrfspec$hrf_fun` is error-prone.
- Safe modification: Document the expected HRF function signature clearly. Add explicit type checking and early validation of HRF objects. Write tests for both single and per-block HRF scenarios.
- Test coverage: Tests exist for HRF generation but may not cover all fallback paths in attribute resolution.

**Model Matrix Rank Deficiency Handling:**
- Files: `R/event_vector.R` (lines 1448-1459, 1507-1515), `R/event_model.R` (lines 468-475)
- Why fragile: Special case for single-level factors exists but general rank-deficiency handling relies on assumptions about column order and naming consistency.
- Safe modification: Always test with rank-deficient designs (missing factor levels, redundant contrasts). Add explicit rank checking before and after `model.matrix()` call. Test that column names survive rank reduction.
- Test coverage: Basic rank deficiency tests exist but edge cases with multiple rank-deficient factors may not be covered.

**Sampling Frame Dependencies:**
- Files: `R/event_model.R` (lines 150-164), `R/baseline_model.R` (lines 697-699), `R/event_vector.R` (lines 1150-1153)
- Why fragile: Package depends heavily on `fmrihrf::sampling_frame` object structure. Changes to fmrihrf API could break design matrix construction.
- Safe modification: Use defensive coding when accessing sampling_frame attributes. Don't rely on internal structure. Use only public fmrihrf functions. Write integration tests with fmrihrf.
- Test coverage: Tests use sampling frames but may not cover all possible fmrihrf versions or future API changes.

**Contrast Specification and Application:**
- Files: `R/contrast.R` (entire file, 2237 lines), `R/event_model.R` (lines 358-475)
- Why fragile: Complex contrast logic with multiple specification types (numeric, formula-based, pattern-matching). Application involves string pattern matching and basis filtering. Each contrast type has different validation paths.
- Safe modification: Add comprehensive contrast specification validation at construction time. Test all contrast types against all term types. Document contrast naming conventions and pattern syntax thoroughly.
- Test coverage: 898 lines of contrast tests exist, indicating substantial coverage but many edge cases possible given complexity.

## Scaling Limits

**Block-Wise Design Matrix Assembly:**
- Current capacity: Code handles multi-block experiments through block-wise processing.
- Limit: For very large numbers of blocks (100+) or very long blocks (10,000+ timepoints), memory and time complexity could become problematic.
- Scaling path: Profile block processing. Consider out-of-core approaches or chunked processing for very long fMRI runs.

**Number of Event Terms:**
- Current capacity: Design matrix column binding via `do.call(cbind)` can handle typical experimental designs (10-50 terms).
- Limit: Designs with 1000+ columns may become slow due to R's column binding overhead and memory allocation.
- Scaling path: For very large designs, profile the cbind operation and consider lower-level matrix building or sparse matrix optimizations.

## Dependencies at Risk

**Critical Dependency: fmrihrf Package:**
- Risk: Package has strong coupling to `fmrihrf` (137+ internal calls). Version 0.1.0 minimum specified but API stability unknown.
- Impact: Changes to fmrihrf HRF evaluation, sampling_frame structure, or public API could break fmridesign.
- Migration plan: Maintain compatibility shim layer. Document fmrihrf API assumptions. Consider vendoring critical fmrihrf functionality if API instability becomes issue. Test against multiple fmrihrf versions.

**Testing Dependency on Splines:**
- Risk: Several tests skip if splines package not installed, but splines is in base R since R 3.0.
- Impact: Tests may be overly defensive but not critical.
- Migration plan: Remove redundant `skip_if_not_installed("splines")` checks. Simplify test prerequisites.

## Test Coverage Gaps

**Error Condition Coverage:**
- What's not tested: Only 34 tests use `expect_error()`, `expect_warning()`, or `expect_message()` out of 4484+ lines of test code. Error paths are under-tested.
- Files: `tests/testthat/` (all test files)
- Risk: Error handling code and edge cases may fail silently or produce cryptic errors in production use.
- Priority: High - error handling is critical for user experience with complex statistical models.

**Rank Deficiency Edge Cases:**
- What's not tested: Single-level factor handling exists but tests for interactions of multiple rank-deficient dimensions not visible.
- Files: `R/event_vector.R` (special case at lines 1450-1459), tests not comprehensive
- Risk: Subtle naming errors when multiple factors have missing levels simultaneously.
- Priority: High - affects correctness of statistical inference.

**Contrast Application Under Rank Deficiency:**
- What's not tested: Applying contrasts when design matrix has fewer columns than expected (due to rank deficiency) may not be adequately tested.
- Files: `R/contrast.R`, `R/event_model.R` (lines 358-475)
- Risk: Contrasts may be applied to wrong columns or fail silently.
- Priority: High - contrasts are core to hypothesis testing.

**Parallel Processing Feature:**
- What's not tested: No tests for actual parallel execution (feature is non-functional but exists).
- Files: `R/event_model_helpers.R` (lines 271-276)
- Risk: If parallel processing is enabled in future, untested code may have race conditions or resource issues.
- Priority: Medium - feature is currently non-functional.

**Character Variable Handling in HRF:**
- What's not tested: Character columns in event data are not explicitly tested in HRF formula parsing.
- Files: `R/hrf-formula.R` (line 76 TODO), tests don't show char variable tests
- Risk: Unexpected behavior when users pass character instead of factor data.
- Priority: Medium - partial workaround may exist (implicit coercion).

**Per-Block HRF Application:**
- What's not tested: Limited test coverage for per-block HRF specifications (different HRF per block).
- Files: `R/event_vector.R` (lines 1030, 1098, 1164-1175)
- Risk: HRF application may silently use wrong HRF for some blocks.
- Priority: Medium - feature is specialized but critical when used.

---

*Concerns audit: 2026-01-25*
