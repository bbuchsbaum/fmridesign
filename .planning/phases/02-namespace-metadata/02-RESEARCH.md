# Phase 2: Namespace & Metadata - Research

**Researched:** 2026-01-25
**Domain:** R package NAMESPACE and DESCRIPTION metadata
**Confidence:** HIGH

## Summary

Phase 2 focuses on CRAN-compliant namespace management and DESCRIPTION metadata. Research reveals that the fmridesign package currently exports 93 regular functions and 136 S3 methods (229 total exports). Analysis of the codebase identified several exported functions that appear to be internal utilities rather than user-facing APIs. The DESCRIPTION file is mostly compliant but missing the 'cph' (copyright holder) role in Authors@R, which is strongly recommended by CRAN.

The critical dependency fmrihrf is confirmed available on CRAN (version 0.1.0, published 2025-09-16), satisfying REQ-DESC-05. No ::: calls to unexported functions were found (REQ-NS-03 satisfied). The DESCRIPTION Title and Description fields are properly formatted.

**Primary recommendation:** Audit the 93 exported functions to identify internal utilities that should be unexported and marked with @keywords internal, then add 'cph' role to Authors@R field.

## Standard Stack

### Core Tools
| Tool | Version | Purpose | Why Standard |
|------|---------|---------|--------------|
| roxygen2 | 7.3.3 | Documentation & NAMESPACE generation | CRAN standard, automatic S3method detection |
| devtools | latest | Package development workflow | R community standard for package development |
| usethis | latest | Package setup utilities | Companion to devtools, automates common tasks |

### Validation Tools
| Tool | Version | Purpose | When to Use |
|------|---------|---------|-------------|
| devtools::check() | - | R CMD check wrapper | Every phase, identifies NAMESPACE/DESCRIPTION issues |
| spelling::spell_check_package() | - | Spell checking | Already used in Phase 1 |
| urlchecker::url_check() | - | URL validation | Already used in Phase 1 |

**Installation:**
```bash
# Core tools already in Imports/Suggests
R -e "install.packages(c('devtools', 'usethis', 'roxygen2'))"
```

## Architecture Patterns

### NAMESPACE Management Pattern

**Current State:**
```
NAMESPACE (370 lines total):
- 136 S3method() declarations (automatic via roxygen2)
- 93 export() declarations (functions)
- 141 import/importFrom declarations

Total public API: 229 exports
```

### Pattern 1: Minimal Exports Principle

**What:** Export only user-facing functions; keep internal utilities unexported
**When to use:** Always, especially for CRAN packages
**Benefits:**
- Reduces namespace pollution
- Prevents conflicts with other packages
- Clearer API surface for users
- Freedom to refactor internals without breaking changes

**Example classification:**
```r
# USER-FACING (keep exported):
event_model()        # Core constructor
baseline_model()     # Core constructor
design_matrix()      # Key generic
contrasts()          # User queries design
validate_contrasts() # User validation tool

# INTERNAL HELPERS (should be unexported):
sanitize()                      # Naming utility
basis_suffix()                  # Naming utility
feature_suffix()                # Naming utility
make_term_tag()                 # Internal naming logic
column_groups_by_condition()    # Internal design helper
split_by_block()                # Internal utility
```

### Pattern 2: Extension Registry Functions

**What:** Functions supporting external package extensions
**Classification:** These ARE user-facing for package developers extending fmridesign
**Keep exported:**
```r
register_hrfspec_extension()
is_external_hrfspec()
get_external_hrfspec_info()
list_external_hrfspecs()
requires_external_processing()
get_external_hrfspec_functions()
get_all_external_hrf_functions()
```

**Rationale:** External packages (like afnireg) need these to register custom HRF types. These form the extension API.

### Pattern 3: S3 Method Export

**What:** roxygen2 automatically generates S3method() declarations
**Current practice:** Use @export tag, roxygen2 detects S3 methods automatically

**Example from codebase:**
```r
#' @export
design_matrix.event_model <- function(x, ...) {
  # Implementation
}
```

Generates in NAMESPACE:
```
S3method(design_matrix,event_model)
```

**Anti-pattern to avoid:**
- Don't manually edit NAMESPACE (roxygen2 manages it)
- Don't forget @export on S3 methods (roxygen2 will warn)

### Pattern 4: Internal Function Marking

**What:** Mark non-exported functions with @keywords internal and @noRd
**Example:**
```r
#' Internal helper for zero-padding numbers
#' @keywords internal
#' @noRd
zeropad <- function(i, n_total) {
  # Implementation
}
```

**Current compliance:** 14 files already use @keywords internal, but several exported functions should have it

### DESCRIPTION Authors@R Pattern

**Current:**
```
Authors@R: person("Bradley", "Buchsbaum", role = c("aut", "cre"),
                  email = "brad.buchsbaum@gmail.com")
```

**CRAN-recommended pattern:**
```
Authors@R: person("Bradley", "Buchsbaum",
                  role = c("aut", "cre", "cph"),
                  email = "brad.buchsbaum@gmail.com")
```

**Role definitions:**
- `aut`: Author (made significant contributions)
- `cre`: Creator/maintainer (current maintainer, exactly one required)
- `cph`: Copyright holder (legal holder of copyright)

**When to use 'cph':** Always for individual package authors unless copyright is assigned to an employer/institution (in which case add separate person() entry for the institution)

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| NAMESPACE generation | Manual editing | roxygen2 @export tags | Automatic, correct S3method detection |
| DESCRIPTION validation | Custom checks | devtools::check() | CRAN-compliant checks built-in |
| Package structure setup | Manual file creation | usethis::use_* functions | Correct templates, avoids errors |
| Copyright/licensing | Custom license text | usethis::use_*_license() | Legal correctness, CRAN compliance |

**Key insight:** NAMESPACE management is complex with S3 methods. Roxygen2 handles the subtleties (S3method vs export, generic detection, method registration). Manual editing introduces errors.

## Common Pitfalls

### Pitfall 1: Over-Exporting Internal Utilities

**What goes wrong:** Utility functions like `sanitize()`, `basis_suffix()`, `feature_suffix()` are exported when they should be internal-only

**Why it happens:**
- Functions have roxygen documentation with @export
- They're used across multiple files
- Initially exported "just in case"

**How to avoid:**
1. Ask: "Would a user call this directly?"
2. If no, remove @export and add @keywords internal, @noRd
3. Functions can still be used across package files without export

**Warning signs:**
- Function name suggests utility (e.g., `make_*`, `check_*`, `sanitize`)
- Function only useful in context of other package functions
- Function manipulates internal data structures

**Impact:** Exported count for fmridesign: 93 functions. Expected for this domain: 40-60. Too many exports clutter user namespace.

### Pitfall 2: Forgetting 'cph' Role

**What goes wrong:** DESCRIPTION Authors@R doesn't include 'cph' (copyright holder) role

**Why it happens:**
- Template only includes 'aut' and 'cre'
- Copyright holder role seems redundant when author is maintainer

**How to avoid:**
- Always add 'cph' for individual authors
- If employer owns copyright, add separate person() entry with role 'cph'

**Warning signs:**
- devtools::check() may not flag this (it's "strongly recommended" not mandatory)
- CRAN reviewers may request addition

**Impact:** May delay CRAN acceptance, requires resubmission

### Pitfall 3: Using ::: in Package Code

**What goes wrong:** Package code uses `package:::internal_function()` to access unexported functions

**Why it happens:**
- Need to access internal function from another package
- Temporary workaround becomes permanent

**How to avoid:**
- Never use ::: in package code (CRAN rejects this)
- Request package maintainer to export needed function
- Copy/vendor the function with attribution
- Find alternative approach

**Warning signs:**
- `grep ':::' R/*.R` finds matches
- Package depends on internal implementation of another package

**Impact:** CRAN automatic rejection

**Current status for fmridesign:** ✓ No ::: calls found

### Pitfall 4: S3 Methods Not Auto-Detected

**What goes wrong:** Function name has dots but isn't detected as S3 method, or vice versa

**Why it happens:**
- Function named `foo.bar()` but isn't a method for class 'bar'
- Generic not imported/visible when roxygen2 runs

**How to avoid:**
- Use roxygen2's automatic detection (just @export)
- For ambiguous cases, use @exportS3Method generic class explicitly
- Import generics that methods extend

**Warning signs:**
- roxygen2 warnings about S3 methods
- devtools::check() "Undocumented S3 methods" NOTE
- NAMESPACE has export() instead of S3method()

**Current status for fmridesign:** 136 S3method() entries suggest good detection. Verify during audit.

## Code Examples

### Properly Marking Internal Functions

```r
# From naming-utils.R (currently exported, should be internal)

#' Internal helper for zero-padding numbers
#' @param i Integer to pad
#' @param n_total Total number to determine width
#' @return Zero-padded string
#' @keywords internal
#' @noRd
zeropad <- function(i, n_total) {
  log_width <- if (n_total < 1) 1L else ceiling(log10(n_total + 1e-9))
  final_width <- if (n_total > 1) max(2L, as.integer(log_width)) else as.integer(log_width)
  sprintf(paste0("%0", final_width, "d"), i)
}
```

**Note:** Remove @export tag to make internal. @keywords internal + @noRd suppress documentation generation.

### Proper User-Facing Function Documentation

```r
# From naming-utils.R (correctly exported)

#' Sanitize Strings for Use in R Names
#'
#' Wraps `make.names` but allows control over dot replacement.
#'
#' @param x A character vector.
#' @param allow_dot Logical, if `FALSE`, dots (`.`) are replaced with underscores (`_`).
#' @return A sanitized character vector.
#' @export
#' @examples
#' sanitize("a.b c")
#' sanitize("a.b c", allow_dot = FALSE)
sanitize <- function(x, allow_dot = TRUE) {
  # Implementation
}
```

**Audit decision:** This is exported, but is it user-facing? Users working with custom design matrices might need it. Borderline case - recommend reviewing with maintainer.

### Extension API Pattern

```r
# From extension_registry.R (correctly exported)

#' Register an External HRF Specification Type
#'
#' Register a new HRF specification class that can be used in event models.
#' This allows external packages to extend fmridesign with their own HRF types.
#'
#' @param spec_class Character string naming the class to register
#' @param package Character string naming the package providing the class
#' @param convolved_class Optional character string naming the associated convolved term class
#' @param requires_external_processing Logical indicating if this spec should be skipped
#'   during standard convolution (e.g., for AFNI terms that are processed externally)
#' @param formula_functions Optional character vector of function names that should
#'   be recognised in formulas and mapped to this HRF specification class.
#'
#' @return Invisible NULL
#' @export
register_hrfspec_extension <- function(spec_class, package,
                                       convolved_class = NULL,
                                       requires_external_processing = FALSE,
                                       formula_functions = NULL) {
  # Implementation
}
```

**Keep exported:** This is the extension API for other packages.

### Authors@R with Copyright Holder

```r
# In DESCRIPTION file

# Current (missing 'cph'):
Authors@R: person("Bradley", "Buchsbaum", role = c("aut", "cre"),
                  email = "brad.buchsbaum@gmail.com")

# Recommended (includes 'cph'):
Authors@R: person("Bradley", "Buchsbaum",
                  role = c("aut", "cre", "cph"),
                  email = "brad.buchsbaum@gmail.com")

# If employer holds copyright:
Authors@R: c(
    person("Bradley", "Buchsbaum",
           role = c("aut", "cre"),
           email = "brad.buchsbaum@gmail.com"),
    person("Institution Name",
           role = "cph")
)
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| Manual NAMESPACE editing | roxygen2 @export tags | ~2013 (roxygen2 3.0) | Automatic S3 detection, fewer errors |
| Author() in DESCRIPTION | Authors@R with roles | ~2015 (R 3.2) | Structured metadata, ORCID support |
| Exporting everything | Minimal exports | Ongoing evolution | Smaller API surface, less namespace pollution |
| @exportMethod for S3 | @export (auto-detects) | ~2018 (roxygen2 6.0) | Simpler, one tag for all exports |

**Deprecated/outdated:**
- Manual NAMESPACE: Don't edit directly, use roxygen2
- @exportMethod: Use @export, roxygen2 auto-detects S3/S4
- Author() field: Use Authors@R instead

## Open Questions

### Question 1: Which exported functions are truly user-facing?

**What we know:**
- 93 functions currently exported
- Naming conventions suggest some are internal (sanitize, basis_suffix, feature_suffix, etc.)
- Extension registry functions (7 total) ARE part of extension API

**What's unclear:**
- Maintainer's intent for borderline cases (e.g., sanitize, condition_basis_list)
- Whether any users depend on currently-exported internal functions
- Trade-off between API stability and namespace cleanliness

**Recommendation:** Audit all 93 exports with classification:
1. KEEP: Core user-facing API
2. KEEP: Extension API for package developers
3. REMOVE: Clear internal utilities
4. DISCUSS: Borderline cases (get maintainer input)

### Question 2: Should specialized constructors remain exported?

**What we know:**
- `event_matrix()`, `event_basis()`, `event_factor()`, `event_variable()` are exported
- These are specialized constructors for event objects
- Main entry point is `event_term()` or `event_model()`

**What's unclear:**
- Are advanced users using these directly?
- Are these part of the documented API surface?

**Recommendation:** Check if these appear in documentation/vignettes. If not user-documented, consider unexporting.

### Question 3: Are low-level HRF generators user-facing?

**What we know:**
- `boxcar_hrf_gen()` and `weighted_hrf_gen()` are exported
- These return generator functions for custom HRF creation
- Used internally by trialwise() and other HRF functions

**What's unclear:**
- Do users build custom HRF generators using these?
- Are they documented as user API?

**Recommendation:** Review documentation. If advanced feature, keep exported with clear docs. If internal, unexport.

## Sources

### Primary (HIGH confidence)
- [Managing imports and exports • roxygen2](https://roxygen2.r-lib.org/articles/namespace.html) - Official roxygen2 documentation on NAMESPACE
- [DESCRIPTION File Issues – The CRAN Cookbook](https://contributor.r-project.org/cran-cookbook/description_issues.html) - CRAN's official guidance on DESCRIPTION
- [CRAN: Package fmrihrf](https://cran.r-project.org/web/packages/fmrihrf/fmrihrf.pdf) - Confirms fmrihrf 0.1.0 on CRAN (2025-09-16)
- [R: Persons](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/person.html) - Official R documentation on person() and roles

### Secondary (MEDIUM confidence)
- [Who Did What? The Roles of R Package Authors and How to Refer to Them](https://journal.r-project.org/articles/RJ-2012-009/) - R Journal article on author roles
- [9 DESCRIPTION – R Packages (2e)](https://r-pkgs.org/description.html) - Hadley Wickham's R Packages book
- [Chapter 11 Namespace | R Packages](https://bookdown.dongzhuoer.com/hadley/r-pkgs/namespace) - R Packages book on namespace

### Tertiary (LOW confidence)
- None - all findings verified with official sources

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - roxygen2/devtools are established standards
- Architecture: HIGH - Patterns verified in official roxygen2 docs
- Pitfalls: HIGH - Based on CRAN documentation and codebase analysis
- Function classification: MEDIUM - Requires maintainer input on intent

**Research date:** 2026-01-25
**Valid until:** 60 days (namespace/DESCRIPTION practices stable, slow-changing domain)

**Current state summary:**
- NAMESPACE: 93 exports + 136 S3 methods = 229 total
- No ::: calls found ✓
- fmrihrf on CRAN ✓
- DESCRIPTION Title/Description properly formatted ✓
- Authors@R missing 'cph' role (easily fixed)
- Likely 20-40 functions that should be unexported (requires audit)
