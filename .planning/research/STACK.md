# Technology Stack for CRAN Submission

**Project:** fmridesign v0.5.0
**Researched:** 2025-01-25
**Context:** Brownfield R package preparing for CRAN submission

## Executive Summary

The 2025 R package CRAN submission workflow has consolidated around the **r-lib ecosystem** (devtools, usethis, rcmdcheck) with **rhub v2** (GitHub Actions-based) replacing the old rhub v1 service. The modern workflow emphasizes automation via GitHub Actions, pre-submission validation with multiple tools, and systematic checking across platforms.

**Key shift in 2025:** rhub v2 now uses GitHub Actions instead of dedicated servers, requiring package repositories to be on GitHub.

---

## Core Development Tools

### devtools (v2.4.6+)
| Aspect | Details |
|--------|---------|
| **Version** | 2.4.6 (October 2025) |
| **Purpose** | Orchestrates package development workflow |
| **Why Essential** | Meta-package that coordinates all other tools; standard interface for check/build/test |
| **Key Functions** | `check()`, `build()`, `install()`, `test()`, `document()`, `release()` |
| **Installation** | `install.packages("devtools")` |
| **Confidence** | HIGH - Official tidyverse tool, actively maintained |

**Rationale:** devtools has "uncoupled" into focused packages (see below) but remains the standard entry point. Use `devtools::check(args = c('--as-cran'))` for CRAN-equivalent checking locally.

**Current architecture:** devtools now delegates to specialized packages:
- `testthat` - testing
- `roxygen2` - documentation
- `pkgbuild` - building binaries
- `pkgload` - simulating package loading
- `rcmdcheck` - running R CMD check
- `usethis` - project setup

### usethis (v3.2.1+)
| Aspect | Details |
|--------|---------|
| **Version** | 3.2.1+ (required by devtools 2.4.6) |
| **Purpose** | Automates package setup and configuration |
| **Why Essential** | Generates release checklists, configures GitHub Actions, sets up infrastructure |
| **Key Functions** | `use_release_issue()`, `use_github_action()`, `use_spell_check()`, `use_pkgdown_github_pages()` |
| **Installation** | `install.packages("usethis")` (auto-installed with devtools) |
| **Confidence** | HIGH - r-lib official tool |

**Rationale:** `usethis::use_release_issue()` generates a comprehensive, package-specific CRAN submission checklist as a GitHub issue. This is the canonical checklist for 2025, replacing static checklists.

**Critical for CRAN prep:**
- `use_release_issue()` - Creates submission checklist
- `use_spell_check()` - Configures spelling package
- `use_github_action("check-standard")` - Sets up multi-platform CI

### rcmdcheck
| Aspect | Details |
|--------|---------|
| **Version** | Latest on CRAN |
| **Purpose** | Runs R CMD check from R and captures results programmatically |
| **Why Essential** | Powers devtools::check(), enables CI/CD integration |
| **Key Functions** | `rcmdcheck()` |
| **Installation** | `install.packages("rcmdcheck")` (auto-installed with devtools) |
| **Confidence** | HIGH - r-lib tool, underlies devtools |

**Rationale:** While you'll typically use `devtools::check()`, rcmdcheck is what actually runs the check. Understanding this helps debug CI failures.

---

## CRAN Pre-Submission Validation

### R CMD check --as-cran (Built-in)
| Aspect | Details |
|--------|---------|
| **Purpose** | Official CRAN validation - the ultimate gatekeeper |
| **Why Essential** | Required to pass for CRAN acceptance (0 ERRORS, 0 WARNINGS, minimal NOTEs) |
| **How to Run** | `devtools::check(args = c('--as-cran'))` or `R CMD check --as-cran pkg.tar.gz` |
| **Confidence** | HIGH - Official R tooling |

**Rationale:** The `--as-cran` flag performs additional checks beyond standard R CMD check. This is non-negotiable for CRAN submission.

**Critical flags:**
- `--as-cran` - Enables CRAN-specific checks
- `--run-donttest` - Runs \donttest{} examples (R 4.0+ does this automatically with --as-cran)

**Requirements:**
- 0 ERRORS
- 0 WARNINGS
- Minimal NOTEs (ideally 0, but some are unavoidable and acceptable if documented)

### goodpractice (v1.0.5)
| Aspect | Details |
|--------|---------|
| **Version** | 1.0.5 (June 2024) |
| **Purpose** | Comprehensive package quality checker |
| **Why Essential** | Bundles rcmdcheck + covr + lintr + cyclocomp; catches issues R CMD check misses |
| **Key Functions** | `goodpractice()` or `gp()` |
| **Installation** | `install.packages("goodpractice")` |
| **Confidence** | MEDIUM - Community tool (rOpenSci), not official but widely used |

**Rationale:** Performs ~230+ checks covering CRAN policies, code quality, documentation completeness. Maintained by rOpenSci review-tools.

**What it checks:**
- R CMD check via rcmdcheck
- Code coverage via covr
- Code linting via lintr
- Cyclomatic complexity via cyclocomp
- CRAN policy compliance
- Documentation completeness

**Usage:**
```r
library(goodpractice)
gp(".")  # Run on current package
```

### checkhelper (v0.2.0+)
| Aspect | Details |
|--------|---------|
| **Version** | Latest on CRAN (released Dec 2023) |
| **Purpose** | Detect CRAN-specific documentation issues |
| **Why Essential** | Finds missing @return/@noRd tags that cause CRAN rejections |
| **Key Functions** | `find_missing_tags()`, `check_clean_userspace()` |
| **Installation** | `install.packages("checkhelper")` |
| **Confidence** | MEDIUM - ThinkR community tool, CRAN-specific focus |

**Rationale:** Specifically addresses common CRAN rejection causes that R CMD check may not flag clearly.

**Key checks:**
- `find_missing_tags()` - Detects exported functions missing @return or @noRd
- `check_clean_userspace()` - Detects files created during examples/tests/vignettes that violate CRAN policy

**Usage:**
```r
library(checkhelper)
find_missing_tags(".")
check_clean_userspace()
```

---

## Platform-Specific Checking

### rhub v2 (v2.0.1+)
| Aspect | Details |
|--------|---------|
| **Version** | 2.0.1 (March 2025) |
| **Purpose** | Multi-platform R CMD check via GitHub Actions |
| **Why Essential** | Tests on Linux/Windows/macOS, multiple R versions, without owning those machines |
| **Key Functions** | `rhub_setup()`, `rhub_check()` |
| **Installation** | `install.packages("rhub")` |
| **Confidence** | HIGH - Official R Consortium service, complete rewrite in 2024 |

**CRITICAL CHANGE in v2:** rhub v2 is a **complete rewrite** using GitHub Actions. Old rhub v1 functions are deprecated/defunct.

**How it works:**
1. **Setup:** `rhub::rhub_setup()` adds GitHub Actions workflow to your repo
2. **Check:** `rhub::rhub_check()` triggers workflow on GitHub using your GitHub account
3. **Results:** View on GitHub Actions tab

**Platforms available:**
- `linux` - Ubuntu with various R versions
- `windows` - Windows Server 2022
- `macos` - macOS (Intel and Apple Silicon)
- Multiple R versions: R-devel, R-release, R-oldrel

**Advantages over old rhub:**
- No queue delays (uses your GitHub Actions minutes)
- Full CI/CD integration
- Customizable workflows
- Free for public repos (GitHub Actions free tier)

**Requirements:**
- Package must be on GitHub
- GitHub Actions enabled

**Alternative (if not on GitHub):**
- Use `rc_*()` functions to run on R Consortium shared runners at https://github.com/r-hub2

**Usage:**
```r
# One-time setup
rhub::rhub_setup()  # Adds .github/workflows/rhub.yaml

# Run checks
rhub::rhub_check()  # Prompts for platform selection
rhub::rhub_check(platforms = c("linux", "windows", "macos"))
```

### win-builder (Official CRAN Service)
| Aspect | Details |
|--------|---------|
| **Service** | https://win-builder.r-project.org/ |
| **Purpose** | Test on Windows with CRAN's exact setup |
| **Why Essential** | Windows CRAN machines are identical to win-builder; required for Windows compatibility verification |
| **Maintained By** | Uwe Ligges (CRAN Windows maintainer) |
| **Confidence** | HIGH - Official CRAN infrastructure |

**Rationale:** "Probably has the most recent CRAN checks activated" - tests on the exact Windows environment CRAN uses.

**Platforms:**
- R-devel (development version)
- R-release (current release)
- R-oldrel (previous release)

**Hardware (as of Oct 2025):**
- R-oldrel: Windows Server 2022, 2x Intel Xeon E5-2680 v4 (14 cores, 2.4 GHz), 192GB RAM
- R-devel/release: Windows Server 2022, 2x AMD EPYC 7443 (24 cores, 2.85 GHz), 256GB RAM

**How to use:**

*Option 1: devtools (recommended)*
```r
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()
```

*Option 2: Web upload*
- Visit https://win-builder.r-project.org/upload.aspx
- Upload source package (.tar.gz)

*Option 3: FTP*
- Upload to ftp://win-builder.r-project.org

**Timeline:** Results emailed to maintainer in ~30 minutes.

**When to use:** Before CRAN submission to catch Windows-specific issues. Not required but strongly recommended.

### mac-builder (Official CRAN Service)
| Aspect | Details |
|--------|---------|
| **Service** | https://mac.r-project.org/macbuilder/submit.html |
| **Purpose** | Test on macOS (Intel + Apple Silicon) with CRAN's exact setup |
| **Why Essential** | Tests on both x86_64 and arm64 architectures |
| **Maintained By** | R for macOS development team |
| **Confidence** | HIGH - Official CRAN infrastructure |

**Rationale:** Similar to win-builder but for macOS. Especially important for packages with compiled code (C/C++/Fortran).

**Build Environment (2025):**
- Xcode 14.2/14.3
- macOS 11.3 SDK
- Target: macOS 11 (Big Sur)
- GNU Fortran 14.2 (for R 4.5.0)
- Both architectures: x86_64 (Intel) and arm64 (Apple Silicon)

**How to use:**

*Option 1: devtools*
```r
devtools::check_mac_release()
```

*Option 2: Web upload*
- Visit https://mac.r-project.org/macbuilder/submit.html
- Upload source package

**When to use:** If your package has compiled code or dependencies with compiled code. Less critical than win-builder for pure R packages.

---

## Documentation Quality

### spelling (v2.3.1+)
| Aspect | Details |
|--------|---------|
| **Version** | Latest dated August 2025 |
| **Purpose** | Spell-check documentation, vignettes, DESCRIPTION |
| **Why Essential** | Spelling errors are unprofessional and may cause CRAN delays |
| **Key Functions** | `spell_check_package()`, `spell_check_setup()`, `update_wordlist()` |
| **Installation** | `install.packages("spelling")` |
| **Confidence** | HIGH - rOpenSci tool, widely used |

**Rationale:** Automatically parses Rd files, Rmd vignettes, DESCRIPTION fields and spell-checks text (not code). Uses hunspell engine.

**Setup workflow:**
```r
# One-time setup
usethis::use_spell_check()  # Adds spelling to Suggests, creates WORDLIST

# Check spelling
spelling::spell_check_package()

# Update WORDLIST with custom terms
spelling::update_wordlist()
```

**Configuration:**
- Specify language in DESCRIPTION: `Language: en-US` or `Language: en-GB`
- Custom terms go in `inst/WORDLIST` file
- `spell_check_setup()` adds unit test that runs during R CMD check (never fails, just warns)

**What it checks:**
- Package DESCRIPTION file (Title, Description fields)
- Rd documentation files (man/*.Rd)
- Vignettes (.Rmd, .Rnw)

### urlchecker
| Aspect | Details |
|--------|---------|
| **Version** | Latest on CRAN |
| **Purpose** | Validate URLs in documentation, catch broken/redirected links |
| **Why Essential** | Invalid URLs slow CRAN submission; redirected URLs should be updated |
| **Key Functions** | `url_check()`, `url_update()` |
| **Installation** | `install.packages("urlchecker")` |
| **Confidence** | HIGH - r-lib tool, addresses common CRAN issue |

**Rationale:** CRAN checks all URLs and will delay/reject packages with broken links. urlchecker finds these before submission.

**What it checks:**
- URLs in .Rd files (documentation)
- URLs in DESCRIPTION
- URLs in vignettes
- Redirected URLs (suggests updating to final destination)

**Usage:**
```r
library(urlchecker)
url_check()  # Check all URLs in package
url_update()  # Interactively update redirected URLs
```

**When to use:** Run before every CRAN submission. Also useful to run via GitHub Actions to catch URL rot over time.

---

## Code Quality

### lintr (v3.3.0-1)
| Aspect | Details |
|--------|---------|
| **Version** | 3.3.0-1 (November 2025) |
| **Purpose** | Static code analysis, style checking |
| **Why Essential** | Catches code smells, enforces consistent style, finds potential bugs |
| **Key Functions** | `lint_package()`, `lint_dir()`, `lint()` |
| **Installation** | `install.packages("lintr")` |
| **Confidence** | HIGH - r-lib tool, actively maintained |

**Rationale:** Performs static analysis to detect syntax errors, semantic issues, style violations. Default config follows tidyverse style guide.

**What it checks:**
- Style violations (spacing, indentation, naming)
- Potential bugs (T/F instead of TRUE/FALSE, missing closures)
- Code smells (unused variables, overly complex functions)
- Best practices violations

**Configuration:**
Create `.lintr` file in project root (Debian control format):
```
linters: linters_with_defaults(line_length_linter(120))
exclusions: list("tests/testthat.R")
```

**IDE Integration:**
- RStudio: On-the-fly linting
- VS Code: R extension
- Emacs, Vim, Sublime: Plugins available

**Usage:**
```r
library(lintr)
lint_package()  # Lint entire package
```

**Companion tool:** `styler` package can auto-fix many lintr findings.

### styler (v1.11.0)
| Aspect | Details |
|--------|---------|
| **Version** | 1.11.0 (October 2025) |
| **Purpose** | Automatic code formatting to tidyverse style |
| **Why Essential** | Automatically fixes style issues lintr detects |
| **Key Functions** | `style_pkg()`, `style_file()`, `style_dir()`, `style_text()` |
| **Installation** | `install.packages("styler")` |
| **Confidence** | HIGH - r-lib tool, complements lintr |

**Rationale:** Non-invasive formatting that transforms code to tidyverse style without changing behavior. "Style guide enforcement as code."

**File support:**
- .R (R scripts)
- .Rmd (R Markdown)
- .qmd (Quarto)
- .Rnw (Sweave)
- .Rmarkdown
- .Rprofile

**Key features:**
- **Dry mode:** Preview changes without writing (`dry = "on"`)
- **Cache:** Speeds up re-styling by caching already-styled code
- **Custom style guides:** Not limited to tidyverse style
- **RStudio Addins:** Style selected code or active file

**Usage:**
```r
library(styler)
style_pkg()  # Style entire package
style_pkg(dry = "on")  # Preview without changing files
```

**Workflow:** Run `styler::style_pkg()`, then `lintr::lint_package()` to verify.

### covr (Latest)
| Aspect | Details |
|--------|---------|
| **Version** | Latest on CRAN (November 2025) |
| **Purpose** | Test coverage measurement and reporting |
| **Why Essential** | Quantifies test completeness, identifies untested code paths |
| **Key Functions** | `package_coverage()`, `file_coverage()`, `report()` |
| **Installation** | `install.packages("covr")` |
| **Confidence** | HIGH - r-lib tool, standard for coverage |

**Rationale:** While CRAN doesn't require specific coverage levels, coverage measurement helps ensure test quality. Used by goodpractice.

**What it tracks:**
- R code coverage
- Compiled code coverage (C/C++/Fortran)
- Branch coverage
- Function coverage

**Reporting:**
- Interactive HTML report: `covr::report()`
- Codecov integration: `covr::codecov()`
- Coveralls integration: `covr::coveralls()`

**Usage:**
```r
library(covr)
cov <- package_coverage()
report(cov)  # Interactive HTML report
```

**CI Integration:**
```r
# In GitHub Actions, see use_github_action("test-coverage")
usethis::use_github_action("test-coverage")
```

**CRAN relevance:** Not directly required, but high coverage correlates with better-tested packages less likely to have issues.

---

## GitHub Actions CI/CD

### r-lib/actions (v2)
| Aspect | Details |
|--------|---------|
| **Repository** | https://github.com/r-lib/actions |
| **Purpose** | Reusable GitHub Actions workflows for R packages |
| **Why Essential** | Automates multi-platform checking, coverage, pkgdown deployment |
| **Key Workflows** | `check-standard`, `test-coverage`, `pkgdown` |
| **Setup** | `usethis::use_github_action("workflow-name")` |
| **Confidence** | HIGH - Official r-lib workflows |

**Rationale:** Pre-configured GitHub Actions that replicate CRAN checks across platforms. The modern standard for R package CI/CD.

**Available Workflows:**

| Workflow | Purpose | When to Use |
|----------|---------|-------------|
| `check-standard` | R CMD check on Linux/Mac/Windows, multiple R versions | Always - baseline for CRAN prep |
| `test-coverage` | Compute coverage and upload to Codecov | If using test coverage tracking |
| `pkgdown` | Build and deploy pkgdown site to GitHub Pages | If package has pkgdown site |
| `pr-commands` | Enable /document and /style commands in PRs | For collaborative development |
| `check-release` | R CMD check on latest R only | Lighter alternative to check-standard |
| `lint` | Run lintr on package | If enforcing code style |
| `style` | Run styler on package | If auto-formatting code |

**Setup for CRAN preparation:**
```r
# Recommended: Check on multiple platforms and R versions
usethis::use_github_action("check-standard")

# Optional but recommended
usethis::use_github_action("test-coverage")
usethis::use_github_action("pkgdown")
```

**check-standard details:**
- Tests on: Linux, macOS, Windows
- R versions: R-devel, R-release, R-oldrel
- Mimics CRAN's multi-platform testing

**Key Actions (reusable components):**
- `setup-r` - Install R
- `setup-r-dependencies` - Install package dependencies
- `check-r-package` - Run R CMD check
- `setup-pandoc` - Install Pandoc

**v2 improvements (2022):**
- `working-directory` parameter for monorepos
- Works on all x86_64 Linux distros
- Better dependency caching

---

## Website/Documentation

### pkgdown (Latest)
| Aspect | Details |
|--------|---------|
| **Version** | Latest on CRAN |
| **Purpose** | Generate static website from package documentation |
| **Why Essential** | Creates user-friendly HTML docs from Rd files; standard for R packages |
| **Key Functions** | `build_site()`, `build_reference()`, `build_articles()` |
| **Installation** | `install.packages("pkgdown")` |
| **Confidence** | HIGH - r-lib tool, widely used |

**Rationale:** Automatically converts package documentation into a searchable website. Increasingly expected for modern R packages.

**What it generates:**
- Homepage from README.md
- Function reference from man/*.Rd
- Articles from vignettes/
- Changelog from NEWS.md
- Search functionality

**Setup:**
```r
# Create pkgdown config and site
usethis::use_pkgdown()

# Build site locally
pkgdown::build_site()

# Set up GitHub Pages with automated deployment
usethis::use_pkgdown_github_pages()
```

**GitHub Actions integration:**
- `use_pkgdown_github_pages()` sets up automatic deployment
- Site updates on every push to main/master
- Published to https://username.github.io/packagename/

**Configuration:**
- `_pkgdown.yml` - Customize site appearance, navigation
- Supports custom themes, templates, additional pages

**For CRAN:** Not required, but URL in DESCRIPTION should work. Having a pkgdown site demonstrates documentation quality.

---

## Optional but Recommended

### pkgcheck (rOpenSci)
| Aspect | Details |
|--------|---------|
| **Repository** | https://github.com/ropensci-review-tools/pkgcheck |
| **Purpose** | rOpenSci peer review compliance checking |
| **Why Relevant** | Comprehensive quality checks beyond CRAN requirements |
| **Key Functions** | `pkgcheck()`, GitHub Action available |
| **Installation** | `remotes::install_github("ropensci-review-tools/pkgcheck")` |
| **Confidence** | MEDIUM - rOpenSci tool, not for CRAN specifically but high quality |

**Rationale:** While focused on rOpenSci peer review, pkgcheck performs extensive quality validation that exceeds CRAN requirements. Useful for catching issues.

**Usage:**
```r
# Not on CRAN, install from GitHub
remotes::install_github("ropensci-review-tools/pkgcheck")

pkgcheck::pkgcheck(".")
```

**GitHub Action:**
```r
pkgcheck::use_github_action_pkgcheck()
```

**When to use:** If seeking very high quality standards or planning rOpenSci submission. Overkill for basic CRAN submission but catches many subtle issues.

### foghorn
| Aspect | Details |
|--------|---------|
| **Version** | Latest on CRAN |
| **Purpose** | Summarize CRAN check results and win-builder queue |
| **Why Relevant** | Monitor CRAN check status after submission |
| **Key Functions** | `cran_results()`, `winbuilder_queue()` |
| **Installation** | `install.packages("foghorn")` |
| **Confidence** | MEDIUM - Community tool |

**Rationale:** After CRAN submission, use foghorn to monitor check results across platforms without visiting CRAN web page.

**Usage:**
```r
library(foghorn)
cran_results("fmridesign")  # Check results for your package
winbuilder_queue()  # See win-builder queue length
```

**When to use:** Post-submission monitoring. Not essential for pre-submission prep.

---

## Installation Workflow

For a package preparing for CRAN submission, install this stack:

```r
# Core tools (likely already installed)
install.packages(c(
  "devtools",     # Meta-package, includes usethis/rcmdcheck
  "roxygen2",     # Documentation (if not already present)
  "testthat",     # Testing (if not already present)
  "knitr"         # Vignettes (if not already present)
))

# CRAN validation
install.packages(c(
  "goodpractice",  # Comprehensive quality checks
  "checkhelper",   # CRAN-specific doc checks
  "rhub",          # Multi-platform testing (v2)
  "spelling",      # Spell checking
  "urlchecker"     # URL validation
))

# Code quality (optional but recommended)
install.packages(c(
  "lintr",         # Static analysis
  "styler",        # Auto-formatting
  "covr"           # Coverage
))

# Website (if using pkgdown)
install.packages("pkgdown")

# Post-submission monitoring (optional)
install.packages("foghorn")
```

---

## Pre-Submission Checklist (2025)

The canonical checklist is generated by `usethis::use_release_issue()`, but here's a manual workflow:

### 1. Local Checks
```r
# Spell check
spelling::spell_check_package()

# URL check
urlchecker::url_check()

# Code style (optional)
styler::style_pkg()
lintr::lint_package()

# R CMD check
devtools::check(args = c('--as-cran'))

# Comprehensive quality
goodpractice::gp(".")

# CRAN-specific checks
checkhelper::find_missing_tags(".")
checkhelper::check_clean_userspace()
```

### 2. Multi-Platform Checks
```r
# GitHub Actions (if set up)
# Push to GitHub, check Actions tab

# rhub v2
rhub::rhub_check(platforms = c("linux", "windows", "macos"))

# Windows
devtools::check_win_devel()
devtools::check_win_release()

# macOS (if compiled code)
devtools::check_mac_release()
```

### 3. Documentation
```r
# Update documentation
devtools::document()

# Build and check website (if using pkgdown)
pkgdown::build_site()

# Verify README, NEWS, DESCRIPTION
```

### 4. Version and Submission
```r
# Update version number in DESCRIPTION
# Update NEWS.md
# Update cran-comments.md

# Final check
devtools::check(args = c('--as-cran'))

# Submit (creates cran-comments.md if needed)
devtools::release()
# or
devtools::submit_cran()
```

---

## Confidence Assessment

| Category | Tool(s) | Confidence | Rationale |
|----------|---------|------------|-----------|
| Core Development | devtools, usethis, rcmdcheck | HIGH | Official r-lib tools, actively maintained, industry standard |
| Platform Testing | rhub v2, win-builder, mac-builder | HIGH | Official services (win/mac-builder are CRAN infrastructure) |
| CRAN Validation | R CMD check --as-cran | HIGH | Built into R, official CRAN requirement |
| Quality Checking | goodpractice | MEDIUM | Community tool (rOpenSci), widely used but not official |
| Documentation | spelling, urlchecker | HIGH | r-lib tools, address specific CRAN requirements |
| Code Quality | lintr, styler, covr | HIGH | r-lib tools, well-established |
| CI/CD | r-lib/actions | HIGH | Official r-lib GitHub Actions |
| Website | pkgdown | HIGH | r-lib tool, standard for R packages |
| Domain-Specific | checkhelper | MEDIUM | ThinkR community tool, CRAN-focused but newer |

---

## Key Changes Since Previous Versions

### rhub v2 (2024 Rewrite)
- **OLD:** rhub v1 used dedicated R-hub servers, functions like `check()`, `check_on_windows()`, etc.
- **NEW:** rhub v2 uses GitHub Actions, requires GitHub repo, functions are `rhub_setup()`, `rhub_check()`
- **Migration:** Old functions are deprecated/defunct, removed in next version
- **Impact:** Must have package on GitHub to use rhub v2; alternative is shared R Consortium runners via `rc_*()`

### devtools "Uncoupling"
- **OLD:** devtools was monolithic
- **NEW:** Functionality split into focused packages (rcmdcheck, pkgload, pkgbuild, etc.)
- **Impact:** Installing devtools still gets you everything, but you can use components independently

### usethis Release Checklist
- **OLD:** Static checklists on websites
- **NEW:** `usethis::use_release_issue()` generates package-specific checklist as GitHub issue
- **Impact:** Checklist adapts to your package (detects CRAN status, GitHub setup, etc.)

---

## Platform Coverage Matrix

| Platform | Local Check | GitHub Actions | rhub v2 | win-builder | mac-builder |
|----------|-------------|----------------|---------|-------------|-------------|
| Linux (your OS) | devtools::check() | check-standard | Yes | N/A | N/A |
| Windows | No (unless on Windows) | check-standard | Yes | Yes (official) | N/A |
| macOS | No (unless on macOS) | check-standard | Yes | N/A | Yes (official) |
| R-devel | No (unless installed) | check-standard | Yes | Yes | Limited |
| R-release | Yes | check-standard | Yes | Yes | Yes |
| R-oldrel | No (unless installed) | check-standard | Yes | Yes | No |

**Recommendation:** Use GitHub Actions `check-standard` + win-builder for comprehensive coverage.

---

## Workflow Integration

### Recommended GitHub Actions Setup
```r
# One-time setup
usethis::use_github_action("check-standard")  # Multi-platform R CMD check
usethis::use_github_action("test-coverage")   # Coverage tracking
usethis::use_pkgdown_github_pages()           # Website deployment
```

This creates `.github/workflows/`:
- `R-CMD-check.yaml` - Runs on every push/PR
- `test-coverage.yaml` - Computes coverage, uploads to Codecov
- `pkgdown.yaml` - Builds site, deploys to gh-pages branch

### Pre-Commit Local Workflow
```r
# Before committing
styler::style_pkg()               # Auto-format
devtools::document()              # Update Rd files
devtools::test()                  # Run tests
spelling::spell_check_package()   # Check spelling
```

### Pre-CRAN-Submission Workflow
```r
# 1. Generate release checklist
usethis::use_release_issue()  # Creates GitHub issue with todos

# 2. Run local checks
devtools::check(args = c('--as-cran'))
goodpractice::gp(".")
spelling::spell_check_package()
urlchecker::url_check()

# 3. Platform checks
rhub::rhub_check()           # Linux/Windows/macOS
devtools::check_win_devel()  # Windows (official)

# 4. Review checklist, then submit
devtools::release()
```

---

## Sources

### Official R Documentation
- [CRAN Submission Checklist](https://cran.r-project.org/web/packages/submission_checklist.html)
- [R Packages (2e) - Releasing to CRAN](https://r-pkgs.org/release.html)
- [win-builder Service](https://win-builder.r-project.org/)
- [mac-builder Service](https://mac.r-project.org/macbuilder/submit.html)

### r-lib Ecosystem
- [devtools Package](https://devtools.r-lib.org/)
- [usethis Package](https://usethis.r-lib.org/)
- [rhub Package](https://r-hub.github.io/rhub/)
- [R-hub v2 Announcement](https://blog.r-hub.io/2024/04/11/rhub2/)
- [lintr Package](https://lintr.r-lib.org/)
- [styler Package](https://styler.r-lib.org/)
- [covr Package](https://covr.r-lib.org/)
- [pkgdown Package](https://pkgdown.r-lib.org/)
- [r-lib/actions GitHub Repository](https://github.com/r-lib/actions)

### Community Tools
- [goodpractice Package (rOpenSci)](https://docs.ropensci.org/goodpractice/)
- [spelling Package (rOpenSci)](https://docs.ropensci.org/spelling/)
- [checkhelper Package (ThinkR)](https://cran.r-project.org/web/packages/checkhelper/index.html)
- [pkgcheck Package (rOpenSci)](https://docs.ropensci.org/pkgcheck/)

### Additional Resources
- [ThinkR CRAN Preparation Guide](https://github.com/ThinkR-open/prepare-for-cran)
- [R-hub Blog - URL Checks](https://blog.r-hub.io/2020/12/01/url-checks/)
- [R-hub Blog - Win-Builder Guide](https://blog.r-hub.io/2020/04/01/win-builder/)
