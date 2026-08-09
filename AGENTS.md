# AGENTS.md

## Cursor Cloud specific instructions

`fmridesign` is an R package (a library — there is no server, daemon, or web app to run).
The "application" is the R package itself; you exercise it from an R session. Standard
development commands (`devtools::load_all()`, `devtools::test()`, `devtools::check()`,
`devtools::document()`) are documented in `CLAUDE.md` — use those.

### Environment (already provisioned in the snapshot)
- R 4.6.1 and the required system libraries are pre-installed. The startup update script
  only refreshes R package dependencies (`pak::local_install_dev_deps(".")` plus `devtools`
  and `rcmdcheck`), so you should not need to reinstall R itself.
- The CRAN mirror is pinned to the Posit Package Manager binary repo for Ubuntu noble via
  `$(R RHOME)/etc/Rprofile.site`, and the site libraries under `/usr/local/lib/R/site-library`
  and `/usr/lib/R/site-library` are world-writable. This means `install.packages()` / `pak`
  install precompiled binaries quickly and do **not** need `sudo`.
- `fmrihrf` (the key upstream dependency) is available on the pinned repo at the required
  version, so no GitHub install is needed for it.

### Non-obvious gotchas
- `devtools::build_vignettes()` fails with "package 'remotes' is required" (`remotes` is not
  installed by default). To build/preview vignettes, either `install.packages("remotes")`
  first, or render directly: after `devtools::load_all(".")`, call
  `rmarkdown::render("vignettes/a_01_introduction.Rmd", output_file = tempfile(fileext=".html"))`.
- No LaTeX/`pdflatex` is installed, so `R CMD check` cannot build the PDF manual. Run checks
  with `--no-manual` (e.g. `rcmdcheck::rcmdcheck(".", args = "--no-manual")`); the skipped PDF
  manual is expected and not an error.
- If you run check with `--no-build-vignettes`, it emits spurious WARNINGs about a missing
  `inst/doc` directory. A normal check builds vignettes fine (pandoc is installed), so only use
  `--no-build-vignettes` when you intentionally want to skip them.
- The `albersdown` theme (`Config/Needs/website`) is only needed for the pkgdown website, not
  for building the vignettes — the vignettes ship their own local `albers.css`/`albers.js`.
