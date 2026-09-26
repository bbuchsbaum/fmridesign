# Changelog

## fmridesign (development version)

### CRAN compliance

- Design-column convolution no longer reaches into fmrihrf’s unexported
  `evaluate_regressor_cpp()`. The shared-HRF fast path now evaluates
  each live column with the public
  [`fmrihrf::regressor()`](https://bbuchsbaum.github.io/fmrihrf/reference/regressor.html)
  /
  [`fmrihrf::evaluate()`](https://bbuchsbaum.github.io/fmrihrf/reference/evaluate.html)
  API. Results are identical to the previous path on every design built
  by the test suite; building a design matrix is roughly 1.3-2x slower
  (tens of milliseconds on a 4-run, 1200-scan design).

- [`boxcar_hrf_gen()`](https://bbuchsbaum.github.io/fmridesign/reference/boxcar_hrf_gen.md)
  and
  [`weighted_hrf_gen()`](https://bbuchsbaum.github.io/fmridesign/reference/weighted_hrf_gen.md)
  call
  [`fmrihrf::hrf_boxcar()`](https://bbuchsbaum.github.io/fmrihrf/reference/hrf_boxcar.html)
  and
  [`fmrihrf::hrf_weighted()`](https://bbuchsbaum.github.io/fmrihrf/reference/hrf_weighted.html)
  directly instead of looking them up in fmrihrf’s namespace.

- fmridesign no longer registers its own
  [`print()`](https://rdrr.io/r/base/print.html) method for fmrihrf’s
  `sampling_frame` class, which overwrote fmrihrf’s method on load
  (“Registered S3 method overwritten by ‘fmridesign’”). Sampling frames
  now print with fmrihrf’s method. `plot(<sampling_frame>)` is unchanged
  and is documented under
  [`?plot.sampling_frame`](https://bbuchsbaum.github.io/fmridesign/reference/plot.sampling_frame.md).

- [`onsets()`](https://bbuchsbaum.github.io/fmrihrf/reference/onsets.html),
  [`durations()`](https://bbuchsbaum.github.io/fmrihrf/reference/durations.html),
  [`blockids()`](https://bbuchsbaum.github.io/fmrihrf/reference/blockids.html)
  and
  [`nbasis()`](https://bbuchsbaum.github.io/fmrihrf/reference/nbasis.html)
  are now true re-exports of fmrihrf’s generics. fmridesign previously
  defined its own generics with the same names, so dispatch was split:
  for example `fmrihrf::onsets(<event_term>)` found no method and
  `fmridesign::nbasis(HRF_SPMG3)` failed, and which one a bare call
  reached depended on attach order. Attaching fmridesign no longer masks
  these four names from fmrihrf.

### Bug fixes

- `hrf(..., summate = FALSE)` is honoured again for sustained events.
  The shared-HRF fast path ignored `summate` and always produced the
  `summate = TRUE` design.
- An event with a negative onset in the first run now fails with
  fmrihrf’s “`onsets` must be non-negative” error, as the legacy path
  and
  [`fmrihrf::regressor()`](https://bbuchsbaum.github.io/fmrihrf/reference/regressor.html)
  always did; the fast path had silently accepted it. The out-of-frame
  warning is still raised first.

## fmridesign 0.6.1

### Plotting overhaul

- All plotting methods share one visual system: the new exported
  [`theme_fmridesign()`](https://bbuchsbaum.github.io/fmridesign/reference/theme_fmridesign.md)
  and
  [`fmridesign_palette()`](https://bbuchsbaum.github.io/fmridesign/reference/fmridesign_palette.md),
  a colour-vision-checked categorical palette, one diverging palette
  centred on zero, readable column labels (e.g. `face × high`,
  `drift 3`, `tx`) and one way of marking runs.
- `plot(<event_model>)` draws one row per condition with event onset and
  duration marks. Rows of a term share an amplitude scale (`y_scale`).
  Basis sets get one row per basis function, and trialwise or long-basis
  designs switch to a heatmap with a per-trial overlap column. New
  `style`, `show_events`, `y_scale`, `time_range`, `title` and
  `subtitle` arguments.
- [`design_map()`](https://bbuchsbaum.github.io/fmridesign/reference/design_map.md)
  is an SPM-style image grouped by term, with raw column ranges and a
  near-flat (dead) column check. Run boundaries are now placed
  correctly; they were previously computed from per-event block ids.
- [`correlation_map()`](https://bbuchsbaum.github.io/fmridesign/reference/correlation_map.md)
  shows the lower triangle with each column’s variance inflation factor
  (run means removed) on the diagonal and flags aliased columns.
- [`plot_contrasts()`](https://bbuchsbaum.github.io/fmridesign/reference/plot_contrasts.md)
  shows every design column, printed weights, the sum of weights and
  each contrast’s standard error per unit noise given the design.
- `plot(<baseline_model>)` shows every time-varying term (motion
  regressors were previously dropped), splitting translations and
  rotations into separate lanes. User-supplied nuisance column names are
  kept for display.
- `plot(<sampling_frame>)` no longer draws an empty panel; it adds
  `"lane"` and `"grid"` styles and an `events =` overlay for checking
  coverage.

### Bug fixes

- **User-visible correction: multi-basis column names change.** For
  [`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md)
  terms with more than one basis function (`"spmg2"`, `"spmg3"`, FIR,
  B-spline, tent, custom `nbasis > 1`) and two or more conditions,
  design columns were filled condition-major
  (`A_b01, A_b02, B_b01, ...`) but named basis-major
  (`A_b01, B_b01, A_b02, ...`), so every column except the first and
  last was mislabelled. For example, `task_task.scene_b01` held face’s
  temporal derivative. The data layout is unchanged; column names,
  `conditions(term, expand_basis = TRUE)`, and every name-based contrast
  ([`pair_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/pair_contrast.md),
  [`oneway_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/oneway_contrast.md),
  [`column_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/column_contrast.md),
  [`unit_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/unit_contrast.md),
  [`contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/contrast.md),
  [`interaction_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/interaction_contrast.md))
  now follow the documented condition-major `term_condition_b##` layout,
  matching
  [`design_colmap()`](https://bbuchsbaum.github.io/fmridesign/reference/design_colmap.md).
  Contrast weights and coefficient labels from earlier versions for such
  terms were attached to the wrong columns and should be recomputed.
  Code that indexed multi-basis columns by position assuming basis-major
  order must be updated
  ([\#23](https://github.com/bbuchsbaum/fmridesign/issues/23)).
- [`covariate()`](https://bbuchsbaum.github.io/fmridesign/reference/covariate.md)
  now expands matrix/data-frame arguments into one non-convolved
  regressor per column. Named inputs preserve sanitized column names,
  unnamed matrices use `f01`, `f02`, … suffixes, and final names follow
  the standard `<term_tag>_<condition_tag>` grammar (`cov_x` by default,
  or `motion_x` with `id = "motion"`). Covariate condition accessors and
  per-column metadata now expose the individual regressor identities
  instead of a concatenated multi-variable term name
  ([\#19](https://github.com/bbuchsbaum/fmridesign/issues/19)).
- [`column_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/column_contrast.md)
  patterns now match the design-matrix column names, as documented
  (`term_tag_condition_tag[_b##]`, e.g. `"^cond_cond\\.A$"` or
  `"^cond_cond\\.A_b01$"`). Previously they were matched only against
  term-level condition names (`cond.A`), so documented patterns selected
  nothing. Term-level patterns still work: they are tried only when a
  pattern matches no design-matrix column, and select the same columns.
  A pattern that selects different columns in the two namespaces is now
  an error, and a pattern that matches nothing warns with the available
  column names
  ([\#24](https://github.com/bbuchsbaum/fmridesign/issues/24)).
- `design_matrix(<baseline_term>, blockid = )` now returns each active
  column once. With `intercept = "global"`, requesting several runs used
  to return one duplicate `constant_global` column per run (so
  `blockid = 1:3` gave three identical columns rather than one); the
  result now always equals the requested rows and non-zero columns of
  the full term matrix, in the term’s column order. A
  `basis = "constant", intercept = "global"` drift term likewise
  returned zero columns for any run but the first. Terms without block
  structure now error on `blockid` instead of returning an empty matrix.
- `Fcontrasts(<event_model>)` no longer returns an all-zero matrix with
  an “unmatched row names” warning for multi-basis terms. The term-level
  contrast is expanded to `kronecker(C, diag(nbasis))`, testing the
  condition effect jointly in every basis function
  ([\#25](https://github.com/bbuchsbaum/fmridesign/issues/25)).
- [`Fcontrasts()`](https://bbuchsbaum.github.io/fmridesign/reference/Fcontrasts.md)
  for multi-factor terms (e.g. `hrf(task, load)`) assigned row names in
  the wrong order, so the matrix labelled `task` tested `load` and vice
  versa. Rows now follow
  [`conditions()`](https://bbuchsbaum.github.io/fmridesign/reference/conditions.md)
  order.
- [`interaction_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/interaction_contrast.md)
  now names its rows with the term’s canonical condition tags
  (e.g. `task.face_load.low`) and expands them across basis functions.
  It previously used raw cell labels (`face_low`) that matched no design
  column, producing all-zero weights with a warning.
- [`unit_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/unit_contrast.md)
  and formula contrasts (`contrast(~ face - obj)`), and differences of
  contrasts built from them, now expand their weights across the basis
  functions of a multi-basis term. They previously returned zero-row
  weights with an “unmatched row names” warning.
- [`unit_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/unit_contrast.md)
  now applies a logical selector in `A`: previously
  `unit_contrast(~ cond == "A")` ignored `A` and averaged over every
  cell (weights 0.5/0.5 for two levels); it now selects level A only
  (weight 1). A bare factor (`~ cond`) still averages over all cells.
- `Fcontrasts(<event_model>)` skips terms with no categorical variable
  (e.g. `hrf(rt)`, covariates) instead of failing for the whole model; a
  model with only such terms returns an empty list.
- [`condition_basis_list()`](https://bbuchsbaum.github.io/fmridesign/reference/condition_basis_list.md)
  now works for bare `event_term` objects without a `term_tag`, which
  previously returned an empty list.
- `baseline_model(nuisance_list = ...)` now keeps the user’s nuisance
  column names
  ([\#28](https://github.com/bbuchsbaum/fmridesign/issues/28)). Columns
  are named `nuis_<name>_block_<run>` (e.g. `nuis_trans_x_block_1`),
  matching the drift columns (`base_poly1_block_1`); names are sanitised
  to syntactic tokens and made unique within a run, and unnamed columns
  fall back to their original column index (`nuis_2_block_1`), which is
  preserved when `nuisance_check = "drop"` removes columns. This
  replaces the previous `nuis#<run>_<col>` names, so code that matched
  those names must be updated. The original names are kept in the
  nuisance term’s `source_colnames` field.
- `design_colmap(<baseline_model>)` now reports nuisance columns with
  role `"nuisance"` (they were reported as `"intercept"`), takes their
  `run` from the block structure (it was parsed from the column index,
  so a 3-run model with 6 regressors per run reported runs 1 to 6), and
  labels them with the user’s column names in `basis_label`.

## fmridesign 0.6.0

### New features

- [`baseline_model()`](https://bbuchsbaum.github.io/fmridesign/reference/baseline_model.md)
  now checks `nuisance_list` inputs during construction for
  zero-variance columns, duplicate or near-duplicate columns, non-finite
  values, nuisance rank deficiency, and columns aliased with baseline
  terms.
- Added `nuisance_check = c("warn", "error", "drop", "none")` to control
  whether nuisance problems warn, stop, are dropped with an audit
  warning, or are skipped.
- Added
  [`check_nuisance()`](https://bbuchsbaum.github.io/fmridesign/reference/check_nuisance.md)
  and
  [`clean_nuisance()`](https://bbuchsbaum.github.io/fmridesign/reference/clean_nuisance.md)
  helpers for inspecting and repairing block-wise nuisance regressors
  before model construction.

### Performance

- Sped up
  [`event_model()`](https://bbuchsbaum.github.io/fmridesign/reference/event_model.md)
  design-matrix construction by replacing the per-term
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  /
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
  calls used to assemble column metadata with a lightweight, validated
  `tibble` constructor. This removes the metadata-building hotspot (~15%
  faster end-to-end on a representative multi-term, multi-run model)
  with byte-identical design matrices, column names,
  `col_indices`/`term_spans`, and metadata values.
- [`convolve.event_term()`](https://bbuchsbaum.github.io/fmridesign/reference/convolve.md)
  now skips columns that are all-zero within a block instead of building
  and evaluating an empty `fmrihrf` regressor for each. For
  block-diagonal-ish designs (trialwise/LSS single-trial models, or
  factor levels present only in some runs) this is a large speedup
  (~2.2x faster on a representative 360-column trialwise model) while
  producing bit-identical output. Designs where every column is
  populated in every block are unaffected (a fast-exit keeps the
  original path), and blocks containing `NA`/`NaN` fall back to the
  previous full-column path so filtering semantics are unchanged.
- Convolution hot path now shares one fine-grid HRF matrix across all
  columns and blocks and calls `fmrihrf`’s C++ evaluator directly,
  skipping per-column `Reg` construction / `prep_reg_inputs` overhead.
  Combined with a single global output matrix (no per-block zero-alloc +
  `rbind`) and deferred tibble materialization in
  `build_event_model_design_matrix()`, this is ~2–3.5× faster end-to-end
  on trialwise/LSS and multi-term workloads while remaining
  bit-identical to `fmrihrf::evaluate(regressor(...))`. Per-onset
  `hrf_fun` lists and NA-misaligned designs keep the previous path.
- Added `bench/` cross-library design-matrix benchmarks against nilearn
  (the FitLins first-level design-matrix hot path). Run with
  `bash bench/run_compare.sh`; see `bench/RESULTS.md` and
  `bench/OPTIMIZATION_NOTES.md`.

### Bug fixes

- [`contrast_weights()`](https://bbuchsbaum.github.io/fmridesign/reference/contrast_weights.md)
  now removes rows for factor levels excluded by an
  `hrf(..., subset = )` term from the returned term-local `weights`,
  keeping them consistent with the reconciled full-design
  `offset_weights`
  ([\#17](https://github.com/bbuchsbaum/fmridesign/issues/17)).
- [`convolve_design()`](https://bbuchsbaum.github.io/fmridesign/reference/convolve_design.md)
  now extracts each condition column with `dmat[[i]]`, so it produces
  correct regressors for base `data.frame` inputs (its documented
  example). The previous `dmat[, i][[1]]` collapsed a data frame column
  to its first element; the tibble-based internal call path was
  unaffected.
- [`contrast_weights()`](https://bbuchsbaum.github.io/fmridesign/reference/contrast_weights.md)
  and
  [`Fcontrasts()`](https://bbuchsbaum.github.io/fmridesign/reference/Fcontrasts.md)
  for `event_model` objects now name interaction-term contrasts with the
  same term tags used by design-matrix `col_indices`, preventing
  downstream consumers from dropping crossed-term contrasts because of
  `:`/`_` key mismatches
  ([\#9](https://github.com/bbuchsbaum/fmridesign/issues/9)).
- [`event_model()`](https://bbuchsbaum.github.io/fmridesign/reference/event_model.md)
  now warns when continuous parametric modulators are all-zero or have
  zero variance, catching degenerate design columns before model fitting
  while preserving the existing design-matrix shape
  ([\#8](https://github.com/bbuchsbaum/fmridesign/issues/8)).
- Fixed list-based
  [`event_model()`](https://bbuchsbaum.github.io/fmridesign/reference/event_model.md)
  specifications so `hrf(..., subset = )` expressions can use base
  operators and helper functions from the calling environment, matching
  formula-interface subset behavior.
- Fixed event-model column metadata construction for factor and
  multi-basis HRF terms that expand to multiple design columns.
- Event terms whose subsets select zero events now retain their
  canonical condition-column names and metadata on the resulting
  all-zero design matrix, rather than falling back to generic `col_1`,
  `col_2`, … names.
- Suppressed exact, known false-positive metadata warnings produced when
  decorated HRFs are reconstructed by `fmrihrf` 0.3.0, while continuing
  to surface unrelated warnings.
