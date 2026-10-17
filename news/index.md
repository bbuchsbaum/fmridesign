# Changelog

## fmridesign (development version)

### Changes that affect results

- `hrf(..., basis = "fir", nbasis = k)` with `k` other than 12 now
  builds a true FIR basis (`k` indicator bins over 24 s, as
  [`fmrihrf::hrf_fir_generator()`](https://bbuchsbaum.github.io/fmrihrf/reference/hrf_fir_generator.html)).
  It used to build overlapping tent functions, so design matrices for
  such terms change.
- When `nbasis` is omitted,
  [`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md),
  [`trialwise()`](https://bbuchsbaum.github.io/fmridesign/reference/trialwise.md)
  and
  [`feature()`](https://bbuchsbaum.github.io/fmridesign/reference/feature.md)
  give basis sets their standard size: 12 bins for `"fir"` and 5
  functions for `"bspline"`, `"tent"` and `"fourier"`. Previously
  `"fir"` and `"fourier"` got a single function and `"bspline"` failed
  to evaluate. An explicit `nbasis`, and the spmg/gamma/gaussian bases,
  are unaffected.
- Event terms that cross a single-level factor with other variables now
  get the right columns: `hrf(cond, task)` with a constant `task` gives
  one column per `cond` level (it gave a single column of ones), and
  `hrf(cond, rt)` with a one-level `cond` is modulated by `rt` (it was
  all ones).
- [`pair_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/pair_contrast.md),
  [`oneway_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/oneway_contrast.md)
  and
  [`poly_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/poly_contrast.md)
  apply `basis_weights` across all basis functions when `basis` is
  `NULL` or `"all"`, as documented
  (e.g. `basis_weights = c(1, 0, 0, 0, 0)` is equivalent to
  `basis = 1`). The weights used to be ignored silently; a length
  mismatch is now an error.
- `interaction_contrast(~ A * B * C)` tests the full highest-order
  interaction. It silently used only the first two factors. Two-factor
  results are unchanged.
- Contrasts on factor-by-modulator terms such as `hrf(cond, RT)` now
  work: cell weights go on the modulator columns, repeated per basis or
  modulator column.
  [`pair_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/pair_contrast.md),
  [`unit_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/unit_contrast.md)
  and
  [`poly_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/poly_contrast.md)
  used to fail with “No conditions were selected”, and
  [`oneway_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/oneway_contrast.md),
  [`interaction_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/interaction_contrast.md)
  and formula contrasts returned all-zero weights with only a warning.
- [`contrast_weights()`](https://bbuchsbaum.github.io/fmridesign/reference/contrast_weights.md)
  and
  [`Fcontrasts()`](https://bbuchsbaum.github.io/fmridesign/reference/Fcontrasts.md)
  on an `event_model` no longer give zero weights when the model renames
  a term to keep names unique (e.g.
  `hrf(Ident(RT1, RT2)) + hrf(RT1, RT2, contrasts = ...)`).
- `where` clauses and the condition-selecting formulas of the contrast
  constructors are evaluated in the formula’s own environment, so they
  can use variables from the code that created them (e.g.
  `where = ~ cond == lvl` inside a function). They used to fail or pick
  up package-internal objects.
- [`design_colmap()`](https://bbuchsbaum.github.io/fmridesign/reference/design_colmap.md)
  and
  [`design_meta()`](https://bbuchsbaum.github.io/fmridesign/reference/design_meta.md)
  record `basis_name` as the basis family spelled as in
  `hrf(basis = ...)` (`"spmg1"`, `"spmg3"`, `"fir"`, `"bspline"`, …; a
  plain function is named after its argument, e.g. `"my_fun"`, or
  `"custom"`), instead of the HRF’s first class (`"SPMG3_HRF"`,
  `"FIR_HRF"`, or just `"HRF"`). Lagged, blocked and normalised SPMG2,
  SPMG3 and FIR terms now get proper `basis_label`s (`canonical` /
  `derivative` / `dispersion`, `lag_00`, …) instead of `component_01`,
  …, which also changes their pretty names and plot labels. spmg1 terms
  get the same `basis_name` with every fmrihrf version.
- Within-run
  [`correlation_map()`](https://bbuchsbaum.github.io/fmridesign/reference/correlation_map.md)
  draws every cell one cell wide. Baseline models with two columns per
  run (e.g. `basis = "poly", degree = 2`) used to show no cells.
- [`design_colmap()`](https://bbuchsbaum.github.io/fmridesign/reference/design_colmap.md)
  reports the single global constant drift column
  (`basis = "constant", intercept = "global"`) as shared across runs
  (`is_block_diagonal = FALSE`).

### New features

- Added
  [`check_estimability()`](https://bbuchsbaum.github.io/fmridesign/reference/check_estimability.md)
  for full-design rank, exact condition numbers, named weak directions,
  and contrast-specific estimability and variance factors. Event models
  include run intercepts by default and report event coverage per run;
  an explicit baseline/nuisance design can also be supplied.
  [`check_collinearity()`](https://bbuchsbaum.github.io/fmridesign/reference/check_collinearity.md)
  now documents the limitations of pairwise checks
  ([\#32](https://github.com/bbuchsbaum/fmridesign/issues/32)).

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

- fmridesign now supplies every name and accessor method for its own
  classes, so downstream packages can use plain dispatch on the exported
  generics instead of defining duplicates or reaching into fmridesign’s
  namespace. New methods:
  [`longnames()`](https://bbuchsbaum.github.io/fmridesign/reference/longnames.md)
  and
  [`shortnames()`](https://bbuchsbaum.github.io/fmridesign/reference/shortnames.md)
  for `event_model`, `convolved_term`, `feature_term` and bare events
  (`event_seq`);
  [`columns()`](https://bbuchsbaum.github.io/fmridesign/reference/columns.md)
  and
  [`cells()`](https://bbuchsbaum.github.io/fmridesign/reference/cells.md)
  for `event_model`;
  [`conditions()`](https://bbuchsbaum.github.io/fmridesign/reference/conditions.md),
  [`event_table()`](https://bbuchsbaum.github.io/fmridesign/reference/event_table.md),
  [`nbasis()`](https://bbuchsbaum.github.io/fmrihrf/reference/nbasis.html)
  and
  [`design_matrix()`](https://bbuchsbaum.github.io/fmridesign/reference/design_matrix.md)
  for `convolved_term`; and
  [`conditions()`](https://bbuchsbaum.github.io/fmridesign/reference/conditions.md)
  for `baseline_model`. They previously lived in fmrireg.
  [`longnames()`](https://bbuchsbaum.github.io/fmridesign/reference/longnames.md)
  uses fmridesign’s canonical `variable.level` form (`condition.A`),
  which is the design-matrix column name without its `<term>_` prefix
  (`condition_condition.A`);
  [`?longnames`](https://bbuchsbaum.github.io/fmridesign/reference/longnames.md)
  documents the relationship. Design-matrix column names are unchanged.

- [`longnames()`](https://bbuchsbaum.github.io/fmridesign/reference/longnames.md),
  [`shortnames()`](https://bbuchsbaum.github.io/fmridesign/reference/shortnames.md)
  and
  [`condition_map()`](https://bbuchsbaum.github.io/fmridesign/reference/condition_map.md)
  now honour `drop.empty = TRUE` (the default): an interaction cell with
  no events is left out, as it is from the design matrix, so the names
  line up one-to-one with the columns. `drop.empty = FALSE` lists the
  full grid of factor levels, which is what
  [`conditions()`](https://bbuchsbaum.github.io/fmridesign/reference/conditions.md)
  always returns (documented).

- `condition_map(<event_model>)` finds each condition’s column by its
  exact name, `<term tag>_<canonical>`, instead of by position. It
  previously returned `column_name = NA` for every row of a term with an
  empty cell or a multi-basis HRF.

- `blockids(<event_model>)` is documented
  ([`?blockids.event_model`](https://bbuchsbaum.github.io/fmridesign/reference/blockids.event_model.md)):
  it returns one run id per event. Per-scan ids come from
  `blockids(x$sampling_frame)`; `blocklens(x)` gives scans per run.
  Because fmrireg previously overrode this method with per-scan ids, the
  first call in a session prints a one-time message. The message will be
  removed in the next release.

- `correlation_map(<baseline_model>)` gains `within_run = TRUE`: run
  intercepts are dropped, columns are centred within each run, and
  run-specific columns are correlated on their own run, with pairs from
  different runs left out. This reproduces fmrireg’s former method
  exactly; `within_run = FALSE` gives the previous fmridesign behaviour
  (runs concatenated). Cells are labelled by default when there are at
  most 12 columns, as in fmrireg’s method (event models keep the
  20-column default). Both methods accept `label_values` as an alias for
  `annotate`, and the `event_model` method accepts `within_run` too
  (default `FALSE`).

- [`correlation_map()`](https://bbuchsbaum.github.io/fmridesign/reference/correlation_map.md)
  now errors on arguments that `geom_tile()` does not accept, instead of
  passing them on to be dropped with a warning. The deprecated `size` is
  still accepted.

### Bug fixes

- [`contrast_set()`](https://bbuchsbaum.github.io/fmridesign/reference/contrast_set.md)
  now combines individual specifications with nested contrast sets,
  including those returned by
  [`one_against_all_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/one_against_all_contrast.md).
  It preserves leaf names and order, and reports invalid entries by
  argument path
  ([\#33](https://github.com/bbuchsbaum/fmridesign/issues/33)).
- `hrf(..., summate = FALSE)` is honoured again for sustained events.
  The shared-HRF fast path ignored `summate` and always produced the
  `summate = TRUE` design.
- An event with a negative onset in the first run now fails with
  fmrihrf’s “`onsets` must be non-negative” error, as the legacy path
  and
  [`fmrihrf::regressor()`](https://bbuchsbaum.github.io/fmrihrf/reference/regressor.html)
  always did; the fast path had silently accepted it. The out-of-frame
  warning is still raised first.
- [`term_matrices()`](https://bbuchsbaum.github.io/fmridesign/reference/term_matrices.md)
  returns one entry per term, with `NULL` for a term without design
  columns, for both event and baseline models; such a term used to shift
  every later matrix onto the wrong term name.
- [`cells()`](https://bbuchsbaum.github.io/fmridesign/reference/cells.md)
  on multi-basis convolved terms lists only observed cells with
  correctly aligned counts, labels ten or more basis functions
  `basis01`, `basis02`, … in order, and works for terms with continuous
  variables.
- [`split_onsets()`](https://bbuchsbaum.github.io/fmridesign/reference/split_onsets.md)
  labels each condition’s onsets correctly when a parametric modulator
  is zero for every event of a condition.
- [`Fcontrasts()`](https://bbuchsbaum.github.io/fmridesign/reference/Fcontrasts.md)
  no longer fails for terms crossing a single-level factor, and
  [`conditions()`](https://bbuchsbaum.github.io/fmridesign/reference/conditions.md)
  no longer errors on a multi-variable term containing a factor without
  levels.
- [`condition_basis_list()`](https://bbuchsbaum.github.io/fmridesign/reference/condition_basis_list.md)
  names its result by condition for terms with a single continuous
  variable (the name was empty).
- Formula contrasts
  ([`contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/contrast.md))
  work on continuous terms with several variables, such as
  `hrf(Ident(RT1, RT2))` with `~ RT1 - RT2`.
- [`poly_contrast()`](https://bbuchsbaum.github.io/fmridesign/reference/poly_contrast.md)
  reports a `value_map` that misses factor levels instead of failing
  with “subscript out of bounds”.
- [`validate_contrasts()`](https://bbuchsbaum.github.io/fmridesign/reference/validate_contrasts.md)
  accepts a list of contrast vectors, names unnamed entries `contrast1`,
  `contrast2`, …, and returns a zero-row data frame when nothing can be
  validated.
- [`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md) no
  longer errors when `basis` is an HRF object or function and `hrf_fun`
  is also given; it warns that `hrf_fun` takes precedence.
- The basis names `"bs"` and `"tent"`, documented in
  [`?hrf`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md),
  are accepted.
- Degree-1 polynomial modulators (`hrf(Poly(x, 1))`), and polynomial
  terms whose subset keeps a single event, no longer fail with
  “Processed event value is not a matrix”.
  [`predict()`](https://rdrr.io/r/stats/predict.html) works on subsetted
  `Poly` and `BSpline` bases.
- `baseline_model(nuisance_check = "drop")` no longer fails when every
  nuisance column of a run is dropped, and zero-column nuisance matrices
  are accepted by
  [`baseline_model()`](https://bbuchsbaum.github.io/fmridesign/reference/baseline_model.md)
  and
  [`check_nuisance()`](https://bbuchsbaum.github.io/fmridesign/reference/check_nuisance.md).
- [`construct()`](https://bbuchsbaum.github.io/fmridesign/reference/construct.md)
  on a
  [`nuisance()`](https://bbuchsbaum.github.io/fmridesign/reference/nuisance.md)
  specification works (it always failed), and
  [`nuisance()`](https://bbuchsbaum.github.io/fmridesign/reference/nuisance.md)
  terms no longer trigger tibble’s name-repair warning.
- Runwise intercept columns no longer carry stray `assign` / `contrasts`
  attributes from
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html); their
  values are unchanged.
- [`requires_external_processing()`](https://bbuchsbaum.github.io/fmridesign/reference/requires_external_processing.md)
  accepts a registered class name, and
  [`get_all_external_hrf_functions()`](https://bbuchsbaum.github.io/fmridesign/reference/get_all_external_hrf_functions.md)
  returns `character(0)` rather than `NULL` when nothing is registered.
- [`print()`](https://rdrr.io/r/base/print.html) on baseline models and
  contrast specifications returns the object invisibly.
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for event
  models no longer warns “Ignoring unknown parameters: inherit.aes”
  (ggplot2 \< 3.5) or, with `label_mode = "compact"`, about non-ASCII
  characters; per-run TRs in its subtitle are no longer padded.
- [`correlation_map()`](https://bbuchsbaum.github.io/fmridesign/reference/correlation_map.md)
  no longer errors with `annotate = FALSE` when there is nothing to
  correlate, can be drawn with ggplot2 \< 3.5.0 when it has more than 10
  columns, and says when no two regressors share a run instead of
  claiming that no regressor varies.
- [`convolve()`](https://bbuchsbaum.github.io/fmridesign/reference/convolve.md)
  warnings for terms built directly with
  [`event_term()`](https://bbuchsbaum.github.io/fmridesign/reference/event_term.md)
  name the term, and `event_term(list())` fails with a clear message.
- The `event_model`, `event_factor`, `event_matrix` and `labels.event`
  help pages no longer contain misplaced text or wrong example output.

### Performance

- Building event models is 5-40 times faster with the current fmrihrf,
  whose convolution engine no longer starts OpenMP threads and computes
  only the samples the scan grid needs. Results are unchanged.
- [`baseline_model()`](https://bbuchsbaum.github.io/fmridesign/reference/baseline_model.md)
  and
  [`check_nuisance()`](https://bbuchsbaum.github.io/fmridesign/reference/check_nuisance.md)
  share their drift construction and block assembly (about 25% faster),
  and
  [`validate_contrasts()`](https://bbuchsbaum.github.io/fmridesign/reference/validate_contrasts.md)
  computes the design rank once (about twice as fast with many
  contrasts).

### Packaging and internal changes

- `Remotes:` tracks `bbuchsbaum/fmrihrf` instead of a pre-release
  commit, so GitHub installs and CI use the same fmrihrf as r-universe.
  fmridesign still requires only fmrihrf (\>= 0.4.0) and is tested
  against 0.4.0.
- stringr is no longer imported; tidyr and Matrix move to Suggests (only
  the vignettes and tests use them); ggplot2 (\>= 3.4.0) is declared.
- Large internal simplification: the contrast constructors and
  [`contrast_weights()`](https://bbuchsbaum.github.io/fmridesign/reference/contrast_weights.md)
  methods share validation and packaging helpers,
  [`contrast_weights()`](https://bbuchsbaum.github.io/fmridesign/reference/contrast_weights.md)
  and
  [`Fcontrasts()`](https://bbuchsbaum.github.io/fmridesign/reference/Fcontrasts.md)
  on event models share one weight-embedding helper, the scaling bases
  share one z-score helper, and the plotting code shares its colour,
  scale and glyph helpers. Dead code (`dctbasis()`,
  `translate_legacy_pattern()`, never-hit caches, duplicate `%||%`
  definitions, an fmrihrf \<= 0.4.0 warning shim, …) and unused imports
  are gone.

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
