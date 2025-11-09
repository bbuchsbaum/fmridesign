# fmridesign

## Micro-DSL v3.4 Source Format Grammar

This is the HUMAN-READABLE source format for v3.4. It extends v3.3 with
semantic annotations and constrained types while maintaining clarity and
completeness. A separate compilation step produces the compressed
format.

**Budget**: Target ≤ 250 lines or ≤ 2,500 tokens for optimal LLM
processing.

**1. Document Structure:**

    @pkg package_name | description
    [Type Aliases section]
    [Constraint Definitions section]
    [Legend section if needed]
    # Package Name
    [Sections with entries]
    [Dependencies section]
    [Meta-Footer]

**2. Sigils (Same as v3.3):**

    @pkg - Package declaration
    @f   - Function
    @d   - Data object
    @x   - Re-export from another package

    S3 System:
    @s3g - S3 generic (UseMethod)
    @s3m - S3 method (generic.class)
    @s3c - S3 class definition

    S4 System:
    @s4g - S4 generic (setGeneric)
    @s4m - S4 method (setMethod)
    @s4c - S4 class (setClass)

    S7 System:
    @s7g - S7 generic (new_generic)
    @s7m - S7 method
    @s7c - S7 class (new_class)

    R6 System:
    @r6c - R6 class (R6Class)

**3. Type System (v3.4 Enhanced with Constraints):**

    Scalars (default): int, dbl, chr, lgl, raw, cpl
    Vectors: vec<type> or type[]
    Matrices: mat<type> or mat<type,rows,cols>
    Arrays: arr<type,dims>
    Lists: lst<type> or lst{field:type, ...} (structured)
    Data frames: df, tbl, data.table
    Factors: fct, ord
    Dates: Date, POSIXct, POSIXlt

    Union types: type1|type2|type3
    Nullable: type? (shorthand for type|NULL)
    Any type: any
    Ellipsis: ... or ...:type (e.g., ...:expr for NSE)

    Class types: s3:classname, s4:classname, r6:classname, s7:classname

    CONSTRAINED TYPES (v3.4):
    Enums: chr["opt1"|"opt2"|"opt3"]
    Ranges: int[min..max], dbl[min..max]
    Patterns: chr[/regex/]
    Exclusions: int[1..100]&!=[13,17]
    References: @ref:constraint_name

**4. Entry Format (v3.4 Enhanced):**

    @sigil name (param1:type1[constraint]?=default, param2:type2, ...) | Description -> return_type | return_schema
      +tag:value +tag:value
      !cov [Class1, Class2] (for generics)
      - param1 : Additional description
        @annotation:value @annotation:value
      - param2 : (constants: "a", "b", CONST) Valid values
        @requires:condition @affects:target
      - param3 : (key_funcs: func1, func2) Related functions
        @lifecycle:init @units:measurement
      ```verbatim
      # Optional verbatim R code block

    **5. Type Aliases Section (v3.4 Enhanced):**
    ```markdown
    ## Type Aliases:
    DF = df|tbl|data.table              # Standard data frame types
    V<T> = vec<T>                       # Vector shorthand
    Fml = s3:formula                    # Formula objects
    Gg = s3:ggplot                      # ggplot2 objects
    Config = lst{method:chr, opts:lst}  # Structured config
    ValidPort = int[1024..65535]       # Constrained port range

**Standard aliases** (use these by default): - `DF` for data frame
arguments - `Fml` for formula arguments (not `fml`) - `V<T>` for vectors
when brevity helps

**6. Constraint Definitions (v3.4 New):**

``` markdown
## Constraint Definitions:
@constraint positive_weights | Positive numeric weights
  type: vec<dbl>
  validates: all(. > 0)
  length: @env:nrow(data)
  
@constraint valid_identifier | Valid R identifier
  type: chr
  pattern: /^[a-zA-Z_][a-zA-Z0-9_.]*$/
  not_reserved: TRUE
```

**7. Class Documentation (v3.4 Enhanced):**

    @s4c ClassName | One-line description
      - slots: name1 (type1[constraint]) @annotation:value
               name2 (type2) @lifecycle:init @immutable
      - extends: ParentClass
      - validity: Description of validity rules

    @r6c ClassName | One-line description
      - fields: field1 (type1[constraint]) @purpose:role
                field2 (type2) @lazy @cached
      - methods: method1 (args) -> ret_type
                 method2 (args) -> ret_type
      - inherits: ParentClass

**8. Metadata Tags (v3.3 + v3.4 additions):**

    +family:group_name           # Function family
    +pipe:in|out                # Pipe compatibility (in, out, or both)
    +nse:param1,param2          # Parameters using NSE
    +side:effect[details]       # Side effects with sub-facets
      - fs[read|write|delete]   # File system operations
      - plot[device|file]       # Graphics output
      - console[print|message|warning]  # Console output
      - network[http|socket|download]   # Network operations
      - options[get|set|env]    # Global options/environment
      - db[read|write|query]    # Database operations
    +perf:O(complexity)         # Performance complexity
    +mem:usage                  # Memory usage pattern
    +compute:intensity          # Computational intensity
    +deprecated:replacement     # Deprecation with suggested alternative
    +wraps:function            # This function wraps another
    +calls:func1,func2         # Functions called internally
    +see:related1,related2     # Related functions to consider
    +parallel:capable          # Can use parallel processing (v3.4)
    +deterministic:false       # Non-deterministic results (v3.4)
    +pure:false               # Has side effects (v3.4)

**9. Semantic Annotations (v3.4 New):**

    BEHAVIORAL:
    @controls:aspect          # Parameter controls specific behavior
    @affects:target          # Changes affect another component
    @modifies:target         # Directly modifies target

    DEPENDENCY:
    @requires:condition      # Prerequisite condition
    @conflicts:parameter     # Mutually exclusive with
    @extends:base           # Extends functionality

    VALIDATION:
    @validates:constraint    # Validation rule
    @range:[min,max]        # Numeric range
    @length:constraint      # Length requirement
    @pattern:regex          # Pattern matching

    SEMANTIC ROLE:
    @purpose:role           # Semantic purpose
    @units:measurement      # Physical/logical units
    @example:value          # Example values
    @default-reason:why     # Why this default

    LIFECYCLE:
    @lifecycle:stage        # When relevant (init|config|runtime|cleanup)
    @immutable             # Cannot be modified
    @cached                # Result is cached
    @lazy                  # Evaluated on demand

    CONDITIONAL:
    @when:condition        # Conditional applicability
    @implies:consequence   # Logical implication
    @if:cond @then:result  # If-then constraints

**10. Structured Return Types (v3.4 New):**

    -> lst{
      field1: type1 @annotation,
      field2: type2[constraint] @annotation,
      nested: lst{
        subfield: type
      }
    }

**11. Example Entry (v3.4):**

``` markdown
@f analyze_model (
  model:s3:lm,
  type:chr["summary"|"anova"|"diagnostics"]?="summary",
  conf.level:dbl[0.5..0.99]?=0.95
) | Analyze fitted model -> lst{
  statistics: df @purpose:results,
  plots: lst<s3:ggplot>? @when:type="diagnostics",
  interpretation: chr @purpose:summary
}
  +family:analysis +compute:light
  - model : @requires:fitted @validates:has-residuals
  - type : @controls:output-format @affects:return-structure
  - conf.level : @purpose:confidence @affects:statistics.ci
```

**12. Conditional Constraints (v3.4):**

``` markdown
@f process_data (
  data:df,
  method:chr["scale"|"center"|"none"]?="none",
  scale.center:lgl?=TRUE,
  scale.scale:lgl?=TRUE
) | Process data with scaling options -> df
  - method : @controls:processing
  - scale.center : @when:method="scale" @requires:TRUE
                   @when:method="center" @implies:scale.scale=FALSE
  - scale.scale : @when:method="scale" @default:TRUE
                  @conflicts:method="center"
```

**13. Best Practices (v3.4):** - Use specific sigils (@s3g not @g) -
Always specify vector vs scalar types - Use standard type aliases (DF,
Fml, V) - Add constraints from match.arg/stopifnot/checks - Keep !cov
lists short (3-6 classes max) - Document semantic relationships
concisely - Use structured types for complex returns - Define reusable
constraints with @constraint - Include conditional logic with
@when/@implies - Group related functions with +family tags - Mark side
effects with detailed sub-facets - Stay within budget (≤250 lines)

**14. Meta-Footer:**

``` markdown
---
## Meta-Footer
- Micro-DSL Version: v3.4-source
- Package: {pkg} (Version: X.Y.Z)
- Generated: [ISO-8601 timestamp]
- Features: types[constrained] sigils[specific] metadata[rich] semantics[annotated]
- Coverage: {n_documented_exports} / {n_total_exports} exports
- Provenance: exports[NAMESPACE], enums[match.arg/switch], constraints[assertions/checks]
```

**15. Export Detection Priority:** 1. NAMESPACE file: `export()`,
`S3method()`, `exportClasses()`, `exportMethods()`, `exportPattern()` 2.
Roxygen tags: `@export` in documentation 3. If neither present: skip the
symbol (do not guess or include)

**16. Inference Heuristics (apply silently):** - Type from defaults:
TRUE/FALSE → lgl, “text” → chr, 1L → int, 1.0 → dbl - Common patterns:
data/df/tbl → DF, formula → Fml, weights → vec - Enums: match.arg(x,
c(“a”,“b”)) → chr\[“a”\|“b”\] - Ranges: stopifnot(x \>= 0 && x \<= 1) →
dbl\[0..1\] - Side effects: file.\* → fs, plot/ggplot → plot,
message/cat → console - Determinism: runif/sample/rnorm →
+deterministic:false

------------------------------------------------------------------------

@pkg fmridesign \| Design Matrix Construction for fMRI Analysis

## Type Aliases:

DF = df\|tbl\|data.table V = vec Fml = s3:formula Gg = s3:ggplot

## Constraint Definitions:

@constraint baseline_basis \| Baseline basis type type:
chr\[“constant”\|“poly”\|“bs”\|“ns”\] validates: match.arg

@constraint intercept_type \| Intercept type type:
chr\[“runwise”\|“global”\|“none”\] validates: match.arg

@constraint positive_degree \| Degree ≥ 1 type: int\[1..\] validates:
degree \>= 1

@constraint blocklens_positive \| Block lengths positive type:
int\[1..\] validates: all(. \> 0)

## 1. Core Model Construction

@f baseline_model (
basis:chr\[“constant”\|“poly”\|“bs”\|“ns”\]?=“constant”,
degree:int\[1..\]?=1, sframe:any,
intercept:chr\[“runwise”\|“global”\|“none”\]?=“runwise”,
nuisance_list:lst\<mat\>?=NULL ) \| Build baseline model for
drift/nuisance -\> s3:baseline_model +family:baseline +pipe:in\|out -
basis : @controls:drift-basis @validates:baseline_basis - degree :
@controls:basis-complexity @validates:positive_degree - sframe :
@purpose:sampling-frame - intercept : @controls:intercept-type
@validates:intercept_type - nuisance_list : @purpose:nuisance-regressors

@f baseline ( degree:int\[1..\]?=1,
basis:chr\[“constant”\|“poly”\|“bs”\|“ns”\]?=“constant”, name:chr?=NULL,
intercept:chr\[“runwise”\|“global”\|“none”\]?=“runwise” ) \| Create
baseline drift spec -\> s3:baselinespec +family:baseline

@f block (x:any) \| Create block variable spec -\> s3:blockspec
+family:baseline

@f nuisance (x:mat) \| Create nuisance term spec -\> s3:nuisancespec
+family:baseline

@f construct (x:any, …) \| Construct term from spec -\> any
+family:construct !cov \[baselinespec, blockspec, nuisancespec,
covariatespec\]

@f baseline_term (varname:chr, mat:mat, colind:lst\<int\[\]\>,
rowind:lst\<int\[\]\>) \| Construct baseline term -\> s3:baseline_term
+family:baseline

## 2. Design Matrix and Visualization

@s3g design_matrix (x:any, …) \| Extract design matrix -\> mat
+family:design +pipe:in\|out !cov \[baseline_model, baseline_term\]

@s3g term_matrices (x:any, …) \| Extract per-term matrices -\>
lst\<mat\> +family:design !cov \[baseline_model\]

@s3g cells (x:any, drop.empty:lgl?=TRUE, …) \| Extract cell table -\> df
+family:design !cov \[baseline_model\]

@s3g term_names (x:any, …) \| Extract term names -\> chr\[\]
+family:design !cov \[baseline_model\]

@s3g baseline_terms (x:any, …) \| Extract baseline terms -\> lst
+family:design !cov \[baseline_model\]

@f design_map ( x:s3:baseline_model, block_separators:lgl?=TRUE,
rotate_x_text:lgl?=TRUE, fill_midpoint:dbl?=NULL, fill_limits:vec?=NULL,
…:expr ) \| Plot baseline design matrix heatmap -\> Gg +family:visual
+side:plot\[device\]

@f correlation_map ( x:s3:baseline_model,
method:chr\[“pearson”\|“spearman”\]?=“pearson”, half_matrix:lgl?=FALSE,
absolute_limits:lgl?=TRUE, …:expr ) \| Plot baseline regressor
correlation heatmap -\> Gg +family:visual +side:plot\[device\]

@f plot ( x:s3:baseline_model, term_name:chr?=NULL, title:chr?=NULL,
xlab:chr?=“Time”, ylab:chr?=“Design Matrix Value”, line_size:dbl?=1,
color_palette:chr?=“Set1”, …:expr ) \| Plot baseline model terms -\> Gg
+family:visual +side:plot\[device\] +pipe:in

@s3g print (x:any, …) \| Print summary to console -\> invisible
+family:console +side:console\[print\] !cov \[baseline_model,
contrast_set, contrast_spec, contrast, poly_contrast_spec,
contrast_diff_spec\]

## 3. Parametric Basis Constructors

@f Ident (…:any) \| Identity basis for variables -\> s3:Ident
+family:basis

@f Poly (x:vec, degree:int\[1..\]) \| Orthogonal polynomial basis -\>
s3:Poly +family:basis

@f BSpline (x:vec, degree:int\[1..\]) \| B-spline basis -\> s3:BSpline
+family:basis

@f Standardized (x:vec) \| Standardize vector (z-score) -\>
s3:Standardized +family:basis

@f Scale (x:vec) \| Z-score basis (global) -\> s3:Scale +family:basis

@f ScaleWithin (x:vec, g:fct) \| Z-score within groups -\>
s3:ScaleWithin +family:basis

@f RobustScale (x:vec) \| Robust scale (median/MAD) -\> s3:RobustScale
+family:basis

@s3g predict (object:any, newdata:vec, …) \| Predict basis values -\>
mat +family:basis !cov \[BSpline, Poly, Ident, Standardized, Scale,
ScaleWithin, RobustScale, ParametricBasis\]

@s3g sub_basis (x:any, subset:int\[\]\|lgl\[\]) \| Subset basis object
-\> s3:ParametricBasis +family:basis !cov \[BSpline, Poly, Ident,
Standardized, Scale, ScaleWithin, RobustScale\]

@s3g levels (x:any, …) \| Basis or factor levels -\> chr\[\]
+family:basis !cov \[BSpline, Poly, Ident, Standardized, Scale,
ScaleWithin, RobustScale\]

@s3g columns (x:any, …) \| Basis column names -\> chr\[\] +family:basis
!cov \[BSpline, Poly, Ident, Standardized, Scale, ScaleWithin,
RobustScale\]

@s3g nbasis (x:any, …) \| Number of basis functions -\> int
+family:basis !cov \[BSpline, Poly, Ident, Standardized, Scale,
ScaleWithin, RobustScale, covariate_convolved_term, hrfspec\]

## 4. Contrast Specification and Weights

@f contrast ( form:Fml, name:chr, where:Fml?=NULL ) \| Formula-based
contrast spec -\> s3:contrast_formula_spec +family:contrast

@f unit_contrast ( A:Fml, name:chr, where:Fml?=NULL ) \| Unit contrast
(sum=1) -\> s3:unit_contrast_spec +family:contrast

@f pair_contrast ( A:Fml, B:Fml, name:chr, where:Fml?=NULL,
basis:int\[\]\|chr\[“all”\]?=NULL, basis_weights:vec?=NULL ) \| Pairwise
sum-to-zero contrast -\> s3:pair_contrast_spec +family:contrast

@f oneway_contrast ( A:Fml, name:chr, where:Fml?=NULL,
basis:int\[\]\|chr\[“all”\]?=NULL, basis_weights:vec?=NULL ) \| One-way
(main effect) contrast -\> s3:oneway_contrast_spec +family:contrast

@f interaction_contrast ( A:Fml, name:chr, where:Fml?=NULL ) \|
Interaction contrast spec -\> s3:interaction_contrast_spec
+family:contrast

@f poly_contrast ( A:Fml, name:chr, where:Fml?=NULL,
degree:int\[1..\]?=1, value_map:lst?=NULL,
basis:int\[\]\|chr\[“all”\]?=NULL, basis_weights:vec?=NULL ) \|
Polynomial trend contrast -\> s3:poly_contrast_spec +family:contrast

@f column_contrast ( pattern_A:chr, pattern_B:chr?=NULL, name:chr,
where:Fml?=NULL ) \| Contrast by column regex -\>
s3:column_contrast_spec +family:contrast

@f contrast_set (…:s3:contrast_spec) \| Set of contrasts -\>
s3:contrast_set +family:contrast

@f pairwise_contrasts ( levels:chr\[\], facname:chr, where:Fml?=NULL,
name_prefix:chr?=“con” ) \| All pairwise contrasts -\> s3:contrast_set
+family:contrast

@f one_against_all_contrast ( levels:chr\[\], facname:chr,
where:Fml?=NULL ) \| Each level vs others -\> s3:contrast_set
+family:contrast

@f sliding_window_contrasts ( levels:chr\[\], facname:chr,
window_size:int\[1..\]?=2, where:Fml?=NULL, name_prefix:chr?=“win” ) \|
Disjoint window contrasts -\> s3:contrast_set +family:contrast

@s3g contrast_weights (x:any, term:any, …) \| Compute contrast weights
-\> lst\|mat +family:contrast !cov \[unit_contrast_spec,
oneway_contrast_spec, interaction_contrast_spec, poly_contrast_spec,
pair_contrast_spec, column_contrast_spec, contrast_formula_spec,
contrast_diff_spec, contrast_set\]

@s3g Fcontrasts (x:any, …) \| Compute F-contrasts -\> lst\<mat\>
+family:contrast !cov \[convolved_term, event_model, event_term\]

@s3g print
(x:s3:contrast_set\|s3:contrast_spec\|s3:contrast\|s3:poly_contrast_spec\|s3:contrast_diff_spec,
…) \| Print contrast info -\> invisible +family:contrast
+side:console\[print\]

@f plot_contrasts (x:any, …) \| Plot contrast weights heatmap -\> Gg
+family:contrast +side:plot\[device\] !cov \[event_model\]

@f -.contrast_spec (e1:s3:contrast_spec, e2:s3:contrast_spec, …) \|
Difference of contrasts -\> s3:contrast_diff_spec +family:contrast

## 5. Covariate and Event Model Utilities

@f covariate ( …:expr, <data:DF>, id:chr?=NULL, prefix:chr?=NULL,
subset:expr?=NULL ) \| Covariate term (non-convolved) -\>
s3:covariatespec +family:covariate

@s3g construct (x:s3:covariatespec, model_spec:any,
sampling_frame:any?=NULL, …) \| Construct covariate term -\>
s3:covariate_convolved_term +family:covariate

@s3g event_table (x:s3:covariate_convolved_term, …) \| Covariate event
table -\> df +family:covariate

@s3g nbasis (x:s3:covariate_convolved_term, …) \| Number of covariate
columns -\> int +family:covariate

## 6. Design Metadata and Diagnostics

@s3g design_colmap (x:any, …) \| Column metadata tibble -\> df
+family:design !cov \[event_model, baseline_model\]

@s3g term_indices (x:any, …) \| Map terms to column indices -\>
lst\<int\[\]\> +family:design !cov \[default\]

## 7. S3 Generics for Event/Baseline Models

@s3g event_model (formula_or_list:Fml\|lst, <data:DF>, block:Fml,
sampling_frame:any, drop_empty:lgl?=TRUE, durations:vec?=0, …) \| Create
event model -\> s3:event_model +family:model

@s3g event_terms (x:any, …) \| Extract event terms -\> lst +family:model

@s3g conditions (x:any, drop.empty:lgl?=TRUE, expand_basis:lgl?=FALSE,
…) \| Condition names -\> chr\[\] +family:model

@s3g convolve (x:any, hrf:any, sampling_frame:any, drop.empty:lgl?=TRUE,
summate:lgl?=TRUE, precision:dbl?=0.1, …) \| Convolve events with HRF
-\> mat +family:model

@s3g elements (x:any, what:chr\[“values”\|“labels”\]?=“values”,
transformed:lgl?=TRUE, …) \| Extract event elements -\> any
+family:model

@s3g event_conditions (x:any, drop.empty:lgl?=FALSE, …) \| Per-event
condition assignments -\> fct +family:model

@s3g events (x:any, drop.empty:lgl?=FALSE, …) \| Event info table -\> df
+family:model

@s3g is_categorical (x:any, …) \| Is object categorical? -\> lgl
+family:model

@s3g is_continuous (x:any, …) \| Is object continuous? -\> lgl
+family:model

@s3g longnames (x:any, …) \| Fully qualified event names -\> chr\[\]
+family:model

@s3g shortnames (x:any, …) \| Short event names -\> chr\[\]
+family:model

@s3g regressors (x:any, …) \| Extract regressor names -\> chr\[\]
+family:model

@s3g split_by_block (x:any, …) \| Split object by block/run -\> lst
+family:model

@s3g split_onsets (x:any, sframe:any, global:lgl?=FALSE,
blocksplit:lgl?=FALSE, …) \| Split onsets by block/condition -\>
lst\<vec\> +family:model

@s3g residualize (x:any, <data:mat>\|df, cols:int\[\]\|chr\[\]?=NULL, …)
\| Residualize data against design -\> mat +family:model

## 8. Miscellaneous

@f check_collinearity (x:mat\|df, tol:dbl?=1e-8) \| Check for
collinearity in matrix -\> lgl +family:diagnostic

@f basis_suffix (nbasis:int\[1..\], pad:int\[1..\]?=2) \| Generate basis
suffixes -\> chr\[\] +family:utility

@f condition_basis_list ( x:s3:event_term, hrf:any, sampling_frame:any,
…:expr, output:chr\[“condition_list”\|“matrix”\]?=“condition_list” ) \|
Split event term by condition basis -\> lst\<mat\>\|mat +family:utility

## 9. Re-exports

@x blockids \| Block IDs from fmrihrf -\> int\[\]
+wraps:fmrihrf::blockids +see:fmrihrf::blockids

@x blocklens \| Block lengths from fmrihrf -\> int\[\]
+wraps:fmrihrf::blocklens +see:fmrihrf::blocklens

@x HRF \| HRF object from fmrihrf -\> s3:HRF +wraps:fmrihrf::HRF
+see:fmrihrf::HRF

@x as_hrf \| Coerce to HRF from fmrihrf -\> s3:HRF
+wraps:fmrihrf::as_hrf +see:fmrihrf::as_hrf

@x durations \| Event durations from fmrihrf -\> vec
+wraps:fmrihrf::durations +see:fmrihrf::durations

@x onsets \| Event onsets from fmrihrf -\> vec +wraps:fmrihrf::onsets
+see:fmrihrf::onsets

@x gen_hrf \| Generate HRF from fmrihrf -\> s3:HRF
+wraps:fmrihrf::gen_hrf +see:fmrihrf::gen_hrf

@x hrf_spmg1 \| SPMG1 HRF from fmrihrf -\> s3:HRF
+wraps:fmrihrf::hrf_spmg1 +see:fmrihrf::hrf_spmg1

## Dependencies

- Imports: fmrihrf (\>= 0.1.0), stats, assertthat, rlang, stringr,
  dplyr, tidyr, purrr, tibble, Matrix, splines, plotly, ggplot2, utils,
  cli
- Suggests: testthat, knitr, rmarkdown, covr

------------------------------------------------------------------------

## Meta-Footer

- Micro-DSL Version: v3.4-source
- Package: fmridesign (Version: 0.1.0)
- Generated: 2024-06-29T00:00:00Z
- Features: types\[constrained\] sigils\[specific\] metadata\[rich\]
  semantics\[annotated\]
- Coverage: 70 / 70 exports
- Provenance: exports\[NAMESPACE\], enums\[match.arg/switch\],
  constraints\[assertions/checks\]
