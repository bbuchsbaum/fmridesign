# Summary of Vignette Fixes for fmridesign Package

## Issues Fixed

### 1. Namespace conflict with evaluate function
- **Problem**: fmrihrf's internal `.memo_hrf` function called `evaluate()` without namespace prefix, conflicting with knitr's evaluate
- **Resolution**: User fixed the bug in fmrihrf by qualifying the call as `fmrihrf::evaluate()`
- **Action taken**: Removed all workarounds from vignettes after the fix

### 2. Missing design_map functionality
- **Problem**: design_map was accidentally left in fmrireg when fmridesign was extracted
- **Resolution**: Created new `design_map.R` file with:
  - `design_map.event_model()` - visualizes design matrix as heatmap
  - `correlation_map.event_model()` - visualizes correlation matrix
  - `plot.event_model()` - plots predicted BOLD timecourses
- **Key fix**: Updated grep pattern in plot method to handle underscore separators in column names

### 3. Documentation and namespace issues
- Fixed parameter documentation for baseline_model methods
- Added blockid and allrows parameters to design_matrix.baseline_model
- Fixed S3 method signature for labels.event (changed x to object)
- Added missing imports:
  - `stats::contr.sum` for contrast matrices
  - `stats::sd` for standard deviation
  - `rlang::=` for tidy evaluation
- Created missing `matrix_term()` internal function

### 4. Vignette-specific fixes

#### a_02_regressor.Rmd
- Changed from `onsets(reg)` to `reg$onsets` for direct field access
- Updated all references to use fmrihrf methods correctly

#### a_03_baseline_model.Rmd  
- Added time and blockids fields to sampling_frame for plotting:
  ```r
  sframe$time <- fmrihrf::samples(sframe_raw, global = TRUE)
  sframe$blockids <- fmrihrf::blockids(sframe_raw)
  ```
- Fixed term names from "poly" to "drift"

#### a_04_event_models.Rmd
- Changed library(fmrireg) to library(fmridesign)
- Fixed plot calls and removed faceting that didn't work with multiple blocks
- Fixed RT_centered term plotting by updating grep pattern

## Current Status
- All vignettes build successfully
- R CMD check passes with minor notes about hidden .claude directory
- Package is ready for use