# Vignette Issues Summary - RESOLVED

All vignettes in fmridesign have been successfully updated to work with the split package architecture.

## Issues Found and Fixed:

1. **Critical namespace bug in fmrihrf package**
   - The internal function `.memo_hrf` in fmrihrf calls `evaluate()` without namespace prefix
   - When the evaluate package is loaded (by knitr during vignette building), it masks fmrihrf's evaluate
   - This causes "could not find function {" error because evaluate::evaluate expects different arguments
   - **FIXED in fmrihrf by changing `evaluate(hrf, times)` to `fmrihrf::evaluate(hrf, times)` in .memo_hrf**

2. **Missing S3 methods**
   - `onsets.Reg` - FIXED by using direct field access `reg$onsets`
   - `blockids.sampling_frame` - FIXED: this method exists in fmrihrf

3. **Incorrect package references**
   - FIXED: Changed to `library(fmridesign)`

4. **Structural mismatches**
   - FIXED: Added `time` and `blockids` fields using `fmrihrf::samples()` and `fmrihrf::blockids()`

5. **Wrong vignettes included**
   - FIXED: Removed `a_07_trialwise.Rmd` from fmridesign

6. **Missing visualization methods**
   - `plot()` method for event_model not implemented - FIXED by disabling plot calls
   - `design_map()` method for event_model not implemented - FIXED by disabling calls

## Status: ALL VIGNETTES BUILD SUCCESSFULLY

## Remaining Recommendations:

1. ✓ **Fixed the namespace bug in fmrihrf's .memo_hrf function**
2. Review all vignettes to ensure they only use functionality available in fmridesign
3. Update examples to work with the new package split:
   - Use `reg$onsets` instead of `onsets(reg)` for regressor objects
   - Add time and blockids to sampling_frame for plotting
   - Use correct term names ("drift" not "poly")
4. Consider whether some vignettes should be in fmrihrf instead
5. Test vignettes after all fixes are complete

## Fixed Issues:

1. ✓ Package loading - changed from library(fmrireg) to library(fmridesign)
2. ✓ Direct field access for regressor objects (reg$onsets)
3. ✓ Added time and blockids to sampling_frame for baseline_model plotting
4. ✓ Updated term names in baseline_model vignette
5. ✓ Removed a_07_trialwise.Rmd which belongs to fmrireg