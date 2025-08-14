# Golden Test Implementation Tracking

This file tracks the implementation status of golden tests across languages.

Last updated: 2024-01-15

## Implementation Status

| Test ID | R | Python | Rust | Notes |
|---------|---|--------|------|-------|
| event_model_basic_hrf | ✅ | ⏳ | ⏳ | Core functionality |
| event_model_multiple_conditions | ✅ | ⏳ | ⏳ | Factorial designs |
| event_model_continuous_regressors | ✅ | ⏳ | ⏳ | Parametric modulation |
| event_model_multi_block | ✅ | ⏳ | ⏳ | Multi-run support |
| baseline_model_polynomial_drift | ✅ | ⏳ | ⏳ | Polynomial detrending |
| baseline_model_spline_drift | ✅ | ⏳ | ⏳ | Spline detrending |
| hrf_bases_spm_derivatives | ✅ | ⏳ | ⏳ | SPM basis set |

### Legend
- ✅ Complete
- ⏳ Pending
- 🚧 In Progress
- ❌ Blocked (see notes)
- ⚠️ Partial (see notes)

## Recently Added Tests

### 2024-01-15
- Initial test suite created with 7 core tests
- All tests implemented in R
- Python and Rust implementations pending

## Blocked/Partial Implementations

None currently.

## Notes for Implementers

### Python
- Requires numpy, scipy for numeric operations
- Consider using niBabel for fMRI-specific functionality
- HRF functions may need custom implementation or port from R

### Rust
- Consider ndarray for matrix operations
- May need custom HRF implementation
- Performance optimizations welcome if outputs match

## How to Update This File

When you implement a test in your language:
1. Change ⏳ to ✅ for your language
2. Add date to "Recently Added Tests" if completing a set
3. Note any implementation challenges in "Notes"
4. Submit PR with your implementation