# Golden Tests for fmridesign

Language-agnostic test specifications ensuring semantic equivalence across implementations.

## Quick Start

1. **For R users**: Run `Rscript validators/R/validate_specs.R` from this directory
2. **For porters**: See [INTEGRATION_GUIDE.md](INTEGRATION_GUIDE.md)
3. **For methodology**: See [GOLDEN_TEST_METHODOLOGY.md](GOLDEN_TEST_METHODOLOGY.md)

## Documentation

- [GOLDEN_TEST_METHODOLOGY.md](GOLDEN_TEST_METHODOLOGY.md) - Overall methodology and design principles
- [INTEGRATION_GUIDE.md](INTEGRATION_GUIDE.md) - How to integrate golden tests into new language ports
- [ADDING_NEW_TESTS.md](ADDING_NEW_TESTS.md) - How to add new golden tests
- [TEST_TRACKING.md](TEST_TRACKING.md) - Current implementation status across languages

## Structure

```
golden_tests/
├── specs/                  # XML test specifications
│   ├── core/              # Core functionality tests
│   ├── integration/       # Integration tests
│   └── edge_cases/        # Edge case tests
├── schema/                # XML schema definition
├── validators/            # Language-specific validators
│   └── R/                # R reference validator
└── *.md                   # Documentation files
```

## Quick Overview

### For R Package Maintainers
- Add new tests to `specs/` when adding features
- Run validator to ensure tests pass
- Document new tests in TEST_TRACKING.md

### For New Language Implementers
1. Copy/submodule these specs into your implementation
2. Create a validator in your test suite
3. Implement functionality to pass tests
4. Add your implementation to the XML files
5. Submit PR to share your implementation

### For Contributors
- See failing tests in TEST_TRACKING.md
- Pick a test to implement
- Follow INTEGRATION_GUIDE.md
- Update tracking when complete