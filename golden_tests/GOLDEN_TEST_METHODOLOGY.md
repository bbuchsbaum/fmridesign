# Golden Test Specification Methodology

## Overview

This document describes a methodology for creating language-agnostic test specifications that ensure semantic equivalence across multiple implementations of the same software package. These "golden tests" focus on capturing the mathematical and algorithmic behavior of a system rather than implementation-specific details.

## Motivation

When porting software between languages (e.g., R → Python → Rust), ensuring functional equivalence is critical but challenging. Traditional unit tests are language-specific and test implementation details. Golden tests instead:

- Define expected behavior in semantic terms
- Focus on numeric outputs and mathematical relationships
- Allow each language to implement the same semantics differently
- Provide a "Rosetta Stone" for cross-language validation

## Core Principles

### 1. Language Agnosticism
Tests should describe WHAT the code should do, not HOW it does it:
- ❌ "Create a DataFrame with columns A and B"
- ✅ "Create a two-column numeric matrix with 50 rows"

### 2. Numeric Focus
All validations should be based on numeric outputs:
- Matrix dimensions
- Specific values at known locations
- Statistical properties (sums, means, ranges)
- Relative tolerances for floating-point comparison

### 3. Semantic Descriptions
Each test must include:
- Purpose: What aspect of functionality is being tested
- Algorithm: Step-by-step description of the mathematical operations
- Expected behavior: Clear specification of outputs given inputs

### 4. Progressive Enhancement
Each language adds its implementation without modifying the core specification:
```
Spec (language-agnostic) → R implementation → Python implementation → Rust implementation
```

## Test Specification Format

### XML Structure

```xml
<?xml version="1.0" encoding="UTF-8"?>
<golden_test>
  <metadata>
    <id>unique_test_identifier</id>
    <version>1.0</version>
    <description>Brief description of test</description>
    <tags>
      <tag>category1</tag>
      <tag>category2</tag>
    </tags>
  </metadata>
  
  <semantic_description>
    <purpose>What functionality is being tested</purpose>
    <algorithm>
      Step-by-step mathematical description
    </algorithm>
  </semantic_description>
  
  <inputs>
    <!-- Structured input data -->
  </inputs>
  
  <expected_outputs>
    <!-- Numeric validations -->
  </expected_outputs>
  
  <implementations>
    <!-- Language-specific code -->
  </implementations>
</golden_test>
```

### Key Sections

#### Metadata
- **id**: Unique identifier for the test
- **version**: Specification version (not implementation version)
- **description**: One-line summary
- **tags**: Categories for organization and filtering

#### Semantic Description
- **purpose**: Clear statement of what aspect is being tested
- **algorithm**: Mathematical/algorithmic steps, language-agnostic

#### Inputs
Structured data representing test inputs:
- Use standard formats (numbers, arrays, key-value pairs)
- Avoid language-specific types
- Include all parameters needed to reproduce the test

#### Expected Outputs
Numeric validations with tolerances:
```xml
<numeric_checks>
  <check>
    <type>exact_value|range|relative|statistical</type>
    <location>where to check (e.g., matrix position)</location>
    <expected>expected value or range</expected>
    <tolerance>acceptable deviation</tolerance>
  </check>
</numeric_checks>
```

#### Implementations
Each language adds its code in a CDATA section:
```xml
<R><![CDATA[
  # R implementation
]]></R>
<Python><![CDATA[
  # Python implementation
]]></Python>
```

## Validation Types

### 1. Dimensional Checks
- Matrix/array dimensions
- Number of elements
- Shape consistency

### 2. Value Checks
- **Exact**: For integers or categorical outputs
- **Approximate**: For floating-point with tolerance
- **Range**: Value falls within bounds
- **Relative**: Percentage-based tolerance

### 3. Statistical Checks
- Sum, mean, standard deviation
- Min/max values
- Percentiles
- Correlation between outputs

### 4. Structural Checks
- Column/row names (as string arrays)
- Ordering requirements
- Sparsity patterns

## Best Practices

### 1. Test Granularity
- Each test should focus on one specific behavior
- Complex behaviors should be broken into multiple tests
- Include both typical cases and edge cases

### 2. Numeric Tolerances
- Use absolute tolerance for values near zero
- Use relative tolerance for larger values
- Document why specific tolerances were chosen
- Consider numerical stability across platforms

### 3. Documentation
- Every test must be self-documenting
- Include references to academic papers or algorithms
- Explain any non-obvious numeric values

### 4. Reproducibility
- Fix all random seeds
- Specify all parameters explicitly
- Avoid relying on defaults that might differ

## Implementation Guidelines

### Adding a New Test

1. Create XML specification file
2. Define semantic behavior clearly
3. Specify all numeric checks
4. Implement in reference language (R)
5. Validate outputs match specification
6. Commit with descriptive message

### Adding a New Language

1. Do not modify existing specifications
2. Add implementation block for new language
3. Ensure all numeric checks pass
4. Document any language-specific considerations
5. Update validation scripts

### Validation Process

1. Parse XML specification
2. Extract implementation for target language
3. Execute code with specified inputs
4. Perform all numeric checks
5. Report pass/fail with details
6. Generate comparison reports

## Repository Structure and Test Sharing

### Cross-Repository Architecture

Golden tests are designed to be shared across multiple language implementations. The recommended structure is:

```
# Original implementation (R)
fmridesign/
├── golden_tests/
│   ├── GOLDEN_TEST_METHODOLOGY.md    # This file
│   ├── specs/                        # Test specifications (canonical source)
│   │   ├── core/
│   │   ├── integration/
│   │   └── edge_cases/
│   └── schema/
│       └── golden_test.xsd
├── tests/
│   └── test_golden.R                 # R-specific validator
└── R/
    └── ... (implementation files)

# Python implementation
fmridesign-py/
├── golden_tests/                     # Submodule or copy from R repo
│   └── specs/                        # Shared specifications
├── tests/
│   └── test_golden.py               # Python-specific validator
└── fmridesign/
    └── ... (implementation files)

# Rust implementation  
fmridesign-rust/
├── golden_tests/                     # Submodule or copy from R repo
│   └── specs/                        # Shared specifications
├── tests/
│   └── golden_tests.rs              # Rust-specific validator
└── src/
    └── ... (implementation files)
```

### Sharing Test Specifications

There are several approaches to sharing the golden test specifications:

#### Option 1: Git Submodule (Recommended)
```bash
# In Python repository
git submodule add https://github.com/user/fmridesign.git fmridesign-r
ln -s fmridesign-r/golden_tests golden_tests

# Or just the golden_tests as a separate repo
git submodule add https://github.com/user/fmridesign-golden-tests.git golden_tests
```

#### Option 2: Separate Repository
Create a dedicated repository for golden tests:
```
fmridesign-golden-tests/
├── GOLDEN_TEST_METHODOLOGY.md
├── specs/
├── schema/
└── examples/
    ├── example_validator.R
    ├── example_validator.py
    └── example_validator.rs
```

#### Option 3: Package Distribution
Include golden tests in package distributions:
- R: Include in `inst/golden_tests/`
- Python: Include in package data
- Rust: Include in `tests/fixtures/`

### Validator Location

Each language implementation should maintain its own validator in its natural test location:

```python
# Python: tests/test_golden.py
import xml.etree.ElementTree as ET
from pathlib import Path
import numpy as np
import pytest

class GoldenTestValidator:
    def __init__(self, specs_dir='../golden_tests/specs'):
        self.specs_dir = Path(specs_dir)
    
    def validate_spec(self, spec_path):
        # Implementation specific to Python package
        pass

# Use pytest for integration
@pytest.mark.golden
def test_all_golden_specs():
    validator = GoldenTestValidator()
    for spec in validator.specs_dir.glob('**/*.xml'):
        validator.validate_spec(spec)
```

```r
# R: tests/test_golden.R or tests/testthat/test-golden.R
test_that("golden tests pass", {
  validator <- GoldenTestValidator$new("../../golden_tests/specs")
  specs <- list.files(validator$specs_dir, pattern = "\\.xml$", 
                      recursive = TRUE, full.names = TRUE)
  
  for (spec in specs) {
    expect_true(validator$validate_spec(spec), 
                info = paste("Failed:", basename(spec)))
  }
})
```

### Adding Implementations to Specifications

When adding a new language implementation:

1. **Fork or clone** the repository containing golden tests
2. **Add your implementation** to the existing XML files:
   ```xml
   <implementations>
     <R><![CDATA[...]]></R>
     <Python><![CDATA[
       # Your Python implementation here
     ]]></Python>
   </implementations>
   ```
3. **Submit a pull request** to add your implementation
4. **Keep specifications synchronized** across repositories

### Continuous Integration

Configure CI to run golden tests:

```yaml
# .github/workflows/golden-tests.yml
name: Golden Tests
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
        with:
          submodules: true  # If using submodules
      
      - name: Set up environment
        # Language-specific setup
      
      - name: Run golden tests
        run: |
          # Language-specific test command
          pytest tests/test_golden.py -v
```

## Example Workflow

1. **Identify Behavior**: "HRF convolution should produce correct peak timing"
2. **Write Specification**: Define inputs, algorithm, expected outputs
3. **Implement Reference**: Write R code that produces expected outputs
4. **Validate**: Ensure R implementation matches specification
5. **Port**: Other languages implement same specification
6. **Cross-Validate**: All implementations produce same numeric outputs

## Benefits

- **Clarity**: Specifications document expected behavior precisely
- **Portability**: New languages can be added without modifying specs
- **Regression Testing**: Detect when changes break compatibility
- **Documentation**: Specs serve as executable documentation
- **Confidence**: Numeric validation ensures true equivalence

## Limitations and Considerations

- Cannot test UI/visualization differences
- Performance characteristics not captured
- Memory usage patterns may differ
- Language-specific optimizations allowed if outputs match
- Some algorithms may have acceptable variations

## Future Extensions

- Automated specification generation from reference implementation
- Statistical validation for stochastic algorithms
- Performance benchmarking framework
- Automated cross-language comparison reports
- Integration with CI/CD pipelines

---

This methodology provides a robust framework for ensuring semantic equivalence across implementations while allowing each language to leverage its strengths. The focus on numeric outputs and mathematical relationships creates a solid foundation for cross-language validation.