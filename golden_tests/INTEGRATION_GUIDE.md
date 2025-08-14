# Golden Test Integration Guide for New Language Ports

This guide explains how to integrate the golden test system when creating a new implementation of fmridesign in another language (Python, Rust, Julia, etc.).

## Overview

The golden test system ensures semantic equivalence across implementations. When porting to a new language, you'll:

1. Get access to the golden test specifications (via submodule, copy, or separate repo)
2. Implement the functionality based on semantic descriptions
3. Create a language-specific validator in your test suite
4. Add your implementation code to the test specifications
5. Verify numeric outputs match expected values
6. Contribute your implementations back to the shared specs

## Step-by-Step Integration Process

### Step 1: Understand the Test Specifications

Before implementing, study the XML test files in `specs/`:

```bash
golden_tests/specs/
├── core/
│   ├── event_model/
│   │   ├── basic_hrf.xml          # Start here - simplest case
│   │   ├── multiple_conditions.xml
│   │   ├── continuous_regressors.xml
│   │   └── multi_block.xml
│   ├── baseline_model/
│   └── hrf_bases/
```

Each XML file contains:
- **semantic_description**: Mathematical algorithm (language-agnostic)
- **inputs**: Test data and parameters
- **expected_outputs**: Numeric values to match
- **implementations/R**: Reference implementation

### Step 2: Set Up Golden Tests in Your Repository

First, get the golden test specifications into your repository:

```bash
# Option 1: Git submodule (recommended for staying in sync)
cd your-python-repo
git submodule add https://github.com/user/fmridesign.git fmridesign-r
ln -s fmridesign-r/golden_tests golden_tests

# Option 2: Copy the specs (simpler but requires manual updates)
cp -r ../fmridesign/golden_tests/specs ./golden_tests/specs
cp -r ../fmridesign/golden_tests/schema ./golden_tests/schema
```

### Step 3: Create Your Language's Validator

Create a validator in your test suite (NOT in the golden_tests directory):

#### Python Validator Example Structure:

```python
# tests/test_golden.py (in your Python package)
import xml.etree.ElementTree as ET
import numpy as np
from pathlib import Path
import pytest

class GoldenTestValidator:
    def __init__(self, specs_dir='../golden_tests/specs'):
        self.specs_dir = Path(specs_dir)
        
    def parse_golden_test(self, xml_path):
        """Parse XML specification"""
        # Extract metadata, expected outputs, Python code
        
    def perform_numeric_check(self, matrix, check):
        """Execute a numeric validation"""
        # Implement each check type (peak_value, column_sum, etc.)
        
    def validate_test(self, xml_path):
        """Run a single test"""
        # 1. Parse XML
        # 2. Execute Python code
        # 3. Validate outputs
        # 4. Report results

# Integrate with pytest
@pytest.mark.golden
class TestGoldenSpecs:
    def test_all_specs(self):
        validator = GoldenTestValidator()
        for spec in validator.specs_dir.glob('**/*.xml'):
            with self.subTest(spec=spec.name):
                result = validator.validate_test(spec)
                assert result['success'], f"Failed: {result['error']}"
```

### Step 4: Implement Core Functionality

Start with the simplest test (`basic_hrf.xml`) and implement the required functionality:

1. **Sampling Frame**: Time series structure (blocks, TR)
2. **Event Model**: HRF convolution, design matrix generation
3. **Baseline Model**: Drift correction
4. **HRF Functions**: Hemodynamic response functions

### Step 5: Add Python Implementation to XML

Once your code works, add it to the XML specification:

```xml
<implementations>
  <R><![CDATA[
    # Existing R code
  ]]></R>
  
  <Python><![CDATA[
import fmridesign as fd
import numpy as np

# Create sampling frame
sframe = fd.SamplingFrame(block_lengths=[100], tr=2.0)

# Create event data
events = fd.EventData({
    'onset': [10.0, 30.0, 50.0, 70.0],
    'condition': ['A', 'B', 'A', 'B'],
    'block': [1, 1, 1, 1]
})

# Create event model
ev_model = fd.EventModel(
    formula='onset ~ hrf(condition)',
    data=events,
    block='block',
    sampling_frame=sframe,
    precision=0.3
)

# Get design matrix
dm = ev_model.design_matrix()
dm_matrix = dm.to_numpy()  # Convert to numpy for validation
  ]]></Python>
</implementations>
```

### Step 6: Validate Your Implementation

Run your validator to check all tests:

```bash
# From your Python package root
pytest tests/test_golden.py -v

# Or for a specific test pattern:
pytest tests/test_golden.py -k "basic_hrf" -v

# Or run directly:
python -m pytest tests/test_golden.py::TestGoldenSpecs::test_all_specs
```

### Step 7: Handle Numeric Differences

Common issues and solutions:

1. **Floating-point precision**: Use appropriate tolerances
2. **Algorithm variations**: Document acceptable differences
3. **Indexing**: Convert between 0-based and 1-based as needed
4. **Matrix storage**: Row-major vs column-major ordering

### Step 8: Contribute Back Your Implementation

Once your tests pass:

1. Fork the repository containing the golden tests
2. Add your Python implementation to each XML file
3. Submit a pull request
4. The implementations now serve as a Rosetta Stone for future porters

### Step 9: Create Cross-Language Comparison

Once validated, create comparison reports:

```python
# compare_implementations.py
def compare_languages(xml_path, languages=['R', 'Python']):
    """Compare outputs across implementations"""
    results = {}
    for lang in languages:
        results[lang] = validate_language(xml_path, lang)
    
    # Generate comparison report
    # Show where implementations agree/differ
```

## Best Practices for New Implementations

### 1. Start Simple
- Begin with `basic_hrf.xml`
- Get one test fully working before moving on
- Build complexity gradually

### 2. Match Semantics, Not Syntax
- Focus on mathematical equivalence
- Your API can be idiomatic to your language
- Internal implementation can differ

### 3. Document Deviations
If your implementation must deviate:
```xml
<implementation_notes>
  <Python>
    Uses scipy.signal for convolution instead of custom implementation.
    Results match within tolerance of 1e-6.
  </Python>
</implementation_notes>
```

### 4. Contribute Back
- Add your implementation to all relevant test files
- Update this guide with language-specific tips
- Share any new test cases you develop

## Complete Example: Python Implementation Workflow

Here's a step-by-step example for Python:

### 1. Set up your Python package structure:
```
fmridesign-py/
├── fmridesign/
│   ├── __init__.py
│   ├── event_model.py
│   ├── baseline_model.py
│   └── hrf.py
├── tests/
│   ├── __init__.py
│   └── test_golden.py      # Your validator lives here
├── golden_tests/            # Submodule or copy
│   ├── specs/              # The shared XML specifications
│   └── schema/             # XML schema
└── setup.py
```

### 2. Link to golden tests:
```bash
# Add as submodule
git submodule add https://github.com/user/fmridesign.git upstream-r
ln -s upstream-r/golden_tests golden_tests

# Or if golden tests are in separate repo
git submodule add https://github.com/user/fmridesign-golden-tests.git golden_tests
```

### 3. Implement based on semantic descriptions:
```python
# fmridesign/event_model.py
class EventModel:
    """
    Following golden_tests/specs/core/event_model/basic_hrf.xml:
    1. Accept events with onsets and conditions
    2. Convolve with HRF
    3. Sample at TR intervals
    """
    def __init__(self, formula, data, block, sampling_frame, precision=0.3):
        # Implementation following semantic description
```

### 4. Create validator and run tests:
```python
# tests/test_golden.py
import pytest
from pathlib import Path
import xml.etree.ElementTree as ET
import numpy as np

class GoldenTestValidator:
    # Your validator implementation here
    pass

@pytest.mark.golden
@pytest.mark.parametrize("xml_file", 
    Path(__file__).parent.parent.joinpath('golden_tests/specs').glob('**/*.xml'))
def test_golden_spec(xml_file):
    validator = GoldenTestValidator()
    result = validator.validate_test(xml_file)
    assert result['success'], f"Failed: {result['error']}"
```

### 5. Run tests:
```bash
# During development
pytest tests/test_golden.py::test_golden_spec -v

# Run specific test
pytest tests/test_golden.py::test_golden_spec[basic_hrf.xml] -v
```

### 6. Add your implementation to XML files and contribute back!

## Adding Tests for New Features

If your implementation adds new features:

1. Create new XML specification
2. Include only your language initially
3. Document the feature thoroughly
4. Other implementations can add support later

## Continuous Integration

Add golden tests to your CI pipeline:

```yaml
# .github/workflows/golden_tests.yml
- name: Run Golden Tests
  run: |
    cd golden_tests/validators/Python
    python validate_specs.py
    python generate_report.py
```

## FAQ

**Q: What if my language doesn't support a specific feature?**
A: Document it in the XML and skip that test. Partial compatibility is OK.

**Q: Can I use different algorithms if outputs match?**
A: Yes! The tests verify outputs, not implementations.

**Q: Should I translate variable names?**
A: Use idiomatic names for your language. The XML shows the mapping.

**Q: How exact must numeric matches be?**
A: Use the specified tolerances. Document if you need different tolerances.

## Next Steps

1. Start with basic_hrf.xml
2. Create your validator
3. Implement core functionality
4. Add your code to XML files
5. Validate and iterate
6. Share your implementation!

The golden test system ensures that users get consistent results regardless of which implementation they choose, while allowing each language to leverage its strengths.