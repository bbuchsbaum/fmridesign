# Adding New Golden Tests

This guide explains how to add new golden tests and ensure they propagate correctly to all language implementations.

## When to Add New Golden Tests

Add a golden test when:
- Implementing new functionality that should work identically across languages
- Discovering edge cases that need consistent handling
- Finding bugs that should be prevented in all implementations
- Adding features that define core semantic behavior

## Workflow for Adding Tests

### Step 1: Create the Test Specification

Create a new XML file in the appropriate directory:

```bash
# For core functionality
golden_tests/specs/core/[feature]/new_test.xml

# For integration tests
golden_tests/specs/integration/new_test.xml

# For edge cases
golden_tests/specs/edge_cases/new_test.xml
```

### Step 2: Write the Specification

Start with the semantic description and expected behavior:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<golden_test xmlns="http://golden-tests.org/schema">
  <metadata>
    <id>feature_edge_case_name</id>
    <version>1.0</version>
    <description>Test edge case in feature X</description>
    <tags>
      <tag>feature_x</tag>
      <tag>edge_case</tag>
    </tags>
  </metadata>
  
  <semantic_description>
    <purpose>What behavior is being tested</purpose>
    <algorithm>
      Mathematical/algorithmic description
    </algorithm>
  </semantic_description>
  
  <inputs>
    <!-- Define inputs -->
  </inputs>
  
  <expected_outputs>
    <!-- Define expected numeric outputs -->
  </expected_outputs>
  
  <implementations>
    <!-- Initially only R implementation -->
    <R><![CDATA[
      # R implementation
    ]]></R>
  </implementations>
</golden_test>
```

### Step 3: Implement and Validate in R

1. Run the R validator to ensure your test works:
   ```bash
   cd golden_tests/validators/R
   Rscript validate_specs.R ../../specs/core/feature/new_test.xml
   ```

2. Adjust tolerances and expected values based on actual outputs

3. Ensure the test captures the intended behavior

### Step 4: Document the Propagation Status

Add a `propagation_status` section to track implementation status:

```xml
<propagation_status>
  <implementation lang="R" status="complete" date="2024-01-15"/>
  <implementation lang="Python" status="pending"/>
  <implementation lang="Rust" status="pending"/>
</propagation_status>
```

### Step 5: Notify Other Implementations

The propagation depends on how golden tests are shared:

#### If Using Git Submodules

Other implementations will get the new test when they update:

```bash
# In Python repo
git submodule update --remote golden_tests
# They'll see the new test and failing tests

# In Rust repo  
git submodule update --remote golden_tests
```

#### If Using a Separate Golden Tests Repository

1. Push the new test to the golden tests repository
2. Other implementations pull the updates
3. Their CI/CD will show failing tests for unimplemented features

#### If Copying Specs

1. Create an issue in each implementation's repository
2. Or maintain a `PENDING_TESTS.md` file listing tests awaiting implementation

### Step 6: Implementations Add Support

As each implementation adds support:

1. They add their code to the XML:
   ```xml
   <implementations>
     <R><![CDATA[...]]></R>
     <Python><![CDATA[
       # Python implementation
       import fmridesign as fd
       # ...
     ]]></Python>
   </implementations>
   ```

2. Update propagation status:
   ```xml
   <implementation lang="Python" status="complete" date="2024-01-20"/>
   ```

3. Submit PR to update the shared spec

## Best Practices for New Tests

### 1. Start Simple
- Test one specific behavior
- Use minimal inputs
- Focus on clear numeric outputs

### 2. Document Thoroughly
- Explain WHY this test exists
- Describe the mathematical behavior
- Include references if applicable

### 3. Consider Variations
- Does this behavior vary by parameter?
- Should you test multiple cases?
- Are there related edge cases?

### 4. Set Appropriate Tolerances
- Tighter for deterministic operations
- Looser for iterative algorithms
- Document why specific tolerances were chosen

## Handling Implementation Differences

Sometimes a test reveals that implementations MUST differ:

```xml
<implementation_notes>
  <note lang="Python">
    Python uses scipy.linalg which may produce slightly different eigenvalues
    in edge cases. Tolerance increased to 1e-6 for this test.
  </note>
  <note lang="Rust">
    Rust implementation uses single precision for performance. 
    Cannot achieve same precision as R/Python.
  </note>
</implementation_notes>
```

## Example: Adding a Test for New Feature

Let's say R adds support for RETROICOR baseline correction:

### 1. Create the test:
```bash
touch golden_tests/specs/core/baseline_model/retroicor.xml
```

### 2. Write the specification:
```xml
<?xml version="1.0" encoding="UTF-8"?>
<golden_test xmlns="http://golden-tests.org/schema">
  <metadata>
    <id>baseline_model_retroicor</id>
    <version>1.0</version>
    <description>Test RETROICOR physiological noise correction</description>
    <tags>
      <tag>baseline_model</tag>
      <tag>retroicor</tag>
      <tag>physiological</tag>
      <tag>new_feature</tag>
    </tags>
  </metadata>
  
  <semantic_description>
    <purpose>Verify RETROICOR basis functions for cardiac/respiratory correction</purpose>
    <algorithm>
      1. Generate sine/cosine basis functions for cardiac phase
      2. Generate sine/cosine basis functions for respiratory phase  
      3. Create interaction terms up to specified order
      4. Combine into design matrix for physiological noise regression
    </algorithm>
  </semantic_description>
  
  <!-- ... rest of spec ... -->
  
  <propagation_status>
    <implementation lang="R" status="complete" date="2024-01-15"/>
    <implementation lang="Python" status="pending">
      Waiting for scipy.signal.retroicor or custom implementation
    </implementation>
    <implementation lang="Rust" status="pending"/>
  </propagation_status>
</golden_test>
```

### 3. Python/Rust implementations can:
- See the new test in their test suite (it will fail)
- Implement the feature based on the semantic description
- Add their implementation when ready
- Update the propagation status

## Versioning Considerations

When tests need updates:

1. Increment the test version
2. Document what changed:
   ```xml
   <version>1.1</version>
   <version_history>
     <change version="1.1" date="2024-02-01">
       Adjusted tolerance for column_sum from 0.01 to 0.05
       to accommodate single-precision implementations
     </change>
   </version_history>
   ```

3. All implementations should update to match new expectations

## Summary

Adding new golden tests is straightforward:
1. Create test in the R implementation
2. Other implementations see failing tests  
3. They implement based on semantic description
4. Everyone stays in sync

This ensures that new features are properly specified and that all implementations can provide equivalent functionality.