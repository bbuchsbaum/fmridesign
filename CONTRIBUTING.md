# Contributing to fmridesign

Bug reports and focused pull requests are welcome. Before opening an issue,
please search the [existing issues](https://github.com/bbuchsbaum/fmridesign/issues)
and include a minimal reproducible example, the observed result, the expected
result, and the output of `sessionInfo()`.

For code changes:

1. Create a branch from the current `main` branch.
2. Add or update tests for behavior changes.
3. Run `devtools::test()` and `devtools::check()` locally.
4. Open a pull request describing the user-visible effect and verification.

Please keep pull requests narrowly scoped. Changes to exported APIs or
statistical behavior should explain the compatibility and scientific
implications explicitly.
