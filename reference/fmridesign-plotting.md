# Shared visual system for fmridesign plots

Every plotting method in the package draws from the same theme,
palettes, and column metadata so that a regressor keeps its colour and
its label across
[`plot()`](https://rdrr.io/r/graphics/plot.default.html),
[`design_map()`](https://bbuchsbaum.github.io/fmridesign/reference/design_map.md),
[`correlation_map()`](https://bbuchsbaum.github.io/fmridesign/reference/correlation_map.md)
and
[`plot_contrasts()`](https://bbuchsbaum.github.io/fmridesign/reference/plot_contrasts.md).
