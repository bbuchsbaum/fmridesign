# ggplot2 theme for fmridesign plots

A restrained theme: left-aligned titles, recessive grid, no panel
border, and small uppercase-free strip labels. All plotting methods in
the package use it; add it to your own plots to match.

## Usage

``` r
theme_fmridesign(
  base_size = getOption("fmridesign.base_size", getOption("neuro.base_size", 11)),
  base_family = getOption("fmridesign.base_family", getOption("neuro.base_family", ""))
)
```

## Arguments

- base_size:

  Base font size in points. Defaults to the option
  `fmridesign.base_size`, then the shared `neuro.base_size`, then 11, so
  a host application can set the size of every figure at once.

- base_family:

  Base font family. Defaults to the option `fmridesign.base_family`,
  then `neuro.base_family`, then `""` (the device's default sans).

## Value

A ggplot2 theme object.

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    theme_fmridesign()
}
```
