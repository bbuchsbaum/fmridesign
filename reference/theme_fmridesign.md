# ggplot2 theme for fmridesign plots

A restrained theme: left-aligned titles, recessive grid, no panel
border, and small uppercase-free strip labels. All plotting methods in
the package use it; add it to your own plots to match.

## Usage

``` r
theme_fmridesign(base_size = 11, base_family = "")
```

## Arguments

- base_size:

  Base font size in points.

- base_family:

  Base font family.

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
