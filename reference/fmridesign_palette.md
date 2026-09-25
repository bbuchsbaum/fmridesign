# Colour palettes used by fmridesign plots

`fmridesign_palette()` returns the package's categorical palette (a
colour-vision-deficiency-checked ordering) or its diverging ramp, so
custom plots can match the built-in ones.

## Usage

``` r
fmridesign_palette(type = c("categorical", "diverging"), n = NULL)
```

## Arguments

- type:

  One of `"categorical"` or `"diverging"`.

- n:

  Number of colours. For `"categorical"`, at most 7 distinct hues are
  available; more than that is an error because recycled hues are
  indistinguishable. For `"diverging"`, the ramp is interpolated to `n`.

## Value

A character vector of hex colours.

## Examples

``` r
fmridesign_palette("categorical", 4)
#> [1] "#0072B2" "#D55E00" "#009E73" "#C28A00"
fmridesign_palette("diverging", 11)
#>  [1] "#1D4E79" "#4474A2" "#709BC6" "#A7C2DE" "#D2DFEB" "#F7F6F3" "#F2D2C4"
#>  [8] "#E6A992" "#D0775B" "#AF4E36" "#8A2A1B"
```
