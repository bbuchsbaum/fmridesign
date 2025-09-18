# fmridesign

Design Matrix Construction for fMRI Analysis

## Overview

`fmridesign` provides tools for constructing design matrices for functional magnetic resonance imaging (fMRI) analyses. It includes facilities for creating event models with flexible hemodynamic response functions (HRFs), baseline models for nuisance regression, and utilities for handling experimental designs.

This package focuses on the design aspect of fMRI analysis and is intended to be used with analysis packages like `fmrireg`.

## Installation

```r
# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("bbuchsbaum/fmridesign")
```

## Key Features

- **Event Models**: Create experimental design matrices with flexible HRF specifications
- **Baseline Models**: Build nuisance regressors for drift, motion, and other confounds  
- **Flexible HRFs**: Support for various hemodynamic response functions via the `fmrihrf` package
- **Block Designs**: Handle multi-run/session fMRI experiments

## Basic Usage

```r
library(fmridesign)

# Create event data
event_data <- data.frame(
  condition = factor(c("A", "B", "A", "B")),
  onsets = c(1, 10, 20, 80),
  run = c(1, 1, 1, 1)
)

# Define sampling frame
sframe <- sampling_frame(blocklens = 100, TR = 2)

# Create event model
ev_model <- event_model(
  onsets ~ hrf(condition),
  data = event_data,
  block = ~run,
  sampling_frame = sframe
)

# Create baseline model
base_model <- baseline_model(
  basis = "bs",
  degree = 3,
  sframe = sframe
)

# Extract design matrices
event_dm <- design_matrix(ev_model)
baseline_dm <- design_matrix(base_model)
```

## Related Packages

- `fmrireg`: Analysis and fitting of fMRI models
- `fmrihrf`: Hemodynamic response functions
- `fmridataset`: fMRI dataset structures

## Documentation & Vignettes

- Online site (pkgdown): https://bbuchsbaum.github.io/fmridesign/
- Articles index: https://bbuchsbaum.github.io/fmridesign/articles/

From R, you can also open installed vignettes:

```r
vignette("a_01_introduction", package = "fmridesign")
vignette("a_03_baseline_model", package = "fmridesign")
vignette("a_04_event_models", package = "fmridesign")
vignette("a_05_contrasts", package = "fmridesign")
```

## License

GPL (>= 2)
