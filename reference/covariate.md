# Construct a Covariate Term

Creates a covariate term that is added directly to the fMRI model
without being convolved with a hemodynamic response function (HRF). This
is useful for including nuisance variables, continuous covariates, or
any other regressors that should not undergo HRF convolution.

## Usage

``` r
covariate(..., data, id = NULL, prefix = NULL, subset = NULL)
```

## Arguments

- ...:

  A variable argument set of covariate names.

- data:

  A data.frame containing the variables.

- id:

  An optional identifier for the covariate term.

- prefix:

  An optional prefix to add to the covariate names.

- subset:

  Optional expression used to subset the covariate data.

## Value

A list containing information about the covariate term with class
'covariatespec' that can be used within an event_model.

## Details

In fMRI analysis, some predictors should not be convolved with the HRF
because they represent:

- Continuous physiological measurements (e.g., heart rate, respiration)

- Motion parameters from head movement correction

- Scanner drift or other technical artifacts

- Behavioral measures that directly correlate with BOLD signal

- Global signal or other nuisance variables

The covariate term can be combined with standard HRF-convolved event
terms in the same model. For example:

    model <- event_model(onset ~ hrf(stimulus) + covariate(motion_x, motion_y, data = cov_data),
                        data = events, block = ~ 1, sampling_frame = sframe)

## See also

- [`event_model()`](https://bbuchsbaum.github.io/fmridesign/reference/event_model.md)
  for creating complete fMRI models

- [`hrf()`](https://bbuchsbaum.github.io/fmridesign/reference/hrf.md)
  for creating HRF-convolved event terms

## Examples

``` r
# Add motion parameters as covariates
motion_data <- data.frame(
  x = rnorm(100),  # x translation
  y = rnorm(100)   # y translation
)
cv <- covariate(x, y, data = motion_data, prefix = "motion")

# Combine with event model
sframe <- sampling_frame(blocklens = c(100), TR = 2)
# 50 events, strictly increasing onsets per block
event_data <- data.frame(
  stimulus = factor(rep(c("A", "B"), 25)),
  onset = seq(0, by = 4, length.out = 50)
)

# Full model with both HRF-convolved events and non-convolved covariates
model <- event_model(
  onset ~ hrf(stimulus) + covariate(x, y, data = motion_data, id = "motion"),
  data = event_data,
  block = ~ 1,
  sampling_frame = sframe
)
```
