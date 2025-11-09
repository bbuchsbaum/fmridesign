# Extract regressors

Convolve the event-term design matrix with an HRF and return the
resulting regressors.

## Usage

``` r
regressors(x, ...)

# S3 method for class 'event_term'
regressors(x, hrf, sampling_frame, summate = FALSE, drop.empty = TRUE, ...)
```

## Arguments

- x:

  The object.

- ...:

  Additional arguments.

- hrf:

  HRF function

- sampling_frame:

  sampling_frame object

- summate:

  Logical; sum HRF responses

- drop.empty:

  Logical; drop empty conditions

## Value

Character vector of regressor names for `x`.

## Examples

``` r
# Create an event term with two conditions
term <- event_term(
  list(condition = factor(c("A", "B", "A", "B"))),
  onsets = c(0, 10, 20, 30),
  blockids = c(1, 1, 1, 1)
)

# Create a sampling frame for timing information
sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 2)

# Extract regressors convolved with canonical HRF
reg <- regressors(term, hrf = fmrihrf::HRF_SPMG1, sampling_frame = sframe)
names(reg)  # Shows regressor names: "condition.A" "condition.B"
#> [1] "condition.A" "condition.B"
```
