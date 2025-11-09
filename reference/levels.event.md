# Extract Levels from fmrireg Objects

Extract levels from various fmrireg objects. These methods extend the
base R [`levels`](https://rdrr.io/r/base/levels.html) generic to work
with fmrireg-specific classes.

## Usage

``` r
# S3 method for class 'Scale'
levels(x, ...)

# S3 method for class 'ScaleWithin'
levels(x, ...)

# S3 method for class 'RobustScale'
levels(x, ...)

# S3 method for class 'event'
levels(x, ...)

# S3 method for class 'event'
columns(x, ...)
```

## Arguments

- x:

  An object from which to extract levels. Can be:

  - An `event` object - returns factor levels or column names

  - A `Scale` object - returns the variable name

  - A `ScaleWithin` object - returns the variable name

  - A `RobustScale` object - returns the variable name

- ...:

  Additional arguments (currently unused).

## Value

A character vector of levels or names, depending on the object type:

- For categorical events: the factor levels

- For continuous events: the column names (matrices) or variable name
  (vectors)

- For scale objects: the variable name being scaled

## Functions

- `columns(event)`: Alias for levels.event

## Examples

``` r
# Create a categorical event
fac_event <- event_factor(
  factor(c("A", "B", "A", "B")),
  name = "condition",
  onsets = c(1, 10, 20, 30),
  blockids = rep(1, 4)
)
levels(fac_event)  # Returns: c("A", "B")
#> [1] "A" "B"

# Create a continuous event
cont_event <- event_variable(
  c(1.2, 0.8, 1.5, 0.9),
  name = "reaction_time",
  onsets = c(1, 10, 20, 30),
  blockids = rep(1, 4)
)
levels(cont_event)  # Returns: "reaction_time"
#> [1] "reaction_time"
#> attr(,"orig_names")
#> [1] "reaction_time"
```
