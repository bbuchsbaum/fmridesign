# Check if continuous

Check if continuous

## Usage

``` r
is_continuous(x, ...)

# S3 method for class 'event'
is_continuous(x, ...)
```

## Arguments

- x:

  The object.

- ...:

  Additional arguments.

## Value

Logical scalar indicating whether `x` is continuous.

## Examples

``` r
# Create a continuous event from numeric vector
cont_event <- event_variable(
  c(1.2, 0.8, 1.5, 0.9),
  name = "reaction_time",
  onsets = c(0, 10, 20, 30),
  blockids = rep(1, 4)
)
is_continuous(cont_event)  # Returns TRUE
#> [1] TRUE

# Create a continuous event from matrix
mat_event <- event_matrix(
  matrix(c(1.1, 0.9, 1.2, 0.8, 2.1, 1.9, 2.2, 1.8), nrow = 4),
  name = "coordinates",
  onsets = c(0, 10, 20, 30),
  blockids = rep(1, 4)
)
is_continuous(mat_event)  # Returns TRUE
#> [1] TRUE

# Categorical event is not continuous
cat_event <- event_factor(
  factor(c("faces", "houses", "faces", "houses")),
  name = "condition",
  onsets = c(0, 10, 20, 30),
  blockids = rep(1, 4)
)
is_continuous(cat_event)  # Returns FALSE
#> [1] FALSE

# Event term with all continuous events
cont_term <- event_term(
  list(rt = c(1.1, 0.9, 1.2, 0.8),
       accuracy = c(0.95, 0.87, 0.92, 0.88)),
  onsets = c(0, 10, 20, 30),
  blockids = rep(1, 4)
)
is_continuous(cont_term)  # Returns TRUE
#> [1] TRUE
```
