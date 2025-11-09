# Check if categorical

Check if categorical

## Usage

``` r
is_categorical(x, ...)

# S3 method for class 'event'
is_categorical(x, ...)
```

## Arguments

- x:

  The object.

- ...:

  Additional arguments.

## Value

Logical scalar indicating whether `x` is categorical.

## Examples

``` r
# Create a categorical event from factor data
cat_event <- event_factor(
  factor(c("faces", "houses", "faces", "houses")),
  name = "condition",
  onsets = c(0, 10, 20, 30),
  blockids = rep(1, 4)
)
is_categorical(cat_event)  # Returns TRUE
#> [1] TRUE

# Create a continuous event from numeric data
cont_event <- event_variable(
  c(1.2, 0.8, 1.5, 0.9),
  name = "reaction_time",
  onsets = c(0, 10, 20, 30),
  blockids = rep(1, 4)
)
is_categorical(cont_event)  # Returns FALSE
#> [1] FALSE

# Event term with mixed types is considered categorical
mixed_term <- event_term(
  list(condition = factor(c("A", "B", "A", "B")),
       modulator = c(1.1, 0.9, 1.2, 0.8)),
  onsets = c(0, 10, 20, 30),
  blockids = rep(1, 4)
)
is_categorical(mixed_term)  # Returns TRUE
#> [1] TRUE
```
