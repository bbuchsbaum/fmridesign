# Plot Event Model

Creates a line plot visualization of the predicted BOLD response for
each regressor in an event_model object.

## Usage

``` r
# S3 method for class 'event_model'
plot(x, term_name = NULL, ...)
```

## Arguments

- x:

  An `event_model` object.

- term_name:

  Character. Name of specific term to plot. If NULL, plots all terms.

- ...:

  Additional arguments (currently unused).

## Value

A ggplot2 object showing the predicted BOLD timecourses.
