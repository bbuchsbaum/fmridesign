# Extract elements from an object

Extract elements from an object

## Usage

``` r
elements(x, ...)

# S3 method for class 'event'
elements(x, what = c("values", "labels"), transformed = TRUE, ...)

# S3 method for class 'event_term'
elements(x, what = c("values", "labels"), ...)
```

## Arguments

- x:

  The object to extract elements from.

- ...:

  Additional arguments.

- what:

  Character string specifying what to extract: "values" for
  numeric/actual values, or "labels" for descriptive labels/names.

- transformed:

  Logical indicating whether to return transformed values. Default is
  TRUE.

## Value

Requested elements; structure depends on method (e.g., numeric values or
labels).

## Examples

``` r
# Create an event term with mixed categorical and continuous events
term <- event_term(
  list(
    condition = factor(c("A", "B", "A", "B")),
    intensity = c(1.2, 0.8, 1.5, 0.9)
  ),
  onsets = c(0, 10, 20, 30),
  blockids = c(1, 1, 1, 1)
)

# Extract values (actual numeric/factor codes)
elements(term, what = "values")
#> $condition
#>      condition
#> [1,]         1
#> [2,]         2
#> [3,]         1
#> [4,]         2
#> 
#> $intensity
#>      intensity
#> [1,]       1.2
#> [2,]       0.8
#> [3,]       1.5
#> [4,]       0.9
#> 

# Extract labels (descriptive names/levels)
elements(term, what = "labels")
#> $condition
#> [1] A B A B
#> Levels: A B
#> 
#> $intensity
#> [1] "intensity" "intensity" "intensity" "intensity"
#> 
```
