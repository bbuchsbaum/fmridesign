# Extract event table

Extract event table

## Usage

``` r
event_table(x, ...)

# S3 method for class 'convolved_term'
event_table(x, ...)

# S3 method for class 'feature_term'
event_table(x, ...)
```

## Arguments

- x:

  The object.

- ...:

  Additional arguments.

## Value

A data.frame/tibble of event rows.

## Examples

``` r
term <- event_term(
  list(condition = factor(c("A", "B", "A"))),
  onsets = c(0, 10, 20),
  blockids = c(1, 1, 1)
)
event_table(term)
#> # A tibble: 3 × 1
#>   condition
#>   <fct>    
#> 1 A        
#> 2 B        
#> 3 A        
```
