# Extract shortnames

Extract shortnames

## Usage

``` r
shortnames(x, ...)
```

## Arguments

- x:

  The object.

- ...:

  Additional arguments.

## Value

Character vector of short names.

## Examples

``` r
# Create a simple event term with one condition factor
term <- event_term(
  list(condition = factor(c("A", "B", "A"))),
  onsets = c(0, 10, 20),
  blockids = c(1, 1, 1)
)
shortnames(term)  # Returns: "A" "B"
#> [1] "A" "B"

# Create event term with multiple factors
term2 <- event_term(
  list(
    category = factor(c("face", "scene", "face")),
    attention = factor(c("attend", "attend", "ignore"))
  ),
  onsets = c(0, 10, 20),
  blockids = c(1, 1, 1)
)
shortnames(term2)  # Returns: "face:attend" "scene:attend" "face:ignore"
#> [1] "face:attend"  "scene:attend" "face:ignore" 
```
