# Extract longnames

Extract longnames

## Usage

``` r
longnames(x, ...)
```

## Arguments

- x:

  The object.

- ...:

  Additional arguments.

## Value

Character vector of long (fully qualified) names.

## Examples

``` r
# Create a simple event term with one condition factor
term <- event_term(
  list(condition = factor(c("A", "B", "A"))),
  onsets = c(0, 10, 20),
  blockids = c(1, 1, 1)
)
longnames(term)  # Returns: "condition.A" "condition.B"
#> [1] "condition.A" "condition.B"

# Create event term with multiple factors
term2 <- event_term(
  list(
    category = factor(c("face", "scene", "face")),
    attention = factor(c("attend", "attend", "ignore"))
  ),
  onsets = c(0, 10, 20),
  blockids = c(1, 1, 1)
)
longnames(term2)
#> [1] "category.face_attention.attend"  "category.scene_attention.attend"
#> [3] "category.face_attention.ignore"  "category.scene_attention.ignore"
# Returns: "category.face_attention.attend"
#          "category.scene_attention.attend"
#          "category.face_attention.ignore"
```
