# Create an event model term from a named list of variables.

Generates an `event_term` object which represents the combination of one
or more event sequences (e.g., a factor crossed with a numeric
modulator). It takes a list of variables (factors, numeric vectors,
matrices, basis objects) along with shared onsets, block IDs, and
durations. It uses the `EV` factory internally to create standardized
`event` objects for each variable.

## Usage

``` r
event_term(evlist, onsets, blockids, durations = 0, subset = NULL)
```

## Arguments

- evlist:

  A named list of variables (factors, numeric, matrices, ParametricBasis
  objects). The names are used as variable identifiers within the term.

- onsets:

  Numeric vector of onset times (in seconds).

- blockids:

  Numeric vector of block numbers (non-decreasing integers).

- durations:

  Numeric vector of event durations (seconds, default is 0). Can be
  scalar (recycled) or vector matching length of `onsets`.

- subset:

  Optional logical vector indicating which events to retain (applied
  before processing).

## Value

A list object with class `c("event_term", "event_seq")`. Contains:

- varname:

  Concatenated variable names from `evlist`.

- events:

  A named list of the processed `event` objects.

- subset:

  The `subset` vector used.

- event_table:

  A tibble representing the combinations of descriptive levels/names for
  each event in the term, constructed using
  `elements(..., values=FALSE)`.

- onsets:

  Numeric vector of onsets (after processing/subsetting).

- blockids:

  Numeric vector of block IDs (after processing/subsetting).

- durations:

  Numeric vector of durations (after processing/subsetting).

## Examples

``` r
x1 <- factor(rep(letters[1:3], 10))
x2 <- factor(rep(1:3, each = 10))
onsets <- seq(1, 100, length.out = 30)
blockids <- rep(1:3, each = 10)

eterm <- event_term(list(Condition = x1, Group = x2),
                    onsets = onsets,
                    blockids = blockids)
print(eterm)
#> 
#> ── Event Term: Condition:Group ─────────────────────────────────────────────────
#> * Number of Events: 30
#> * Variables: Condition and Group
#> 
#> ── Variable Types ──
#> 
#> * Condition: Categorical
#> * Group: Categorical
#> 
#> ── Timing ──
#> 
#> * Onset Range: 1.00 - 100.00 sec
#> * Duration Range: 0.00 - 0.00 sec
#> 
#> ── Blocks ──
#> 
#> * Number of Blocks: 3
#> * Block IDs: 1, 2, 3
#> * Events per Block: 10, 10, 10
head(event_table(eterm))
#> # A tibble: 6 × 2
#>   Condition Group
#>   <fct>     <fct>
#> 1 a         1    
#> 2 b         1    
#> 3 c         1    
#> 4 a         1    
#> 5 b         1    
#> 6 c         1    
levels(eterm)
#> NULL
head(design_matrix(eterm))
#> # A tibble: 6 × 9
#>   Condition.a_Group.1 Condition.b_Group.1 Condition.c_Group.1
#>                 <dbl>               <dbl>               <dbl>
#> 1                   1                   0                   0
#> 2                   0                   1                   0
#> 3                   0                   0                   1
#> 4                   1                   0                   0
#> 5                   0                   1                   0
#> 6                   0                   0                   1
#> # ℹ 6 more variables: Condition.a_Group.2 <dbl>, Condition.b_Group.2 <dbl>,
#> #   Condition.c_Group.2 <dbl>, Condition.a_Group.3 <dbl>,
#> #   Condition.b_Group.3 <dbl>, Condition.c_Group.3 <dbl>

term <- event_term(
  list(condition = factor(c("A", "B", "A"))),
  onsets = c(0, 10, 20),
  blockids = c(1, 1, 1)
)
head(design_matrix(term))
#> # A tibble: 3 × 2
#>   condition.A condition.B
#>         <dbl>       <dbl>
#> 1           1           0
#> 2           0           1
#> 3           1           0
```
