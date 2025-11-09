# Extract term names

Extract term names

## Usage

``` r
term_names(x, ...)
```

## Arguments

- x:

  The object.

- ...:

  Additional arguments.

## Value

Character vector of term names.

## Examples

``` r
# Create sample event data
des <- data.frame(
  onset = c(0, 10, 20, 30),
  run = 1,
  cond = factor(c("A", "B", "A", "B"))
)
sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)

# Event model example
emod <- event_model(onset ~ hrf(cond), data = des, block = ~run, sampling_frame = sframe)
term_names(emod)  # Returns "cond"
#>   cond 
#> "cond" 

# Baseline model example
bmod <- baseline_model(basis = "poly", degree = 3, sframe = sframe)
term_names(bmod)  # Returns c("constant", "baseline_poly_3")
#>             drift             block 
#> "baseline_poly_3"        "constant" 
```
