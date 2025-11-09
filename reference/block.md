# Create a Block Variable

Returns a block variable that is constant over the span of a scanning
run.

## Usage

``` r
block(x)
```

## Arguments

- x:

  The block variable.

## Value

An object of class "blockspec".

## Examples

``` r
block(run)
#> $name
#> run
#> 
#> $label
#> [1] "block(run)"
#> 
#> attr(,"class")
#> [1] "blockspec"
```
