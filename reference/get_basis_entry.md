# Look Up a Registered Basis Entry

Accepts either a single class name or a class vector (the typical output
of `class(x)`). Returns the first matching registration entry, walking
the class vector from most-specific to least-specific so a subclass's
registration takes precedence over its parent.

## Usage

``` r
get_basis_entry(class_name)
```

## Arguments

- class_name:

  Character string or character vector of class names.

## Value

Registration entry (a list) or `NULL` if no class is registered.

## Examples

``` r
get_basis_entry("Poly")
#> $class_name
#> [1] "Poly"
#> 
#> $prefix
#> [1] "poly"
#> 
#> $modulation
#> [1] "parametric"
#> 
#> $description
#> [1] "Orthogonal polynomial expansion"
#> 
```
