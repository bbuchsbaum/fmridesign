# Print a Baseline Model

Displays key information about the baseline model components and a
preview of the design matrix.

Print a contrast set.

Print a contrast specification.

Print a contrast.

Print a polynomial contrast specification.

Print a contrast difference specification.

Provides a concise summary of an event object using cli.

Provides a concise summary of an `event_model` object using `cli`.

Provides a concise summary of an event_term object using cli.

## Usage

``` r
# S3 method for class 'baseline_model'
print(x, ...)

# S3 method for class 'contrast_set'
print(x, ...)

# S3 method for class 'contrast_spec'
print(x, ...)

# S3 method for class 'contrast'
print(x, ...)

# S3 method for class 'poly_contrast_spec'
print(x, ...)

# S3 method for class 'contrast_diff_spec'
print(x, ...)

# S3 method for class 'event'
print(x, ...)

# S3 method for class 'event_model'
print(x, ...)

# S3 method for class 'fmri_term'
print(x, ...)

# S3 method for class 'convolved_term'
print(x, ...)

# S3 method for class 'event_term'
print(x, ...)
```

## Arguments

- x:

  An event_term object.

- ...:

  Additional arguments (unused).

## Value

The input object, invisibly.

## Examples

``` r
sframe <- fmrihrf::sampling_frame(blocklens = 5, TR = 1)
bmod <- baseline_model(sframe = sframe)
print(bmod)
#> ================================================
#>            Baseline Model                       
#> ================================================
#>   Drift Components                           
#>     * Name: baseline_constant_1          
#>     * Basis type: constant               
#>     * Degree: 1                          
#>     * Drift columns: 1                   
#> 
#>   Additional Components                    
#>     * Constant columns: 0                
#>     * Nuisance columns: 0                
#> 
#>   Model Summary                            
#>     * Total columns: 1                   
#> 
#>   Design Matrix Preview                    
#>      1.000                               
#>      1.000                               
#>      1.000                               
#> ================================================
```
