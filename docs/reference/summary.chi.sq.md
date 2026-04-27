# Summarize Results of chi.sq

Displays results of chi.sq

## Usage

``` r
# S3 method for class 'chi.sq'
summary(object, ...)
```

## Arguments

- object:

  Object returned by
  [`chi.sq`](https://burrelvannjr.github.io/vannstats/reference/chi.sq.md).

- ...:

  Additional parameters to pass on.

## Value

Matrix of values for results from chi square test.

## Examples

``` r
data1 <- mtcars
x2 <- chi.sq(data1, vs, am)

summary(x2)
#>            Length Class  Mode     
#> call       4      -none- call     
#> results    4      -none- numeric  
#> name       1      -none- character
#> comparison 1      -none- character
#> post_hoc   0      -none- logical  
#> cramers    0      -none- logical  
```
