# Summarize Results of chi.sq

Displays results of chi.sq

## Usage

``` r
# S3 method for class 'chisquare'
summary(object, ...)
```

## Arguments

- object:

  Object returned by [`chi.sq`](chi.sq.md).

- ...:

  Additional parameters to pass on.

## Value

Matrix of values for results from chi square test.

## Examples

``` r
data1 <- mtcars
x2 <- chi.sq(data1, vs, am)

summary(x2)
#> Call:
#> chi.sq(df = data1, var1 = vs, var2 = am)
#> 
#> Pearson's Chi-squared test: 
#> 
#>       χ² Critical χ² df p-value
#>  0.90688     3.84100  1  0.3409
#> 
```
