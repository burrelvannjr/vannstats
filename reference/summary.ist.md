# Summarize Results of is.t

Displays results of is.t

## Usage

``` r
# S3 method for class 'ist'
summary(object, ...)
```

## Arguments

- object:

  Object returned by
  [`is.t`](https://vannstats.burrelvannjr.com/reference/is.t.md).

- ...:

  Additional parameters to pass on.

## Value

Matrix of values for results from independent samples t-test.

## Examples

``` r
data1 <- mtcars
ttest <- is.t(data1, mpg, am)

summary(ttest)
#> Call:
#> is.t(df = data1, var1 = mpg, var2 = am)
#> 
#> Independent Samples (Two Sample) t-test: 
#> 
#>        𝑡 Critical 𝑡 df  p-value    
#>  -4.1061     2.0420 30 0.000285 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Group Means:
#>     x̅: 0     x̅: 1 
#> 17.14737 24.39231 
#> 
```
