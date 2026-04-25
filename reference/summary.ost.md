# Summarize Results of os.t

Displays results of os.t

## Usage

``` r
# S3 method for class 'ost'
summary(object, ...)
```

## Arguments

- object:

  Object returned by [`os.t`](os.t.md).

- ...:

  Additional parameters to pass on.

## Value

Matrix of values for results from one sample t-test.

## Examples

``` r
data1 <- mtcars
ttest <- os.t(data1,mpg,3)

summary(ttest)
#> Call:
#> os.t(df = data1, var1 = mpg, mu = 3)
#> 
#> One Sample t-test: 
#> 
#>       𝑡 Critical 𝑡 df   p-value    
#>  16.041      2.040 31 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample and Population Means:
#>       x̅:       μ: 
#> 20.09062  3.00000 
#> 
```
