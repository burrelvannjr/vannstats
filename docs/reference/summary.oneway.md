# Summarize Results of ow.anova

Displays results of ow.anova

## Usage

``` r
# S3 method for class 'oneway'
summary(object, ...)
```

## Arguments

- object:

  Object returned by [`ow.anova`](ow.anova.md).

- ...:

  Additional parameters to pass on.

## Value

Matrix of values for results from One-Way ANOVA test.

## Examples

``` r
data1 <- mtcars
ow <- ow.anova(data1, mpg, cyl)

summary(ow)
#> Call:
#> ow.anova(df = data1, var1 = mpg, by1 = cyl)
#> 
#> One-Way Analysis of Variance (ANOVA): 
#>                           df      SS      MS      F   p-value    
#> Between Groups (cyl)   2.000 824.785 412.392 39.697 4.979e-09 ***
#> Within Groups (cyl)   29.000 301.263  10.388                     
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
```
