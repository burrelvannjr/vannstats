# Summarize Results of ow.anova

Displays results of ow.anova

## Usage

``` r
# S3 method for class 'ow.anova'
summary(object, ...)
```

## Arguments

- object:

  Object returned by
  [`ow.anova`](https://vannstats.burrelvannjr.com/reference/ow.anova.md).

- ...:

  Additional parameters to pass on.

## Value

Matrix of values for results from One-Way ANOVA test.

## Examples

``` r
data1 <- mtcars
ow <- ow.anova(data1, mpg, cyl)

summary(ow)
#>          Length Class  Mode   
#> call      4     -none- call   
#> results  10     -none- numeric
#> post_hoc  0     -none- logical
```
