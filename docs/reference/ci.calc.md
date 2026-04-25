# Simplified Confidence Interval Calculation

This function calculates the confidence interval (for a given confidence
level) for a variable in a given data frame.

## Usage

``` r
ci.calc(df, var1, cl)
```

## Arguments

- df:

  data frame to read in.

- var1:

  the variable of interest for which the CI will be calculated.

- cl:

  the desired confidence level (in percentages, ranging from \\1\\ to
  \\100\\).

## Value

This function returns the mean, lower bound, upper bound, and standard
error.

## Examples

``` r
data <- mtcars

ci.calc(data,mpg,95)
#>       Mean   CI lower   CI upper Std. Error 
#>  20.090625  18.002394  22.178856   1.065424 
```
