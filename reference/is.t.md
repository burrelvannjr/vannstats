# Simplified Independent Samples T-Test

This function simplifies the call for the independent samples t-test
(is.t) on a given data frame.

## Usage

``` r
is.t(df, var1, var2, var.equal = TRUE, two.tailed = TRUE)
```

## Arguments

- df:

  data frame to read in.

- var1:

  the dependent/outcome variable, \\Y\\.

- var2:

  the main independent/predictor variable, \\X\\.

- var.equal:

  logical (default set to `T`). When set to `var.equal = F`, will employ
  Welsh's correction to the t-test (for data that violate the equal
  variances assumption).

- two.tailed:

  logical (default set to `T`). When set to `two.tailed = F`, will
  return results of a one-sided t-test.

## Value

This function returns the summary results table for an independent
samples t-test, examining the mean differences of `var1` (in data frame
`df`) between groups in `var2`.

## Examples

``` r
data <- mtcars

ttest <- is.t(data,mpg,am)
summary(ttest)
#> Call:
#> is.t(df = data, var1 = mpg, var2 = am)
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
