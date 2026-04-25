# Simplified One Sample T-Test

This function simplifies the call for the one sample t-test (os.t) on a
given data frame.

## Usage

``` r
os.t(df, var1, mu)
```

## Arguments

- df:

  data frame to read in.

- var1:

  the dependent/outcome variable, \\Y\\.

- mu:

  the population mean, \\\mu\\.

## Value

This function returns the summary results table for an one sample
t-test, examining the mean differences between `var1` (in data frame
`df`) and the population mean `mu`.

## Examples

``` r
data <- mtcars

ttest <- os.t(data,mpg,3)
summary(ttest)
#> Call:
#> os.t(df = data, var1 = mpg, mu = 3)
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
