# Simplified Correlation

This function simplifies the call for Pearson's Product-Moment
Correlation Coefficient (p.corr) on a given data frame.

## Usage

``` r
p.corr(df, var1, var2)
```

## Arguments

- df:

  data frame to read in.

- var1:

  the dependent/outcome variable, \\Y\\.

- var2:

  the main independent/predictor variable, \\X\\.

## Value

This function returns the summary results table for a Pearson's
correlation, examining the relationship between `var1` from data frame
`df`, and `var2`.

## Examples

``` r
data <- mtcars

p.corr(data,mpg,wt)
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  mpg and wt
#> 𝒓 = -0.86766, df = 30, p-value = 1.294e-10
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.9338264 -0.7440872
#> sample estimates:
#>         𝒕 
#> -9.559044 
#> 
```
