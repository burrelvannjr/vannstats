# Simplified Paired Samples (Repeated Measures) T-Test

This function simplifies the call for the paired samples t-test (ps.t)
on a given data frame.

## Usage

``` r
ps.t(df, t2, t1, var.equal = TRUE)
```

## Arguments

- df:

  data frame to read in.

- t2:

  the \\t_2\\ or post-test variable.

- t1:

  the \\t_1\\ or pre-test variable.

- var.equal:

  logical (default set to `T`). When set to `var.equal = F`, will employ
  Welsh's correction to the t-test (for data that violate the equal
  variances assumption).

## Value

This function returns the summary results table for an paired samples
t-test, examining the mean differences between `t2` and `t1` (in data
frame `df`).

## Examples

``` r
data <- mtcars
data$mpg2 <- c(21.8,14.7,5.9,24.0,12.2,14.9,33.0,27.7,
29.9,33.9,16.0,14.7,13.3,23.1,38.0,39.7,24.1,33.9,
25.7,24.2,28.3,37.3,20.3,36.0,18.1,32.8,28.7,30.3,6.6,
26.4,35.8,32.8)

ttest <- ps.t(data,mpg2,mpg)
summary(ttest)
#> Call:
#> ps.t(df = data, t2 = mpg2, t1 = mpg)
#> 
#> Paired Samples (Repeated Measures) t-test: 
#> 
#>       𝑡 Critical 𝑡 df p-value  
#>  2.5388     2.0400 31 0.01636 *
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Group Means and Difference:
#>         x̅: mpg2          x̅: mpg mean difference 
#>        25.12813        20.09062         5.03750 
#> 
```
