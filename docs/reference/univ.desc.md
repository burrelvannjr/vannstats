# Simplified Descriptive Statistics

This function returns univariate/descriptive statistics (univ.desc) on a
variable within a given data frame, and uses simplified calls within the
function to parse the descriptives by another variable.

## Usage

``` r
univ.desc(df, var1, by1)
```

## Arguments

- df:

  data frame to read in.

- var1:

  the dependent/outcome variable, \\Y\\. The variable of interest .

- by1:

  the main independent/predictor variable, \\X_1\\. A grouping variable
  by which the descriptive statistics for `var1` should be parsed.

## Value

This function returns the descriptive statistics for `var1` in data
frame `df`. Can be split to return descriptives for `var1` in data frame
`df`, broken out by `var2`.

## Examples

``` r
data <- mtcars

univ.desc(data,mpg)
#>    n     mean       sd variance median  min  max skewness  kurtosis       se
#> 1 32 20.09062 6.026948  36.3241   19.2 10.4 33.9 0.610655 -0.372766 1.065424
```
