# Simplified Z Scores

This function calculates the Z score for a given value, relative to the
mean and standard deviation for a variable in a given data frame.

## Usage

``` r
z.calc(df, var1, raw, tails = NULL)
```

## Arguments

- df:

  data frame to read in.

- var1:

  the variable of interest for which the mean and standard deviations
  will be calculated.

- raw:

  the desired raw score to compare with the mean and standard deviation
  of `var1`.

- tails:

  to report a p-value (level of significance) for the reported Z score,
  user must select a desired number of tails (either `tails = 1` for a
  one-tailed test, or `tails = 2` for a two-tailed test). Default set to
  `NULL`, and does not report a p-value.

## Value

This function returns the raw score, mean, and z-score for a given raw
score.

## Examples

``` r
data <- mtcars

z.calc(data,mpg,12)
#> Raw Score      Mean   Z Score 
#> 12.000000 20.090625 -1.342408 
```
