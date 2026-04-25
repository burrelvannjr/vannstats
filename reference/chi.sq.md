# Simplified Chi Square

This function simplifies the call for Pearson's Chi Square test (chi.sq)
on a given data frame.

## Usage

``` r
chi.sq(
  df,
  var1,
  var2,
  correct = FALSE,
  post = FALSE,
  plot = FALSE,
  cramer = FALSE
)
```

## Arguments

- df:

  data frame to read in.

- var1:

  the dependent/outcome variable, \\Y\\.

- var2:

  the main independent/predictor variable, \\X\\.

- correct:

  logical (default set to `F`). When set to `correct = T`, will employ
  Yates' continuity correction (for data that violate the normality
  assumption).

- post:

  logical (default set to `F`). When set to `post = T`, will return
  results of post-hoc (Z) tests of the standardized residual for each
  cell (the standardized difference between observed and expected
  frequencies), using Bonferroni's alpha adjustment, and returns an
  adjusted p-value for each cell/comparison.

- plot:

  logical (default set to `F`). When set to `plot = T`, will print a
  corrplot-style plot for showing both the value of difference between
  the standardized residual (Z) and the related level of significance of
  this difference (for each cell comparison) as well as a gradient color
  representing the relative value of this residual. Will also return
  results of post-hoc (Z) tests of the standardized residual for each
  cell (the standardized difference between observed and expected
  frequencies), using Bonferroni's alpha adjustment, and returns an
  adjusted p-value for each cell/comparison.

- cramer:

  logical (default set to `F`). When set to `post = T`, will return
  results of Cramer's V, a measure of the strength of the association
  between the two variables.

## Value

This function returns the summary results table for a Pearson's Chi
Square test, examining the relationship between `var1` from data frame
`df`, and `var2`.

## Examples

``` r
data <- mtcars

x2 <- chi.sq(data,vs,am)
summary(x2)
#> Call:
#> chi.sq(df = data, var1 = vs, var2 = am)
#> 
#> Pearson's Chi-squared test: 
#> 
#>       χ² Critical χ² df p-value
#>  0.90688     3.84100  1  0.3409
#> 
```
