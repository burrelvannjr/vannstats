# Simplified Correlation Matrix

This function creates a correlation (cormat) on a data frame of the
variables in an equation.

## Usage

``` r
cormat(df, formula)
```

## Arguments

- df:

  data frame to read in.

- formula:

  the variables in the regression model, \\Y = X_1 + X_2 + ... + X_m\\,
  written as `Y ~ X1 + X2`...

## Value

This function returns a correlation matrix for the variables provided in
the formula.

## Examples

``` r
data <- mtcars

cormat(data, mpg ~ wt + am)
#>       mpg    wt am
#> mpg     1         
#> wt  -0.87     1   
#> am    0.6 -0.69  1
```
