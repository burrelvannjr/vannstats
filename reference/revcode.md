# Reverse Coding for Scales

This function applies reverse-coding to a variable of interest.

## Usage

``` r
revcode(df, var, missing = c(""))
```

## Arguments

- df:

  data frame to read in.

- var:

  the variable to be recoded.

- missing:

  a list of values in the variable that are “missing” values.

## Value

This function updates the data frame with a new variable with the
recoded values.

## Examples

``` r
data <- GSS2014

revcode(data, amcult)
```
