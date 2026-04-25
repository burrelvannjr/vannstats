# Standard Error Calculation

This function calculates the standard error for a variable in a given
data frame.

## Usage

``` r
se(var, na.rm = TRUE)
```

## Arguments

- var:

  variable to read in.

- na.rm:

  logical (default set to `T`). When set to `na.rm = F`, will include
  NA's in calculation.

## Value

This function returns the standard error for a given variable

## Examples

``` r
data <- mtcars

se(data$mpg)
#> [1] 1.065424
```
