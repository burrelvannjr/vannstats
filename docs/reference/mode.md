# Mode Function

This function returns the mode for a given data frame.

## Usage

``` r
mode(x, na.rm = FALSE)
```

## Arguments

- x:

  variable within data frame or a list of values.

- na.rm:

  remove the NAs, default is FALSE.

## Value

This function returns the mode for a variable within a data frame or a
list of values.

## Examples

``` r

data <- mtcars

mode(data$mpg)
#> [1] 21
```
