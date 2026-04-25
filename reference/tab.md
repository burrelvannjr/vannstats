# Simplified Crosstabs

This function returns a crosstab (tab) on a given data frame, and using
simplified calls within the function for two variables, to return the
observed and expected frequencies.

## Usage

``` r
tab(df, var1, var2)
```

## Arguments

- df:

  data frame to read in.

- var1:

  a first grouping variable.

- var2:

  a second grouping variable.

## Value

This function returns the observed and expected frequencies of a
bivariate relationship between `var1` and `var2` in data frame `df`.

## Examples

``` r
data <- mtcars

tab(data,mpg,cyl)
#> $`Observed Frequencies`
#>           cyl: 4 6  8 Total
#> mpg: 10.4      0 0  2     2
#> 13.3           0 0  1     1
#> 14.3           0 0  1     1
#> 14.7           0 0  1     1
#> 15             0 0  1     1
#> 15.2           0 0  2     2
#> 15.5           0 0  1     1
#> 15.8           0 0  1     1
#> 16.4           0 0  1     1
#> 17.3           0 0  1     1
#> 17.8           0 1  0     1
#> 18.1           0 1  0     1
#> 18.7           0 0  1     1
#> 19.2           0 1  1     2
#> 19.7           0 1  0     1
#> 21             0 2  0     2
#> 21.4           1 1  0     2
#> 21.5           1 0  0     1
#> 22.8           2 0  0     2
#> 24.4           1 0  0     1
#> 26             1 0  0     1
#> 27.3           1 0  0     1
#> 30.4           2 0  0     2
#> 32.4           1 0  0     1
#> 33.9           1 0  0     1
#> Total         11 7 14    32
#> 
#> $`Expected Frequencies`
#>             cyl: 4       6       8 Total
#> mpg: 10.4  0.68750 0.43750  0.8750     2
#> 13.3       0.34375 0.21875  0.4375     1
#> 14.3       0.34375 0.21875  0.4375     1
#> 14.7       0.34375 0.21875  0.4375     1
#> 15         0.34375 0.21875  0.4375     1
#> 15.2       0.68750 0.43750  0.8750     2
#> 15.5       0.34375 0.21875  0.4375     1
#> 15.8       0.34375 0.21875  0.4375     1
#> 16.4       0.34375 0.21875  0.4375     1
#> 17.3       0.34375 0.21875  0.4375     1
#> 17.8       0.34375 0.21875  0.4375     1
#> 18.1       0.34375 0.21875  0.4375     1
#> 18.7       0.34375 0.21875  0.4375     1
#> 19.2       0.68750 0.43750  0.8750     2
#> 19.7       0.34375 0.21875  0.4375     1
#> 21         0.68750 0.43750  0.8750     2
#> 21.4       0.68750 0.43750  0.8750     2
#> 21.5       0.34375 0.21875  0.4375     1
#> 22.8       0.68750 0.43750  0.8750     2
#> 24.4       0.34375 0.21875  0.4375     1
#> 26         0.34375 0.21875  0.4375     1
#> 27.3       0.34375 0.21875  0.4375     1
#> 30.4       0.68750 0.43750  0.8750     2
#> 32.4       0.34375 0.21875  0.4375     1
#> 33.9       0.34375 0.21875  0.4375     1
#> Total     11.00000 7.00000 14.0000    32
#> 
```
