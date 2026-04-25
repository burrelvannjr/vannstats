# Simplified Histogram

This function plots a histogram (hst) on a given data frame, and uses
simplified calls within the function to parse the histogram by up to 2
variables.

## Usage

``` r
hst(df, var1, by1, by2)
```

## Arguments

- df:

  data frame to read in.

- var1:

  the dependent/outcome variable, \\Y\\. The variable of interest that
  should be plotted.

- by1:

  the main independent/predictor variable, \\X_1\\. A grouping variable
  by which the histogram for `var1` should be parsed.

- by2:

  a potential second independent/predictor variable, \\X_2\\. A second
  grouping variable by which the histogram for `var1` (already parsed by
  `by1`) should be parsed.

## Value

This function returns the histogram for `var1` in data frame `df`. Can
be split to return a histogram for `var1` in data frame `df`, broken out
by `var2`.

## Examples

``` r
data <- mtcars

hst(data,mpg,cyl)
```
