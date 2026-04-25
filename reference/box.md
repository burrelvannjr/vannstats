# Simplified Boxplot

This function plots a Box-and-Whisker (box) on a given data frame, and
uses simplified calls within the function to parse the boxplot by up to
2 variables.

## Usage

``` r
box(df, var1, by1, by2)
```

## Arguments

- df:

  data frame to read in.

- var1:

  the dependent/outcome variable, \\Y\\. The variable of interest that
  should be plotted.

- by1:

  the main independent/predictor variable, \\X_1\\. A grouping variable
  by which the boxplot for `var1` should be parsed.

- by2:

  a potential second independent/predictor variable, \\X_2\\. A second
  grouping variable by which the boxplot for `var1` (already parsed by
  `by1`) should be parsed.

## Examples

``` r
data <- mtcars

box(data,mpg,cyl)
```
