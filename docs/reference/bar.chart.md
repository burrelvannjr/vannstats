# Simplified Bar Chart

This function plots a bar chart (bar.chart) on a given data frame.

## Usage

``` r
bar.chart(df, var1, lab = FALSE)
```

## Arguments

- df:

  data frame to read in.

- var1:

  the dependent/outcome variable, \\Y\\. The variable of interest that
  should be plotted.

- lab:

  logical (default set to `FALSE`). When set to `lab = TRUE`, will add
  frequency label for each bar in chart.

## Value

This function returns the bar chart for `var1` in data frame `df`.

## Examples

``` r
data <- mtcars

bar.chart(data,cyl)
```
