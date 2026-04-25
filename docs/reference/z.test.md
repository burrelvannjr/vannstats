# Simplified Z Tests

This function runs a one-sample Z-test, comparing the proportion in your
sample to the proportion in the population.

## Usage

``` r
z.test(df, var1, var2, prop)
```

## Arguments

- df:

  data frame to read in.

- var1:

  variable with the total number of events, by sub-unit (e.g. cities
  within a county).

- var2:

  variable with number of events for a specific group.

- prop:

  proportion to compare to (between 0 and 1).

## Value

This function returns the Z score and p-value for the z-test.

## Examples

``` r
data <- UCR2015[UCR2015$state=="California",]
data$total_part2 <- data$burglary + data$larceny + data$mv_theft + data$arson

z.test(data,total_part2,burglary,.25)
#>            Proportion Comparison Proportion               Z Score 
#>          3.355459e-01          2.500000e-01          1.846700e+01 
#>               p-value 
#>          3.782115e-76 
```
