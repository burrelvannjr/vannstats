# Creating Dummy-Code Columns for Values of a Variable

This function applies dummy-coding to a variable of interest, enabling
the creation of *n* or *n-1* columns/variables based on *n* number of
attributes for the variable.

## Usage

``` r
dummy(df, var, remove = FALSE)
```

## Arguments

- df:

  data frame to read in.

- var:

  the variable to be dummy-coded. Is automatically converted to a
  character string.

- remove:

  logical (default set to `F`). When set to `remove = T`, will return a
  data frame using the true number of dummy coded columns (e.g. *n-1*).

## Value

This function updates the data frame with new variables (columns)
representing unique values of a selected variable, and a binary score
(0/1) for the absence or presence of a column's represented value for
each observation.

## Examples

``` r
data <- howell_aids_long

dummy(data, student)
```
