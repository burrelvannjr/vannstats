# Defendants, 2025 (Individual-Level)

This is a simulated data set, created in 2025. These data represent
cases for individual defendants held at the Richard J. Donovan
Correctional Facility in San Diego, CA. These data were simulated by Dr.
Burrel Vann Jr, and represent a random sample of individuals held in the
Center in 2025. Each observation in the data set represents a unique
individual defendant, and the unique characteristics tied to their court
case.

## Usage

``` r
Defendants2025
```

## Format

A data frame with 1738 observations and 11 variables.

|  |  |
|----|----|
|  | id |
| Unique defendant identifier | age |
| The defendant's age | race |
| Race of the defendant | race_binary |
| race, broken into a binary/dummy variable, measuring whether or not the defendant is white | charge |
| The crime the defendant was charged with | gang |
| Whether or not the defendant is affiliated with a gang | priors |
| The number of prior misdemeanors the defendant has | gun |
| Whether or not a gun was involved in this case | risk_score |
| A judge's risk-of-reoffending score for the defendant | bail |
| The bail amount for the defendant | perkins |
| Whether or not a Perkins Operation was conducted on defendant while in custody |  |
