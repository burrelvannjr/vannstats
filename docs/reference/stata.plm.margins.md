# Simplified STATA Predictive Margins

This function returns a data frame with interactive margins and standard
errors similar to those returned in the STATA margins call. The function
can also return a margins plot.

## Usage

``` r
stata.plm.margins(mod, plot = FALSE, error = NULL)
```

## Arguments

- mod:

  a plm model object.

- plot:

  logical (default set to `FALSE`). When set to `plot = TRUE`, will
  return a an margins plot of the interaction terms.

- error:

  the number standard deviation units for which the margins will be
  calculated (default set to 2).

## Value

This function creates a data frame of predictive margins for the
dependent variable, given values of the variables in the interaction.

## Examples

``` r
library(plm)
#> Registered S3 method overwritten by 'lfe':
#>   method    from 
#>   nobs.felm broom
data <- UCR2015
summary(mod <- plm(dui_pct ~ pct_poverty*pct_unemp +
income_inequality, data=data, index=c("state","county"),
model="within"))
#> Oneway (individual) effect Within Model
#> 
#> Call:
#> plm(formula = dui_pct ~ pct_poverty * pct_unemp + income_inequality, 
#>     data = data, model = "within", index = c("state", "county"))
#> 
#> Unbalanced Panel: n = 48, T = 1-126, N = 1415
#> 
#> Residuals:
#>      Min.   1st Qu.    Median   3rd Qu.      Max. 
#> -0.697662 -0.185043 -0.066196  0.036027 13.944436 
#> 
#> Coefficients:
#>                         Estimate Std. Error t-value  Pr(>|t|)    
#> pct_poverty           -0.0048697  0.0055117 -0.8835 0.3771060    
#> pct_unemp             -0.0546932  0.0138733 -3.9423 8.479e-05 ***
#> income_inequality     -0.5820328  0.5761619 -1.0102 0.3125837    
#> pct_poverty:pct_unemp  0.0016866  0.0005050  3.3397 0.0008612 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Total Sum of Squares:    481.8
#> Residual Sum of Squares: 475.93
#> R-Squared:      0.012166
#> Adj. R-Squared: -0.024797
#> F-statistic: 4.19651 on 4 and 1363 DF, p-value: 0.0022145

stata.plm.margins(mod)
#> Error in eval(predvars, data, env): object 'dui_pct' not found
```
