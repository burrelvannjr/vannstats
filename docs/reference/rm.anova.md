# Simplified One-Way Repeated Measures Analysis of Variance

This function simplifies the call for repeated measures ANOVA (rm.anova)
on a given data frame. Also allows calls for sphericity correction
(correct), as well as a sphericity test table (sph).

## Usage

``` r
rm.anova(
  df,
  id,
  times,
  scores = NULL,
  correct = TRUE,
  sph = FALSE,
  phc = FALSE
)
```

## Arguments

- df:

  data frame to read in.

- id:

  the main grouping variable by which `times` will be analyzed

- times:

  dependent variable values at the time points measured. If data are in
  wide form (where time points are listed as separate variables for each
  observation), read in as a list of time point variables (e.g.
  `c("t1", "t2", "t3", ..., "tn")`), where the values represent the
  scores at the various time points. Read in as list if data are in wide
  form. If data are in long form, the `times` variable is one column
  (rather than multiple columns) in the data frame, and must be paired
  with the `scores` variable for actual values (listed below).

- scores:

  if data are in long form (where each group has multiple observations),
  a `scores` variable must be read in, which represents the values at
  the specific time points represented in the `times` variable.

- correct:

  logical (default set to `T`). Corrects the results in the repeated
  measures ANOVA table – adjusts the degrees of freedom (\\df\\) by
  multiplying the sphericity assumed degrees of freedom (\\df\\) by the
  Greenhouse-Geisser Epsilon value. When set to `correct = F`, will
  print results of repeated measures ANOVA with sphericity assumed.

- sph:

  logical (default set to `F`). When set to `sph = T`, will print a
  sphericity tests table with Mauchly's W, as well as two Epsilon values
  (Greenhouse-Geisser and Huynh-Feldt).

- phc:

  logical (default set to `F`). When set to `phc = T`, will print a
  post-hoc comparisons table with Bonferroni's adjusted alpha levels
  (and p-values).

## Examples

``` r
data <- howell_aids_wide
rm.anova(data, student, c("t1","t2","t3"))
#> ANOVA Table (type III tests)
#> 
#> $ANOVA
#>   Effect DFn DFd      F     p p<.05   pes
#> 1   time   2   6 14.891 0.005     * 0.832
#> 
#> $`Mauchly's Test for Sphericity`
#>   Effect    W    p p<.05
#> 1   time 0.54 0.54      
#> 
#> $`Sphericity Corrections`
#>   Effect   GGe     DF[GG] p[GG] p[GG]<.05   HFe    DF[HF] p[HF] p[HF]<.05
#> 1   time 0.685 1.37, 4.11 0.015         * 1.066 2.13, 6.4 0.005         *
#> 
#> 
#> Repeated Measures ANOVA
#> 
#>          df      F p-value eta^2
#>   time 1.37 14.891 0.015 * 0.832
#>  error 4.11                     
#> ---
#> Signif. codes: '***' 0.001 '**' 0.01 '*' 0.05
#> Note: Model with Greenhouse-Geisser Adjusted df

data2 <- howell_aids_long
rm.anova(data2, student, time, scores=knowledge)
#> 
#> Repeated Measures ANOVA
#> 
#>          df      F p-value eta^2
#>   time 1.37 14.891 0.015 * 0.832
#>  error 4.11                     
#> ---
#> Signif. codes: '***' 0.001 '**' 0.01 '*' 0.05
#> Note: Model with Greenhouse-Geisser Adjusted df
```
