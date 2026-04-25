# `vannstats`: simplifying statistical procedures in the social sciences


<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/vannstats?color=green)](https://cran.r-project.org/package=vannstats)
[![](http://cranlogs.r-pkg.org/badges/grand-total/vannstats?color=blue)](https://cran.r-project.org/package=vannstats)
[![](http://cranlogs.r-pkg.org/badges/last-month/vannstats?color=blue)](https://cran.r-project.org/package=vannstats)
<!--[![](http://cranlogs.r-pkg.org/badges/last-week/vannstats?color=green)](https://cran.r-project.org/package=vannstats)-->

<!-- badges: end -->



<!-- additional logo: start -->
<!--<img src="man/figures/logo.png" align="right" height="139" alt="vannstats logo"/>-->
<!-- additional logo: end -->



<!-- text: start -->

## Overview

**vannstats** is an R package designed to simplify the statistical workflows commonly used in undergraduate and graduate-level social science courses — particularly those transitioning from SPSS to R. The package provides intuitive wrappers for normality diagnostics, descriptive statistics, bivariate analyses, and multivariate techniques, producing output that mirrors the look and feel of SPSS results.

Whether you are assessing distributional assumptions, running crosstabs, computing correlations, or fitting ANOVA models, `vannstats` reduces the complexity of base R syntax into concise, readable function calls.

---

## Installation

Install the released version from CRAN:

```r
install.packages("vannstats")
```

Then load the package:

```r
library(vannstats)
```

---

## Functions

### Diagnostic Plots

| Function | Description |
|---|---|
| `hst()` | Histogram — can be parsed by up to 2 grouping variables |
| `box()` | Box-and-Whisker plot — can be parsed by up to 2 grouping variables |
| `qq()` | Q-Q (Quantile-Quantile) plot — can be parsed by up to 2 grouping variables |
| `residplot()` | Residuals plot for a regression equation |
| `scatter()` | Scatterplot with an overlaid fit line |
| `bar.chart()` | Bar chart for a given variable |

### Descriptive Statistics

| Function | Description |
|---|---|
| `univ.desc()` | Univariate descriptive statistics, optionally parsed by a grouping variable |
| `mode()` | Mode of a variable |
| `z.calc()` | Z-score for a given value relative to a variable's mean and SD |
| `se()` | Standard error of a variable |
| `ci()` | Confidence interval for a variable at a specified confidence level |

### Bivariate Comparisons & Analyses

| Function | Description |
|---|---|
| `tab()` | Crosstab of observed and expected frequencies for two categorical variables |
| `chi.sq()` | Pearson's Chi-Square test (with optional post-hoc comparisons and Cramér's V) |
| `p.corr()` | Pearson's Product-Moment Correlation Coefficient |
| `cormat()` | Correlation matrix for variables in a formula |
| `ow.anova()` | One-Way ANOVA (with optional Tukey's HSD post-hoc test and means plot) |
| `rm.anova()` | One-Way Repeated Measures ANOVA (with sphericity correction and test) |

### Data Management

| Function | Description |
|---|---|
| `dummy()` | Creates dummy-coded columns for the values of a categorical variable |
| `revcode()` | Reverse-codes a scale variable |
| `stata.plm.margins()` | Predictive margins from a panel linear model, mirroring STATA's `margins` output |

---

## Examples

### Histogram

```r
data <- mtcars
hst(data, mpg, cyl)
```

### Box-and-Whisker Plot

```r
data <- mtcars
box(data, mpg, cyl)
```

### Q-Q Plot

```r
data <- mtcars
qq(data, mpg, cyl)
```

### Crosstab

```r
data <- mtcars
tab(data, vs, am)
```

### Chi-Square Test

```r
data <- mtcars
x2 <- chi.sq(data, vs, am)
summary(x2)
```

### Pearson's Correlation

```r
data <- mtcars
p.corr(data, mpg, wt)
```

### Correlation Matrix

```r
data <- mtcars
cormat(data, mpg ~ wt + am)
```

### One-Way ANOVA

```r
data <- mtcars
ow <- ow.anova(data, mpg, cyl)
summary(ow)
```

### Repeated Measures ANOVA

```r
# Wide format
data <- howell_aids_wide
rm.anova(data, student, c("t1", "t2", "t3"))

# Long format
data2 <- howell_aids_long
rm.anova(data2, student, time, scores = knowledge)
```

### Residuals Plot

```r
data <- mtcars
residplot(data, mpg ~ wt + cyl)
```

---

## Included Datasets

`vannstats` ships with several built-in datasets useful for teaching and demonstration purposes:

| Dataset | Description |
|---|---|
| `GSS2014` | General Social Survey, 2014 — individual survey responses on a range of social topics (NORC at the University of Chicago) |
| `UCR2015` | Uniform Crime Reports, 2015 — county-level crime data |
| `howell_aids_wide` | Howell Student AIDS Knowledge Data in wide format (3 time points) |
| `howell_aids_long` | Howell Student AIDS Knowledge Data in long format |
| `Defendants2025` | Simulated data representing individual defendants at the Richard J. Donovan Correctional Facility, San Diego, CA (1,738 observations, 11 variables) |

---

## Dependencies

`vannstats` builds on a suite of well-established R packages including `ggplot2`, `dplyr`, `tidyverse`, `car`, `rstatix`, `plm`, `purrr`, `gplots`, `rlang`, `stringr`, `MASS`, `formula.tools`, `gdata`, and `stats`.

---

