% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mode.R
\name{mode}
\alias{mode}
\title{Mode Function}
\usage{
mode(x, na.rm = FALSE)
}
\arguments{
\item{x}{variable within data frame or a list of values.}

\item{na.rm}{remove the NAs, default is FALSE.}
}
\value{
This function returns the mode for a variable within a data frame or a list of values.
}
\description{
This function returns the mode for a given data frame.
}
\examples{

data <- mtcars

mode(data$mpg)
}
