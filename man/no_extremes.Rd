% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ztils.R
\name{no_extremes}
\alias{no_extremes}
\title{No extremes}
\usage{
no_extremes(df, x)
}
\arguments{
\item{df}{The dataframe to subset}

\item{x}{The variable to subset by}
}
\description{
This function returns a dataframe subsetted to not include observations that
are beyond the extremes of the specified variable.
}
\details{
Extremes are defined by the quantiles +- 3 times the interquartile range.
}
