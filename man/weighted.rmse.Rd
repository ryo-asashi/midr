% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_functions.R
\name{weighted.rmse}
\alias{weighted.rmse}
\alias{weighted.mae}
\alias{weighted.medae}
\title{Weighted Loss Functions}
\usage{
weighted.rmse(x, y = NULL, w = NULL, ..., na.rm = FALSE)

weighted.mae(x, y = NULL, w = NULL, ..., na.rm = FALSE)

weighted.medae(x, y = NULL, w = NULL, ..., na.rm = FALSE)
}
\arguments{
\item{x}{a numeric vector of deviations based on which the loss is calculated.}

\item{y}{an optional numeric vector. If 'y' is passed, the loss is calculated based on the difference of x minus y.}

\item{w}{a numeric vector of the same length as 'x' giving the weights to use for elements of it.}

\item{...}{optional augments passed to other functions and methods.}

\item{na.rm}{logical. If TRUE, any \code{NA} and \code{NaN}s are removed from 'x' before the quantiles are computed.}
}
\description{
Returns a weighted loss calculated for the given vector(s). \code{weighted.rmse} is for the root mean square error, \code{weighted.mae} is for the mean absolute error, and \code{weighted.medae} is for the median absolute error
}
