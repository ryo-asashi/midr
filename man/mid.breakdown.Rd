% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mid_breakdown.R
\name{mid.breakdown}
\alias{mid.breakdown}
\alias{print.mid.breakdown}
\title{Calculate MID Breakdown}
\usage{
mid.breakdown(
  object,
  data = NULL,
  sort = TRUE,
  digits = 6L,
  format = c("\%s", "\%s, \%s")
)

\method{print}{mid.breakdown}(x, digits = max(3L, getOption("digits") - 2L), ...)
}
\arguments{
\item{object}{a "mid" object.}

\item{data}{a data.frame containing a single observation to be used to calculate the MID breakdown. If \code{NULL}, data is extracted from \code{parent.env()} based on the function call of the "mid" object.}

\item{sort}{logical. If \code{TRUE}, the output data frame is sorted by MID .}

\item{digits}{an integer specifying the minimum number of significant digits.}

\item{format}{a character vector of length two to be used as the formats of the \code{sprintf()} function for each value or pair of values of predictor variables.}

\item{x}{a "mid.importance" object to be printed.}

\item{...}{additional parameters to be passed to \code{print.data.frame()} to print the importance of component functions.}
}
\value{
\code{mid.breakdown()} returns an object of the class "mid.breakdown" containing the following components.
\item{breakdown}{the data frame containing the breakdown of the prediction.}
\item{data}{the data frame containing the values of predictor variables used for the prediction.}
\item{intercept}{the intercept of the MID model.}
\item{prediction}{the predicted value.}
}
\description{
\code{mid.breakdown()} calculates the MID breakdown of a prediction of the MID model.
}
\details{
\code{mid.breakdown()} returns an object of class "mid.breakdown".
}
\examples{
data(airquality, package = "datasets")
mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
mbd <- mid.breakdown(mid, airquality[1L, ])
mbd
}
