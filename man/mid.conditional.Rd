% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mid_conditional.R
\name{mid.conditional}
\alias{mid.conditional}
\alias{print.mid.conditional}
\alias{summary.mid.conditional}
\title{Calculate and Visualize MID-based Individual Conditional Expectation}
\usage{
mid.conditional(
  object,
  variable,
  data,
  keep.effects = TRUE,
  partition = 100L,
  max.nrow = 100000L,
  type = c("response", "link")
)

\method{print}{mid.conditional}(x, ...)

\method{summary}{mid.conditional}(object, ...)
}
\arguments{
\item{object}{a mid object to compute the individual conditional expectations.}

\item{variable}{a character or an expression specifying the predictor variable to calculate the individual conditional expectations for.}

\item{data}{a data frame representing the observation results.}

\item{keep.effects}{logical. If TRUE, the effects of component terms are stored in the output object.}

\item{partition}{an integer specifying the number of the sample values.}

\item{max.nrow}{the maximum number of rows of the output data frame.}

\item{type}{the type of the prediction to use when the model has a link function. The default is \code{response}.}

\item{x}{a mid.conditional object to print.}

\item{...}{additional arguments to be passed to the methods for \code{data.frame}.}
}
\value{
\code{mid.conditional()} returns a 'mid.conditional' object that contains the following components:
\item{terms}{a character vector of relevant terms.}
\item{observed}{a data frame of the observations and the corresponding predictions.}
\item{conditional}{a data frame of the hypothetical observations.}
\item{values}{a numeric vector of the representative values of the target variable.}
}
\description{
Creates a data frame to be used to visualize the individual conditional expectation.
}
\examples{
data(airquality, package = "datasets")
mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
mc <- mid.conditional(mid, "Wind", airquality)
mc
}
