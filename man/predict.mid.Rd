% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_mid.R
\name{predict.mid}
\alias{predict.mid}
\alias{mid.f}
\title{Predict Method for MID-based Surrogate Models}
\usage{
\method{predict}{mid}(
  object,
  newdata = NULL,
  na.action = getOption("na.action"),
  type = c("response", "link", "terms"),
  terms = object$terms,
  ...
)

mid.f(object, term, x, y = NULL)
}
\arguments{
\item{object}{a mid object to be used as a surrogate model.}

\item{newdata}{data frame for which the predictions are to be made.}

\item{na.action}{a function which indicates what should happen when the data contain missing values (NAs).}

\item{type}{the type of prediction required. The default is on the scale of the response varialbe; the alternative "link" is on the scale of the linear predictors. The "terms" option returns a matrix giving the fitted values of each term in the model formula on the linear predictor scale.}

\item{terms}{a character vector, specifying names of the terms to be used to make predictions.}

\item{...}{not used.}

\item{term}{a character, specifying the name of the term of the decomposed function.}

\item{x}{a vector to be used as inputs to the first argument of the decomposed function. If a matrix or data.frame is passed, inputs are extracted from the matrix or data.frame.}

\item{y}{a vector to be used as inputs to the second argument of the decomposed interaction function.}
}
\description{
Returns predictions of the fitted mid object and optionally breakdown of those predictions into the functional decomposition terms.
}
