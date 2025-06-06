% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_mid_conditional.R
\name{plot.mid.conditional}
\alias{plot.mid.conditional}
\title{Plot ICE of MID Model with graphics Package}
\usage{
\method{plot}{mid.conditional}(
  x,
  type = c("iceplot", "centered"),
  theme = NULL,
  term = NULL,
  var.alpha = NULL,
  var.color = NULL,
  var.linetype = NULL,
  var.linewidth = NULL,
  reference = 1L,
  dots = TRUE,
  sample = NULL,
  ...
)
}
\arguments{
\item{x}{a "mid.conditional" object to be visualized.}

\item{type}{a character string specifying the type of the plot. One of "iceplot" or "centered". If "centered", the ICE values of each observation are set to zero at the leftmost point of the varriable.}

\item{theme}{a character string specifying the color theme or any item that can be used to define "color.theme" object.}

\item{term}{an optional character string specifying the interaction term. If passed, the ICE for the specified term is plotted.}

\item{var.alpha}{a name of the variable or an expression to be used to set \code{alpha}.}

\item{var.color}{a name of the variable or an expression to be used to set \code{colour}.}

\item{var.linetype}{a name of the variable or an expression to be used to set \code{linetype}.}

\item{var.linewidth}{a name of the variable or an expression to be used to set \code{linewidth}.}

\item{reference}{an integer specifying the index of the sample points to be used as reference point for the centered ICE plot. Default is \code{1}. If negative, the maximum value of the variable is used.}

\item{dots}{logical. If \code{TRUE}, the points representing the predictions for each observation are plotted.}

\item{sample}{an optional vector specifying the names of observations to be plotted.}

\item{...}{optional parameters to be passed to the graphing function. Possible arguments are "col", "fill", "pch", "cex", "lty", "lwd" and aliases of them.}
}
\value{
\code{plot.mid.conditional()} produces an ICE plot and invisibly returns the ICE matrix used for the plot.
}
\description{
For "mid.conditional" objects, \code{plot()} visualizes ICE curves of a MID model.
}
\details{
The S3 method of \code{plot()} for "mid.conditional" objects creates an visualization of ICE curves of a fitted MID model using the functions of the graphics package.
}
\examples{
data(airquality, package = "datasets")
library(midr)
mid <- interpret(Ozone ~ .^2, airquality, lambda = 0.1)
ice <- mid.conditional(mid, "Temp", data = airquality)
plot(ice, var.color = "Wind")
plot(ice, type = "centered", theme = "Purple-Yellow",
     var.color = factor(Month), var.linetype = Wind > 10)
}
