% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_mid.R
\name{plot.mid}
\alias{plot.mid}
\title{Plot MID Values with graphics Package}
\usage{
\method{plot}{mid}(
  x,
  term,
  add.intercept = FALSE,
  include.main.effects = FALSE,
  scale.type = "default",
  scale.palette = c("#2f7a9a", "#FFFFFF", "#7e1952"),
  m = 100L,
  ...
)
}
\arguments{
\item{x}{mid object to be visualized.}

\item{term}{name of term to be plotted.}

\item{add.intercept}{logical. If TRUE, the intercept is added to the mid values and the scale for the plot is shifted.}

\item{include.main.effects}{logical. If TRUE, the main effects are added to the interaction mid values.}

\item{scale.type}{color type of interaction plots. One of "default", "viridis", "gradient" or a function that returns a continuous colour scale for \code{fill} aestetics like \code{ggplot2::scale_fill_viridis_c}.}

\item{scale.palette}{a character vector of color names, specifying the colors to be used in the interaction plot.}

\item{m}{an integer specifying the coarseness of the grid for a interaction plot.}

\item{...}{optional parameters to be passed to \code{plot()}, \code{barplot()} or \code{filled.contour()}.}
}
\value{
\code{plot.mid()} produces a line plot or a bar plot for the main effect and a filled contour plot for the interaction.
}
\description{
Creates a plot showing mid values of the functional decomposition term.
}
\examples{
data(airquality, package = "datasets")
airquality$Month <- factor(airquality$Month)
mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
plot(mid, "Temp")
plot(mid, "Month")
plot(mid, "Wind:Temp")
plot(mid, "Solar.R:Month", scale.type = "viridis",
     add.intercept = TRUE, include.main.effects = TRUE)
}
