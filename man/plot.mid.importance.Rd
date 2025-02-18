% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_mid_importance.R
\name{plot.mid.importance}
\alias{plot.mid.importance}
\title{Plot MID Importance with graphics Package}
\usage{
\method{plot}{mid.importance}(
  x,
  type = c("barplot", "dotchart", "heatmap", "boxplot"),
  theme = NULL,
  max.bars = 30L,
  ...
)
}
\arguments{
\item{x}{a "mid.importance" object to be visualized.}

\item{type}{a character string specifying the type of the plot. One of "barplot", "heatmap", "dotchart" or "boxplot".}

\item{theme}{a character string specifying the color theme or any item that can be used to define "color.theme" object.}

\item{max.bars}{an integer specifying the maximum number of bars in the barplot, boxplot and dotchart.}

\item{...}{optional parameters to be passed to the graphing function. Possible arguments are "col", "fill", "pch", "cex", "lty", "lwd" and aliases of them.}
}
\value{
\code{plot.mid.importance()} produces a plot and returns \code{NULL}.
}
\description{
For "mid.importance" objects, \code{plot()} visualizes the importance of MID component functions.
}
\details{
The S3 method of \code{plot()} for "mid.importance" objects creates a visualization of the MID importance using the functions of the graphics package.
}
\examples{
data(diamonds, package = "ggplot2")
set.seed(42)
idx <- sample(nrow(diamonds), 1e4)
mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
imp <- mid.importance(mid)
plot(imp, theme = "Tableau 10")
plot(imp, type = "dotchart", theme = "Okabe-Ito")
plot(imp, type = "heatmap", theme = "Blues")
plot(imp, type = "boxplot", theme = "Accent")
}
