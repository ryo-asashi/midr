% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggmid_mid_importance.R
\name{ggmid.mid.importance}
\alias{ggmid.mid.importance}
\alias{autoplot.mid.importance}
\title{Plot MID Importance with ggplot2 Package}
\usage{
\method{ggmid}{mid.importance}(
  object,
  type = c("barplot", "dotchart", "heatmap", "boxplot"),
  theme = NULL,
  max.bars = 30L,
  ...
)

\method{autoplot}{mid.importance}(object, ...)
}
\arguments{
\item{object}{a "mid.importance" object to be visualized.}

\item{type}{a character string specifying the type of the plot. One of "barplot", "heatmap", "dotchart" or "boxplot".}

\item{theme}{a character string specifying the color theme or any item that can be used to define "color.theme" object.}

\item{max.bars}{an integer specifying the maximum number of bars in the barplot, boxplot and dotchart.}

\item{...}{optional parameters to be passed to the main layer.}
}
\value{
\code{ggmid.mid.importance()} returns a "ggplot" object.
}
\description{
For "mid.importance" objects, \code{ggmid()} visualizes the importance of MID component functions.
}
\details{
The S3 method of \code{ggmid()} for "mid.importance" objects creates a "ggplot" object that visualizes the term importance of a fitted MID model.
The main layer is drawn using \code{geom_col()}, \code{geom_tile()}, \code{geom_point()} or \code{geom_boxplot()}.
}
\examples{
data(diamonds, package = "ggplot2")
set.seed(42)
idx <- sample(nrow(diamonds), 1e4)
mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
imp <- mid.importance(mid)
ggmid(imp, theme = "Tableau 10")
ggmid(imp, type = "dotchart", theme = "Okabe-Ito", size = 3)
ggmid(imp, type = "heatmap", theme = "Blues")
ggmid(imp, type = "boxplot", theme = "Accent")
}
