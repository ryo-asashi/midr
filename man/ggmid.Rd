% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggmid.R
\name{ggmid}
\alias{ggmid}
\alias{ggmid.mid}
\alias{autoplot.mid}
\title{Plot MID Values with ggplot2 Package}
\usage{
ggmid(object, ...)

\method{ggmid}{mid}(
  object,
  term,
  limits = c(NA, NA),
  plot.main = TRUE,
  add.intercept = FALSE,
  include.main.effects = FALSE,
  interaction.type = c("default", "raster", "rectangle"),
  scale.type = "default",
  scale.palette = c("#2f7a9a", "#FFFFFF", "#7e1952"),
  partition = 100L,
  ...
)

\method{autoplot}{mid}(object, ...)
}
\arguments{
\item{object}{a mid object to be visualized.}

\item{...}{optional parameters to be passed to the main layer (\code{geom_line}, \code{geom_path}, \code{geom_bar}, or \code{geom_rect}) of each plot.}

\item{term}{a name of the functional decomposition term to be plotted.}

\item{limits}{NULL or a numeric vector of length two providing limits of the scale. NA is replaced by the minimum or maximum mid value.}

\item{plot.main}{logical. If TRUE, lines, bars or rectangles representing mid values are drawn.}

\item{add.intercept}{logical. If TRUE, the intercept is added to the mid values and the scale for the plot is shifted.}

\item{include.main.effects}{logical. If TRUE, the main effects are added to the interaction mid values.}

\item{interaction.type}{a character, specifying the plotting method of interaction effects.}

\item{scale.type}{color type of interaction plots. One of "default", "viridis", "gradient" or a function that returns a continuous colour scale for \code{fill} aestetics like \code{ggplot2::scale_fill_viridis_c}.}

\item{scale.palette}{a character vector of color names, specifying the colors to be used in the interaction plot.}

\item{partition}{an integer specifying the coarseness of the grid for a "raster" type interaction plot.}
}
\description{
Creates a ggplot object representing mid values of the functional decomposition term.
}
\examples{
data(diamonds, package = "ggplot2")
model <- lm(price ~ carat + cut + color + clarity + carat:clarity, diamonds)
mid <- interpret(price ~ carat + cut + color + clarity + carat:clarity,
                 data = diamonds, model = model)
ggmid(mid, "carat")
ggplot2::autoplot(mid, "clarity")
ggmid(mid, "carat:clarity")
ggmid(mid, "carat:clarity", add.intercept = TRUE,
      include.main.effects = TRUE, scale.type = "viridis")
}
