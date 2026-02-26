#' Compare MID Component Functions across Multiple Models
#'
#' @description
#' For "mid" objects (i.e., fitted MID models), \code{plot()} visualizes a single component function specified by the \code{term} argument.
#'
#' @details
#' This is an S3 method for the \code{plot()} generic that produces a plot from a "mid" object, visualizing a component function of the fitted MID model.
#'
#' The \code{type} argument controls the visualization style.
#' The default, \code{type = "effect"}, plots the component function itself.
#' In this style, the plotting method is automatically selected based on the effect's type:
#' a line plot for quantitative main effects; a bar plot for qualitative main effects; and a filled contour (level) plot for interactions.
#' The \code{type = "data"} option creates a scatter plot of \code{data}, colored by the values of the component function.
#' The \code{type = "compound"} option combines both approaches, plotting the component function alongside the data points.
#'
#' @param x a "mid" object to be visualized.
#' @param ... optional parameters to be passed to the graphing function. Possible arguments are "col", "fill", "pch", "cex", "lty", "lwd" and aliases of them.
#'
#' @returns
#' \code{plot.midlist()} produces a plot as a side-effect and returns \code{NULL} invisibly.
#'
#' @seealso \code{\link{plot.mid}}, \code{\link{ggmid.midlist}}
#'
#' @exportS3Method base::plot
#'
plot.midlist <- function(x, ...) {
  lapply(X = x, FUN = plot.mid, ...)
  invisible(NULL)
}
