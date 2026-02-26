#' Compare MID Breakdowns in a Collection
#'
#' @description
#' For "mid.breakdown" objects, \code{plot()} visualizes the breakdown of a prediction by component functions.
#'
#' @details
#' This is an S3 method for the \code{plot()} generic that produces a breakdown plot from a "mid.breakdown" object, visualizing the contribution of each component function to a single prediction.
#'
#' The \code{type} argument controls the visualization style.
#' The default, \code{type = "waterfall"}, creates a waterfall plot that shows how the prediction builds from the intercept, with each term's contribution sequentially added or subtracted.
#' The \code{type = "barplot"} option creates a standard bar plot where the length of each bar represents the magnitude of the term's contribution.
#' The \code{type = "dotchart"} option creates a dot plot showing the contribution of each term as a point connected to a zero baseline.
#'
#' @param x a "mid.breakdown" object to be visualized.
#' @param ... optional parameters passed on to the graphing function. Possible arguments are "col", "fill", "pch", "cex", "lty", "lwd" and aliases of them.
#'
#' @returns
#' \code{plot.midlist.breakdown()} produces a plot as a side effect and returns \code{NULL} invisibly.
#'
#' @seealso \code{\link{plot.mid.breakdown}}, \code{\link{ggmid.midlist.breakdown}}
#'
#' @exportS3Method base::plot
#'
plot.midlist.breakdown <- function(x, ...) {
  lapply(X = x, FUN = plot.mid.breakdown, ...)
  invisible(NULL)
}
