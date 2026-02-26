#' Compare MID Breakdowns across Multiple Models with ggplot2
#'
#' @description
#' For "mid.breakdown" objects, \code{ggmid()} visualizes the breakdown of a prediction by component functions.
#'
#' @details
#' This is an S3 method for the \code{ggmid()} generic that creates a breakdown plot from a "mid.breakdown" object, visualizing the contribution of each component function to a single prediction.
#'
#' The \code{type} argument controls the visualization style.
#' The default, \code{type = "waterfall"} (default), creates a waterfall plot that shows how the prediction is built up from the intercept, with each term's contribution sequentially added or subtracted.
#' The \code{type = "barplot"} option creates a standard bar plot where the length of each bar represents the magnitude of the term's contribution.
#' The \code{type = "dotchart"} option creates a dot plot showing the contribution of each term as a point connected to a zero baseline.
#'
#' @param object a "mid.breakdown" object to be visualized.
#' @param ... optional parameters passed on to the main layer.
#'
#' @returns
#' \code{ggmid.midlist.breakdown()} returns a "ggplot" object.
#'
#' @seealso \code{\link{ggmid.mid.breakdown}}, \code{\link{plot.mid.breakdown}}
#'
#' @exportS3Method midr::ggmid
#'
ggmid.midlist.breakdown <- function(object, ...) {
  lapply(X = object, FUN = ggmid.mid.breakdown, ...)
}

#' @rdname ggmid.midlist.breakdown
#'
#' @exportS3Method ggplot2::autoplot
#'
autoplot.midlist.breakdown <- function(object, ...) {
  ggmid.midlist.breakdown(object = object, ...)
}
