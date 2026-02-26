#' Compare MID Conditional Expectations with ggplot2
#'
#' @description
#' For "mid.conditional" objects, \code{ggmid()} visualizes Individual Conditional Expectation (ICE) curves derived from a fitted MID model.
#'
#' @details
#' This is an S3 method for the \code{ggmid()} generic that produces ICE curves from a "mid.conditional" object.
#' ICE plots are a model-agnostic tool for visualizing how a model's prediction for a single observation changes as one feature varies.
#' This function plots one line for each observation in the data.
#'
#' The \code{type} argument controls the visualization style:
#' The default, \code{type = "iceplot"}, plots the raw ICE curves.
#' The \code{type = "centered"} option creates the centered ICE (c-ICE) plot, where each curve is shifted to start at zero, making it easier to compare the slopes of the curves.
#'
#' The \code{var.color}, \code{var.alpha}, etc., arguments allow you to map aesthetics to other variables in your data using (possibly) unquoted expressions.
#'
#' @param object a "mid.conditional" object to be visualized.
#' @param ... optional parameters passed on to the main layer.
#'
#' @returns
#' \code{ggmid.midlist.conditional()} returns a "ggplot" object.
#'
#' @seealso \code{\link{ggmid.midcon}}, \code{\link{plot.midcons}}
#'
#' @exportS3Method midr::ggmid
#'
ggmid.midcons <- function(object, ...) {
  mcall <- match.call(expand.dots = TRUE)
  mcall[[1L]] <- quote(lapply)
  mcall[["object"]] <- NULL
  mcall[["X"]] <- object
  mcall[["FUN"]] <- quote(ggmid.mid.conditional)
  eval(mcall, parent.frame())
}

#' @rdname ggmid.midcons
#'
#' @exportS3Method ggplot2::autoplot
#'
autoplot.midcons <- function(object, ...) {
  message("not implemented")
  return(NULL)
}
