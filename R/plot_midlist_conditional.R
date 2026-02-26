#' Compare MID Conditional Expectations across Multiple Models
#'
#' @description
#' For "midlist.conditional" objects, \code{plot()} visualizes Individual Conditional Expectation (ICE) curves derived from a fitted MID model.
#'
#' @details
#' This is an S3 method for the \code{plot()} generic that produces ICE curves from a "mid.conditional" object.
#' ICE plots are a model-agnostic tool for visualizing how a model's prediction for a single observation changes as one feature varies.
#' This function plots one line for each observation in the data.
#'
#' The \code{type} argument controls the visualization style:
#' The default, \code{type = "iceplot"}, plots the row ICE curves.
#' The \code{type = "centered"} option creates the centered ICE (c-ICE) plot, where each curve is shifted so start at zero, which makes it easier to compare the slopes of the curves.
#'
#' The \code{var.color}, \code{var.alpha}, etc., arguments allow you to map aesthetics to other variables in your data using (possibly) unquoted expressions.
#'
#' @param x a "mid.conditional" object to be visualized.
#' @param ... optional parameters passed on to the graphing functions.
#'
#' @returns
#' \code{plot.midlist.conditional()} produces an ICE plot as a side-effect and invisibly returns the ICE matrix used for the plot.
#'
#' @seealso \code{\link{plot.mid.conditional}}, \code{\link{ggmid.midlist.conditional}}
#'
#' @exportS3Method base::plot
#'
plot.midlist.conditional <- function(x, ...) {
  mcall <- match.call(expand.dots = TRUE)
  mcall[[1L]] <- quote(lapply)
  mcall[["x"]] <- NULL
  mcall[["X"]] <- x
  mcall[["FUN"]] <- quote(plot.mid.conditional)
  eval(mcall, parent.frame())
  invisible(NULL)
}
