#' Plot MID Conditional Expectations with ggplot2
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
#' @param type the plotting style. One of "iceplot" or "centered".
#' @param theme a character string or object defining the color theme. See \code{\link{color.theme}} for details.
#' @param term an optional character string specifying an interaction term. If passed, the ICE curve for the specified term is plotted.
#' @param var.alpha a variable name or expression to map to the alpha aesthetic.
#' @param var.color a variable name or expression to map to the color aesthetic.
#' @param var.linetype a variable name or expression to map to the linetype aesthetic.
#' @param var.linewidth a variable name or expression to map to the linewidth aesthetic.
#' @param reference an integer specifying the index of the evaluation point to use as the reference for centering the c-ICE plot.
#' @param dots logical. If \code{TRUE}, points representing the actual predictions for each observation are plotted.
#' @param sample an optional vector specifying the names of observations to be plotted.
#' @param ... optional parameters passed on to the main layer.
#'
#' @examples
#' data(airquality, package = "datasets")
#' library(midr)
#' mid <- interpret(Ozone ~ .^2, airquality, lambda = 0.1)
#' ice <- mid.conditional(mid, "Temp", data = airquality)
#'
#' # Create an ICE plot, coloring lines by 'Wind'
#' ggmid(ice, var.color = "Wind")
#'
#' # Create a centered ICE plot, mapping color and linetype to other variables
#' ggmid(ice, type = "centered", theme = "Purple-Yellow",
#'       var.color = factor(Month), var.linetype = Wind > 10)
#' @returns
#' \code{ggmid.mid.conditional()} returns a "ggplot" object.
#'
#' \code{ggmid.midlist.conditional()} returns a list of "ggplot" objects.
#'
#' @seealso \code{\link{mid.conditional}}, \code{\link{ggmid}}, \code{\link{plot.mid.conditional}}
#'
#' @exportS3Method midr::ggmid
#'
ggmid.mid.conditional <- function(
    object, type = c("iceplot", "centered"), theme = NULL, term = NULL,
    var.alpha = NULL, var.color = NULL, var.linetype = NULL, var.linewidth = NULL,
    reference = 1L, dots = TRUE, sample = NULL, ...) {
  type <- match.arg(type)
  theme <- color.theme(theme)
  use.theme <- inherits(theme, "color.theme")
  variable <- object$variable
  n <- attr(object, "n")
  obs <- object$observed
  con <- object$conditional
  values <- object$values
  yvar <- "yhat"
  if (!is.null(term)) {
    if (is.null(object$conditional.effects))
      stop("the term effects are not stored in the object")
    term <- term.check(term, mid.terms(object), stop = TRUE)
    yvar <- paste0("mid(", term, ")")
    obs[, yvar] <- object$observed.effects[, term]
    con[, yvar] <- object$conditional.effects[, term]
  }
  if (type == "centered") {
    if (reference < 0) reference <- length(values)
    ref <- values[min(length(values), max(1L, reference))]
    stp <- con[con[[variable]] == ref, yvar]
    ynew <- paste0("centered ", yvar)
    obs[, ynew] <- obs[, yvar] - stp
    con[, ynew] <- con[, yvar] - stp
    yvar <- ynew
  }
  if (!is.null(sample)) {
    obs <- obs[obs$.id %in% sample, ]
    con <- con[con$.id %in% sample, ]
    n <- nrow(obs)
  }
  if (n == 0L) {
    message("no observations found")
    return(invisible(NULL))
  }
  pl <- ggplot2::ggplot(
    data = obs, ggplot2::aes(x = .data[[variable]], y = .data[[yvar]])
  )
  if (!is.null(alp <- substitute(var.alpha))) {
    if (is.character(alp)) alp <- str2lang(alp)
    obs$.alp <- eval(alp, envir = obs)
    con$.alp <- eval(alp, envir = con)
    pl <- pl + ggplot2::aes(alpha = .data[[".alp"]]) +
      ggplot2::labs(alpha = alp)
  }
  if (set.color <- !is.null(col <- substitute(var.color))) {
    if (is.character(col)) col <- str2lang(col)
    obs$.col <- eval(col, envir = obs)
    con$.col <- eval(col, envir = con)
    pl <- pl + ggplot2::aes(colour = .data[[".col"]]) +
      ggplot2::labs(colour = col)
  }
  if (!is.null(lty <- substitute(var.linetype))) {
    if (is.character(lty)) lty <- str2lang(lty)
    obs$.lty <- eval(lty, envir = obs)
    con$.lty <- eval(lty, envir = con)
    pl <- pl + ggplot2::aes(linetype = .data[[".lty"]]) +
      ggplot2::labs(linetype = lty)
    if (!is.discrete(obs$.lty))
      pl <- pl + ggplot2::scale_linetype_binned()
  }
  if (!is.null(lwd <- substitute(var.linewidth))) {
    if (is.character(lwd)) lwd <- str2lang(lwd)
    obs$.lwd <- eval(lwd, envir = obs)
    con$.lwd <- eval(lwd, envir = con)
    pl <- pl + ggplot2::aes(linewidth = .data[[".lwd"]]) +
      ggplot2::labs(linewidth = lwd)
    if (is.discrete(obs$.lwd)) {
      pl <- pl + ggplot2::scale_linewidth_discrete(range = c(0, 1))
    } else {
      pl <- pl + ggplot2::scale_linewidth_continuous(range = c(0, 1))
    }
  }
  pl <- pl + ggplot2::geom_line(
    data = con, mapping = ggplot2::aes(group = .data[[".id"]]), ...)
  if (dots) {
    pl <- pl + ggplot2::geom_point(data = obs)
  }
  if (set.color) {
    if (!use.theme)
      theme <- if (is.discrete(obs$.col))
        getOption("midr.qualitative", "HCL") else
        getOption("midr.sequential", "bluescale")
    pl <- pl + scale_color_theme(theme = theme,
                                 discrete = is.discrete(obs$.col))
  }
  pl
}

#' @rdname ggmid.mid.conditional
#' @exportS3Method ggplot2::autoplot
#'
autoplot.mid.conditional <- function(object, ...) {
  mcall <- match.call(expand.dots = TRUE)
  mcall[[1L]] <- quote(ggmid.mid.conditional)
  mcall[["object"]] <- object
  eval(mcall, parent.frame())
}

#' @rdname ggmid.mid.conditional
#' @exportS3Method midr::ggmid
#'
ggmid.midlist.conditional <- function(object, ...) {
  mcall <- match.call(expand.dots = TRUE)
  mcall[[1L]] <- quote(lapply)
  mcall[["object"]] <- NULL
  mcall[["X"]] <- object
  mcall[["FUN"]] <- quote(ggmid.mid.conditional)
  eval(mcall, parent.frame())
}
