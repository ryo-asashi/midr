#' Plot ICE of MID Model with ggplot2 Package
#'
#' For "mid.conditional" objects, \code{ggmid()} visualizes ICE curves of a MID model.
#'
#' The S3 method of \code{ggmid()} for "mid.conditional" objects creates a "ggplot" object that visualizes ICE curves of a fitted MID model using \code{geom_line()}.
#'
#' @param object a "mid.conditional" object to be visualized.
#' @param type a character string specifying the type of the plot. One of "iceplot" or "centered". If "centered", the ICE values of each observation are set to zero at the leftmost point of the varriable.
#' @param theme a character string specifying the color theme or any item that can be used to define "color.theme" object.
#' @param term an optional character string specifying an interaction term. If passed, the ICE curve for the specified term is plotted.
#' @param var.alpha a name of the variable or an expression to be used to set \code{alpha}.
#' @param var.color a name of the variable or an expression to be used to set \code{colour}.
#' @param var.linetype a name of the variable or an expression to be used to set \code{linetype}.
#' @param var.linewidth a name of the variable or an expression to be used to set \code{linewidth}.
#' @param reference an integer specifying the index of the sample points to be used as reference point for the centered ICE plot. Default is \code{1}. If negative, the maximum value of the variable is used.
#' @param dots logical. If \code{TRUE}, the points representing the predictions for each observation are plotted.
#' @param sample an optional vector specifying the names of observations to be plotted.
#' @param ... optional parameters to be passed to the main layer.
#' @examples
#' data(airquality, package = "datasets")
#' library(midr)
#' mid <- interpret(Ozone ~ .^2, airquality, lambda = 0.1)
#' ice <- mid.conditional(mid, "Temp", data = airquality)
#' ggmid(ice, var.color = "Wind")
#' ggmid(ice, type = "centered", theme = "Purple-Yellow",
#'       var.color = factor(Month), var.linetype = Wind > 10)
#' @returns
#' \code{ggmid.mid.conditional()} returns a "ggplot" object.
#' @exportS3Method midr::ggmid
#'
ggmid.mid.conditional <- function(
    object, type = c("iceplot", "centered"), theme = NULL, term = NULL,
    var.alpha = NULL, var.color = NULL, var.linetype = NULL, var.linewidth = NULL,
    reference = 1L, dots = TRUE, sample = NULL, ...) {
  type <- match.arg(type)
  theme <- color.theme(theme)
  use.theme <- inherits(theme, "color.theme")
  variable <- attr(object, "variable")
  n <- attr(object, "n")
  obs <- object$observed
  con <- object$conditional
  values <- object$values
  yvar <- "yhat"
  if (!is.null(term)) {
    if (is.null(object$conditional.effects))
      stop("the term effects are not stored in the object")
    term <- term.check(term, object$terms, stop = TRUE)
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
      theme <- "bluescale"
    pl <- pl + scale_color_theme(theme = theme,
                                 discrete = is.discrete(obs$.col))
  }
  pl
}


#' @rdname ggmid.mid.conditional
#' @exportS3Method ggplot2::autoplot
#'
autoplot.mid.conditional <- function(object, ...) {
  args <- as.list(match.call())
  args[[1L]] <- NULL
  do.call("ggmid.mid.conditional", args)
}
