#' Plot ICE of MID Model with ggplot2 Package
#'
#' For 'mid.conditional' objects, \code{ggmid()} visualizes ICE curves of a MID model.
#'
#' The S3 method of \code{ggmid()} for 'mid.conditional' objects creates a 'ggplot' object that visualizes ICE curves of a fitted MID model using \code{geom_line()}.
#' For the details of ICE, see Goldstein et al. (2013).
#'
#' @param object a 'mid.conditional' object to be visualized.
#' @param limits \code{NULL} or a numeric vector of length two specifying the limits of the scale. \code{NA}s are replaced by the minimum and maximum MID values.
#' @param plot.main logical. If \code{FALSE}, the main layer is not drawn.
#' @param centered logical. If \code{TRUE}, the ICE values of each observation are set to zero at the leftmost point.
#' @param draw.dots logical. If \code{TRUE}, the points representing the predictions for each observation are plotted.
#' @param sample a vector specifying the names of observations to be plotted.
#' @param term an optional character string specifying the interaction term. If passed, the ICE for the specified term is plotted.
#' @param variable.alpha a name of the variable to use to set \code{alpha}.
#' @param variable.colour a name of the variable to use to set \code{colour}.
#' @param variable.linetype a name of the variable to use to set \code{linetype}.
#' @param variable.linewidth a name of the variable to use to set \code{linewidth}.
#' @param ... optional parameters to be passed to the main layer.
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
#' mc <- mid.conditional(mid, "Wind", airquality)
#' ggmid(mc, variable.colour = "Solar.R", centered = TRUE)
#' @returns
#' \code{ggmid.mid.conditional()} returns a 'ggplot' object.
#' @exportS3Method midr::ggmid
#'
ggmid.mid.conditional <- function(
    object, limits = c(NA, NA), plot.main = TRUE, centered = FALSE,
    draw.dots = TRUE, sample = NULL, term = NULL,
    variable.alpha = NULL, variable.colour = NULL,
    variable.linetype = NULL, variable.linewidth = NULL, ...) {
  cl <- match.call(expand.dots = TRUE)
  variable <- attr(object, "variable")
  n <- attr(object, "n")
  obs <- object$observed
  con <- object$conditional
  yvar <- "yhat"
  if (!is.null(term)) {
    if (!inherits(object, "mid.conditional.effects"))
      stop("the term effects are not stored in the object")
    term <- term.check(term, object$terms, stop = TRUE)
    yvar <- paste0("mid(", term, ")")
    obs[, yvar] <- object$observed.effects[, term]
    con[, yvar] <- object$conditional.effects[, term]
  }
  if (centered) {
    stp <- con[, yvar][1L:n]
    ynew <- paste0("centered ", yvar)
    obs[, ynew] <- obs[, yvar] - stp
    con[, ynew] <- con[, yvar] - stp
    yvar <- ynew
  }
  if (!is.null(sample)) {
    obs <- obs[obs$id %in% sample, ]
    con <- con[con$id %in% sample, ]
    n <- nrow(obs)
  }
  pl <- ggplot2::ggplot(data = obs,
                        ggplot2::aes(x = .data[[variable]], y = .data[[yvar]]))
  if (plot.main) {
    if (!is.null(cl$variable.alpha)) {
      alp <- characterize(cl$variable.alpha)
      pl <- pl +
        ggplot2::aes(alpha = .data[[alp]]) +
        ggplot2::labs(alpha = alp)
    }
    if (!is.null(cl$variable.colour)) {
      col <- characterize(cl$variable.colour)
      pl <- pl +
        ggplot2::aes(colour = .data[[col]]) +
        ggplot2::labs(colour = col)
    }
    if (!is.null(cl$variable.linetype)) {
      lty <- characterize(cl$variable.linetype)
      pl <- pl +
        ggplot2::aes(linetype = .data[[lty]]) +
        ggplot2::labs(linetype = lty)
      if (!is.discrete(obs[, lty]))
        pl <- pl + ggplot2::scale_linetype_binned()
    }
    if (!is.null(cl$variable.linewidth)) {
      lwd <- characterize(cl$variable.linewidth)
      pl <- pl +
        ggplot2::aes(linewidth = .data[[lwd]]) +
        ggplot2::labs(linewidth = lwd)
      if (is.discrete(obs[, lwd])) {
        pl <- pl + ggplot2::scale_linewidth_discrete(range = c(0, 1))
      } else {
        pl <- pl + ggplot2::scale_linewidth_continuous(range = c(0, 1))
      }
    }
    pl <- pl + ggplot2::geom_line(data = con,
      mapping = ggplot2::aes(group = .data[["id"]]), ...)
  }
  if (draw.dots)
    pl <- pl + ggplot2::geom_point()
  if (!is.null(limits))
    pl <- pl + ggplot2::scale_y_continuous(limits = limits)
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
