#' Plot MID Individual Conditional Expectations with ggplot2 Package
#'
#' Creates a ggplot object representing the MID-based individual conditional expectations
#'
#' @param object a mid.conditional object to visualize.
#' @param limits NULL or a numeric vector of length two providing limits of the scale. NA is replaced by the minimum or maximum mid value.
#' @param plot.main logical. If TRUE, lines representing the individual conditional expectations are drawn.
#' @param centered logical.
#' @param draw.dots logical. If TRUE, points representing the predictions at the observed values are
#' @param sample a vector specifying the set of names of the observations to be plotted.
#' @param term an optional character specifying one of the relevant terms. If passed, the individual conditional expectations for the specified term are plotted.
#' @param variable.alpha a name of the predictor variable to use to set \code{alpha} for each plot.
#' @param variable.colour a name of the predictor variable to use to set \code{color} for each plot.
#' @param variable.linetype a name of the predictor variable to use to set \code{linetype} for each plot.
#' @param variable.linewidth a name of the predictor variable to use to set \code{linewidth} for each plot.
#' @param ... optional parameters to be directly passed to \code{ggplot2::geom_line()}.
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
#' mc <- mid.conditional(mid, "Wind", airquality)
#' ggmid(mc, variable.colour = "Solar.R", centered = TRUE)
#' @returns
#' \code{ggmid.mid.conditional()} returns a \code{ggplot} object.
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


#'
#' @rdname ggmid.mid.conditional
#' @exportS3Method ggplot2::autoplot
#'
autoplot.mid.conditional <- function(object, ...) {
  args <- as.list(match.call())
  args[[1L]] <- NULL
  do.call("ggmid.mid.conditional", args)
}
