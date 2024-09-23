#' Plot MID Individual Conditional Expectations with ggplot2 Package
#'
#' Creates a ggplot object representing the MID-based individual conditional expectations
#'
#' @param object a mid.conditional object to visualize.
#' @param limits NULL or a numeric vector of length two providing limits of the scale. NA is replaced by the minimum or maximum mid value.
#' @param plot.main logical. If TRUE, lines representing the individual conditional expectations are drawn.
#' @param centered logical.
#' @param show.dots logical. If TRUE, points representing the predictions at the observed values are
#' @param sample a vector specifying the set of names of the observations to be plotted.
#' @param term an optional character specifying one of the relevant terms. If passed, the individual conditional expectations for the specified term are plotted.
#' @param variable.alpha a name of the predictor variable to use to set \code{alpha} for each plot.
#' @param variable.colour a name of the predictor variable to use to set \code{color} for each plot.
#' @param variable.linetype a name of the predictor variable to use to set \code{linetype} for each plot.
#' @param variable.linewidth a name of the predictor variable to use to set \code{linewidth} for each plot.
#' @param ... additional parameters to be directly passed to \code{ggplot2::geom_line()}.
#' @exportS3Method midr::ggmid
#'
ggmid.mid.conditional <- function(
    object, limits = c(NA, NA), plot.main = TRUE, centered = FALSE,
    show.dots = TRUE, sample = NULL, term = NULL,
    variable.alpha = NULL, variable.colour = NULL,
    variable.linetype = NULL, variable.linewidth = NULL, ...) {
  mc <- match.call(expand.dots = TRUE)
  v <- attr(object, "variable")
  obs <- object$observed
  con <- object$conditional
  yvar <- "yhat"
  if (!is.null(term)) {
    if (!inherits(object, "mid.conditional.effects"))
      stop("the term effects are not stored in the object")
    if (!term %in% object$terms) {
      term <- paste0(rev(unlist(strsplit(term, ":"))), collapse = ":")
      if (!term %in% object$terms)
        stop(paste0("'", term, "' is not a relevant term"))
    }
    yvar <- paste0("mid(", term, ")")
    obs[, yvar] <- object$observed.effects[, term]
    con[, yvar] <- object$conditional.effects[, term]
  }
  if (centered) {
    n <- attr(object, "n")
    stp <- con[, yvar][1:n]
    ynew <- paste0("centered ", yvar)
    obs[, ynew] <- obs[, yvar] - stp
    con[, ynew] <- con[, yvar] - stp
    yvar <- ynew
  }
  if (!is.null(sample)) {
    obs <- obs[obs$id %in% sample, ]
    con <- con[con$id %in% sample, ]
  }
  pl <- ggplot2::ggplot(data = obs,
                        ggplot2::aes(x = .data[[v]], y = .data[[yvar]]))
  if (plot.main) {
    if (!is.null(mc$variable.alpha)) {
      alp <- as.character(mc$variable.alpha)
      variable.alpha <- quote(.data[[alp]])
      pl <- pl + ggplot2::aes(alpha = eval(variable.alpha)) +
        ggplot2::labs(alpha = alp)
    }
    if (!is.null(mc$variable.colour)) {
      col <- as.character(mc$variable.colour)
      variable.colour <- quote(.data[[col]])
      pl <- pl + ggplot2::aes(colour = eval(variable.colour)) +
        ggplot2::labs(colour = col)
    }
    if (!is.null(mc$variable.linetype)) {
      lty <- as.character(mc$variable.linetype)
      variable.linetype <- quote(.data[[lty]])
      pl <- pl + ggplot2::aes(linetype = eval(variable.linetype)) +
        ggplot2::labs(linetype = lty)
    }
    if (!is.null(mc$variable.linewidth)) {
      lwd <- as.character(mc$variable.linewidth)
      variable.linewidth <- quote(.data[[lwd]])
      pl <- pl + ggplot2::aes(linewidth = eval(variable.linewidth)) +
        ggplot2::labs(linewidth = lty)
    }
    pl <- pl + ggplot2::geom_line(data = con,
      mapping = ggplot2::aes(group = .data[["id"]]), ...)
  }
  if (show.dots)
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
  ggmid.mid.conditional(object, ...)
}
