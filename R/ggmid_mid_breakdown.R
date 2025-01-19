#' Plot MID Breakdown with ggplot2 Package
#'
#' For "mid.breakdown" objects, \code{ggmid()} visualizes the breakdown of a prediction by component functions.
#'
#' The S3 method of \code{ggmid()} for "mid.breakdown" objects creates a "ggplot" object that visualizes the breakdown of a single model prediction.
#' The main layer is drawn using \code{geom_col()}.
#'
#' @param object a "mid.breakdown" object to be visualized.
#' @param type a character string specifying the type of the plot. One of "barplot", "dotchart" or "waterfall".
#' @param plot.main logical. If \code{TRUE}, the main layer is not drawn.
#' @param max.bars an integer specifying the maximum number of bars in the plot.
#' @param sep a character string to separate terms and values. Default is the return.
#' @param width a numeric value specifying the width of the bars.
#' @param ... optional parameters to be passed to the main layer.
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' idx <- sample(nrow(diamonds), 1e4)
#' mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
#' mbd <- mid.breakdown(mid, diamonds[1L, ])
#' ggmid(mbd)
#' ggmid(mbd, type = "dotchart")
#' ggmid(mbd, type = "waterfall")
#' @returns
#' \code{ggmid.mid.breakdown()} returns a "ggplot" object.
#' @exportS3Method midr::ggmid
#'
ggmid.mid.breakdown <- function(
    object, type = c("barplot", "dotchart", "waterfall"),
    plot.main = TRUE, max.bars = NA, sep = "\n", width = .8,
    ...) {
  dots <- list(...)
  type = match.arg(type)
  bd <- object$breakdown
  nmax <- min(max.bars, nrow(bd), na.rm = TRUE)
  bd$term <- paste0(bd$term, sep, bd$value)
  if (nmax < nrow(bd)) {
    resid <- sum(bd[nmax:nrow(bd), "mid"])
    bd <- bd[1L:(nmax - 1L), ]
    bd[nmax, "mid"] <- resid
    bd[nmax, "term"] <- paste0("others")
  }
  bd$term <- factor(bd$term, levels = rev(bd$term))
  if (type == "barplot" || type == "dotchart") {
    pl <- ggplot2::ggplot(
      bd, ggplot2::aes(x = .data[["mid"]], y = .data[["term"]])
    ) + ggplot2::labs(y = NULL)
    if (plot.main) {
      if (type == "barplot") {
        pl <- pl + ggplot2::geom_col(width = width, ...)
      } else if (type == "dotchart") {
        pl <- pl + ggplot2::geom_linerange(
          ggplot2::aes(xmin = 0, xmax = .data[["mid"]]), lty = 3L) +
          ggplot2::geom_point(...)
      }
    }
  } else if (type == "waterfall") {
    hw <- width / 2
    bd$ymin <- as.integer(bd$term) - hw
    bd$ymax <- as.integer(bd$term) + hw
    cs <- cumsum(c(object$intercept, bd$mid))
    bd$xmin <- cs[1L:nrow(bd)]
    bd$xmax <- cs[2L:(nrow(bd) + 1L)]
    pl <- ggplot2::ggplot(
      bd, ggplot2::aes(y = .data[["term"]])
    ) + ggplot2::labs(y = NULL, x = "yhat")
    if (plot.main) {
      col <- ifnot.null(c(dots$colour, dots$color, dots$col)[1L], "gray15")
      lty <- ifnot.null(c(dots$linetype, dots$lty)[1L], 1L)
      pl <- pl +
        ggplot2::geom_linerange(
          ggplot2::aes(x = .data[["xmax"]],
                       ymin = pmax(.data[["ymin"]] - 1, 1 - hw),
                       ymax = .data[["ymax"]]), colour = col, linetype = lty) +
        ggplot2::geom_rect(
          ggplot2::aes(xmin = .data[["xmin"]], xmax = .data[["xmax"]],
                       ymin = .data[["ymin"]], ymax = .data[["ymax"]]), ...)
    }
  }
  return(pl)
}


#' @rdname ggmid.mid.breakdown
#' @exportS3Method ggplot2::autoplot
#'
autoplot.mid.breakdown <- function(object, ...) {
  ggmid.mid.breakdown(object = object, ...)
}
