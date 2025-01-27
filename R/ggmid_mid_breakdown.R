#' Plot MID Breakdown with ggplot2 Package
#'
#' For "mid.breakdown" objects, \code{ggmid()} visualizes the breakdown of a prediction by component functions.
#'
#' The S3 method of \code{ggmid()} for "mid.breakdown" objects creates a "ggplot" object that visualizes the breakdown of a single model prediction.
#' The main layer is drawn using \code{geom_col()}.
#'
#' @param object a "mid.breakdown" object to be visualized.
#' @param type a character string specifying the type of the plot. One of "waterfall", "barplot" or "dotchart".
#' @param theme a character string specifying the color theme or any item that can be used to define "color.theme" object.
#' @param terms an optional character vector specifying the terms to be displayed.
#' @param max.bars an integer specifying the maximum number of bars in the plot.
#' @param width a numeric value specifying the width of the bars.
#' @param vline logical. If \code{TRUE}, the vertical line is drawn at zero or the intercept.
#' @param catchall a character string to be used as the catchall label.
#' @param format a character string to be used as the format of the \code{sprintf()} function for each component.
#' @param ... optional parameters to be passed to the main layer.
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' idx <- sample(nrow(diamonds), 1e4)
#' mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
#' mbd <- mid.breakdown(mid, diamonds[1L, ])
#' ggmid(mbd, type = "waterfall")
#' ggmid(mbd, type = "waterfall", theme = "midr")
#' ggmid(mbd, type = "barplot", theme = "Set 1")
#' ggmid(mbd, type = "dotchart", size = 3, theme = "Cividis")
#' @returns
#' \code{ggmid.mid.breakdown()} returns a "ggplot" object.
#' @exportS3Method midr::ggmid
#'
ggmid.mid.breakdown <- function(
    object, type = c("waterfall", "barplot", "dotchart"), theme = NULL,
    terms = NULL, max.bars = 15L, width = NULL, vline = TRUE,
    catchall = "others", format = "%s=%s", ...) {
  dots <- list(...)
  type <- match.arg(type)
  theme <- color.theme(theme)
  use.theme <- inherits(theme, "color.theme")
  bd <- object$breakdown
  bd$term <- as.character(bd$term)
  use.catchall <- FALSE
  if (!is.null(terms)) {
    rowid <- match(terms, bd$term, nomatch = 0L)
    resid <- sum(bd[-rowid, "mid"])
    bd <- bd[rowid, ]
    bd[nrow(bd) + 1L, "mid"] <- resid
    use.catchall <- TRUE
  }
  nmax <- min(max.bars, nrow(bd), na.rm = TRUE)
  if (nmax < nrow(bd)) {
    resid <- sum(bd[nmax:nrow(bd), "mid"])
    bd <- bd[1L:(nmax - 1L), ]
    bd[nmax, "mid"] <- resid
    use.catchall <- TRUE
  }
  for (i in seq_len(nrow(bd) - as.numeric(use.catchall))) {
    term <- bd[i, "term"]
    bd[i, "term"] <- sprintf(format, bd[i, "term"], bd[i, "value"])
  }
  if (use.catchall)
    bd[nrow(bd), "term"] <- catchall
  bd$term <- factor(bd$term, levels = rev(bd$term))
  # barplot and dotchart
  if (type == "barplot" || type == "dotchart") {
    pl <- ggplot2::ggplot(
      bd, ggplot2::aes(x = .data[["mid"]], y = .data[["term"]])
    ) + ggplot2::labs(y = NULL)
    if (type == "barplot") {
      pl <- pl + ggplot2::geom_col(width = width, ...)
      if (use.theme) {
        pl <- pl + if (theme$type == "qualitative") {
          ggplot2::aes(fill = .data[["mid"]] > 0)
        } else {
          ggplot2::aes(fill = .data[["mid"]])
        }
        pl <- pl + scale_fill_theme(theme = theme)
      }
    } else if (type == "dotchart") {
      pl <- pl + ggplot2::geom_linerange(
        ggplot2::aes(xmin = 0, xmax = .data[["mid"]]), lty = 3L) +
        ggplot2::geom_point(...)
      if (use.theme) {
        pl <- pl + if (theme$type == "qualitative") {
          ggplot2::aes(color = .data[["mid"]] > 0)
        } else {
          ggplot2::aes(color = .data[["mid"]])
        }
        pl <- pl + scale_color_theme(theme = theme)
      }
    }
    if (vline) {
      tli <- ggplot2::theme_get()$line
      pl <- pl + ggplot2::geom_vline(
        xintercept = 0, lwd = ifnot.null(tli$linewidth, .5) * .5)
    }
    return(pl)
  # waterfall
  } else if (type == "waterfall") {
    width <- ifnot.null(width, .6)
    hw <- width / 2
    bd$ymin <- as.integer(bd$term) - hw
    bd$ymax <- as.integer(bd$term) + hw
    cs <- cumsum(c(object$intercept, bd$mid))
    bd$xmin <- cs[1L:nrow(bd)]
    bd$xmax <- cs[2L:(nrow(bd) + 1L)]
    pl <- ggplot2::ggplot(
      bd, ggplot2::aes(y = .data[["term"]])) +
      ggplot2::labs(y = NULL, x = "yhat")
    tli <- ggplot2::theme_get()$line
    col <- ifnot.null(c(dots$colour, dots$color, dots$col)[1L],
                      ifnot.null(tli$colour, "black"))
    lty <- ifnot.null(c(dots$linetype, dots$lty)[1L],
                      ifnot.null(tli$linetype, 1L))
    lwd <- ifnot.null(c(dots$linewidth, dots$lwd)[1L],
                      ifnot.null(tli$linewidth, 0.5))
    if (vline) {
      pl <- pl + ggplot2::geom_vline(
        xintercept = object$intercept, lwd = ifnot.null(tli$linewidth, .5) * .5)
    }
    pl <- pl +
      ggplot2::geom_rect(
        ggplot2::aes(xmin = .data[["xmin"]], xmax = .data[["xmax"]],
                     ymin = .data[["ymin"]], ymax = .data[["ymax"]]), ...) +
      ggplot2::geom_linerange(
        ggplot2::aes(x = .data[["xmax"]], ymax = .data[["ymax"]],
                     ymin = pmax(.data[["ymin"]] - 1, 1 - hw)),
        col = col, lty = lty, lwd = lwd)
    if (use.theme) {
      pl <- pl + if (theme$type == "qualitative") {
        ggplot2::aes(fill = .data[["mid"]] > 0)
      } else {
        ggplot2::aes(fill = .data[["mid"]])
      }
      pl <- pl + scale_fill_theme(theme = theme)
    }
    return(pl)
  }
}


#' @rdname ggmid.mid.breakdown
#' @exportS3Method ggplot2::autoplot
#'
autoplot.mid.breakdown <- function(object, ...) {
  ggmid.mid.breakdown(object = object, ...)
}
