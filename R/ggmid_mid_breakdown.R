#' Plot MID Breakdowns with ggplot2
#'
#' @description
#' For "mid.breakdown" objects, \code{ggmid()} visualizes the breakdown of a prediction by component functions.
#'
#' @details
#' This is an S3 method for the \code{ggmid()} generic that creates a breakdown plot from a "mid.breakdown" object, visualizing the contribution of each component function to a single prediction.
#'
#' The \code{type} argument controls the visualization style.
#' The default, \code{type = "waterfall"} (default), creates a waterfall plot that shows how the prediction builds from the intercept, with each term's contribution sequentially added or subtracted.
#' The \code{type = "barplot"} option creates a standard bar plot where the length of each bar represents the magnitude of the term's contribution.
#' The \code{type = "dotchart"} option creates a dot plot showing the contribution of each term as a point connected to a zero baseline.
#'
#' @param object a "mid.breakdown" object to be visualized.
#' @param type a character string specifying the type of the plot. One of "waterfall", "barplot" or "dotchart".
#' @param theme a character string specifying the color theme or any item that can be used to define "color.theme" object.
#' @param terms an optional character vector specifying which terms to display.
#' @param max.terms the maximum number of terms to display in the plot. Less important terms will be grouped into a "catchall" category.
#' @param width a numeric value specifying the width of the bars.
#' @param vline logical. If \code{TRUE}, a vertical line is drawn at the zero or intercept line.
#' @param catchall a character string for the catchall label.
#' @param format a character string or character vector of length two to be used as the format of the axis labels. Use "\%t" for the term name (e.g., "Wind") and "\%v" for the values (e.g., "30").
#' @param ... optional parameters passed on to the main layer.
#'
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' idx <- sample(nrow(diamonds), 1e4)
#' mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
#' mbd <- mid.breakdown(mid, diamonds[1L, ])
#'
#' # Create a waterfall plot
#' ggmid(mbd, type = "waterfall")
#'
#' # Create a bar plot with a different theme
#' ggmid(mbd, type = "barplot", theme = "highlight")
#'
#' # Create a dot chart
#' ggmid(mbd, type = "dotchart", size = 3)
#' @returns
#' \code{ggmid.mid.breakdown()} returns a "ggplot" object.
#'
#' @seealso \code{\link{mid.breakdown}}, \code{\link{ggmid}}, \code{\link{plot.mid.breakdown}}
#'
#' @exportS3Method midr::ggmid
#'
ggmid.mid.breakdown <- function(
    object, type = c("waterfall", "barplot", "dotchart"), theme = NULL,
    terms = NULL, max.terms = 15L, width = NULL, vline = TRUE,
    catchall = "others", format = c("%t=%v", "%t"), ...) {
  dots <- list(...)
  type <- match.arg(type)
  if (missing(theme))
    theme <- getOption("midr.sequential", getOption("midr.qualitative", NULL))
  theme <- color.theme(theme)
  use.theme <- inherits(theme, "color.theme")
  bd <- object$breakdown
  bd$term <- as.character(bd$term)
  if (any(!grepl("%t", format) & !grepl("%v", format)))
    stop("all format strings must contain at least one of '%t' and '%v'")
  if (length(format) == 1L)
    format <- c(format, format)
  use.catchall <- FALSE
  if (!is.null(terms)) {
    rowid <- match(terms, bd$term, nomatch = 0L)
    resid <- sum(bd[-rowid, "mid"])
    bd <- bd[rowid, ]
    bd[nrow(bd) + 1L, "mid"] <- resid
    use.catchall <- TRUE
  }
  nmax <- min(max.terms, nrow(bd), na.rm = TRUE)
  if (nmax < nrow(bd)) {
    resid <- sum(bd[nmax:nrow(bd), "mid"])
    bd <- bd[1L:(nmax - 1L), ]
    bd[nmax, "mid"] <- resid
    use.catchall <- TRUE
  }
  for (i in seq_len(nrow(bd) - as.numeric(use.catchall))) {
    term <- bd[i, "term"]
    fmt <- if (grepl(":", term)) format[2L] else format[1L]
    bd[i, "term"] <-
      gsub("%v", bd[i, "value"], gsub("%t", bd[i, "term"], fmt))
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
        pl <- if (theme$type == "qualitative") {
          pl + ggplot2::aes(fill = .data[["order"]])
        } else {
          pl + ggplot2::aes(fill = .data[["mid"]])
        }
        pl <- pl + scale_fill_theme(theme = theme, na.value = "gray50")
      }
    } else if (type == "dotchart") {
      pl <- pl + ggplot2::geom_linerange(
        ggplot2::aes(xmin = 0, xmax = .data[["mid"]]), lty = 3L) +
        ggplot2::geom_point(...)
      if (use.theme) {
        pl <- if (theme$type == "qualitative") {
          pl + ggplot2::aes(color = .data[["order"]])
        } else {
          pl + ggplot2::aes(color = .data[["mid"]])
        }
        pl <- pl + scale_color_theme(theme = theme, na.value = "gray50")
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
      pl <- if (theme$type == "qualitative") {
        pl + ggplot2::aes(fill = ifelse(.data[["mid"]] > 0, "> 0", "< 0")) +
          ggplot2::labs(fill = "mid")
      } else {
        pl + ggplot2::aes(fill = .data[["mid"]])
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
