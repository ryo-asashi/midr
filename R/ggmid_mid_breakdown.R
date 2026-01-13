#' Plot MID Breakdowns with ggplot2
#'
#' @description
#' For "mid.breakdown" objects, \code{ggmid()} visualizes the breakdown of a prediction by component functions.
#'
#' @details
#' This is an S3 method for the \code{ggmid()} generic that creates a breakdown plot from a "mid.breakdown" object, visualizing the contribution of each component function to a single prediction.
#'
#' The \code{type} argument controls the visualization style.
#' The default, \code{type = "waterfall"} (default), creates a waterfall plot that shows how the prediction is built up from the intercept, with each term's contribution sequentially added or subtracted.
#' The \code{type = "barplot"} option creates a standard bar plot where the length of each bar represents the magnitude of the term's contribution.
#' The \code{type = "dotchart"} option creates a dot plot showing the contribution of each term as a point connected to a zero baseline.
#'
#' @param object a "mid.breakdown" object to be visualized.
#' @param type the plotting style. One of "waterfall", "barplot" or "dotchart".
#' @param theme a character string or object defining the color theme. See \code{\link{color.theme}} for details.
#' @param terms an optional character vector specifying which terms to display.
#' @param max.nterms the maximum number of terms to display in the plot. Less important terms will be grouped into a "catchall" category.
#' @param width a numeric value specifying the width of the bars.
#' @param vline logical. If \code{TRUE}, a vertical line is drawn at the zero or intercept line.
#' @param catchall a character string for the catchall label.
#' @param label.format a character vector of length one or two specifying the format of the axis labels. The first element is used for main effects (default \code{"\%t = \%v"}), and the second is for interactions (default \code{"\%t:\%t"}). Use \code{"\%t"} for the term name and \code{"\%v"} for its value.
#' @param format.args a named list of additional arguments passed to \code{\link[base]{format}} for formatting the values. Common arguments include \code{digits}, \code{nsmall}, and \code{big.mark}.
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
    terms = NULL, max.nterms = 15L, width = NULL, vline = TRUE,
    catchall = "(others)", label.format = c("%t=%v", "%t:%t"),
    format.args = list(), ...) {
  dots <- list(...)
  type <- match.arg(type)
  if (missing(theme))
    theme <- getOption("midr.sequential", getOption("midr.qualitative", NULL))
  theme <- color.theme(theme)
  use.theme <- inherits(theme, "color.theme")
  bd <- object$breakdown
  bd$term <- as.character(bd$term)
  use.catchall <- FALSE
  if (!is.null(terms)) {
    rowid <- match(terms, bd$term, nomatch = 0L)
    resid <- bd[-rowid, "mid"]
    bd <- bd[rowid, ]
    if (length(resid) > 0L) {
      bd[nrow(bd) + 1L, "mid"] <- sum(resid)
      use.catchall <- TRUE
    }
  }
  nmax <- min(max.nterms, nrow(bd), na.rm = TRUE)
  if (nmax < nrow(bd)) {
    resid <- sum(bd[nmax:nrow(bd), "mid"])
    bd <- bd[1L:(nmax - 1L), ]
    bd[nmax, "mid"] <- resid
    use.catchall <- TRUE
  }
  # update labels
  format.args$x <- as.data.frame(object$data)
  values <- unlist(do.call(base::format, format.args))
  for (i in seq_len(nrow(bd) - as.numeric(use.catchall))) {
    term <- bd[i, "term"]
    tags <- term.split(term)
    vals <- values[tags]
    if (length(tags) == 1L) {
      label <- sub("%v", vals[1L], sub("%t", tags[1L], label.format[1L]))
    } else {
      if (length(label.format) == 1L)
        label.format <- c(label.format, "%t:%t")
      label <- sub("%v", vals[1L], sub("%t", tags[1L], label.format[2L]))
      label <- sub("%v", vals[2L], sub("%t", tags[2L], label))
    }
    bd[i, "term"] <- label
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
