#' Compare MID Importance with ggplot2
#'
#' @description
#' For "midlist.importance" objects, \code{ggmid()} visualizes and compares the importance of component functions across multiple fitted MID models.
#'
#' @details
#' This is an S3 method for the \code{ggmid()} generic that creates a comparative importance plot from a "midlist.importance" object. It visualizes the average contribution of component functions to the fitted MID models, allowing for easy comparison across different models.
#'
#' The \code{type} argument controls the visualization style.
#' The default, \code{type = "barplot"}, creates a standard grouped bar plot where the length of each bar represents the overall importance of the term, positioned side-by-side (\code{position_dodge}) by model label.
#' The \code{type = "dotchart"} option creates a grouped dot plot, offering a clean alternative to the bar plot for visualizing and comparing term importance across models.
#'
#' @param object a "midlist.importance" object to be visualized.
#' @param type the plotting style. One of "barplot" or "dotchart".
#' @param theme a character string or object defining the color theme. Defaults to "HCL". See \code{\link{color.theme}} for details.
#' @param terms an optional character vector specifying which terms to display. If \code{NULL}, terms are extracted from the object.
#' @param max.nterms the maximum number of terms to display. Defaults to 30.
#' @param ... optional parameters passed on to the main layer (e.g., \code{\link[ggplot2]{geom_col}}).
#'
#' @returns
#' \code{ggmid.midlist.importance()} returns a "ggplot" object.
#'
#' @seealso \code{\link{ggmid.mid.importance}}, \code{\link{ggmid}}
#'
#' @exportS3Method midr::ggmid
#'
ggmid.midlist.importance <- function(
    object, type = c("barplot", "dotchart"),
    theme = NULL, terms = NULL, max.nterms = 30L, ...
) {
  type <- match.arg(type)
  if (missing(theme))
    theme <- getOption("midr.qualitative", "HCL")
  theme <- color.theme(theme)
  imp <- summary.midlist.importance(object, shape = "long")
  terms <- terms %||% unique(imp$term)
  terms <- terms[1L:min(max.nterms, length(terms), na.rm = TRUE)]
  imp <- imp[order(match(imp$term, terms), na.last = NA), , drop = FALSE]
  imp$term <- factor(imp$term, levels = rev(terms))
  imp$label <- factor(imp$label, levels = unique(imp$label))
  pl <- ggplot2::ggplot(
    imp, ggplot2::aes(y = .data[["term"]], x = .data[["importance"]])
  )
  if (type == "barplot") {
    pl <- pl + ggplot2::geom_col(
      mapping = ggplot2::aes(group = .data[["label"]]),
      position = ggplot2::position_dodge(), ...
    )
    pl <- pl + ggplot2::aes(fill = .data[["label"]]) +
      scale_fill_theme(theme, discrete = TRUE)
  } else if (type == "dotchart") {
    pl <- pl + geom_dotchart(
      mapping = ggplot2::aes(
        xmin = 0, xmax = .data[["importance"]], group = .data[["label"]]
      ),
      position = ggplot2::position_dodge(width = list(...)$width %||% .6), ...
    )
    pl <- pl + ggplot2::aes(color = .data[["label"]]) +
      scale_color_theme(theme, discrete = TRUE)
  }
  pl
}


#' @rdname ggmid.midlist.importance
#'
#' @exportS3Method ggplot2::autoplot
#'
autoplot.midlist.importance <- function(object, ...) {
  ggmid.midlist.importance(object = object, ...)
}
