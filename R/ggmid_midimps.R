#' Compare MID Importance with ggplot2
#'
#' @description
#' For "midimps" collection objects, \code{ggmid()} visualizes and compares the importance of component functions across multiple fitted MID models.
#'
#' @details
#' This is an S3 method for the \code{ggmid()} generic that creates a comparative importance plot from a "midimps" collection object. It visualizes the average contribution of component functions to the fitted MID models, allowing for easy comparison across different models.
#'
#' The \code{type} argument controls the visualization style.
#' The default, \code{type = "barplot"}, creates a standard grouped bar plot where the length of each bar represents the overall importance of the term, positioned side-by-side (\code{position_dodge}) by model label.
#' The \code{type = "dotchart"} option creates a grouped dot plot, offering a clean alternative to the bar plot for visualizing and comparing term importance across models.
#'
#' @param object a "midimps" collection object to be visualized.
#' @param type the plotting style. One of "barplot" or "dotchart".
#' @param theme a character string or object defining the color theme. Defaults to "HCL". See \code{\link{color.theme}} for details.
#' @param terms an optional character vector specifying which terms to display. If \code{NULL}, terms are automatically extracted from the object.
#' @param max.nterms the maximum number of terms to display. Defaults to 30.
#' @param ... optional parameters passed on to the main layer (e.g., \code{\link[ggplot2]{geom_col}}).
#'
#' @examples
#' # Use a lightweight dataset for fast execution
#' data(mtcars, package = "datasets")
#'
#' # Fit two different models for comparison
#' mid1 <- interpret(mpg ~ wt + hp + cyl, data = mtcars)
#' mid2 <- interpret(mpg ~ (wt + hp + cyl)^2, data = mtcars)
#'
#' # Calculate importance for both models and combine them
#' imps <- midlist(
#'   "Main Effects" = mid.importance(mid1),
#'   "Interactions" = mid.importance(mid2)
#' )
#'
#' # Create a comparative grouped bar plot (default)
#' ggmid(imps)
#'
#' # Create a comparative dot chart with a specific theme
#' ggmid(imps, type = "dotchart", theme = "Okabe-Ito")
#' @returns
#' \code{ggmid.midimps()} returns a "ggplot" object.
#'
#' @seealso \code{\link{mid.importance}}, \code{\link{ggmid.midimp}}, \code{\link{midlist}}
#'
#' @exportS3Method midr::ggmid
#'
ggmid.midimps <- function(
    object, type = c("barplot", "dotchart"),
    theme = NULL, terms = NULL, max.nterms = 10L, ...
) {
  type <- match.arg(type)
  if (missing(theme))
    theme <- getOption("midr.qualitative", "HCL")
  theme <- color.theme(theme)
  imp <- summary(object, shape = "long")
  terms <- terms %||% unique(imp$term)
  terms <- utils::head(terms, max.nterms)
  labels <- unique(imp$label)
  grid <- expand.grid(term = terms, label = labels, stringsAsFactors = FALSE)
  imp <- merge(grid, imp, by = c("term", "label"), all.x = TRUE)
  imp$importance[is.na(imp$importance)] <- 0
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
    args <- list(...)
    dodge_width <- args$width %||% 0.6
    pl <- pl + geom_dotchart(
      mapping = ggplot2::aes(
        xmin = 0, xmax = .data[["importance"]], group = .data[["label"]]
      ),
      position = ggplot2::position_dodge(width = dodge_width), ...
    )
    pl <- pl + ggplot2::aes(color = .data[["label"]]) +
      scale_color_theme(theme, discrete = TRUE)
  }
  pl
}

#' @rdname ggmid.midimps
#'
#' @exportS3Method ggplot2::autoplot
#'
autoplot.midimps <- function(object, ...) {
  ggmid.midimps(object = object, ...)
}
