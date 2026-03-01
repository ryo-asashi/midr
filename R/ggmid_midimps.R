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
#' @param type the plotting style. One of "barplot", "dotchart", or "series".
#' @param theme a character string or object defining the color theme. Defaults to "HCL". See \code{\link{color.theme}} for details.
#' @param terms an optional character vector specifying which terms to display. If \code{NULL}, terms are automatically extracted from the object.
#' @param max.nterms the maximum number of terms to display. Defaults to 15.
#' @param labels an optional numeric or character vector to specify the model labels or x-axis coordinates. Defaults to the labels found in the object.
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
    object, type = c("barplot", "dotchart", "series"), theme = NULL,
    terms = NULL, max.nterms = 15L, labels = NULL, ...
) {
  type <- match.arg(type)
  imp <- summary(object, shape = "long")
  olabs <- unique(imp$label)
  labels <- labels %||% olabs
  if (length(labels) != length(olabs)) {
    stop("length of 'labels' must match the number of models in the collection")
  }
  imp$label <- labels[match(imp$label, olabs)]
  parsed <- suppressWarnings(as.numeric(labels))
  discrete <- anyNA(parsed)
  if (!discrete) {
    labels <- parsed
    imp$label <- as.numeric(imp$label)
  } else if (!is.factor(labels)) {
    labels <- factor(labels, levels = unique(labels))
    imp$label <- factor(imp$label, levels = levels(labels))
  }
  theme <- theme %||% (
    if (type == "series" || discrete) getOption("midr.qualitative", "HCL")
    else getOption("midr.sequential", "bluescale")
  )
  theme <- color.theme(theme)
  terms <- terms %||% unique(imp$term)
  terms <- utils::head(terms, max.nterms)
  grid <- expand.grid(term = terms, label = labels, stringsAsFactors = FALSE)
  imp <- merge(grid, imp, by = c("term", "label"), all.x = TRUE)
  imp$importance[is.na(imp$importance)] <- 0
  if (type == "series") {
    imp$term <- factor(imp$term, levels = terms)
    pl <- ggplot2::ggplot(
      imp, ggplot2::aes(x = .data[["label"]], y = .data[["importance"]],
                        color = .data[["term"]], group = .data[["term"]])
    )
    pl <- pl + if (discrete) geom_dotline(...) else ggplot2::geom_line(...)
    pl <- pl + scale_color_theme(theme, discrete = TRUE)
  } else if (type == "barplot") {
    imp$term <- factor(imp$term, levels = rev(terms))
    pl <- ggplot2::ggplot(
      imp, ggplot2::aes(y = .data[["term"]], x = .data[["importance"]])
    ) + ggplot2::geom_col(
      ggplot2::aes(fill = .data[["label"]], group = factor(.data[["label"]])),
      position = ggplot2::position_dodge(), ...
    ) + scale_fill_theme(theme, discrete = discrete)
  } else if (type == "dotchart") {
    args <- list(...)
    dodge_width <- args$width %||% 0.6
    imp$term <- factor(imp$term, levels = rev(terms))
    pl <- ggplot2::ggplot(
      imp, ggplot2::aes(y = .data[["term"]], x = .data[["importance"]])
    ) + geom_dotchart(
      ggplot2::aes(xmin = 0, xmax = .data[["importance"]],
                   color = .data[["label"]], group = factor(.data[["label"]])),
      position = ggplot2::position_dodge(width = dodge_width), ...
    ) + scale_color_theme(theme, discrete = discrete)
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
