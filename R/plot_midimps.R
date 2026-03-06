#' Compare MID Importances
#'
#' @description
#' For "midimps" collection objects, \code{plot()} visualizes and compares the importance of component functions across multiple fitted MID models.
#'
#' @details
#' This is an S3 method for the \code{plot()} generic that creates a comparative importance plot from a "midimps" collection object. It visualizes the average contribution of component functions to the fitted MID models, allowing for easy comparison across different models.
#'
#' The \code{type} argument controls the visualization style:
#' The default, \code{type = "barplot"}, creates a standard grouped bar plot where the length of each bar represents the overall importance of the term, positioned side-by-side by model label.
#' The \code{type = "dotchart"} option creates a grouped dot plot, offering a clean alternative to the bar plot for visualizing and comparing term importance across models.
#' The \code{type = "series"} option plots the importance trend over the models for each component function.
#'
#' @param x a "midimps" collection object to be visualized.
#' @param type the plotting style. One of "barplot", "dotchart", or "series".
#' @param theme a character string or object defining the color theme. Defaults to "HCL". See \code{\link{color.theme}} for details.
#' @param terms an optional character vector specifying which terms to display. If \code{NULL}, terms are automatically extracted from the object.
#' @param max.nterms the maximum number of terms to display. Defaults to 30.
#' @param labels an optional numeric or character vector to specify the model labels. Defaults to the labels found in the object.
#' @param ... optional parameters passed on to the graphing functions. Possible arguments are "col", "fill", "pch", "cex", "lty", "lwd" and aliases of them.
#'
#' @examples
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
#' plot(imps)
#'
#' # Create a comparative dot chart with a specific theme
#' plot(rev(imps), type = "dotchart", theme = "Okabe-Ito")
#'
#' # Create a series plot to observe trends across models
#' plot(imps, type = "series")
#' @returns
#' \code{plot.midimps()} produces a plot as a side effect and returns \code{NULL} invisibly.
#'
#' @seealso \code{\link{plot.midimp}}, \code{\link{ggmid.midimps}}
#'
#' @exportS3Method base::plot
#'
plot.midimps <- function(
    x, type = c("barplot", "dotchart", "series"), theme = NULL,
    terms = NULL, max.nterms = 30L, labels = NULL, ...
) {
  dots <- override(list(), list(...))
  type <- match.arg(type)
  imp <- summary(x, shape = "long")
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
  imp <- imp[order(match(imp$term, terms), na.last = NA), , drop = FALSE]
  imp$term <- factor(imp$term, levels = rev(terms))
  n <- length(terms)
  m <- length(labels)
  mat <- matrix(NA_real_, nrow = n, ncol = m)
  for (i in seq_len(m)) {
    subimp <- imp[imp$label == labels[i], ]
    mat[, i] <- subimp$importance[match(terms, subimp$term)]
  }
  if (type == "barplot" || type == "dotchart") {
    cols <- to.colors(labels, theme)
    args <- list(to = mat, labels = terms,
                 horizontal = TRUE, xlab = "importance")
    if (type == "dotchart") {
      args$type <- "d"
      args$col <- cols
      alpha.on <- "col"
    } else if (type == "barplot") {
      args$type <- "b"
      args$fill <- cols
      alpha.on <- "fill"
    }
    args <- set.alpha(override(args, dots), on = alpha.on)
    do.call(.barplot, args)
  } else if (type == "series") {
    # For series plot, colors should match the number of terms (n), not models (m)
    cols <- theme$palette(n)
    if (discrete) {
      x_pos <- seq_along(labels)
      args <- list(x = x_pos, y = t(mat), type = "b", pch = 16L, col = cols,
                   lty = 1L, xaxt = "n", xlab = "", ylab = "importance")
      args <- set.alpha(override(args, dots), on = "col")
      do.call(graphics::matplot, args)
      graphics::axis(side = 1L, at = x_pos, labels = as.character(labels))
    } else {
      args <- list(x = labels, y = t(mat), type = "l", col = cols,
                   lty = 1L, xlab = "", ylab = "importance")
      args <- set.alpha(override(args, dots), on = "col")
      do.call(graphics::matplot, args)
    }
  }
  invisible(NULL)
}
