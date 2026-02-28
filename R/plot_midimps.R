#' Compare MID Importance
#'
#' @description
#' For "midimps" collection objects, \code{plot()} visualizes and compares the importance of component functions across multiple fitted MID models.
#'
#' @details
#' This is an S3 method for the \code{plot()} generic that creates a comparative importance plot from a "midimps" collection object. It visualizes the average contribution of component functions to the fitted MID models, allowing for easy comparison across different models.
#'
#' The \code{type} argument controls the visualization style.
#' The default, \code{type = "barplot"}, creates a standard grouped bar plot where the length of each bar represents the overall importance of the term, positioned side-by-side by model label.
#' The \code{type = "dotchart"} option creates a grouped dot plot, offering a clean alternative to the bar plot for visualizing and comparing term importance across models.
#'
#' @param x a "midimps" collection object to be visualized.
#' @param type the plotting style. One of "barplot" or "dotchart".
#' @param theme a character string or object defining the color theme. Defaults to "HCL". See \code{\link{color.theme}} for details.
#' @param terms an optional character vector specifying which terms to display. If \code{NULL}, terms are automatically extracted from the object.
#' @param max.nterms the maximum number of terms to display. Defaults to 30.
#' @param ... optional parameters passed on to the graphing functions. Possible arguments are "col", "fill", "pch", "cex", "lty", "lwd" and aliases of them.
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
#' plot(imps)
#'
#' # Create a comparative dot chart with a specific theme
#' plot(imps, type = "dotchart", theme = "Okabe-Ito")
#' @returns
#' \code{plot.midimps()} produces a plot as a side effect and returns \code{NULL} invisibly.
#'
#' @seealso \code{\link{plot.midimp}}, \code{\link{ggmid.midimps}}, \code{\link{midlist}}
#'
#' @exportS3Method base::plot
#'
plot.midimps <- function(
    x, type = c("barplot", "dotchart"),
    theme = NULL, terms = NULL, max.nterms = 30L, ...
) {
  dots <- list(...)
  type <- match.arg(type)
  if (missing(theme))
    theme <- getOption("midr.qualitative", "HCL")
  theme <- color.theme(theme)
  imp <- summary(x, shape = "long")
  labs <- unique(imp$label)
  terms <- terms %||% unique(imp$term)
  terms <- utils::head(terms, max.nterms)
  imp <- imp[order(match(imp$term, terms), na.last = NA), , drop = FALSE]
  imp$term <- factor(imp$term, levels = rev(terms))
  n <- length(terms)
  m <- length(labs)
  mat <- matrix(NA_real_, nrow = n, ncol = m)
  for (i in seq_len(m)) {
    subimp <- imp[imp$label == labs[i], ]
    mat[, i] <- subimp$importance[match(terms, subimp$term)]
  }
  cols <- theme$palette(m)
  args <- list(to = mat, labs = terms,
               horizontal = TRUE, xlab = "importance")
  if (type == "dotchart") {
    args$type <- "d"
    args$col <- cols
  } else if (type == "barplot") {
    args$type <- "b"
    args$fill <- cols
  }
  args <- override(args, dots)
  do.call(barplot2, args)
  invisible(NULL)
}
