#' Compare MID Importance in a Collection
#'
#' @description
#' For "midlist.importance" objects, \code{plot()} visualizes and compares the importance of component functions across multiple fitted MID models.
#'
#' @details
#' This is an S3 method for the \code{plot()} generic that creates a comparative importance plot from a "midlist.importance" object. It visualizes the average contribution of component functions to the fitted MID models, allowing for easy comparison across different models.
#'
#' The \code{type} argument controls the visualization style.
#' The default, \code{type = "barplot"}, creates a standard grouped bar plot where the length of each bar represents the overall importance of the term, positioned side-by-side by model label.
#' The \code{type = "dotchart"} option creates a grouped dot plot, offering a clean alternative to the bar plot for visualizing and comparing term importance across models.
#'
#' @param x a "midlist.importance" object to be visualized.
#' @param type the plotting style. One of "barplot" or "dotchart".
#' @param theme a character string or object defining the color theme. Defaults to "HCL". See \code{\link{color.theme}} for details.
#' @param terms an optional character vector specifying which terms to display. If \code{NULL}, terms are extracted from the object.
#' @param max.nterms the maximum number of terms to display. Defaults to 30.
#' @param ... optional parameters passed on to the graphing functions. Possible arguments are "col", "fill", "pch", "cex", "lty", "lwd" and aliases of them.
#'
#' @returns
#' \code{plot.midlist.importance()} produces a plot as a side effect and returns \code{NULL} invisibly.
#'
#' @seealso \code{\link{plot.mid.importance}}, \code{\link{ggmid.midlist.importance}}
#'
#' @exportS3Method base::plot
#'
plot.midlist.importance <- function(
    x, type = c("barplot", "dotchart"),
    theme = NULL, terms = NULL, max.nterms = 30L, ...
) {
  dots <- list(...)
  type <- match.arg(type)
  if (missing(theme))
    theme <- getOption("midr.qualitative", "HCL")
  theme <- color.theme(theme)
  imp <- summary.midlist.importance(x, shape = "long")
  terms <- terms %||% unique(imp$term)
  terms <- terms[1L:min(max.nterms, length(terms), na.rm = TRUE)]
  imp$term <- factor(imp$term, levels = rev(terms))
  labels <- unique(imp$label)
  n <- length(terms)
  m <- length(labels)
  mat <- matrix(NA_real_, nrow = n, ncol = m)
  for (i in seq_len(m)) {
    subimp <- imp[imp$label == labels[i], ]
    mat[, i] <- subimp$importance[match(terms, subimp$term)]
  }
  cols <- theme$palette(m)
  args <- list(to = mat, labels = terms,
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
