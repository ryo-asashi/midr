#' Plot MID Importance
#'
#' @description
#' For "mid.importance" objects, \code{plot()} visualizes the importance of component functions of the fitted MID model.
#'
#' @details
#' This is an S3 method for the \code{plot()} generic that produces an importance plot from a "mid.importance" object, visualizing the average contribution of component functions to the fitted MID model.
#'
#' The \code{type} argument controls the visualization style.
#' The default, \code{type = "barplot"}, creates a standard bar plot where the length of each bar represents the overall importance of the term.
#' The \code{type = "dotchart"} option creates a dot plot, offering a clean alternative to the bar plot for visualizing term importance.
#' The \code{type = "heatmap"} option creates a matrix-shaped heat map where the color of each cell represents the importance of the interaction between a pair of variables, or the main effect on the diagonal.
#' The \code{type = "boxplot"} option creates a box plot where each box shows the distribution of a term's contributions across all observations, providing insight into the variability of each term's effect.
#'
#' @param x a "mid.importance" object to be visualized.
#' @param type the plotting style. One of "barplot", "dotchart", "heatmap", or "boxplot".
#' @param theme a character string or object defining the color theme. See \code{\link{color.theme}} for details.
#' @param terms an optional character vector specifying which terms to display.
#' @param max.nterms the maximum number of terms to display in the bar, dot and box plots.
#' @param ... optional parameters passed on to the graphing functions. Possible arguments are "col", "fill", "pch", "cex", "lty", "lwd" and aliases of them.
#'
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' idx <- sample(nrow(diamonds), 1e4)
#' mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
#' imp <- mid.importance(mid)
#'
#' # Create a bar plot (default)
#' plot(imp)
#'
#' # Create a dot chart
#' plot(imp, type = "dotchart", theme = "Okabe-Ito", size = 1.5)
#'
#' # Create a heatmap
#' plot(imp, type = "heatmap")
#'
#' # Create a boxplot to see the distribution of effects
#' plot(imp, type = "boxplot")
#' @returns
#' \code{plot.mid.importance()} produces a plot as a side effect and returns \code{NULL} invisibly.
#'
#' @seealso \code{\link{mid.importance}}, \code{\link{ggmid.mid.importance}}
#'
#' @exportS3Method base::plot
#'
plot.mid.importance <- function(
    x, type = c("barplot", "dotchart", "heatmap", "boxplot"),
    theme = NULL, terms = NULL, max.nterms = 30L, ...) {
  dots <- list(...)
  type <- match.arg(type)
  if (missing(theme))
    theme <- getOption("midr.sequential", getOption("midr.qualitative", NULL))
  theme <- color.theme(theme)
  use.theme <- inherits(theme, "color.theme")
  imp <- x$importance
  if (!is.null(terms))
    imp <- imp[match(terms, imp$term, nomatch = 0L), ]
  if (type == "dotchart" || type == "barplot") {
    imp <- imp[1L:min(max.nterms, nrow(imp), na.rm = TRUE), ]
    cols <- if (use.theme) {
      if (theme$type == "qualitative")
        to.colors(imp$order, theme)
      else
        to.colors(imp$importance, theme)
    } else "gray35"
    args <- list(to = imp$importance, labels = as.character(imp$term),
                 horizontal = TRUE, xlab = "importance")
    if (type == "dotchart") {
      args$type <- "d"
      args$col <- cols
    } else {
      args$type <- "b"
      args$fill <- cols
    }
    args <- override(args, dots)
    do.call(barplot2, args)
  } else if (type == "heatmap") {
    rownames(imp) <- terms <- as.character(imp$term)
    tags <- unique(term.split(terms))
    m <- length(tags)
    mat <- matrix(NA, m, m)
    for (i in seq_len(m)) {
      for (j in seq_len(m)) {
        if (tags[i] == tags[j]) {
          term <- tags[i]
        } else {
          term <- paste0(tags[i], ":", tags[j])
          if (!term %in% terms)
            term <- paste0(tags[j], ":", tags[i])
        }
        mat[i, j] <- imp[term, "importance"]
      }
    }
    if (!use.theme)
      theme <- color.theme("grayscale")
    cols <- to.colors(0:11, theme)
    opar <- graphics::par("mai", "mar", "las")
    on.exit(graphics::par(opar))
    graphics::par(mai = adjusted.mai(tags), las = 1L)
    args <- list(x = seq_len(m), y = seq_len(m), z = mat, fill = cols, col = NA,
                 axes = FALSE, xlab = "", ylab = "", lty = 1L, lwd = 1L)
    args <- override(args, dots)
    lcol <- args$col[1L]
    args$col <- NULL
    names(args)[4L] <- "col"
    do.call(graphics::image.default, args)
    at <- seq_len(m + 2) - 1
    graphics::axis(1L, at = at, labels = c("", tags, ""))
    graphics::axis(2L, at = at, labels = c("", tags, ""))
    graphics::axis(3L, labels = FALSE, tick = TRUE, at = at, lwd.ticks = 0)
    graphics::axis(4L, labels = FALSE, tick = TRUE, at = at, lwd.ticks = 0)
    if (!is.na(lcol)) {
      graphics::abline(v = 1:m - 1/2, h = 1:m - 1/2, col = lcol,
                       lty = args$lty, lwd = args$lwd)
    }
  } else if (type == "boxplot") {
    terms <- mid.terms(x)
    terms <- terms[1L:min(max.nterms, length(terms), na.rm = TRUE)]
    opar <- graphics::par("mai", "mar", "las")
    on.exit(graphics::par(opar))
    graphics::par(mai = adjusted.mai(terms), las = 1L)
    plist <- lapply(rev(terms), function(term) x$predictions[, term])
    names(plist) <- rev(terms)
    cols <- if (use.theme) theme$palette(length(terms)) else NA
    args <- list(x = plist, fill = cols, col = "black", pch = 16L,
                 xlab = "mid", ylab = NULL, horizontal = TRUE)
    args <- override(args, dots)
    names(args)[2L:3L] <- c("col", "border")
    do.call(graphics::boxplot.default, args)
  }
  invisible(NULL)
}
