#' Plot MID Importance with graphics Package
#'
#' For "mid.importance" objects, \code{plot()} visualizes the importance of MID component functions.
#'
#' The S3 method of \code{plot()} for "mid.importance" objects creates a visualization of the MID importance using the functions of the graphics package.
#'
#' @param x a "mid.importance" object to be visualized.
#' @param type a character string specifying the type of the plot. One of "barplot", "heatmap", "dotchart" or "boxplot".
#' @param theme a character string specifying the color theme or any item that can be used to define "color.theme" object.
#' @param max.bars an integer specifying the maximum number of bars in the barplot, boxplot and dotchart.
#' @param ... optional parameters to be passed to the graphing function. Possible arguments are "col", "fill", "pch", "cex", "lty", "lwd" and aliases of them.
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' idx <- sample(nrow(diamonds), 1e4)
#' mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
#' imp <- mid.importance(mid)
#' plot(imp, theme = "Tableau 10")
#' plot(imp, type = "dotchart", theme = "Okabe-Ito")
#' plot(imp, type = "heatmap", theme = "Blues")
#' plot(imp, type = "boxplot", theme = "Accent")
#' @returns
#' \code{plot.mid.importance()} produces a plot and returns \code{NULL}.
#' @exportS3Method base::plot
#'
plot.mid.importance <- function(
    x, type = c("barplot", "dotchart", "heatmap", "boxplot"),
    theme = NULL, max.bars = 30L, ...) {
  dots <- list(...)
  type <- match.arg(type)
  theme <- color.theme(theme)
  use.theme <- inherits(theme, "color.theme")
  if (type == "dotchart" || type == "barplot") {
    imp <- x$importance
    imp <- imp[1L:min(max.bars, nrow(imp), na.rm = TRUE), ]
    cols <- if (use.theme) {
      if (theme$type == "qualitative")
        to.colors(imp$degree, theme)
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
    imp <- x$importance
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
    terms <- as.character(attr(x, "terms"))
    terms <- terms[1L:min(max.bars, length(terms), na.rm = TRUE)]
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
