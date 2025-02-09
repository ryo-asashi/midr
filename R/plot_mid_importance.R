#' Plot MID Importance with Basic Functions
#'
#' For "mid.importance" objects, \code{plot()} visualizes the importance of MID component functions.
#'
#' The S3 method of \code{plot()} for "mid.importance" objects creates a visualization of the MID importance using \code{graphics::barplot()}, \code{graphics::image()} or \code{graphics::dotchart()}.
#'
#' @param x a "mid.importance" object to be visualized.
#' @param type a character string specifying the type of the plot. One of "barplot", "heatmap", "dotchart" or "boxplot".
#' @param theme a character string specifying the color theme or any item that can be used to define "color.theme" object.
#' @param max.bars an integer specifying the maximum number of bars in the barplot, boxplot and dotchart.
#' @param ... optional parameters to be passed to the graphing function.
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
  type = match.arg(type)
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
                 col = cols, horizontal = TRUE, xlab = "importance")
    args$type <- if (type == "dotchart") "d" else "b"
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
    cols <- theme$palette(12L)
    opar <- graphics::par("mai", "mar", "las")
    on.exit(graphics::par(opar))
    graphics::par(mai = adjusted.mai(tags), las = 1L)
    args <- list(x = seq_len(m), y = seq_len(m), z = mat,
                 axes = FALSE, col = cols, xlab = "", ylab = "")
    do.call(graphics::image.default, args)
    at <- seq_len(m + 2) - 1
    graphics::axis(1L, at = at, labels = c("", tags, ""))
    graphics::axis(2L, at = at, labels = c("", tags, ""))
    graphics::axis(3L, labels = FALSE, tick = TRUE, at = at, lwd.ticks = 0)
    graphics::axis(4L, labels = FALSE, tick = TRUE, at = at, lwd.ticks = 0)
  } else if (type == "boxplot") {
    terms <- as.character(attr(x, "terms"))
    terms <- terms[1L:min(max.bars, length(terms), na.rm = TRUE)]
    opar <- graphics::par("mai", "mar", "las")
    on.exit(graphics::par(opar))
    graphics::par(mai = adjusted.mai(terms), las = 1L)
    plist <- lapply(rev(terms), function(term) x$predictions[, term])
    names(plist) <- rev(terms)
    cols <- if (use.theme) theme$palette(length(terms)) else NULL
    graphics::boxplot(plist, col = cols,
                      xlab = "mid", ylab = NULL, horizontal = TRUE)
  }
  invisible(NULL)
}
