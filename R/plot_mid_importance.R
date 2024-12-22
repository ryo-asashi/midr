#' Plot Term Importance with graphics Package
#'
#' Creates a plot showing the MID-based term importance
#'
#' @param x a mid.importance object to plot.
#' @param type a character or an integer, specifying the type of the plot. Possible alternatives are "barplot" and "heatmap".
#' @param max.terms an integer, specifying the maximum number of component terms to be plotted in the barplot.
#' @param scale.palette color palette used to draw the interaction heatmap.
#' @param ... optional arguments to be passed to graphic functions.
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' mid <- interpret(price ~ (carat + cut + color + clarity)^2,
#'                  diamonds[sample(nrow(diamonds), 1e4), ])
#' imp <- mid.importance(mid)
#' ggmid(imp)
#' ggmid(imp, type = "heatmap")
#' @returns
#' \code{plot.mid.importance()} produces a bar plot or a heat map for the term importance.
#' @exportS3Method base::plot
#'
plot.mid.importance <- function(
    x, type = c("barplot", "heatmap"),
    max.terms = NA, scale.palette = c("#FFFFFF", "#464646"), ...) {
  type = match.arg(type)
  if (type == "barplot") {
    x <- x[1L:min(max.terms, nrow(x), na.rm = TRUE), ]
    height <- x$importance
    names(height) <- x$term
    graphics::barplot.default(height, ...)
  } else if (type == "heatmap") {
    rownames(x) <- terms <- as.character(x$term)
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
        mat[i, j] <- x[term, "importance"]
      }
    }
    spl <- scale.palette
    if (is.function(spl)) {
      spl <- spl(12L)
    } else {
      if (length(spl) < 2L)
        spl <- c("#FFFFFF", spl)
      spl <- grDevices::colorRampPalette(spl)(12L)
    }
    graphics::image.default(x = seq_len(m), y = seq_len(m), z = mat,
                            axes = FALSE, col = spl, xlab = "", ylab = "", ...)
    graphics::axis(side = 1L, at = seq_len(m), labels = tags)
    graphics::axis(side = 2L, at = seq_len(m), labels = tags)
  }
}
