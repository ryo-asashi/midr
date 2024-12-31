#' Plot MID Importance with Basic Functions
#'
#' For "mid.importance" objects, \code{plot()} visualizes the importance of MID component functions.
#'
#' The S3 method of \code{plot()} for "mid.importance" objects creates a visualization of the MID importance using \code{graphics::barplot()} or \code{graphics::image()}.
#'
#' @param x a "mid.importance" object to be visualized.
#' @param type a character string specifying the type of the plot. One of "barplot" or "heatmap".
#' @param max.bars an integer specifying the maximum number of bars in the barplot.
#' @param scale.palette a character vector of length two to be used as the color palette for the heatmap.
#' @param ... optional parameters to be passed to the graphing function.
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' idx <- sample(nrow(diamonds), 1e4)
#' mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
#' imp <- mid.importance(mid)
#' plot(imp)
#' plot(imp, type = "heatmap")
#' @returns
#' \code{plot.mid.importance()} produces a barplot or heatmap and returns \code{NULL}.
#' @exportS3Method base::plot
#'
plot.mid.importance <- function(
    x, type = c("barplot", "heatmap"),
    max.bars = NA, scale.palette = c("#FFFFFF", "#464646"), ...) {
  type = match.arg(type)
  if (type == "barplot") {
    x <- x[1L:min(max.bars, nrow(x), na.rm = TRUE), ]
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
  invisible(NULL)
}
