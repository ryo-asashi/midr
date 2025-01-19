#' Plot MID Importance with Basic Functions
#'
#' For "mid.importance" objects, \code{plot()} visualizes the importance of MID component functions.
#'
#' The S3 method of \code{plot()} for "mid.importance" objects creates a visualization of the MID importance using \code{graphics::barplot()}, \code{graphics::image()} or \code{graphics::dotchart()}.
#'
#' @param x a "mid.importance" object to be visualized.
#' @param type a character string specifying the type of the plot. One of "barplot", "heatmap", "dotchart" or "boxplot".
#' @param max.bars an integer specifying the maximum number of bars in the barplot, boxplot and dotchart.
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
#' plot(imp, type = "dotchart")
#' @returns
#' \code{plot.mid.importance()} produces a plot and returns \code{NULL}.
#' @exportS3Method base::plot
#'
plot.mid.importance <- function(
    x, type = c("barplot", "heatmap", "dotchart", "boxplot"),
    max.bars = NA, scale.palette = c("#FFFFFF", "gray10"), ...) {
  type = match.arg(type)
  if (type == "dotchart") {
    imp <- x$importance
    imp <- imp[1L:min(max.bars, nrow(imp), na.rm = TRUE), ]
    graphics::dotchart(x = rev(imp$importance), labels = rev(imp$term),
                       xlab = "importance", ...)
  } else if (type == "barplot") {
    imp <- x$importance
    imp <- imp[1L:min(max.bars, nrow(imp), na.rm = TRUE), ]
    opar <- graphics::par("mai", "mar", "las")
    on.exit(graphics::par(opar))
    tmai <- max(graphics::strwidth(imp$term, "inch"), na.rm = TRUE)
    nmai <- opar[["mai"]]
    nm.2 <- nmai[4L] + tmai + 1/16
    if (nmai[2L] < nm.2) nmai[2L] <- nm.2
    graphics::par(mai = nmai, las = 1L)
    graphics::barplot.default(height = rev(imp$importance), xlab = "importance",
                              names.arg = rev(imp$term), horiz = TRUE, ...)
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
    spl <- scale.palette
    if (is.function(spl)) {
      spl <- spl(12L)
    } else {
      if (length(spl) < 2L)
        spl <- c("#FFFFFF", spl)
      spl <- grDevices::colorRampPalette(spl)(12L)
    }
    opar <- graphics::par("mai", "mar", "las")
    on.exit(graphics::par(opar))
    tmai <- max(graphics::strwidth(tags, "inch"), na.rm = TRUE)
    nmai <- opar[["mai"]]
    nm.1 <- nmai[3L] + tmai + 1/16
    nm.2 <- nmai[4L] + tmai + 1/16
    if (nmai[1L] < nm.1) nmai[1L] <- nm.1
    if (nmai[2L] < nm.2) nmai[2L] <- nm.2
    graphics::par(mai = nmai, las = 2L)
    graphics::image.default(x = seq_len(m), y = seq_len(m), z = mat,
                            axes = FALSE, col = spl,
                            xlab = "", ylab = "", ...)
    at <- seq_len(m + 2) - 1
    graphics::axis(side = 1L, at = at, labels = c("", tags, ""))
    graphics::axis(side = 2L, at = at, labels = c("", tags, ""))
    graphics::axis(3L, labels = FALSE, tick = TRUE, at = at, lwd.ticks = 0)
    graphics::axis(4L, labels = FALSE, tick = TRUE, at = at, lwd.ticks = 0)
  } else if (type == "boxplot") {
    terms <- as.character(attr(x, "terms"))
    terms <- terms[1L:min(max.bars, length(terms), na.rm = TRUE)]
    preds <- x$predictions[, terms]
    terms <- factor(terms, levels = rev(terms))
    box <- data.frame(mid = as.numeric(preds),
                      term = rep(terms, each = nrow(preds)))
    opar <- graphics::par("mai", "mar", "las")
    on.exit(graphics::par(opar))
    tmai <- max(graphics::strwidth(terms, "inch"), na.rm = TRUE)
    nmai <- opar[["mai"]]
    nm.2 <- nmai[4L] + tmai + 1/16
    if (nmai[2L] < nm.2) nmai[2L] <- nm.2
    graphics::par(mai = nmai, las = 1L)
    graphics::boxplot(mid ~ term, data = box,
                      xlab = "mid", ylab = NULL, horizontal = TRUE)
  }
  invisible(NULL)
}
