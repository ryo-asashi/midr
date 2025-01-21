#' Plot MID Breakdown with Basic Functions
#'
#' For "mid.breakdown" objects, \code{plot()} visualizes the breakdown of a prediction by component functions.
#'
#' The S3 method of \code{plot()} for "mid.breakdown" objects creates a visualization of the MID breakdown using \code{graphics::barplot()} or \code{graphics::dotchart()}.
#'
#' @param x a "mid.breakdown" object to be visualized.
#' @param type a character string specifying the type of the plot. One of "barplot" or "dotchart".
#' @param max.bars an integer specifying the maximum number of bars in the barplot, boxplot and dotchart.
#' @param vline logical. If \code{TRUE}, the vertical line is drawn at zero or the intercept.
#' @param sep a character string to separate terms and values. Default is the return.
#' @param terms an optional character vector specifying the terms to be displayed.
#' @param catchall a character string to be used as the catchall label.
#' @param ... optional parameters to be passed to the graphing function.
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' idx <- sample(nrow(diamonds), 1e4)
#' mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
#' mbd <- mid.breakdown(mid, diamonds[1L, ])
#' plot(mbd)
#' plot(mbd, type = "dotchart")
#' @returns
#' \code{plot.mid.breakdown()} produces a plot and returns \code{NULL}.
#' @exportS3Method base::plot
#'
plot.mid.breakdown <- function(
    x, type = c("barplot", "heatmap", "dotchart", "boxplot"),
    max.bars = 15L, vline = TRUE, sep = " = ",
    terms = NULL, catchall = "others", ...) {
  type <- match.arg(type)
  bd <- x$breakdown
  use.catchall <- FALSE
  if (!is.null(terms)) {
    rowid <- match(terms, bd$term, nomatch = 0L)
    resid <- sum(bd[-rowid, "mid"])
    bd <- bd[rowid, ]
    bd[nrow(bd) + 1L, "mid"] <- resid
    use.catchall <- TRUE
  }
  nmax <- min(max.bars, nrow(bd), na.rm = TRUE)
  if (nmax < nrow(bd)) {
    resid <- sum(bd[nmax:nrow(bd), "mid"])
    bd <- bd[1L:(nmax - 1L), ]
    bd[nmax, "mid"] <- resid
    use.catchall <- TRUE
  }
  bd$term <- paste0(bd$term, sep, bd$value)
  if (use.catchall) bd[nrow(bd), "term"] <- catchall
  if (type == "dotchart") {
    graphics::dotchart(x = rev(bd$mid), labels = rev(bd$term),
                       xlab = "mid", ...)
  } else if (type == "barplot") {
    opar <- graphics::par("mai", "mar", "las", "yaxs")
    on.exit(graphics::par(opar))
    tmai <- max(graphics::strwidth(bd$term, "inch"), na.rm = TRUE)
    nmai <- opar[["mai"]]
    nm.2 <- nmai[4L] + tmai + 1/16
    if (nmai[2L] < nm.2) nmai[2L] <- nm.2
    graphics::par(mai = nmai, las = 1L)
    graphics::barplot.default(height = rev(bd$mid), xlab = "mid",
                              names.arg = rev(bd$term), horiz = TRUE, ...)
    graphics::box()
  }
  if (vline)
    graphics::abline(v = 0)
  invisible(NULL)
}
