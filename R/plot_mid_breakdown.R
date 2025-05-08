#' Plot MID Breakdown with graphics Package
#'
#' For "mid.breakdown" objects, \code{plot()} visualizes the breakdown of a prediction by component functions.
#'
#' The S3 method of \code{plot()} for "mid.breakdown" objects creates a visualization of the MID breakdown using the functions of the graphics package.
#'
#' @param x a "mid.breakdown" object to be visualized.
#' @param type a character string specifying the type of the plot. One of "barplot" or "dotchart".
#' @param theme a character string specifying the color theme or any item that can be used to define "color.theme" object.
#' @param terms an optional character vector specifying the terms to be displayed.
#' @param max.bars an integer specifying the maximum number of bars in the barplot, boxplot and dotchart.
#' @param width a numeric value specifying the width of the bars.
#' @param vline logical. If \code{TRUE}, the vertical line is drawn at zero or the intercept.
#' @param catchall a character string to be used as the catchall label.
#' @param format a character string or character vector of length two to be used as the format of the axis labels. "t" and "v" immediately after the percent sign are replaced with the corresponding term and value.
#' @param ... optional parameters to be passed to the graphing function. Possible arguments are "col", "fill", "pch", "cex", "lty", "lwd" and aliases of them.
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' idx <- sample(nrow(diamonds), 1e4)
#' mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
#' mbd <- mid.breakdown(mid, diamonds[1L, ])
#' plot(mbd, type = "waterfall")
#' plot(mbd, type = "waterfall", theme = "midr")
#' plot(mbd, type = "barplot", theme = "Set 1")
#' plot(mbd, type = "dotchart", theme = "Cividis")
#' @returns
#' \code{plot.mid.breakdown()} produces a plot and returns \code{NULL}.
#' @exportS3Method base::plot
#'
plot.mid.breakdown <- function(
    x, type = c("waterfall", "barplot", "dotchart"), theme = NULL,
    terms = NULL, max.bars = 15L, width = NULL, vline = TRUE,
    catchall = "others", format = c("%t=%v", "%t"), ...) {
  dots <- list(...)
  type <- match.arg(type)
  theme <- color.theme(theme)
  use.theme <- inherits(theme, "color.theme")
  bd <- x$breakdown
  bd$term <- as.character(bd$term)
  if (any(!grepl("%t", format) & !grepl("%v", format)))
    stop("all format strings must contain '%t' or '%v'")
  if (length(format) == 1L)
    format <- c(format, format)
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
  for (i in seq_len(nrow(bd) - as.numeric(use.catchall))) {
    term <- bd[i, "term"]
    fmt <- if (grepl(":", term)) format[2L] else format[1L]
    bd[i, "term"] <-
      gsub("%v", bd[i, "value"], gsub("%t", bd[i, "term"], fmt))
  }
  if (use.catchall)
    bd[nrow(bd), "term"] <- catchall
  if (type == "barplot" || type == "dotchart") {
    args <- list(to = bd$mid, labels = bd$term,
                 horizontal = TRUE, xlab = "mid")
    cols <- if (use.theme) {
      if (theme$type == "qualitative")
        to.colors(bd$order, theme)
      else
        to.colors(bd$mid, theme)
    } else "gray35"
    if (type == "dotchart") {
      args$type <- "d"
      args$col <- cols
    } else {
      args$type <- "b"
      args$fill <- cols
      args$width <- ifnot.null(width, .8)
    }
    args <- override(args, dots)
    do.call(barplot2, args)
    if (vline)
      graphics::abline(v = 0)
  } else if (type == "waterfall") {
    cols <- if (use.theme) {
      if (theme$type == "qualitative")
        to.colors(bd$mid > 0, theme)
      else
        to.colors(bd$mid, theme)
    } else "gray35"
    width <- ifnot.null(width, .6)
    hw <- width / 2
    n <- nrow(bd)
    cs <- cumsum(c(x$intercept, bd$mid))
    bd$xmin <- cs[1L:n]
    bd$xmax <- cs[2L:(n + 1L)]
    args <- list(to = bd$xmax, from = bd$xmin, labels = bd$term, type = "b",
                 fill = cols, horizontal = TRUE, xlab = "mid", width = width,
                 lty = 1L, lwd = 1L, col = NULL)
    args <- override(args, dots)
    do.call(barplot2, args)
    for (i in seq_len(n)) {
      graphics::lines.default(x = rep.int(bd[i, "xmax"], 2L),
                              y = c(n + 1 - i + hw, max(n - i - hw, 1 - hw)),
                              col = ifnot.null(args$col, 1L),
                              lty = args$lty, lwd = args$lwd)
    }
    if (vline)
      graphics::abline(v = x$intercept, lty = 3L)
  }
  invisible(NULL)
}
