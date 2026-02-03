#' Plot MID Breakdowns
#'
#' @description
#' For "mid.breakdown" objects, \code{plot()} visualizes the breakdown of a prediction by component functions.
#'
#' @details
#' This is an S3 method for the \code{plot()} generic that produces a breakdown plot from a "mid.breakdown" object, visualizing the contribution of each component function to a single prediction.
#'
#' The \code{type} argument controls the visualization style.
#' The default, \code{type = "waterfall"}, creates a waterfall plot that shows how the prediction builds from the intercept, with each term's contribution sequentially added or subtracted.
#' The \code{type = "barplot"} option creates a standard bar plot where the length of each bar represents the magnitude of the term's contribution.
#' The \code{type = "dotchart"} option creates a dot plot showing the contribution of each term as a point connected to a zero baseline.
#'
#' @param x a "mid.breakdown" object to be visualized.
#' @param type the plotting style. One of "waterfall", "barplot" or "dotchart".
#' @param theme a character string or object defining the color theme. See \code{\link{color.theme}} for details.
#' @param terms an optional character vector specifying which terms to display.
#' @param max.nterms the maximum number of terms to display in the plot. Less important terms will be grouped into a "catchall" category.
#' @param width a numeric value specifying the width of the bars.
#' @param vline logical. If \code{TRUE}, a vertical line is drawn at the zero or intercept line.
#' @param others a character string for the catchall label.
#' @param label.pattern a character vector of length one or two specifying the format of the axis labels. The first element is used for main effects (default \code{"\%t = \%v"}), and the second is for interactions (default \code{"\%t:\%t"}). Use \code{"\%t"} for the term name and \code{"\%v"} for its value.
#' @param format.args a named list of additional arguments passed to \code{\link[base]{format}} for formatting the values. Common arguments include \code{digits}, \code{nsmall}, and \code{big.mark}.
#' @param ... optional parameters passed on to the graphing function. Possible arguments are "col", "fill", "pch", "cex", "lty", "lwd" and aliases of them.
#'
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' idx <- sample(nrow(diamonds), 1e4)
#' mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
#' mbd <- mid.breakdown(mid, diamonds[1L, ])
#'
#' # Create a waterfall plot
#' plot(mbd, type = "waterfall")
#'
#' # Create a bar plot with a different theme
#' plot(mbd, type = "barplot", theme = "highlight")
#'
#' # Create a dot chart
#' plot(mbd, type = "dotchart", size = 1.5)
#' @returns
#' \code{plot.mid.breakdown()} produces a plot as a side effect and returns \code{NULL} invisibly.
#'
#' @seealso \code{\link{mid.breakdown}}, \code{\link{ggmid.mid.breakdown}}
#'
#' @exportS3Method base::plot
#'
plot.mid.breakdown <- function(
    x, type = c("waterfall", "barplot", "dotchart"), theme = NULL,
    terms = NULL, max.nterms = 15L, width = NULL, vline = TRUE,
    others = "others", label.pattern = c("%t=%v", "%t:%t"),
    format.args = list(), ...) {
  dots <- list(...)
  type <- match.arg(type)
  if (missing(theme))
    theme <- getOption("midr.sequential", getOption("midr.qualitative", NULL))
  theme <- color.theme(theme)
  use.theme <- inherits(theme, "color.theme")
  bd <- x$breakdown
  bd$term <- as.character(bd$term)
  use.others <- FALSE
  if (!is.null(terms)) {
    rowid <- match(terms, bd$term, nomatch = 0L)
    resid <- bd[-rowid, "mid"]
    bd <- bd[rowid, ]
    if (length(resid) > 0L) {
      bd[nrow(bd) + 1L, "mid"] <- sum(resid)
      use.others <- TRUE
    }
  }
  nmax <- min(max.nterms, nrow(bd), na.rm = TRUE)
  if (nmax < nrow(bd)) {
    resid <- sum(bd[nmax:nrow(bd), "mid"])
    bd <- bd[1L:(nmax - 1L), ]
    bd[nmax, "mid"] <- resid
    use.others <- TRUE
  }
  # update labels
  format.args$x <- as.data.frame(x$data)
  values <- unlist(do.call(base::format, format.args))
  for (i in seq_len(nrow(bd) - as.numeric(use.others))) {
    term <- bd[i, "term"]
    tags <- term.split(term)
    vals <- values[tags]
    if (length(tags) == 1L) {
      label <- sub("%v", vals[1L], sub("%t", tags[1L], label.pattern[1L]))
    } else {
      if (length(label.pattern) == 1L)
        label.pattern <- c(label.pattern, "%t:%t")
      label <- sub("%v", vals[1L], sub("%t", tags[1L], label.pattern[2L]))
      label <- sub("%v", vals[2L], sub("%t", tags[2L], label))
    }
    bd[i, "term"] <- label
  }
  if (use.others)
    bd[nrow(bd), "term"] <- others
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
      args$width <- width %||% .8
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
    width <- width %||% .6
    hw <- width / 2
    n <- nrow(bd)
    cs <- cumsum(c(x$intercept, bd$mid))
    bd$xmin <- cs[1L:n]
    bd$xmax <- cs[2L:(n + 1L)]
    args <- list(to = bd$xmax, from = bd$xmin, labels = bd$term, type = "b",
                 fill = cols, horizontal = TRUE, xlab = "yhat", width = width,
                 lty = 1L, lwd = 1L, col = NULL)
    args <- override(args, dots)
    do.call(barplot2, args)
    for (i in seq_len(n)) {
      graphics::lines.default(x = rep.int(bd[i, "xmax"], 2L),
                              y = c(n + 1 - i + hw, max(n - i - hw, 1 - hw)),
                              col = args$col %||% 1L,
                              lty = args$lty, lwd = args$lwd)
    }
    if (vline)
      graphics::abline(v = x$intercept, lty = 3L)
  }
  invisible(NULL)
}
