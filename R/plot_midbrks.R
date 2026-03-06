#' Compare MID Breakdowns
#'
#' @description
#' For "midbrks" collection objects, \code{plot()} visualizes and compares the breakdown of a prediction by component functions across multiple models using base R graphics.
#'
#' @details
#' This is an S3 method for the \code{plot()} generic that evaluates the component contributions to a single prediction and compares the results across all models in the collection.
#'
#' The \code{type} argument controls the visualization style:
#' The default, \code{type = "barplot"}, creates a grouped bar plot where the bars for each term are placed side-by-side across the models.
#' The \code{type = "dotchart"} option creates a grouped dot plot, offering a cleaner comparison across models.
#' The \code{type = "series"} option plots the contribution trend over the models for each component term.
#'
#' @param x a "midbrks" collection object to be visualized.
#' @param type the plotting style. One of "barplot", "dotchart", or "series".
#' @param theme a character string or object defining the color theme. Defaults to "HCL". See \code{\link{color.theme}} for details.
#' @param terms an optional character vector specifying which terms to display. If \code{NULL}, terms are automatically extracted from the object.
#' @param max.nterms the maximum number of terms to display. Defaults to 15.
#' @param vline logical. If \code{TRUE}, a vertical line is drawn at the zero or intercept line.
#' @param others a character string for the catchall label. Defaults to \code{"others"}.
#' @param pattern a character vector of length one or two specifying the format of the axis labels. The first element is used for main effects (default \code{"\%t = \%v"}), and the second is for interactions (default \code{"\%t:\%t"}). Use \code{"\%t"} for the term name and \code{"\%v"} for its value.
#' @param format.args a named list of additional arguments passed to \code{\link[base]{format}} for formatting the values. Common arguments include \code{digits}, \code{nsmall}, and \code{big.mark}.
#' @param labels an optional numeric or character vector to specify the model labels. Defaults to the labels found in the object.
#' @param ... optional parameters passed on to the main layer (e.g., \code{\link[ggplot2]{geom_col}}).
#'
#' @examples
#' data(mtcars, package = "datasets")
#'
#' # Fit two different models for comparison
#' mid1 <- interpret(mpg ~ wt + hp + cyl, data = mtcars)
#' mid2 <- interpret(mpg ~ (wt + hp + cyl)^2, data = mtcars)
#'
#' # Calculate importance for both models and combine them
#' brks <- midlist(
#'   "Main Effects" = mid.breakdown(mid1),
#'   "Interactions" = mid.breakdown(mid2)
#' )
#'
#' # Create a comparative grouped bar plot (default)
#' plot(brks)
#'
#' # Create a comparative dot chart with a specific theme
#' plot(rev(brks), type = "dotchart", theme = "R4")
#'
#' # Create a series plot to observe trends across models
#' plot(brks, type = "series")
#' @returns
#' \code{plot.midbrks()} produces a plot as a side effect and returns \code{NULL} invisibly.
#'
#' @seealso \code{\link{plot.midbrk}}, \code{\link{ggmid.midbrks}}
#'
#' @exportS3Method base::plot
#'
plot.midbrks <- function(
    x, type = c("barplot", "dotchart", "series"), theme = NULL,
    terms = NULL, max.nterms = 15L, vline = TRUE, others = "others",
    pattern = c("%t=%v", "%t:%t"), format.args = list(), labels = NULL, ...
) {
  dots <- override(list(), list(...))
  type <- match.arg(type)
  brk <- summary(x, shape = "long")
  brk$term <- as.character(brk$term)
  olabs <- unique(brk$label)
  labels <- labels %||% olabs
  if (length(labels) != length(olabs)) {
    stop("length of 'labels' must match the number of models in the collection")
  }
  brk$label <- labels[match(brk$label, olabs)]
  parsed <- suppressWarnings(as.numeric(labels))
  discrete <- anyNA(parsed)
  if (!discrete) {
    labels <- parsed
    brk$label <- as.numeric(brk$label)
  } else if (!is.factor(labels)) {
    labels <- factor(labels, levels = unique(labels))
    brk$label <- factor(brk$label, levels = levels(labels))
  }
  theme <- theme %||% (
    if (type == "series" || discrete) getOption("midr.qualitative", "HCL")
    else getOption("midr.sequential", "bluescale")
  )
  theme <- color.theme(theme)
  terms.all <- as.character(unique(brk$term))
  if (!is.null(terms)) {
    idx <- match(terms, terms.all)
    terms.kept <- terms.all[idx[!is.na(idx)]]
  } else {
    terms.kept <- terms.all
  }
  terms.drop <- setdiff(terms.all, terms.kept)
  nmax <- min(max.nterms, length(terms.kept), na.rm = TRUE)
  if (nmax < length(terms.kept)) {
    terms.drop <- c(terms.drop, terms.kept[nmax:length(terms.kept)])
    terms.kept <- terms.kept[seq_len(nmax - 1L)]
  }
  use.others <- length(terms.drop) > 0L
  if (use.others) {
    brk.drop <- brk[brk$term %in% terms.drop, c("label", "mid"), drop = FALSE]
    df.resid <- stats::aggregate(mid ~ label, data = brk.drop, FUN = sum)
    df.resid$term <- others
    brk <- brk[brk$term %in% terms.kept, c("term", "label", "mid"), drop = FALSE]
    brk <- rbind(brk, df.resid[, c("term", "label", "mid")])
  } else {
    brk <- brk[brk$term %in% terms.kept, c("term", "label", "mid"), drop = FALSE]
  }
  format.args$x <- as.data.frame(x[[1L]]$data)
  values <- unlist(do.call(base::format, format.args))
  formatted <- character(length(terms.kept))
  for (i in seq_along(terms.kept)) {
    term <- terms.kept[i]
    tags <- term.split(term)
    vals <- values[tags]
    if (length(tags) == 1L) {
      label <- sub("%v", vals[1L], sub("%t", tags[1L], pattern[1L]))
    } else {
      if (length(pattern) == 1L) {
        pattern <- c(pattern, "%t:%t")
      }
      label <- sub("%v", vals[1L], sub("%t", tags[1L], pattern[2L]))
      label <- sub("%v", vals[2L], sub("%t", tags[2L], label))
    }
    formatted[i] <- label
  }
  for (i in seq_along(terms.kept)) {
    brk$term[brk$term == terms.kept[i]] <- formatted[i]
  }
  terms.kept <- formatted
  terms <- if (use.others) c(formatted, others) else terms.kept
  n <- length(terms)
  m <- length(labels)
  mat <- matrix(NA_real_, nrow = n, ncol = m)
  for (i in seq_len(m)) {
    subbrk <- brk[brk$label == labels[i], ]
    mat[, i] <- subbrk$mid[match(terms, subbrk$term)]
  }
  if (type == "barplot" || type == "dotchart") {
    cols <- to.colors(labels, theme)
    args <- list(to = mat, labels = terms, horizontal = TRUE, xlab = "mid")
    if (type == "dotchart") {
      args$type <- "d"
      args$col <- cols
      alpha.on <- "col"
    } else if (type == "barplot") {
      args$type <- "b"
      args$fill <- cols
      alpha.on <- "fill"
    }
    args <- set.alpha(override(args, dots), on = alpha.on)
    do.call(.barplot, args)
    if (vline) {
      graphics::abline(
        v = 0,
        lty = graphics::par("lty") %||% 1,
        lwd = graphics::par("lwd") %||% 1,
        col = graphics::par("fg") %||% "black"
      )
    }
  } else if (type == "series") {
    cols <- theme$palette(n)
    if (discrete) {
      x_pos <- seq_len(m)
      args <- list(x = x_pos, y = t(mat), type = "b", pch = 16L, col = cols,
                   lty = 1L, xaxt = "n", xlab = "", ylab = "mid")
      args <- set.alpha(override(args, dots), on = "col")
      do.call(graphics::matplot, args)
      graphics::axis(side = 1L, at = x_pos, labels = as.character(labels))
    } else {
      args <- list(x = labels, y = t(mat), type = "l", col = cols,
                   lty = 1L, xlab = "", ylab = "mid")
      args <- set.alpha(override(args, dots), on = "col")
      do.call(graphics::matplot, args)
    }
  }
  invisible(NULL)
}
