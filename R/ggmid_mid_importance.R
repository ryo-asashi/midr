#' Plot MID Importance with ggplot2
#'
#' @description
#' For "mid.importance" objects, \code{ggmid()} visualizes the importance of component functions of the fitted MID model.
#'
#' @details
#' This is an S3 method for the \code{ggmid()} generic that creates an importance plot from a "mid.importance" object, visualizing the average contribution of component functions to the fitted MID model.
#'
#' The \code{type} argument controls the visualization style.
#' The default, \code{type = "barplot"}, creates a standard bar plot where the length of each bar represents the overall importance of the term.
#' The \code{type = "dotchart"} option creates a dot plot, offering a clean alternative to the bar plot for visualizing term importance.
#' The \code{type = "heatmap"} option creates a matrix-shaped heat map where the color of each cell represents the importance of the interaction between a pair of variables, or the main effect on the diagonal.
#' The \code{type = "boxplot"} option creates a box plot where each box shows the distribution of a term's contributions across all observations, providing insight into the variability of each term's effect.
#'
#' @param object a "mid.importance" object to be visualized.
#' @param type the plotting style. One of "barplot", "dotchart", "heatmap", or "boxplot".
#' @param theme a character string or object defining the color theme. See \code{\link{color.theme}} for details.
#' @param terms an optional character vector specifying which terms to display.
#' @param max.nterms the maximum number of terms to display. Defaults to 30 for bar, dot and box plots.
#' @param ... optional parameters passed on to the main layer.
#'
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' idx <- sample(nrow(diamonds), 1e4)
#' mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
#' imp <- mid.importance(mid)
#'
#' # Create a bar plot (default)
#' ggmid(imp)
#'
#' # Create a dot chart
#' ggmid(imp, type = "dotchart", theme = "Okabe-Ito", size = 3)
#'
#' # Create a heatmap
#' ggmid(imp, type = "heatmap")
#'
#' # Create a boxplot to see the distribution of effects
#' ggmid(imp, type = "boxplot")
#' @returns
#' \code{ggmid.mid.importance()} returns a "ggplot" object.
#'
#' @seealso \code{\link{mid.importance}}, \code{\link{ggmid}}, \code{\link{plot.mid.importance}}
#'
#' @exportS3Method midr::ggmid
#'
ggmid.mid.importance <- function(
    object, type = c("barplot", "dotchart", "heatmap", "boxplot"),
    theme = NULL, terms = NULL, max.nterms = 30L, ...) {
  type <- match.arg(type)
  if (missing(theme))
    theme <- getOption("midr.sequential", getOption("midr.qualitative", NULL))
  theme <- color.theme(theme)
  use.theme <- inherits(theme, "color.theme")
  imp <- object$importance
  if (!is.null(terms))
    imp <- imp[match(terms, imp$term, nomatch = 0L), ]
  if (type != "heatmap")
    imp <- imp[1L:min(max.nterms, nrow(imp), na.rm = TRUE), ]
  # barplot and dotchart
  if (type == "barplot" || type == "dotchart") {
    pl <- ggplot2::ggplot(
      imp, ggplot2::aes(x = .data[["importance"]], y = .data[["term"]])
      ) + ggplot2::labs(y = NULL)
    if (type == "barplot") {
      pl <- pl + ggplot2::geom_col(...)
      if (use.theme) {
        var <- if (theme$type == "qualitative") "order" else "importance"
        pl <- pl + ggplot2::aes(fill = .data[[var]]) +
          scale_fill_theme(theme = theme)
      }
    } else if (type == "dotchart") {
      tli <- ggplot2::theme_get()$line
      pl <- pl + ggplot2::geom_linerange(color = ifnot.null(tli$colour, "black"),
        linewidth = ifnot.null(tli$linewidth, 0.5), linetype = 3L,
        ggplot2::aes(xmin = 0, xmax = .data[["importance"]])) +
        ggplot2::geom_point(...)
      if (use.theme) {
        var <- if (theme$type == "qualitative") "order" else "importance"
        pl <- pl + ggplot2::aes(color = .data[[var]]) +
          scale_color_theme(theme = theme)
      }
    }
    return(pl)
  # heatmap
  } else if (type == "heatmap") {
    terms <- as.character(imp$term)
    spt <- strsplit(terms, ":")
    ftag <- sapply(spt, function(x) x[1L])
    stag <- sapply(spt, function(x) x[2L])
    stag <- ifelse(is.na(stag), ftag, stag)
    levs <- unique(spt)
    fr <- data.frame(x = factor(c(stag, ftag), levels = levs),
                     y = factor(c(ftag, stag), levels = levs),
                     importance = rep.int(imp$importance, 2L))
    fr <- unique(fr)
    pl <- ggplot2::ggplot(data = fr,
      ggplot2::aes(.data[["x"]], .data[["y"]], fill = .data[["importance"]])) +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::geom_tile(...)
    pl <- pl + scale_fill_theme(theme = if (use.theme) theme else "grayscale")
    return(pl)
  # boxplot
  } else if (type == "boxplot") {
    terms <- as.character(imp$term)
    preds <- object$predictions
    preds_vec <- unlist(lapply(terms, function(term) preds[, term]))
    terms <- factor(terms, levels = rev(terms))
    box <- data.frame(mid = preds_vec, term = rep(terms, each = nrow(preds)))
    pl <- ggplot2::ggplot(box) + ggplot2::geom_boxplot(
        ggplot2::aes(x = .data[["mid"]], y = .data[["term"]]), ...
      )
    if (use.theme) {
      pl$data <- merge(box, imp, by = "term", all.x = TRUE)
      var <- if (theme$type == "qualitative") "order" else "importance"
      pl <- pl + ggplot2::aes(fill = .data[[var]], group = .data[["term"]]) +
        scale_fill_theme(theme = theme)
    }
    pl <- pl + ggplot2::labs(y = NULL)
    return(pl)
  }
}


#' @rdname ggmid.mid.importance
#' @exportS3Method ggplot2::autoplot
#'
autoplot.mid.importance <- function(object, ...) {
  ggmid.mid.importance(object = object, ...)
}
