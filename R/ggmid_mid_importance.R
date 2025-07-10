#' Plot MID Importance with ggplot2 Package
#'
#' For "mid.importance" objects, \code{ggmid()} visualizes the importance of MID component functions.
#'
#' The S3 method of \code{ggmid()} for "mid.importance" objects creates a "ggplot" object that visualizes the term importance of a fitted MID model.
#' The main layer is drawn using \code{geom_col()}, \code{geom_tile()}, \code{geom_point()} or \code{geom_boxplot()}.
#'
#' @param object a "mid.importance" object to be visualized.
#' @param type a character string specifying the type of the plot. One of "barplot", "heatmap", "dotchart" or "boxplot".
#' @param theme a character string specifying the color theme or any item that can be used to define "color.theme" object.
#' @param max.bars an integer specifying the maximum number of bars in the barplot, boxplot and dotchart.
#' @param ... optional parameters to be passed to the main layer.
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' idx <- sample(nrow(diamonds), 1e4)
#' mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
#' imp <- mid.importance(mid)
#' ggmid(imp, theme = "Tableau 10")
#' ggmid(imp, type = "dotchart", theme = "Okabe-Ito", size = 3)
#' ggmid(imp, type = "heatmap", theme = "Blues")
#' ggmid(imp, type = "boxplot", theme = "Accent")
#' @returns
#' \code{ggmid.mid.importance()} returns a "ggplot" object.
#' @exportS3Method midr::ggmid
#'
ggmid.mid.importance <- function(
    object, type = c("barplot", "dotchart", "heatmap", "boxplot"),
    theme = NULL, max.bars = 30L, ...) {
  type <- match.arg(type)
  if (missing(theme))
    theme <- getOption("midr.sequential", getOption("midr.qualitative", NULL))
  theme <- color.theme(theme)
  use.theme <- inherits(theme, "color.theme")
  # barplot and dotchart
  if (type == "barplot" || type == "dotchart") {
    imp <- object$importance
    imp <- imp[1L:min(max.bars, nrow(imp), na.rm = TRUE), ]
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
    imp <- object$importance
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
  } else if (type == "boxplot") {
    terms <- as.character(attr(object, "terms"))
    terms <- terms[1L:min(max.bars, length(terms), na.rm = TRUE)]
    preds <- object$predictions[, terms]
    terms <- factor(terms, levels = rev(terms))
    box <- data.frame(mid = as.numeric(preds),
                         term = rep(terms, each = nrow(preds)))
    pl <- ggplot2::ggplot(box)
    if (use.theme) {
      imp <- object$importance$importance
      imp <- imp[1L:min(max.bars, length(terms), na.rm = TRUE)]
      colors <- theme$palette(length(imp))
      pl <- pl + ggplot2::geom_boxplot(
        ggplot2::aes(x = .data[["mid"]], y = .data[["term"]]),
        fill = colors, ...)
    } else {
      pl <- pl + ggplot2::geom_boxplot(
        ggplot2::aes(x = .data[["mid"]], y = .data[["term"]]), ...)
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
