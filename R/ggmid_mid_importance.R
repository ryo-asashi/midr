#' Plot Term Importance with ggplot2 Package
#'
#' Creates a ggplot object representing the MID-based term importance
#'
#' @param object a mid.importance object to be visualized.
#' @param type a character or an integer, specifying the type of the plot. Possible alternatives are "barplot" and "heatmap".
#' @param plot.main logical. If TRUE, lines, bars or rectangles representing mid values are drawn.
#' @param max.terms an integer, specifying the maximum number of component terms to be plotted in the barplot.
#' @param scale.palette color palette used to draw the interaction heatmap.
#' @param ... optional arguments to be passed to graphic functions.
#' @exportS3Method midr::ggmid
#'
ggmid.mid.importance <- function(
    object, type = c("barplot", "heatmap"), plot.main = TRUE,
    max.terms = NA, scale.palette = c("#FFFFFF", "#464646"), ...) {
  type = match.arg(type)
  if (type == "barplot") {
    object <- object[1L:min(max.terms, nrow(object), na.rm = TRUE), ]
    var <- ifelse(inherits(object, "mid.breakdown"), "mid", "importance")
    pl <- ggplot2::ggplot(
      object, ggplot2::aes(x = .data[[var]], y = .data[["term"]])
      ) + ggplot2::labs(y = NULL)
    if (plot.main)
      pl <- pl + ggplot2::geom_col(...)
    return(pl)
  } else if (type == "heatmap") {
    terms <- as.character(object$term)
    spt <- strsplit(terms, ":")
    ftag <- sapply(spt, function(x) x[1L])
    stag <- sapply(spt, function(x) x[2L])
    stag <- ifelse(is.na(stag), ftag, stag)
    fr <- data.frame(x = c(stag, ftag), y = c(ftag, stag),
                     importance = rep.int(object$importance, 2L))
    fr <- unique(fr)
    spl <- ifnot.null(scale.palette, c("#FFFFFF", "#464646"))
    if (is.function(spl))
      spl <- spl(2L)
    if (length(spl) < 2L)
      spl <- c("#FFFFFF", spl)
    pl <- ggplot2::ggplot(data = fr,
      ggplot2::aes(.data[["x"]], .data[["y"]], fill = .data[["importance"]])) +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::scale_fill_gradient(low = spl[1L], high = spl[2L])
    if (plot.main)
      pl <- pl + ggplot2::geom_tile(...)
    return(pl)
  }
}


#' @rdname ggmid.mid.importance
#' @exportS3Method ggplot2::autoplot
#'
autoplot.mid.importance <- function(object, ...) {
  ggmid.mid.importance(object = object, ...)
}
