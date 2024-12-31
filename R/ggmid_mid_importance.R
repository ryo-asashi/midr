#' Plot MID Importance with ggplot2 Package
#'
#' For "mid.importance" objects, \code{ggmid()} visualizes the importance of MID component functions.
#'
#' The S3 method of \code{ggmid()} for "mid.importance" objects creates a "ggplot" object that visualizes the term importance of a fitted MID model.
#' The main layer is drawn using \code{geom_col()} or \code{geom_tile()}.
#'
#' @param object a "mid.importance" object to be visualized.
#' @param type a character string specifying the type of the plot. One of "barplot" or "heatmap".
#' @param plot.main logical. If \code{TRUE}, the main layer is not drawn.
#' @param max.bars an integer specifying the maximum number of bars in the barplot.
#' @param scale.palette a character vector of length two to be used as the color palette for the heatmap.
#' @param ... optional parameters to be passed to the main layer.
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' idx <- sample(nrow(diamonds), 1e4)
#' mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
#' imp <- mid.importance(mid)
#' ggmid(imp)
#' ggmid(imp, type = "heatmap")
#' @returns
#' \code{ggmid.mid.importance()} returns a "ggplot" object.
#' @exportS3Method midr::ggmid
#'
ggmid.mid.importance <- function(
    object, type = c("barplot", "heatmap"), plot.main = TRUE,
    max.bars = NA, scale.palette = c("#FFFFFF", "#464646"), ...) {
  type = match.arg(type)
  if (type == "barplot") {
    object <- object[1L:min(max.bars, nrow(object), na.rm = TRUE), ]
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
