#' Calculate and Visualize MID-based Importance and Breakdown
#'
#' Creates a data frame showing the importance of each functional decomposition term.
#' \code{mid.importance()} returns a object of class "mid.importance", for which methods for \code{ggplot2::autoplot()} and \code{graphics::barplot()} are defined.
#'
#' @param object a mid object.
#' @param data data to be used to calculate the importance. If NULL, the fitted matrix is extracted from the mid object. If the number of row equals to one, the mid breakdown will be calculated.
#' @param weights a numeric vector of weights.
#' @param sort logical. If TRUE, the data.frame will be sorted by magnitude of importance
#' @param measure an integer specifying the type of function to evaluate the importance of each effect. Possible values are "1" for the mean absolute effect, "2" for the root mean square effect, and "3" for the median absolute effect.
#'
#' @examples
#' data(airquality, package = "datasets")
#' model <- glm(Ozone ~ Solar.R + Temp + Wind + Month, "poisson", airquality)
#' mid <- interpret(Ozone ~ Solar.R + Temp + Wind + Month, airquality, model)
#' mid.importance(mid)
#' ggmid(mid.importance(mid))
#' @export mid.importance
#'
mid.importance <- function(
    object, data = NULL, weights = NULL, sort = TRUE, measure = 1L) {
  if (is.null(data)) {
    if (is.null(object$fitted.matrix))
      stop("fitted matrix can't be extracted: 'data' must be passed")
    preds <- object$fitted.matrix
    weights <- object$weights
  } else {
    preds <- predict.mid(object, data, type = "terms")
  }
  if (!is.null(weights) && diff(range(weights, na.rm = TRUE)) == 0)
    weights <- NULL
  fun <- switch(measure, weighted.mae, weighted.rmse, weighted.medae)
  n <- nrow(preds)
  terms <- colnames(preds)
  imp <- apply(preds, MARGIN = 2L, FUN = fun, w = weights)
  if (sort)
    imp <- sort(imp, decreasing = TRUE)
  df <- data.frame(term = factor(names(imp), levels = rev(names(imp))),
                   importance = imp)
  attr(df, "terms") <- as.character(df$term)
  rownames(df) <- NULL
  if (n == 1L) {
    df$mid <- preds[1L, names(imp)]
    class(df) <- c("mid.importance", "mid.breakdown", "data.frame")
  } else {
    class(df) <- c("mid.importance", "data.frame")
  }
  df$degree <-
    as.factor(sapply(strsplit(as.character(df$term), split = ":"), length))
  df
}

#'
#' @rdname mid.importance
#' @param object a mid.importance object to be visualized.
#' @param type a character or an integer, specifying the type of the plot. Possible alternatives are "barplot" and "heatmap".
#' @param plot.main logical. If TRUE, lines, bars or rectangles representing mid values are drawn.
#' @param max.terms an integer, specifying the maximum number of component terms to be plotted in the barplot.
#' @param scale.palette color palette used to draw the interaction heatmap.
#' @param ... optional arguments to be passed to graphic functions.
#'
#' @exportS3Method midr::ggmid
#'
ggmid.mid.importance <- function(
    object, type = c("barplot", "heatmap"), plot.main = TRUE,
    max.terms = NA, scale.palette = c("#FFFFFF", "#464646"), ...) {
  type = match.arg(type)
  if (type == "barplot") {
    object <- object[1L:min(max.terms, nrow(object), na.rm = TRUE), ]
    var <- ifelse(inherits(object, "mid.breakdown"), "mid", "importance")
    pl <- ggplot2::ggplot(object,
                          ggplot2::aes(x = .data[[var]], y = .data[["term"]]))
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
    pl <- ggplot2::ggplot(fr,
      ggplot2::aes(.data[["x"]], .data[["y"]], fill = .data[["importance"]])) +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::scale_fill_gradient(low = spl[1L], high = spl[2L])
    if (plot.main)
      pl <- pl + ggplot2::geom_tile(...)
    return(pl)
  }
}


#'
#' @rdname mid.importance
#' @exportS3Method ggplot2::autoplot
#'
autoplot.mid.importance <- function(object, ...) {
  ggmid.mid.importance(object = object, ...)
}


#'
#' @rdname mid.importance
#' @exportS3Method graphics::barplot
#'
barplot.mid.importance <- function(object, max.terms = NA, ...) {
  object <- object[1L:min(max.terms, nrow(object), na.rm = TRUE), ]
  height <- object$importance
  names(height) <- object$term
  graphics::barplot.default(height, ...)
}

#'
#' @rdname mid.importance
#' @param x a mid.importance object to plot.
#' @exportS3Method base::plot
#'
plot.mid.importance <- function(
    x, type = c("barplot", "heatmap"),
    max.terms = NA, scale.palette = c("#FFFFFF", "#464646"), ...) {
  type = match.arg(type)
  if (type == "barplot") {
    x <- x[1L:min(max.terms, nrow(x), na.rm = TRUE), ]
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
}

