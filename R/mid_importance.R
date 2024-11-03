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
  n <- nrow(preds)
  fun <- switch(measure, weighted.mae, weighted.rmse, weighted.medae)
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
