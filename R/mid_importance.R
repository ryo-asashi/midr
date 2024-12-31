#' Calculate MID Importance
#'
#' \code{mid.importance()} calculates the MID importance of a fitted MID model.
#'
#' \code{mid.importance()} returns an object of class "mid.importance".
#' The MID importance is defined for each component function of a MID model as the mean absolute effect in the given \code{data}.
#'
#' @param object a "mid" object.
#' @param data a data frame containing the observations to be used to calculate the MID importance. If \code{NULL}, the \code{fitted.matrix} of the MID model is used. If the \code{data} has only one observation, the output has the special class "mid.breakdown".
#' @param weights an optional numeric vector of sample weights.
#' @param sort logical. If \code{TRUE}, the output data frame is sorted by MID importance.
#' @param measure an integer specifying the measure of the MID importance. Possible alternatives are \code{1} for the mean absolute effect, \code{2} for the root mean square effect, and \code{3} for the median absolute effect.
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
#' imp <- mid.importance(mid)
#' imp
#' @returns
#' \code{mid.importance} returns a data frame of the class "mid.importance".
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
