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
#' \code{mid.importance} returns an object of the class "mid.importance" containing the following components.
#' \item{importance}{the data frame of calculated importances.}
#' \item{predictions}{the matrix of the fitted or predicted MID values.}
#' \item{measure}{the type of the importance measure.}
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
    preds <- predict.mid(object, data, type = "terms", na.action = "na.pass")
  }
  if (!is.null(weights) && diff(range(weights, na.rm = TRUE)) == 0)
    weights <- NULL
  n <- nrow(preds)
  fun <- switch(measure, weighted.mae, weighted.rmse, weighted.medae)
  imp <- apply(preds, MARGIN = 2L, FUN = fun, w = weights)
  if (sort)
    imp <- base::sort(imp, decreasing = TRUE)
  df <- data.frame(term = factor(names(imp), levels = rev(names(imp))),
                   importance = imp)
  rownames(df) <- NULL
  df$degree <-
    as.factor(sapply(strsplit(as.character(df$term), split = ":"), length))
  out <- list()
  out$importance <- df
  out$predictions <- preds
  out$measure <- switch(measure, "MAE", "RMSE", "MedAE")
  attr(out, "terms") <- as.character(df$term)
  class(out) <- c("mid.importance")
  out
}


#' @rdname mid.importance
#' @param x a "mid.importance" object to be printed.
#' @param digits an integer specifying the minimum number of significant digits to be printed.
#' @param ... additional parameters to be passed to \code{print.data.frame()} to print the importance of component functions.
#' @exportS3Method base::print
#'
print.mid.importance <- function(
    x, digits = max(3L, getOption("digits") - 2L), ...
  ) {
  n <- nrow(x$predictions)
  cat(paste0("\nMID Importance based on ",
             n, " Observation", if (n > 1L) "s", "\n"))
  cat(paste0("\nMeasure: ", x$measure, "\n"))
  cat("\nImportance:\n")
  print.data.frame(x$importance, digits = digits, ...)
}

