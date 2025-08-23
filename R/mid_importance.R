#' Calculate MID Importance
#'
#' @description
#' \code{mid.importance()} calculates the MID importance of a fitted MID model.
#' This is a measure of feature importance that quantifies the average contribution of each component function across a dataset.
#'
#' @details
#' The MID importance of a component function (e.g., a main effect or an interaction) is defined as the mean absolute effect on the predictions within the given data.
#' Terms with higher importance have a larger average impact on the model's overall predictions.
#'
#' @param object a "mid" object.
#' @param data a data frame containing the observations to calculate the importance. If \code{NULL}, the \code{fitted.matrix} from the "mid" object is used.
#' @param weights an optional numeric vector of sample weights.
#' @param sort logical. If \code{TRUE}, the output data frame is sorted by importance in descending order.
#' @param measure an integer specifying the measure of importance. Possible alternatives are \code{1} for the mean absolute effect, \code{2} for the root mean square effect, and \code{3} for the median absolute effect.
#'
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
#'
#' # Calculate importance for a fitted MID model
#' imp <- mid.importance(mid)
#' print(imp)
#'
#' # Calculate importance using Root Mean Square Effect
#' imp <- mid.importance(mid, measure = 2)
#' print(imp)
#' @returns
#' \code{mid.importance()} returns an object of class "mid.importance". This is a list containing the following components:
#' \item{importance}{a data frame with the calculated importance values, sorted by default.}
#' \item{predictions}{the matrix of the fitted or predicted MID values.}
#' \item{measure}{a character string describing the type of the importance measure used.}
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
  df$order <-
    as.factor(sapply(strsplit(as.character(df$term), split = ":"), length))
  out <- list()
  out$importance <- df
  out$predictions <- preds
  out$measure <- switch(measure,
                        "Mean Absolute Contribution",
                        "Root Mean Square Contribution",
                        "Median Absolute Contribution")
  attr(out, "terms") <- as.character(df$term)
  class(out) <- c("mid.importance")
  out
}


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

