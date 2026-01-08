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
#' @param max.nkeeps an integer specifying the maximum number of observations to retain in the \code{predictions} component of the returned object. If the number of observations exceeds this value, a weighted random sample is taken. This helps to reduce memory usage and improve plotting performance.
#'
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, data = airquality, lambda = 1)
#'
#' # Calculate MID importance using median absolute contribution
#' imp <- mid.importance(mid, data = airquality)
#' print(imp)
#'
#' # Calculate MID importance using root mean square contribution
#' imp <- mid.importance(mid, measure = 2)
#' print(imp)
#' @returns
#' \code{mid.importance()} returns an object of class "mid.importance". This is a list containing the following components:
#' \item{importance}{a data frame with the calculated importance values, sorted by default.}
#' \item{predictions}{the matrix of the fitted or predicted MID values. If the number of observations exceeds \code{max.nkeeps}, this matrix contains a sampled subset.}
#' \item{measure}{a character string describing the type of the importance measure used.}
#'
#' @seealso \code{\link{interpret}}, \code{\link{plot.mid.importance}}, \code{\link{ggmid.mid.importance}}
#'
#' @export mid.importance
#'
mid.importance <- function(
    object, data = NULL, weights = NULL, sort = TRUE, measure = 1L,
    max.nkeeps = 10000) {
  if (is.null(data)) {
    data <- model.data(object, env = parent.frame())
    if (is.null(data)) {
      stop("'data' must be provided")
    } else {
      weights <- object$weights
      message("'data' ", if (is.null(weights)) "is " else "and 'weights' are ",
              "extracted from the 'object'")
    }
  }
  preds <- predict.mid(object, data, type = "terms", na.action = "na.pass")
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
  if (!is.null(max.nkeeps) && max.nkeeps >= 1 && n > max.nkeeps) {
    message("The number of predictions exceeds 'max.nkeeps': ", max.nkeeps,
            "predictions are randomly sampled")
    keepids <- sample(n, max.nkeeps, replace = FALSE, prob = weights)
    preds <- preds[keepids, , drop = FALSE]
  }
  out$predictions <- preds
  out$measure <- switch(measure,
                        "Mean Absolute Contribution",
                        "Root Mean Square Contribution",
                        "Median Absolute Contribution")
  attr(out, "n") <- n
  attr(out, "term.labels") <- as.character(df$term)
  class(out) <- c("mid.importance")
  out
}


#' @exportS3Method base::print
#'
print.mid.importance <- function(
    x, digits = max(3L, getOption("digits") - 2L), ...
  ) {
  n <- attr(x, "n")
  cat(paste0("\nMID Importance based on ",
             n, " Observation", if (n > 1L) "s", "\n"))
  cat(paste0("\nMeasure: ", x$measure, "\n"))
  cat("\nImportance:\n")
  print.data.frame(x$importance, digits = digits, ...)
}

