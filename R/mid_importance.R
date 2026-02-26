#' Calculate MID Importance
#'
#' @description
#' \code{mid.importance()} calculates the MID importance of a fitted MID model.
#' This is a measure of feature importance that quantifies the average contribution of each component function across a dataset.
#'
#' @details
#' The MID importance of a component function \eqn{g_S}, where \eqn{S} represents a single feature \eqn{\{j\}} or a feature pair \eqn{\{j, k\}}, is defined as the mean absolute effect on the predictions within the given data:
#'
#' \deqn{\mathbf{I}(g_S) = \frac{1}{n} \sum_{i=1}^n |g_S(\mathbf{x}_{i,S})|}
#'
#' Terms with higher importance values have a larger average impact on the model's overall predictions. Because all components (main effects and interactions) are measured on the same scale as the response variable, these values provide a direct and comparable measure of each term's contribution to the model.
#'
#' @param object a "mid" object.
#' @param data a data frame containing the observations to calculate the importance. If not provided, data is automatically extracted based on the function call.
#' @param weights an optional numeric vector of sample weights.
#' @param sort logical. If \code{TRUE}, the output data frame is sorted by importance in descending order.
#' @param measure an integer specifying the measure of importance. Possible alternatives are \code{1} for the mean absolute effect, \code{2} for the root mean square effect, and \code{3} for the median absolute effect.
#' @param max.nsamples an integer specifying the maximum number of samples to retain in the \code{predictions} component of the returned object. If the number of observations exceeds this value, a weighted random sample is taken.
#' @param seed an integer seed for random sampling. Default is \code{NULL}.
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
#' \code{mid.importance()} returns an object of class "midimp". This is a list containing the following components:
#' \item{importance}{a data frame with the calculated importance values, sorted by default.}
#' \item{predictions}{the matrix of the fitted or predicted MID values. If the number of observations exceeds \code{max.nsamples}, this matrix contains a sampled subset.}
#' \item{measure}{a character string describing the type of the importance measure used.}
#'
#' For "midlist", \code{mid.importance()} returns an object of class "midimps"-"midlist", a list of "midimp" objects.
#'
#' @seealso \code{\link{interpret}}, \code{\link{plot.mid.importance}}, \code{\link{ggmid.mid.importance}}
#'
#' @export mid.importance
#'
mid.importance <- function(
    object, data = NULL, weights = NULL, sort = TRUE, measure = 1L,
    max.nsamples = 1e4L, seed = NULL) {
  if (inherits(object, "mids")) {
    if (missing(max.nsamples))
      max.nsamples <- max(100L, 1e4L %/% length(labels(object)))
    if (missing(seed)) seed <- sample(1e6L, 1L)
    out <- suppressMessages(lapply(
      X = object, FUN = mid.importance, data = data, weights = weights,
      sort = sort, measure = measure, max.nsamples = max.nsamples
    ))
    class(out) <- c("midimps", "midlist")
    return(out)
  }
  if (!inherits(object, "mid"))
    stop("'object' must be 'mid' or 'midlist'")
  if (is.null(data)) {
    data <- model.data(object, env = parent.frame())
    if (is.null(data))
      stop("'data' must be provided")
    naa <- object$na.action
    if (!is.null(naa))
      data <- data[-naa, , drop = FALSE]
    if (is.null(weights))
      weights <- object$weights
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
  if (!is.null(max.nsamples) && n > max.nsamples) {
    if (!is.null(seed)) set.seed(seed)
    keeprows <- sample(n, max.nsamples, replace = FALSE, prob = weights)
    preds <- preds[keeprows, , drop = FALSE]
  }
  out$predictions <- preds
  out$measure <- switch(measure,
                        "Mean Absolute Contribution",
                        "Root Mean Square Contribution",
                        "Median Absolute Contribution")
  attr(out, "n") <- n
  attr(out, "term.labels") <- as.character(df$term)
  class(out) <- c("midimp")
  out
}


#' @exportS3Method base::print
#'
print.midimp <- function(
    x, digits = max(3L, getOption("digits") - 2L), ...
  ) {
  n <- attr(x, "n", exact = TRUE)
  cat(paste0("\nMID Importance based on ",
             n, " Observation", if (n > 1L) "s", "\n"))
  cat(paste0("\nMeasure: ", x$measure, "\n"))
  cat("\nImportance:\n")
  print.data.frame(x$importance, digits = digits, ...)
  invisible(x)
}


#' @exportS3Method base::print
#'
print.midimps <- function(
    x, digits = max(3L, getOption("digits") - 2L), n = 20L, ...
) {
  nobs <- attr(x[[1L]], "n", exact = TRUE)
  cat(paste0("\nMID Importance based on ",
             nobs, " Observation", if (nobs > 1L) "s", "\n"))
  cat(paste0("\nMeasure: ", x[[1L]]$measure, "\n"))
  cat("\nImportance:\n")
  smry <- summary(x, shape = "wide")
  print.data.frame(utils::head(smry, n), digits = digits, ...)
  invisible(smry)
}
