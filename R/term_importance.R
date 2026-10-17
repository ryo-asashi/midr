#' Calculate Term Importance
#'
#' @description
#' \code{term.importance()} calculates the term importance of a fitted model.
#' This is a measure of feature importance that quantifies the average contribution of each component function across a dataset.
#'
#' @details
#' For additive models, the term importance of a component function \eqn{g_S}, where \eqn{S} represents a single feature \eqn{\{j\}} or a feature pair \eqn{\{j, k\}}, is defined as the mean absolute effect on the predictions within the given data:
#'
#' \deqn{\mathbf{I}(g_S) = \frac{1}{n} \sum_{i=1}^n |g_S(\mathbf{x}_{i,S})|}
#'
#' Terms with higher importance values have a larger average impact on the model's overall predictions. Because all components (main effects and interactions) are measured on the same scale as the response variable, these values provide a direct and comparable measure of each term's contribution to the model.
#'
#' @param object a fitted model object.
#' @param data a data frame containing the observations to calculate the importance. If not provided, data is automatically extracted based on the function call.
#' @param weights an optional numeric vector of sample weights.
#' @param sort logical. If \code{TRUE}, the output data frame is sorted by importance in descending order.
#' @param measure an integer specifying the measure of importance. Possible alternatives are \code{1} for the mean absolute effect, \code{2} for the root mean square effect, and \code{3} for the median absolute effect.
#' @param max.nsamples an integer specifying the maximum number of samples to retain in the \code{predictions} component of the returned object. If the number of observations exceeds this value, a weighted random sample is taken.
#' @param seed an integer seed for random sampling. Default is \code{NULL}.
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, data = airquality, lambda = 1)
#'
#' # Calculate MID importance using median absolute contribution
#' imp <- term.importance(mid, data = airquality)
#' print(imp)
#'
#' # Calculate MID importance using root mean square contribution
#' imp <- term.importance(mid, measure = 2)
#' print(imp)
#' @returns
#' \code{term.importance()} returns an object of class "midimp". This is a list containing the following components:
#' \item{importance}{a data frame with the calculated importance values, sorted by default.}
#' \item{predictions}{the matrix of the fitted or predicted MID values. If the number of observations exceeds \code{max.nsamples}, this matrix contains a sampled subset.}
#' \item{measure}{a character string describing the type of the importance measure used.}
#'
#' For a "mids" collection object, \code{term.importance()} returns a collection object of class "midimps"-"midlist".
#'
#' @seealso \code{\link{interpret}}, \code{\link{plot.midimp}}, \code{\link{ggmid.midimp}}
#'
#' @export term.importance
#'
term.importance <- function(object, ...)
UseMethod("term.importance")


#' @rdname term.importance
#'
#' @export mid.importance
#'
mid.importance <- function(object, ...) {
  warning("'mid.importance' is deprecated: please use 'term.importance' instead")
  term.importance(object, ...)
}


#' @rdname term.importance
#'
#' @exportS3Method midr::term.importance
#'
term.importance.default <- function(
    object, data = NULL, weights = NULL, sort = TRUE, measure = 1L,
    max.nsamples = 1e4L, seed = NULL, ...
) {
  if (is.null(data)) {
    data <- tryCatch(
      model.data(object, env = parent.frame()),
      error = function(e) NULL
    )
    if (is.null(data))
      stop("'data' could not be extracted from the model object: please provide it directly")
    if (is.null(weights))
      weights <- stats::model.weights(data)
  }
  preds <- stats::predict(
    object, newdata = data, type = "terms", na.action = stats::na.pass
  )
  if (diff(range(weights, na.rm = TRUE)) == 0)
    weights <- NULL
  n <- nrow(preds)
  fun <- switch(measure, weighted.mae, weighted.rmse, weighted.medae)
  imp <- apply(preds, MARGIN = 2L, FUN = fun, w = weights)
  if (sort) imp <- base::sort(imp, decreasing = TRUE)
  tlabs <- names(imp)
  df <- data.frame(
    term = factor(tlabs, levels = rev(tlabs)),
    importance = imp,
    order = as.factor(lengths(lapply(tlabs, get.variables)))
  )
  rownames(df) <- NULL
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
  class(out) <- c("termimp", "midimp")
  out
}


#' @rdname term.importance
#'
#' @exportS3Method midr::term.importance
#'
term.importance.mids <- function(
    object, data = NULL, weights = NULL, sort = TRUE, measure = 1L,
    max.nsamples = 1e4L, seed = NULL, ...
) {
  if (missing(max.nsamples))
    max.nsamples <- max(100L, 1e4L %/% length(labels(object)))
  if (missing(seed)) seed <- sample(1e6L, 1L)
  out <- suppressMessages(lapply(
    X = object, FUN = term.importance, data = data, weights = weights,
    sort = sort, measure = measure, max.nsamples = max.nsamples, seed = seed
  ))
  class(out) <- c("", "midimps", "midlist")
  return(out)
}

