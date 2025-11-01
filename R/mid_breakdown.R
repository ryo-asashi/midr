#' Calculate MID Breakdowns
#'
#' @description
#' \code{mid.breakdown()} calculates the contribution of each component function of a fitted MID model to a single prediction.
#' It breaks down the total prediction into the effects of the intercept, main effects, and interactions.
#'
#' @details
#' \code{mid.breakdown()} is a method for local interpretability.
#' For a given observation, it provides a clear answer to the question, "How much did each component of the MID model contribute to the final prediction?"
#'
#' The function calculates the value of each term in the MID model's additive structure for the specified observation.
#' The total prediction is the sum of these individual contributions.
#' The prediction, denoted \eqn{\mathcal{F}(\mathbf{x})}, is decomposed as:
#' \deqn{\mathcal{F}(\mathbf{x}) = f_\phi + \sum_{j} f_{j}(x_j) + \sum_{j<k} f_{jk}(x_j, x_k)}
#'
#' The output data frame itemizes the numerical value of each main effect
#' (\eqn{f_{j}(x_j)}) and interaction effect (\eqn{f_{jk}(x_j, x_k)}),
#' along with the intercept (\eqn{f_\phi}). This makes the prediction transparent
#' and easy to understand.
#'
#' @param object a "mid" object.
#' @param data a data frame containing one or more observations for which to calculate the MID breakdown. If not provided, data is automatically extracted based on the function call.
#' @param row an optional numeric value or character string specifying the row of \code{data} to be used for the breakdown. If \code{NULL}, and the \code{data} contains two or more observations, only the first observation is used.
#' @param sort logical. If \code{TRUE}, the output data frame is sorted by the absolute contribution of each effect.
#' @param format a character vector of length two to be used as a format string for \code{sprintf()} to display the values of main effects and interactions, respectively.
#'
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, data = airquality, lambda = 1)
#'
#' # Calculate the breakdown for the first observation in the data
#' mbd <- mid.breakdown(mid, data = airquality, row = 1)
#' print(mbd)
#'
#' # Calculate the breakdown for the third observation in the data
#' mbd <- mid.breakdown(mid, data = airquality, row = 3)
#' print(mbd)
#' @returns
#' \code{mid.breakdown()} returns an object of class "mid.breakdown". This is a list with the following components:
#' \item{breakdown}{a data frame containing the breakdown of the prediction.}
#' \item{data}{the data frame containing the predictor variable values used for the prediction.}
#' \item{intercept}{the intercept of the MID model.}
#' \item{prediction}{the predicted value from the MID model.}
#'
#' @seealso \code{\link{interpret}}, \code{\link{plot.mid.breakdown}}, \code{\link{ggmid.mid.breakdown}}
#'
#' @export mid.breakdown
#'
mid.breakdown <- function(
    object, data = NULL, row = NULL, sort = TRUE, format = c("%s", "%s, %s")) {
  if (is.null(data))
    data <- model.data(object, env = parent.frame())
  if (!is.data.frame(data))
    data <- data.frame(data)
  if (!is.null(row))
    data <- data[row, ]
  if (nrow(data) != 1L) {
    message("'data' contains multiple observations: the first observation is used")
    data <- data[1L, ]
  }
  if (nrow(data) == 0L)
    stop("'data' contains no observations to be used")
  preds <- predict.mid(object, data,
                       type = "terms", na.action = "na.pass")[1L, ]
  data <- model.reframe(object, data)
  terms <- names(if (sort) base::sort(abs(preds), decreasing = TRUE) else preds)
  m <- length(terms)
  orders <- as.factor(sapply(strsplit(terms, split = ":"), length))
  values <- character(m)
  for (i in seq_len(m)) {
    term <- terms[i]
    tags <- term.split(term)
    if (length(tags) == 1L) {
      u <- data[1L, tags[1L]]
      values[i] <- sprintf(format[1L], u)
    } else {
      u <- data[1L, tags[1L]]
      v <- data[1L, tags[2L]]
      values[i] <- sprintf(format[2L], u, v)
    }
  }
  df <- data.frame(term = factor(terms, levels = rev(terms)),
                   value = values, mid = preds[terms], order = orders)
  rownames(df) <- NULL
  out <- list()
  out$breakdown <- df
  out$data <- data
  out$intercept <- object$intercept
  if (!is.null(object$link)) {
    out$linear.predictor <- object$intercept + sum(preds)
    out$prediction <- object$link$linkinv(out$linear.predictor)
  } else {
    out$prediction <- object$intercept + sum(preds)
  }
  attr(out, "term.labels") <- as.character(df$term)
  class(out) <- c("mid.breakdown")
  out
}


#' @exportS3Method base::print
#'
print.mid.breakdown <- function(
    x, digits = max(3L, getOption("digits") - 2L), ...
  ) {
  cat("\nMID Breakdown of a Prediction\n")
  cat(paste0("\nIntercept: ", format(x$intercept, digits = digits), "\n"))
  if (!is.null(x$linear.predictor)) {
    cat(paste0("\nLinear Predictor: ",
               format(x$linear.predictor, digits = digits), "\n"))
  } else {
    cat(paste0("\nPrediction: ", format(x$prediction, digits = digits), "\n"))
  }
  cat("\nBreakdown of Effects:\n")
  print.data.frame(x$breakdown, digits = digits, ...)
}
