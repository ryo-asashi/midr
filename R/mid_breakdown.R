#' Calculate MID Breakdowns
#'
#' @description
#' \code{mid.breakdown()} calculates the contribution of each component function of a fitted MID model to a single prediction.
#' It breaks down the total prediction into the effects of the intercept, main effects, and interactions.
#'
#' @details
#' This function provides local interpretability for a specific observation by decomposing its prediction into the individual contributions of the MID components.
#' For a target observation \eqn{\mathbf{x}}, the total prediction is represented as the sum of all estimated terms:
#'
#' \deqn{g(\mathbf{x}) = g_\emptyset + \sum_{j} g_j(x_j) + \sum_{j<k} g_{jk}(x_j, x_k)}
#'
#' The output data frame itemizes the numerical value of each main effect \eqn{g_j(x_j)} and interaction effect \eqn{g_{jk}(x_j, x_k)}, along with the intercept \eqn{g_\emptyset}.
#' This decomposition makes the model's decision for a single instance fully transparent and easy to attribute to specific features or their combinations.
#'
#' @param object a "mid" object.
#' @param data a data frame containing one or more observations for which to calculate the MID breakdown. If not provided, data is automatically extracted based on the function call.
#' @param row an optional numeric value or character string specifying the row of \code{data} to be used for the breakdown. If \code{NULL}, and the \code{data} contains two or more observations, only the first observation is used.
#' @param sort logical. If \code{TRUE}, the output data frame is sorted by the absolute contribution of each effect.
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
#' For "midlist", \code{mid.breakdown()} returns an object of class "midlist.breakdown", a list of "mid.breakdown" objects.
#'
#' @seealso \code{\link{interpret}}, \code{\link{plot.mid.breakdown}}, \code{\link{ggmid.mid.breakdown}}
#'
#' @export mid.breakdown
#'
mid.breakdown <- function(
    object, data = NULL, row = NULL, sort = TRUE
  ) {
  if (inherits(object, "midlist")) {
    out <- suppressMessages(lapply(
      X = object, FUN = mid.breakdown, data = data, row = row, sort = sort
    ))
    attr(out, "term.labels") <- attr(out[[1L]], "term.labels", TRUE)
    class(out) <- "midlist.breakdown"
    return(out)
  }
  if (!inherits(object, "mid"))
    stop("'object' must be 'mid' or 'midlist'")
  if (is.null(data))
    data <- model.data(object, env = parent.frame())
  if (!is.data.frame(data))
    data <- data.frame(data)
  if (!is.null(row))
    data <- data[row, ]
  if (nrow(data) != 1L) {
    message("'data' contains multiple observations: the first observation is used")
    data <- data[1L, , drop = FALSE]
  }
  if (nrow(data) == 0L)
    stop("'data' contains no observations to be used")
  preds <- predict.mid(object, data, type = "terms", na.action = "na.pass")[1L, ]
  terms <- names(if (sort) base::sort(abs(preds), decreasing = TRUE) else preds)
  orders <- as.factor(sapply(strsplit(terms, split = ":"), length))
  df <- data.frame(term = factor(terms, levels = rev(terms)),
                   mid = preds[terms], order = orders)
  rownames(df) <- NULL
  out <- list()
  out$breakdown <- df
  out$data <- model.reframe(object, data)
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
  invisible(x)
}

#' @exportS3Method base::print
#'
print.midlist.breakdown <- function(
    x, digits = max(3L, getOption("digits") - 2L), n = 20L, ...
) {
  cat("\nMID Breakdown of a Prediction\n")
  intercept <- vapply(X = x, FUN = function(y) y$intercept, 0.0)
  cat(paste0("\nIntercept: ", examples(intercept, digits = digits), "\n"))
  lp <- x[[1L]]$linear.predictor
  if (!is.null(lp)) {
    lp <- vapply(X = x, FUN = function(y) y$linear.predictor, 0.0)
    cat(paste0("\nLinear Predictor: ", examples(lp, digits = digits), "\n"))
  } else {
    prediction <- vapply(X = x, FUN = function(y) y$prediction, 0.0)
    cat(paste0("\nPrediction: ", examples(prediction, digits = digits), "\n"))
  }
  cat("\nBreakdown of Effects:\n")
  smry <- summary.midlist.breakdown(x, shape = "wide")
  print.data.frame(utils::head(smry, n), digits = digits, ...)
  invisible(smry)
}
