#' Calculate MID Breakdown
#'
#' \code{mid.breakdown()} calculates the MID breakdown of a prediction of the MID model.
#'
#' \code{mid.breakdown()} returns an object of class "mid.breakdown".
#'
#' @param object a "mid" object.
#' @param data a data frame containing the observations to be used to calculate the MID importance. If \code{NULL}, the \code{fitted.matrix} of the MID model is used. If the \code{data} has only one observation, the output has the special class "mid.breakdown".
#' @param sort logical. If \code{TRUE}, the output data frame is sorted by MID .
#' @param digits an integer specifying the minimum number of significant digits.
#' @param format a character vector of length two to be used as the formats of the \code{sprintf()} function for each value or pair of values of predictor variables.
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
#' mbd <- mid.breakdown(mid, airquality[1L, ])
#' mbd
#' @returns
#' \code{mid.breakdown()} returns an object of the class "mid.breakdown" containing the following components.
#' \item{breakdown}{the data frame containing the breakdown of the prediction.}
#' \item{data}{the data frame containing the values of predictor variables used for the prediction.}
#' \item{intercept}{the intercept of the MID model.}
#' \item{prediction}{the predicted value.}
#' @export mid.breakdown
#'
mid.breakdown <- function(
    object, data, sort = TRUE, digits = 6L, format = c("%s", "%s, %s")) {
  if (!is.data.frame(data))
    data <- data.frame(data)
  if (nrow(data) != 1L) {
    message("'data' must be a data.frame containing a single observation: the first observation is used")
    data <- data[1L, ]
  }
  preds <- predict.mid(object, data,
                       type = "terms", na.action = "na.pass")[1L, ]
  data <- model.reframe(object, data)
  terms <- names(if (sort) base::sort(abs(preds), decreasing = TRUE) else preds)
  m <- length(terms)
  degrees <- as.factor(sapply(strsplit(terms, split = ":"), length))
  values <- character(m)
  for (i in seq_len(m)) {
    term <- terms[i]
    tags <- term.split(term)
    if (length(tags) == 1L) {
      u <- base::format(data[1L, tags[1L]], digits = digits)
      values[i] <- sprintf(format[1L], u)
    } else {
      u <- base::format(data[1L, tags[1L]], digits = digits)
      v <- base::format(data[1L, tags[2L]], digits = digits)
      values[i] <- sprintf(format[2L], u, v)
    }
  }
  df <- data.frame(term = factor(terms, levels = rev(terms)),
                   value = values, mid = preds[terms], degree = degrees)
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
  attr(out, "terms") <- as.character(df$term)
  class(out) <- c("mid.breakdown")
  out
}


#' @rdname mid.breakdown
#' @param x a "mid.importance" object to be printed.
#' @param ... additional parameters to be passed to \code{print.data.frame()} to print the importance of component functions.
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

