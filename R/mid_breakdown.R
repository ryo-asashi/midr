#' Calculate MID Breakdown
#'
#' \code{mid.breakdown()} calculates the MID breakdown of a prediction of the MID model.
#'
#' \code{mid.breakdown()} returns an object of class "mid.breakdown".
#'
#' @param object a "mid" object.
#' @param data a data frame containing the observations to be used to calculate the MID importance. If \code{NULL}, the \code{fitted.matrix} of the MID model is used. If the \code{data} has only one observation, the output has the special class "mid.breakdown".
#' @param sort logical. If \code{TRUE}, the output data frame is sorted by MID importance.
#' @param digits an integer specifying the minimum number of significant digits to be printed.
#' @param left a character string to be used as the left bracket of the value.
#' @param right a character string to be used as the right bracket of the value.
#' @param sep a character string to separate the two values of interaction terms.
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
#' mbd <- mid.breakdown(mid, airquality[1L, ])
#' mbd
#' @returns
#' \code{mid.importance} returns an object of the class "mid.importance" containing the following components.
#' \item{importance}{the data frame of calculated importances.}
#' \item{predictions}{the matrix of the fitted or predicted MID values.}
#' \item{measure}{the type of the importance measure.}
#' @export mid.breakdown
#'
mid.breakdown <- function(
    object, data, sort = TRUE, digits = max(3L, getOption("digits") - 2L),
    left = "(", right = ")", sep = ", "
  ) {
  if (!is.data.frame(data))
    data <- data.frame(data)
  if (nrow(data) != 1L) {
    message("'data' must be a data.frame containing a single observation: the first observation is used")
    data <- data[1L, ]
  }
  preds <- predict.mid(object, data,
                       type = "terms", na.action = "na.pass")[1L, ]
  if (!is.null(formula <- eval(object$call$formula))) {
    yvar <- deparse(formula[[2L]])
    data[, yvar] <- 0L
    data <- stats::model.frame(formula = formula,
                               data = data, na.action = "na.pass")
    data <- data[, -which(colnames(data) == yvar)]
  }
  terms <- names(if (sort) base::sort(abs(preds), decreasing = TRUE) else preds)
  m <- length(terms)
  degrees <- as.factor(sapply(strsplit(terms, split = ":"), length))
  values <- character(m)
  for (i in seq_len(m)) {
    term <- terms[i]
    tags <- term.split(term)
    if (length(tags) == 1L) {
      values[i] <- paste0(left, format(data[1L, term], digits = digits), right)
    } else {
      values[i] <- paste0(left, format(data[1L, tags[1L]], digits = digits), sep,
                          format(data[1L, tags[2L]], digits = digits), right)
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

