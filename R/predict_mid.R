#' Predict Method for fitted MID Models
#'
#' @description
#' \code{predict.mid()} is an S3 method for "mid" objects that obtains predictions from a fitted MID model.
#' It can be used to predict on new data or to retrieve the fitted values from the original data.
#'
#' @details
#' The \code{type} argument allows you to specify the scale of the prediction.
#' By default (\code{type = "response"}), the function returns predictions on the original scale of the response variable.
#' Alternatively, you can obtain predictions on the scale of the linear predictor by setting \code{type = "link"}.
#' For a detailed breakdown, setting \code{type = "terms"} returns a matrix where each column represents the contribution of a specific model term on the linear predictor scale.
#'
#' The \code{terms} argument allows for predictions based on a subset of the model's component functions, excluding others.
#'
#' @param object a "mid" object to be used to make predictions.
#' @param newdata a data frame of the new observations. If \code{NULL}, the original fitted values are extracted and returned.
#' @param na.action a function or character string specifying what should happen when the data contain \code{NA} values.
#' @param type the type of prediction required. One of "response", "link", or "terms".
#' @param terms a character vector of term labels, specifying a subset of component functions to use for predictions.
#' @param ... arguments to be passed to other methods (not used in this method).
#'
#' @examples
#' data(airquality, package = "datasets")
#' test <- 1:10
#' mid <- interpret(Ozone ~ .^2, airquality[-test, ], lambda = 1, link = "log")
#'
#' # Predict on new data
#' predict(mid, airquality[test, ])
#'
#' # Get predictions on the link scale
#' predict(mid, airquality[test, ], type = "link")
#'
#' # Get the contributions of specific terms
#' predict(mid, airquality[test, ], terms = c("Temp", "Wind"), type = "terms")
#' @returns
#' \code{predict.mid()} returns a numeric vector of MID model predictions, or a matrix if \code{type = "terms"}.
#'
#' @seealso \code{\link{interpret}}, \code{\link{mid.effect}}, \code{\link{get.yhat}}
#'
#' @exportS3Method stats::predict
#'
predict.mid <- function(
    object, newdata = NULL, na.action = "na.pass",
    type = c("response", "link", "terms"), terms = mid.terms(object), ...) {
  type <- match.arg(type)
  if (!missing(terms)) {
    for (i in seq_len(length(terms)))
      terms[i] <- term.check(terms[i], object$terms, stop = FALSE)
    terms <- unique(terms[!is.na(terms)])
  }
  if (is.null(newdata)) {
    preds <- object$fitted.matrix[, terms, drop = FALSE]
    naa <- stats::na.action(object)
  } else {
    newdata <- model.reframe(object, newdata)
    attr(newdata, "na.action") <- NULL
    newdata <- do.call(na.action, list(newdata))
    naa <- stats::na.action(newdata)
    n <- nrow(newdata)
    r <- length(terms)
    preds <- matrix(0, nrow = n, ncol = r)
    colnames(preds) <- terms
    spl <- sapply(strsplit(terms, ":"), length)
    mts <- unique(terms[spl == 1L])
    its <- unique(terms[spl == 2L])
    mmats <- list()
    for (tag in mts)
      mmats[[tag]] <- object$encoders$main.effects[[tag]]$encode(newdata[, tag])
    imats <- list()
    for (tag in unique(term.split(its)))
      imats[[tag]] <- object$encoders$interactions[[tag]]$encode(newdata[, tag])
    for (i in seq_len(r)) {
      term <- terms[i]
      tags <- term.split(term)
      if (length(tags) == 1L) {
        X <- mmats[[term]]
        mid <- object$main.effects[[term]]$mid
      } else if (length(tags) == 2L) {
        n1 <- object$encoders$interactions[[tags[1L]]]$n
        n2 <- object$encoders$interactions[[tags[2L]]]$n
        uni <- as.matrix(expand.grid(1L:n1, 1L:n2))
        X <- matrix(0, nrow = n, ncol = nrow(uni))
        for (j in seq_len(nrow(uni))) {
          X[, j] <- as.numeric(imats[[tags[1L]]][, uni[j, 1L]] *
                               imats[[tags[2L]]][, uni[j, 2L]])
        }
        mid <- object$interactions[[term]]$mid
      }
      preds[, i] <- as.numeric(X %*% mid)
    }
  }
  if (type == "terms") {
    if (inherits(naa, "exclude"))
      return(stats::napredict(naa, preds))
    return(structure(preds, constant = object$intercept, na.action = naa))
  }
  preds <- rowSums(preds) + object$intercept
  if (type == "response" && !is.null(object$link))
    preds <- object$link$linkinv(preds)
  if (inherits(naa, "exclude"))
    return(stats::napredict(naa, preds))
  structure(as.numeric(preds), na.action = naa)
}
