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
#' \code{predict.midlist()} returns a matrix of predictions, or a list of matrix if \code{type = "terms"}.
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
      terms[i] <- term.check(terms[i], mid.terms(object), stop = FALSE)
    terms <- unique(terms[!is.na(terms)])
  }
  if (is.null(newdata)) {
    newdata <- model.data(object, env = parent.frame())
    if (is.null(newdata)) {
      stop("'newdata' must be provided")
    }
  }
  newdata <- model.reframe(object, newdata)
  attr(newdata, "na.action") <- NULL
  newdata <- do.call(na.action, list(newdata))
  naa <- stats::na.action(newdata)
  n <- nrow(newdata)
  m <- length(terms)
  if (type == "terms") {
    preds <- matrix(0, nrow = n, ncol = m, dimnames = list(NULL, terms))
  } else {
    k <- length(object$intercept)
    preds <- matrix(object$intercept, nrow = n, ncol = k, byrow = TRUE)
  }
  ltag <- strsplit(terms %||% character(), ":")
  tlen <- sapply(ltag, length)
  imat <- list()
  for (tag in unique(unlist(ltag[tlen == 2L]))) {
    imat[[tag]] <- object$encoders$interactions[[tag]]$encode(newdata[, tag])
  }
  for (i in seq_len(m)) {
    tags <- ltag[[i]]
    if (length(tags) == 1L) {
      bmat <- object$main.effects[[tags]]$mid
      mmat <- object$encoders$main.effects[[tags]]$encode(newdata[, tags])
      term_preds <- mmat %*% bmat
      mmat <- NULL
    } else if (length(tags) == 2L) {
      bmat <- object$interactions[[terms[i]]]$mid
      mat1 <- imat[[tags[1L]]]
      mat2 <- imat[[tags[2L]]]
      m1 <- ncol(mat1)
      m2 <- ncol(mat2)
      if (NCOL(bmat) == 1L) {
        W <- matrix(as.numeric(bmat), nrow = m1, ncol = m2)
        W[is.na(W)] <- 0
        term_preds <- rowSums((mat1 %*% W) * mat2)
      } else {
        mat3 <- mat1[, rep(seq_len(m1), times = m2), drop = FALSE] *
          mat2[, rep(seq_len(m2), each = m1), drop = FALSE]
        term_preds <- mat3 %*% bmat
      }
    }
    if (type == "terms") {
      preds[, i] <- term_preds
    } else {
      preds <- preds + term_preds
    }
  }
  if (type == "response" && !is.null(object$link)) {
    preds <- object$link$linkinv(preds)
  } else if (type == "terms") {
    attr(preds, "constant") <- object$intercept
  }
  if (inherits(naa, "exclude")) {
    preds <- stats::napredict(naa, preds)
  }
  if (type != "terms" && ncol(preds) == 1L)
    preds <- as.vector(preds)
  return(structure(preds, na.action = naa))
}


#' @exportS3Method stats::predict
#'
predict.midlist <- function(
    object, ...
) {
  type <- list(...)$type
  if (!is.null(type) && type == "terms") {
    lapply(X = object, FUN = predict.mid, ...)
  } else if (inherits(object, "midrib")) {
    predict.mid(object, ...)
  } else {
    sapply(X = object, FUN = predict.mid, ...)
  }
}
