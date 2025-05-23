#' Predict Method for fitted MID Models
#'
#' The method of \code{predict()} for "mid" objects obtains predictions from a fitted MID model.
#'
#' The S3 method of \code{predict()} for MID models returns the model predictions.
#' \code{mid.f()} works as a component function of a MID model.
#'
#' @param object a "mid" object to be used to make predictions.
#' @param newdata a data frame of the new observations.
#' @param na.action a function or character string specifying what should happen when the data contain \code{NA}s.
#' @param type the type of prediction required. The default is on the scale of the response varialbe. The alternative "link" is on the scale of the linear predictors. The "terms" option returns a matrix giving the fitted values of each term in the model formula on the linear predictor scale.
#' @param terms a character vector of term labels, specifying a subset of component functions to be used to make predictions.
#' @param ... not used.
#' @examples
#' data(trees, package = "datasets")
#' idx <- c(5L, 10L, 15L, 20L, 25L, 30L)
#' mid <- interpret(Volume ~ .^2, trees[-idx,], lambda = 1)
#' trees[idx, "Volume"]
#' predict(mid, trees[idx,])
#' predict(mid, trees[idx,], type = "terms")
#' mid.f(mid, "Girth", trees[idx,])
#' mid.f(mid, "Girth:Height", trees[idx,])
#' predict(mid, trees[idx,], terms = c("Girth", "Height"))
#' @returns
#' \code{predict.mid()} returns a numeric vector of MID model predictions.
#' @exportS3Method stats::predict
#'
predict.mid <- function(
    object, newdata = NULL, na.action = "na.pass",
    type = c("response", "link", "terms"), terms = object$terms, ...) {
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
    if (any("mid" == colnames(newdata)))
      colnames(newdata)[colnames(newdata) == "mid"] <- ".mid"
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
      mmats[[tag]] <- object$encoders[["main.effects"]][[tag]]$encode(newdata[, tag])
    imats <- list()
    for (tag in unique(term.split(its)))
      imats[[tag]] <- object$encoders[["interactions"]][[tag]]$encode(newdata[, tag])
    for (i in seq_len(r)) {
      term <- terms[i]
      tags <- term.split(term)
      if (length(tags) == 1L) {
        X <- mmats[[term]]
        mid <- object$main.effects[[term]]$mid
      } else if (length(tags) == 2L) {
        n1 <- object$encoders[["interactions"]][[tags[1L]]]$n
        n2 <- object$encoders[["interactions"]][[tags[2L]]]$n
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

#' @param term a character string specifying the component function of a fitted MID model.
#' @param x a matrix, data frame or vector to be used as the input to the first argument of the component function. If a matrix or data frame is passed, inputs for both \code{x} and \code{y} are extracted from it.
#' @param y a vector to be used as the input to the second argument of the component function.
#' @rdname predict.mid
#' @export mid.f
#'
mid.f <- function(object, term, x, y = NULL) {
  tags <- term.split(term)
  ie <- length(tags) == 2L
  if (is.matrix(x) || is.data.frame(x)) {
    if (ie)
      y <- x[, tags[2L]]
    x <- x[, tags[1L]]
  }
  n <- length(x)
  .term <- term.check(term, object$terms, stop = FALSE)
  if (is.na(.term))
    return(numeric(n))
  if (!ie) {
    enc <- object$encoders[["main.effects"]][[.term]]
    X <- enc$encode(x)
    mid <- object$main.effects[[.term]]$mid
  } else {
    if (length(x) != length(y))
      stop("x and y must have the same length")
    encs <- list(object$encoders[["interactions"]][[tags[1L]]],
                 object$encoders[["interactions"]][[tags[2L]]])
    mats <- list(encs[[1L]]$encode(x), encs[[2L]]$encode(y))
    lr <- if (term == .term) 1L:2L else 2L:1L
    uni <- as.matrix(expand.grid(1L:encs[[lr[1L]]]$n, 1L:encs[[lr[2L]]]$n))
    X <- matrix(0, nrow = n, ncol = nrow(uni))
    for (j in seq_len(nrow(uni))) {
      X[, j] <- as.numeric(mats[[lr[1L]]][, uni[j, 1L]] *
                           mats[[lr[2L]]][, uni[j, 2L]])
    }
    mid <- object$interactions[[.term]]$mid
  }
  mid[is.na(mid)] <- 0
  as.numeric(X %*% mid)
}
