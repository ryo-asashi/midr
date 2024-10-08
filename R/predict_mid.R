#' Predict Method for MID-based Surrogate Models
#'
#' Returns predictions of the fitted mid object and optionally breakdown of those predictions into the functional decomposition terms.
#'
#' @param object a mid object to be used as a surrogate model.
#' @param newdata data frame for which the predictions are to be made.
#' @param na.action a function which indicates what should happen when the data contain missing values (NAs).
#' @param type the type of prediction required. The default is on the scale of the response varialbe; the alternative "link" is on the scale of the linear predictors. The "terms" option returns a matrix giving the fitted values of each term in the model formula on the linear predictor scale.
#' @param terms a character vector, specifying names of the terms to be used to make predictions.
#' @param ... not used.
#'
#' @exportS3Method stats::predict
#'
predict.mid <- function(
    object, newdata = NULL, na.action = getOption("na.action"),
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
    if (!is.null(formula <- eval(object$call$formula))) {
      newdata[, deparse(formula[[2L]])] <- 0L
      newdata <- stats::model.frame(formula = formula,
                                    data = newdata, na.action = NULL)
    }
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
      mmats[[tag]] <- object$me.encoders[[tag]]$encode(newdata[, tag])
    imats <- list()
    for (tag in unique(term.split(its)))
      imats[[tag]] <- object$ie.encoders[[tag]]$encode(newdata[, tag])
    for (i in seq_len(r)) {
      term <- terms[i]
      tags <- term.split(term)
      if (length(tags) == 1L) {
        X <- mmats[[term]]
        mid <- object$main.effects[[term]]$mid
      } else if (length(tags) == 2L) {
        n1 <- object$ie.encoders[[tags[1L]]]$n
        n2 <- object$ie.encoders[[tags[2L]]]$n
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
    attr(preds, "constant") <- object$intercept
  }
  if (type == "terms") {
    if (inherits(naa, "exclude"))
      return(stats::napredict(naa, preds))
    return(structure(preds, na.action = naa))
  }
  preds <- rowSums(preds) + object$intercept
  if (type == "response" && !is.null(object$link))
    preds <- object$link$linkinv(preds)
  if (inherits(naa, "exclude"))
    return(stats::napredict(naa, preds))
  structure(as.numeric(preds), na.action = naa)
}

#' @param term a character, specifying the name of the term of the decomposed function.
#' @param x a vector to be used as inputs to the first argument of the decomposed function. If a matrix or data.frame is passed, inputs are extracted from the matrix or data.frame.
#' @param y a vector to be used as inputs to the second argument of the decomposed interaction function.
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
    enc <- object$me.encoders[[.term]]
    X <- enc$encode(x)
    mid <- object$main.effects[[.term]]$mid
  } else {
    if (length(x) != length(y))
      stop("x and y must have the same length")
    encs <- list(object$ie.encoders[[tags[1L]]],
                 object$ie.encoders[[tags[2L]]])
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
