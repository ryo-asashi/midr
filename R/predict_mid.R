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
  if (!missing(terms))
    terms <- unique(terms)
  if (is.null(newdata)) {
    preds <- object$fitted.matrix
    naa <- stats::na.action(object)
  } else {
    if ("mid" %in% colnames(newdata))
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
    preds <- matrix(0, nrow = n, ncol = r, dimnames = list(NULL, terms))
    for (i in seq_len(r)) {
      term <- terms[i]
      preds[, i] <- mid.f(object, term, x = newdata)
    }
    attr(preds, "constant") <- object$intercept
  }
  if (type == "terms") {
    if (inherits(naa, "exclude"))
      return(stats::napredict(naa, preds))
    return(structure(preds, na.action = naa))
  }
  preds <- apply(preds, MARGIN = 1L, FUN = sum) + object$intercept
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
  if (!term %in% object$terms) {
    message(paste0("The term '", term, "' does not exist."))
    return(0)
  }
  tags <- unlist(strsplit(term, ":"))
  ie <- length(tags) == 2L
  if (is.matrix(x) || is.data.frame(x)) {
    if (ie)
      y <- x[, tags[2L]]
    x <- x[, tags[1L]]
  }
  n <- length(x)
  if (!ie) {
    # main effect
    enc <- object$me.encoders[[term]]
    X <- matrix(0, nrow = n, ncol = enc$n)
    for (j in seq_len(enc$n)) {
      X[, j] <- enc$encode(x, j)
    }
    mid <- object$main.effects[[term]]$mid
  } else {
    # interaction
    if (length(x) != length(y))
      stop("x and y must have the same length.")
    enc1 <- object$ie.encoders[[tags[1]]]
    enc2 <- object$ie.encoders[[tags[2]]]
    uni <- as.matrix(expand.grid(1:enc1$n, 1:enc2$n))
    X <- matrix(0, nrow = n, ncol = nrow(uni))
    for (j in seq_len(nrow(uni))) {
      val <- uni[j, ]
      X[, j] <- as.numeric(enc1$encode(x, val[1]) * enc2$encode(y, val[2]))
    }
    mid <- object$interactions[[term]]$mid
  }
  mid[is.na(mid)] <- 0
  as.numeric(X %*% mid)
}
