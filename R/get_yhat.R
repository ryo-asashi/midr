#' Get Predicted Values from Fitted Models
#'
#' Returns the predicted values or the target class probabilities of a predictive model for the \code{newdata}.
#'
#' @param X.model a model object to be interpreted.
#' @param newdata a data.frame or a matrix.
#' @param ... other parameters that will be passed to the predict function.
#'
#' @export get.yhat
#'
get.yhat <- function(X.model, newdata, ...)
UseMethod("get.yhat")


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.default <- function(X.model, newdata, ...) {
  dots <- list(...)
  target <- ifnot.null(dots$target, 1L)
  yhat <- try(DALEX::yhat(X.model, newdata, ...), silent = TRUE)
  if (inherits(yhat, "try-error")) {
    yhat <- stats::predict(X.model, newdata, ...)
  }
  if (is.matrix(yhat) || is.data.frame(yhat))
    yhat <- yhat[, target]
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.mid <- function(X.model, newdata, ...) {
  yhat <- stats::predict(object = X.model, newdata = newdata,
                         type = "response", na.action = "na.pass")
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.lm <- function(X.model, newdata, ...) {
  yhat <- stats::predict(object = X.model, newdata = newdata,
                         type = "response", na.action = "na.pass")
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.glm <- function(X.model, newdata, ...) {
  yhat <- stats::predict(object = X.model, newdata = newdata,
                         type = "response", na.action = "na.pass")
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.rpart <- function(X.model, newdata, ...) {
  dots <- list(...)
  target <- ifnot.null(dots$target, 1L)
  nclass <- length(attr(X.model, "ylevels"))
  if (nclass > 0L) {
    yhat <- stats::predict(object = X.model, newdata = newdata,
                           type = "prob", na.action = "na.pass")
    if (is.matrix(yhat))
      yhat <- yhat[, target]
  } else {
    yhat <- stats::predict(object = X.model, newdata = newdata,
                           type = "vector", na.action = "na.pass")
  }
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.randomForest <- function(X.model, newdata, ...) {
  dots <- list(...)
  target <- ifnot.null(dots$target, 1L)
  if (X.model$type == "regression") {
    yhat <- stats::predict(object = X.model, newdata = newdata)
  } else {
    yhat <- stats::predict(object = X.model, newdata = newdata, type = "prob")
    if (is.matrix(yhat))
      yhat <- yhat[, target]
  }
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.ranger <- function(X.model, newdata, ...) {
  dots <- list(...)
  target <- ifnot.null(dots$target, 1L)
  if (na.exists <- anyNA(newdata)) {
    newdata <- stats::na.exclude(newdata)
    naa <- stats::na.action(newdata)
  }
  if (X.model$treetype == "Regression") {
    yhat <- stats::predict(object = X.model, data = newdata)$predictions
  } else if (X.model$treetype == "Probability estimation") {
    yhat <- stats::predict(object = X.model, data = newdata)$predictions
    if (is.matrix(yhat))
      yhat <- yhat[, target]
  } else {
    stop("'treetype' must be 'Regression' or 'Probability estimation'")
  }
  if (na.exists)
    yhat <- stats::napredict(naa, yhat)
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.svm <- function(X.model, newdata, ...) {
  dots <- list(...)
  target <- ifnot.null(dots$target, 1L)
  if (X.model$type == 0) {
    yhat <- stats::predict(object = X.model, newdata = newdata,
                           probability = TRUE, na.action = stats::na.exclude)
    yhat <- attr(yhat, "probabilities")
    if (is.matrix(yhat))
      yhat <- yhat[, target]
  } else {
    yhat <- stats::predict(object = X.model, newdata = newdata,
                           na.action = stats::na.exclude)
  }
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.ksvm <- function(X.model, newdata, ...) {
  dots <- list(...)
  target <- ifnot.null(dots$target, 1L)
  if (any(X.model@type == c("eps-svr", "eps-bsvr", "nu-svr"))) {
    yhat <- kernlab::predict(object = X.model, newdata = newdata,
                             type = "response")
  } else if (any(X.model@type == c("C-svc", "nu-svc", "C-bsvm", "spoc-svc"))) {
    yhat <- kernlab::predict(object = X.model, newdata = newdata,
                             type = "probabilities")
    if (is.matrix(yhat))
      yhat <- yhat[, target]
  }
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.AccurateGLM <- function(X.model, newdata, ...) {
  dots <- list(...)
  s <- ifnot.null(dots$s, 0L)
  if (na.exists <- anyNA(newdata)) {
    newdata <- stats::na.exclude(newdata)
    naa <- stats::na.action(newdata)
  }
  predvars <- sapply(X.model@vars_info, function(x) x$name)
  if (length(setdiff(colnames(newdata), predvars)) > 0L)
    newdata <- newdata[, predvars]
  yhat <- stats::predict(object = X.model, newx = newdata,
                         s = s, type = "response", exact = FALSE)
  if (na.exists)
    yhat <- stats::napredict(naa, yhat)
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.model_fit <- function(X.model, newdata, ...) {
  dots <- list(...)
  target <- ifnot.null(dots$target, 1L)
  if (X.model$spec$mode == "regression") {
    yhat <- stats::predict(object = X.model, new_data = newdata,
                           type = "numeric")
    yhat <- yhat$.pred
  } else if (X.model$spec$mode == "classification") {
    yhat <- stats::predict(object = X.model, new_data = newdata, type = "prob")
    yhat <- as.matrix(yhat)
    if (is.matrix(yhat)) {
      colnames(yhat) <- X.model$lvl
      yhat <- yhat[, target]
    }
  }
  as.numeric(yhat)
}

