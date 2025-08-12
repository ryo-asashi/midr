#' Wrapper Prediction Function
#'
#' \code{get.yhat()} works as a proxy prediction function for many classes of fitted models.
#'
#' \code{get.yhat()} is a wrapper prediction function for many classes of models.
#' Although many predictive models have their own method of \code{stats::predict()}, the structure and the type of the output of these methods are not uniform.
#' \code{get.yhat()} is designed to always return a simple numeric vector of model predictions.
#' The design of \code{get.yhat()} is strongly influenced by \code{DALEX::yhat()}.
#'
#' @param object a fitted model object.
#' @param newdata a data.frame or matrix.
#' @param target an integer or character vector specifying the target levels for the prediction, used for the models that returns a matrix or data.frame of class probabilities. Default is \code{-1}, representing the probability of not being the base level.
#' @param ... optional parameters that are passed to the prediction method for the model.
#' @examples
#' data(trees, package = "datasets")
#' model <- glm(Volume ~ ., trees, family = Gamma(log))
#' predict(model, trees[1:5, ], type = "response")
#' get.yhat(model, trees[1:5, ])
#' @returns
#' \code{get.yhat()} returns a numeric vector of model predictions for the \code{newdata}.
#' @export get.yhat
#'
get.yhat <- function(object, newdata, ...)
UseMethod("get.yhat")


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.default <- function(object, newdata, target = -1L, ...) {
  yhat <- stats::predict(object, newdata, ...)
  if ((is.matrix(yhat) || is.data.frame(yhat)) && ncol(yhat) > 1L)
    yhat <- rowSums(yhat[, target, drop = FALSE])
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.mid <- function(object, newdata, ...) {
  yhat <- stats::predict(object = object, newdata = newdata,
                         type = "response", na.action = "na.pass", ...)
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.lm <- function(object, newdata, ...) {
  yhat <- stats::predict(object = object, newdata = newdata,
                         type = "response", na.action = "na.pass", ...)
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.glm <- function(object, newdata, ...) {
  yhat <- stats::predict(object = object, newdata = newdata,
                         type = "response", na.action = "na.pass", ...)
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.rpart <- function(object, newdata, target = -1L, ...) {
  nclass <- length(attr(object, "ylevels"))
  if (nclass > 0L) {
    yhat <- stats::predict(object = object, newdata = newdata,
                           type = "prob", na.action = "na.pass", ...)
    if (is.matrix(yhat) && ncol(yhat) > 1L)
      yhat <- rowSums(yhat[, target, drop = FALSE])
  } else {
    yhat <- stats::predict(object = object, newdata = newdata,
                           type = "vector", na.action = "na.pass", ...)
  }
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.randomForest <- function(object, newdata, target = -1L, ...) {
  if (object$type == "regression") {
    yhat <- stats::predict(object = object, newdata = newdata, ...)
  } else {
    yhat <- stats::predict(object = object, newdata = newdata,
                           type = "prob", ...)
    if (is.matrix(yhat) && ncol(yhat) > 1L)
      yhat <- rowSums(yhat[, target, drop = FALSE])
  }
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.ranger <- function(object, newdata, target = -1L, ...) {
  if (object$treetype == "Regression") {
    yhat <- stats::predict(object = object, data = newdata, ...)$predictions
  } else if (object$treetype == "Probability estimation") {
    yhat <- stats::predict(object = object, data = newdata, ...)$predictions
    if (is.matrix(yhat) && ncol(yhat) > 1L)
      yhat <- rowSums(yhat[, target, drop = FALSE])
  } else {
    stop("'treetype' must be 'Regression' or 'Probability estimation'")
  }
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.svm <- function(object, newdata, target = -1L, ...) {
  if (object$type == 0) {
    yhat <- stats::predict(
      object = object, newdata = newdata, probability = TRUE,
      na.action = stats::na.exclude, ...
    )
    yhat <- attr(yhat, "probabilities")
    if (is.matrix(yhat) && ncol(yhat) > 1L)
      yhat <- rowSums(yhat[, target, drop = FALSE])
  } else {
    yhat <- stats::predict(object = object, newdata = newdata,
                           na.action = stats::na.exclude, ...)
  }
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.ksvm <- function(object, newdata, target = -1L, ...) {
  dots <- list(...)
  if (na.exists <- anyNA(newdata)) {
    newdata <- stats::na.exclude(newdata)
    naa <- stats::na.action(newdata)
  }
  args <- list(object = object, newdata = newdata)
  if (any(object@type == c("eps-svr", "eps-bsvr", "nu-svr"))) {
    args$type <- "response"
  } else if (any(object@type == c("C-svc", "nu-svc", "C-bsvm", "spoc-svc"))) {
    args$type <- "probabilities"
  }
  for (item in names(dots)) {
    if (item %in% c("object", "newdata", "type"))
      next
    args[[item]] <- dots[[item]]
  }
  yhat <- do.call("predict", args = args)
  if (is.matrix(yhat) && ncol(yhat) > 1L)
    yhat <- rowSums(yhat[, target, drop = FALSE])
  if (na.exists)
    yhat <- stats::napredict(naa, yhat)
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.AccurateGLM <- function(object, newdata, ...) {
  dots <- list(...)
  if (na.exists <- anyNA(newdata)) {
    newdata <- stats::na.exclude(newdata)
    naa <- stats::na.action(newdata)
  }
  predvars <- sapply(object@vars_info, function(x) x$name)
  if (length(setdiff(colnames(newdata), predvars)) > 0L)
    newdata <- newdata[, predvars, drop = FALSE]
  args <- list(object = object, newx = newdata, type = "response")
  for (item in names(dots)) {
    if (item %in% c("object", "newx", "type"))
      next
    args[[item]] <- dots[[item]]
  }
  if (is.null(args$s))
    args$s <- 0
  yhat <- do.call(stats::predict, args)
  if (na.exists)
    yhat <- stats::napredict(naa, yhat)
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.glmnet <- function(object, newdata, ...) {
  dots <- list(...)
  if (na.exists <- anyNA(newdata)) {
    newdata <- stats::na.exclude(newdata)
    naa <- stats::na.action(newdata)
  }
  if (!is.matrix(newdata))
    newdata <- as.matrix(newdata)
  args <- list(object = object, newx = newdata, type = "response")
  for (item in names(dots)) {
    if (item %in% c("object", "newx", "type"))
      next
    args[[item]] <- dots[[item]]
  }
  if (is.null(args$s))
    args$s <- 0
  yhat <- do.call(stats::predict, args)
  if (na.exists)
    yhat <- stats::napredict(naa, yhat)
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.model_fit <- function(object, newdata, target = -1L, ...) {
  if (object$spec$mode == "regression") {
    yhat <- stats::predict(object = object, new_data = newdata,
                           type = "numeric")
    yhat <- yhat$.pred
  } else if (object$spec$mode == "classification") {
    yhat <- stats::predict(object = object, new_data = newdata, type = "prob")
    yhat <- as.matrix(yhat)
    if (is.matrix(yhat) && ncol(yhat) > 1L) {
      colnames(yhat) <- object$lvl
      yhat <- rowSums(yhat[, target, drop = FALSE])
    }
  }
  as.numeric(yhat)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.rpf <- function(object, newdata, target = -1L, ...) {
  if (object$mode == "regression") {
    yhat <- stats::predict(object = object, new_data = newdata,
                           type = "numeric")
    yhat <- yhat$.pred
  } else if (object$mode == "classification") {
    yhat <- stats::predict(object = object, new_data = newdata, type = "prob")
    yhat <- as.matrix(yhat)
    if (is.matrix(yhat) && ncol(yhat) > 1L) {
      colnames(yhat) <- object$lvl
      yhat <- rowSums(yhat[, target, drop = FALSE])
    }
  }
  as.numeric(yhat)
}
