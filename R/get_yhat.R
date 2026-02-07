#' Wrapper Prediction Function
#'
#' @description
#' \code{get.yhat()} is a generic function that provides a unified interface for obtaining predictions from various fitted model objects.
#'
#' @details
#' While many predictive models have a \code{stats::predict()} method, the structure and type of their outputs are not uniform.
#' For example, some return a numeric vector, others a matrix of class probabilities, and some a list.
#' This function, \code{get.yhat()}, abstracts away this complexity.
#'
#' For regression models, it returns the numeric prediction in the original scale of the response variable.
#' For classification models, it returns the sum of class probabilities for the classes specified by the \code{target} argument.
#'
#' Furthermore, \code{get.yhat()} provides more consistent handling of missing values.
#' While some \code{stats::predict()} methods may return a shorter vector by omitting \code{NA}s, \code{get.yhat()} is designed to return a vector of the same length as \code{newdata}, preserving \code{NA}s in their original positions.
#'
#' The design of \code{get.yhat()} is strongly influenced by \code{DALEX::yhat()}.
#'
#' @param object a fitted model object.
#' @param newdata a data.frame or matrix.
#' @param target an integer or character vector specifying the target levels used for the classification models that return a matrix or data frame of class probabilities. The default, \code{-1}, represents the probability of not being the base level.
#' @param ... optional arguments passed on to the underlying \code{predict()} method for the \code{object}'s class.
#'
#' @examples
#' data(trees, package = "datasets")
#' model <- glm(Volume ~ ., trees, family = Gamma(log))
#'
#' # The output of stats::predict() might not be in the scale of the response variable
#' predict(model, trees[1:5, ])
#'
#' # get.yhat() returns a numeric vector in the original scale of the response variable
#' get.yhat(model, trees[1:5, ])
#' predict(model, trees[1:5, ], type = "response")
#' @returns
#' \code{get.yhat()} returns a numeric vector of model predictions for \code{newdata}.
#'
#' @seealso \code{\link{predict.mid}}
#'
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
get.yhat.workflow <- function(object, newdata, target = -1L, ...) {
  fit <- object$fit$fit
  if (fit$spec$mode == "regression") {
    yhat <- stats::predict(object = object, new_data = newdata,
                           type = "numeric")
    yhat <- yhat$.pred
  } else if (fit$spec$mode == "classification") {
    yhat <- stats::predict(object = object, new_data = newdata, type = "prob")
    yhat <- as.matrix(yhat)
    if (is.matrix(yhat) && ncol(yhat) > 1L) {
      colnames(yhat) <- fit$lvl
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


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.bundle <- function(
    object, newdata, ...
) {
  dots <- list(...)
  m <- length(object)
  res <- matrix(0, nrow = nrow(newdata), ncol = m)
  nms <- names(object) %||% vapply(object, function(x) class(x)[1L], "")
  colnames(res) <- nms
  for (i in seq_len(m)) {
    name <- nms[i]
    pred.fun <- dots[[paste0("pred.fun.", name)]] %||%
      dots[["pred.fun"]] %||% get.yhat
    pred.args <- dots[[paste0("pred.args.", name)]] %||%
      dots[["pred.args"]] %||% dots
    res[, i] <- do.call(pred.fun, c(list(object[[i]], newdata), pred.args))
  }
  res
}
