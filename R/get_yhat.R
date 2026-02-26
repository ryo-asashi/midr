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
#' @param ... optional named arguments passed on to the underlying \code{predict()} method for the \code{object}'s class.
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
get.yhat <- function(object, newdata, ..., target)
UseMethod("get.yhat")


checkout <- function(yhat, newdata, target = NULL, exclude = NULL) {
  if (!is.numeric(yhat))
    stop("non-numeric predictions found: ", examples(yhat))
  out <- if (NCOL(yhat) == 1L) {
    as.vector(yhat)
  } else if (is.null(target)) {
    as.matrix(yhat)
  } else {
    rowSums(yhat[, target, drop = FALSE])
  }
  out <- stats::napredict(exclude, out)
  if (NROW(out) != NROW(newdata))
    stop("number of predictions doesn't match number of observations")
  out
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.default <- function(
    object, newdata, ..., target = -1L
  ) {
  args <- list(object, newdata)
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  checkout(do.call(stats::predict, args), newdata, target)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.lm <- function(
    object, newdata, ...
  ) {
  args <- list(object = object, newdata = newdata, type = "response",
               na.action = stats::na.pass)
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  checkout(do.call(stats::predict, args), newdata)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.glm <- function(
    object, newdata, ...
  ) {
  args <- list(object = object, newdata = newdata, type = "response",
               na.action = stats::na.pass)
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  checkout(do.call(stats::predict.glm, args), newdata)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.mid <- function(
    object, newdata, ...
  ) {
  args <- list(object = object, newdata = newdata, type = "response",
               na.action = stats::na.pass)
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  checkout(do.call(predict.mid, args), newdata)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.mids <- function(
    object, newdata, ..., target = NULL
  ) {
  args <- list(object = object, newdata = newdata, type = "response",
               na.action = stats::na.pass)
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  checkout(do.call(predict.mids, args), newdata, target)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.rpart <- function(
    object, newdata, ..., target = -1L
  ) {
  args <- list(object = object, newdata = newdata, na.action = stats::na.pass)
  nclass <- length(attr(object, "ylevels"))
  args$type <- if (nclass > 0L) "prob" else "vector"
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  checkout(do.call(stats::predict, args), newdata, target)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.randomForest <- function(
    object, newdata, ..., target = -1L
  ) {
  args <- list(object = object, newdata = newdata)
  args$type <- if (object$type == "regression") "response" else "prob"
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  checkout(do.call(stats::predict, args), newdata, target)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.ranger <- function(
    object, newdata, ..., target = -1L
  ) {
  treetype <- object$treetype
  args <- list(object = object, data = newdata, type = "response")
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  pred <- do.call(stats::predict, args)
  if (treetype == "Survival") {
    target <- NULL
    yhat <- pred$survival
    colnames(yhat) <- pred$unique.death.times
  } else {
    yhat <- pred$predictions
  }
  checkout(yhat, newdata, target)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.rfsrc <- function(
    object, newdata, ..., target = -1L
) {
  args <- list(object = object, newdata = newdata, na.action = "na.impute")
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  pred <- do.call(stats::predict, args)
  if (inherits(object, "surv")) {
    target <- NULL
    yhat <- pred$survival
    colnames(yhat) <- pred$time.interest
  } else {
    yhat <- pred$predicted
  }
  checkout(yhat, newdata, target)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.svm <- function(object, newdata, target = -1L, ...) {
  args <- list(object = object, newdata = newdata,
               na.action = stats::na.exclude)
  args$probability <- (object$type < 3L)
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  yhat <- do.call(stats::predict, args)
  if (args$probability)
    yhat <- attr(yhat, "probabilities")
  checkout(yhat, newdata, target)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.ksvm <- function(
    object, newdata, ..., target = -1L
  ) {
  newdata <- stats::na.exclude(newdata)
  exclude <- stats::na.action(newdata)
  args <- list(object = object, newdata = newdata)
  args$type <- if (any(object@type == c("eps-svr", "eps-bsvr", "nu-svr"))) {
    "response"
  } else if (any(object@type == c("C-svc", "nu-svc", "C-bsvm", "spoc-svc"))) {
    "probabilities"
  }
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  checkout(do.call("predict", args), newdata, target, exclude)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.AccurateGLM <- function(
    object, newdata, ..., target = -1L
  ) {
  newdata <- stats::na.exclude(newdata)
  exclude <- stats::na.action(newdata)
  predvars <- sapply(object@vars_info, function(x) x$name)
  if (length(setdiff(colnames(newdata), predvars)) > 0L)
    newdata <- newdata[, predvars, drop = FALSE]
  args <- list(object = object, newx = newdata, type = "response", s = 0)
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  checkout(do.call(stats::predict, args), newdata, target, exclude)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.glmnet <- function(
    object, newdata, ..., target = -1L
  ) {
  newdata <- stats::na.exclude(newdata)
  exclude <- stats::na.action(newdata)
  newdata <- as.matrix(newdata)
  args <- list(object = object, newx = newdata, type = "response", s = 0)
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  checkout(do.call(stats::predict, args), newdata, target, exclude)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.model_fit <- function(
    object, newdata, ..., target = -1L
  ) {
  mode <- object$spec$mode
  args <- list(object = object, new_data = newdata)
  args$type <- switch(mode, "regression" = "numeric",
                      "classification" = "prob", "survival")
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  pred <- do.call(stats::predict, args)
  yhat <- if (args$type == "numeric") {
    pred$.pred
  } else if (any(args$type == c("prob", "survival", "hazard", "quantile"))) {
    as.matrix(pred$.pred)
  }
  checkout(yhat, newdata, target)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.workflow <- function(
    object, newdata, ..., target = -1L
  ) {
  mode <- object$fit$fit$spec$mode
  args <- list(object = object, new_data = newdata)
  args$type <- switch(mode, "regression" = "numeric", "classification" = "prob")
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  pred <- do.call(stats::predict, args)
  yhat <- if (args$type == "numeric") {
    pred$.pred
  } else if (any(args$type == c("prob", "survival", "hazard", "quantile"))) {
    as.matrix(pred$.pred)
  }
  checkout(yhat, newdata, target)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.rpf <- function(
    object, newdata, ..., target = -1L
  ) {
  mode <- object$mode
  args <- list(object = object, new_data = newdata)
  args$type <- switch(mode, "regression" = "numeric", "classification" = "prob")
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  pred <- do.call(stats::predict, args)
  yhat <- if (args$type == "numeric") {
    pred$.pred
  } else if (any(args$type == c("prob", "survival", "hazard", "quantile"))) {
    as.matrix(pred$.pred)
  }
  checkout(yhat, newdata, target)
}


#' @rdname get.yhat
#' @exportS3Method midr::get.yhat
#'
get.yhat.fitlist <- function(
    object, newdata, ..., target = -1L
) {
  out <- lapply(
    X = object, FUN = get.yhat, newdata = newdata, ..., target = target
  )
  names(out) <- names(object) %||% vapply(object, function(x) class(x)[1L], "")
  checkout(as.matrix(do.call(cbind, out)), newdata)
}
