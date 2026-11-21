# R/get_yhat.R

#' Wrapper Prediction Function
#'
#' @description
#' \code{get_yhat()} is a generic function that provides a unified interface for obtaining predictions from various fitted model objects.
#'
#' @details
#' While many predictive models have a \code{stats::predict()} method, the structure and type of their outputs are not uniform.
#' For example, some return a numeric vector, others a matrix of class probabilities, and some a list.
#' This function, \code{get_yhat()}, abstracts away this complexity.
#'
#' For regression models, it returns the numeric prediction in the original scale of the response variable.
#' For classification models, it returns the sum of class probabilities for the classes specified by the \code{target} argument.
#'
#' Furthermore, \code{get_yhat()} provides more consistent handling of missing values.
#' While some \code{stats::predict()} methods may return a shorter vector by omitting \code{NA}s, \code{get_yhat()} is designed to return a vector of the same length as \code{newdata}, preserving \code{NA}s in their original positions.
#'
#' The design of \code{get_yhat()} is strongly influenced by \code{DALEX::yhat()}.
#'
#' @param object a fitted model object.
#' @param newdata a data.frame or matrix.
#' @param target an integer or character vector specifying the target columns used for the models that return a matrix or data frame: e.g., for classification models, \code{-1} represents the probability of not being the base level. Defaults to \code{NULL}.
#' @param ... optional named arguments passed on to the underlying \code{predict()} method for the \code{object}'s class.
#'
#' @examples
#' data(trees, package = "datasets")
#' model <- glm(Volume ~ ., trees, family = Gamma(log))
#'
#' # The output of stats::predict() might not be in the scale of the response variable
#' predict(model, trees[1:5, ])
#'
#' # get_yhat() returns a numeric vector in the original scale of the response variable
#' get_yhat(model, trees[1:5, ])
#' predict(model, trees[1:5, ], type = "response")
#' @returns
#' \code{get_yhat()} returns a numeric vector or matrix of model predictions for \code{newdata}.
#'
#' @seealso \code{\link{predict.mid}}
#'
#' @export get_yhat
#'
get_yhat <- function(object, newdata, ..., target)
UseMethod("get_yhat")


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
  if (NROW(out) != NROW(newdata)) {
    stop("number of predictions doesn't match number of observations")
  }
  out
}


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.default <- function(
    object, newdata, ..., target = NULL
) {
  args <- list(object, newdata)
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  checkout(do.call(stats::predict, args), newdata, target)
}


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.lm <- function(
    object, newdata, ...
) {
  args <- list(object = object, newdata = newdata, type = "response",
               na.action = stats::na.pass)
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  checkout(do.call(stats::predict, args), newdata)
}


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.glm <- get_yhat.lm


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.gam <- get_yhat.lm


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.mid <- get_yhat.lm


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.mids <- function(
    object, newdata, ..., target = NULL
) {
  args <- list(object = object, newdata = newdata, type = "response",
               na.action = stats::na.pass)
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  checkout(do.call(stats::predict, args), newdata, target)
}


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.rpart <- function(
    object, newdata, ..., target = NULL
) {
  args <- list(object = object, newdata = newdata, na.action = stats::na.pass)
  args$type <- if (identical(object$method, "class")) "prob" else "vector"
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  checkout(do.call(stats::predict, args), newdata, target)
}


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.randomForest <- function(
    object, newdata, ..., target = NULL
) {
  args <- list(object = object, newdata = newdata)
  args$type <- if (identical(object$type, "regression")) "response" else "prob"
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  checkout(do.call(stats::predict, args), newdata, target)
}


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.ranger <- function(
    object, newdata, ..., target = NULL
) {
  treetype <- object$treetype
  args <- list(object = object, data = newdata, type = "response")
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  pred <- do.call(stats::predict, args)
  if (identical(treetype, "Survival")) {
    target <- NULL
    yhat <- pred$survival
    colnames(yhat) <- pred$unique.death.times
  } else {
    yhat <- pred$predictions
  }
  checkout(yhat, newdata, target)
}


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.rfsrc <- function(
    object, newdata, ..., target = NULL
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


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.ObliqueForest <- function(
    object, newdata, ..., target = NULL
) {
  .newdata <- stats::na.exclude(newdata)
  exclude <- stats::na.action(.newdata)
  args <- list(object = object, new_data = .newdata)
  if (inherits(object, "ObliqueForestSurvival")) {
    target <- NULL
    args$pred_type <- "surv"
    args$pred_horizon <- args$pred_horizon %||% object$event_times
  } else if (inherits(object, "ObliqueForestClassification")) {
    args$pred_type <- "prob"
  } else {
    args$pred_type <- "mean"
  }
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  yhat <- do.call(stats::predict, args)
  if (identical(args$pred_type, "surv"))
    colnames(yhat) <- args$pred_horizon
  checkout(yhat, newdata, target, exclude)
}


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.svm <- function(
    object, newdata, ..., target = NULL
) {
  args <- list(object = object, newdata = newdata,
               na.action = stats::na.exclude)
  args$probability <- (object$type < 3L)
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  yhat <- do.call(stats::predict, args)
  if (args$probability)
    yhat <- attr(yhat, "probabilities")
  checkout(yhat, newdata, target)
}


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.ksvm <- function(
    object, newdata, ..., target = NULL
) {
  .newdata <- stats::na.exclude(newdata)
  exclude <- stats::na.action(.newdata)
  args <- list(object = object, newdata = .newdata)
  args$type <- if (any(object@type == c("eps-svr", "eps-bsvr", "nu-svr"))) {
    "response"
  } else if (any(object@type == c("C-svc", "nu-svc", "C-bsvm", "spoc-svc"))) {
    "probabilities"
  }
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  fun <- getExportedValue("kernlab", "predict")
  checkout(do.call(fun, args), newdata, target, exclude)
}


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.AccurateGLM <- function(
    object, newdata, ..., target = NULL
) {
  .newdata <- stats::na.exclude(newdata)
  exclude <- stats::na.action(.newdata)
  predvars <- sapply(object@vars_info, function(x) x$name)
  if (length(setdiff(colnames(.newdata), predvars)) > 0L)
    .newdata <- .newdata[, predvars, drop = FALSE]
  args <- list(object = object, newx = .newdata, type = "response", s = 0)
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  checkout(do.call(stats::predict, args), newdata, target, exclude)
}


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.glmnet <- function(
    object, newdata, ..., target = NULL
) {
  .newdata <- as.matrix(stats::na.exclude(newdata))
  exclude <- stats::na.action(.newdata)
  s <- min(object$lambda)
  if (inherits(object, "coxnet")) {
    target <- NULL
    args <- list(formula = object, newx = .newdata, s = s, se.fit = FALSE)
    args <- utils::modifyList(args, list(...), keep.null = TRUE)
    fun <- getExportedValue("survival", "survfit")
    sfit <- do.call(fun, args)
    yhat <- t(sfit$surv)
    colnames(yhat) <- sfit$time
  } else {
    args <- list(object = object, newx = .newdata, type = "response", s = s)
    args <- utils::modifyList(args, list(...), keep.null = TRUE)
    yhat <- do.call(stats::predict, args)
  }
  checkout(yhat, newdata, target, exclude)
}


predict_parsnip <- function(object, newdata, mode, ..., target = NULL) {
  args <- list(object = object, new_data = newdata)
  args$type <- switch(
    mode,
    "numeric",
    "regression" = "numeric",
    "classification" = "prob",
    "censored regression" = "survival"
  )
  dots <- list(...)
  if (args$type %in% c("survival", "hazard")) {
    if (is.null(dots$eval_time)) {
      message("'eval_time' is not provided: defaults to seq(0, 365, by = 5)")
      dots$eval_time <- seq(0L, 365L, by = 5L)
    }
  }
  args <- utils::modifyList(args, dots, keep.null = TRUE)
  pred <- do.call(stats::predict, args)
  if (args$type == "numeric") {
    yhat <- pred$.pred
  } else if (args$type == "survival") {
    target <- NULL
    yhat <- t(sapply(pred$.pred, function(x) x$.pred_survival))
    colnames(yhat) <- pred$.pred[[1L]]$.eval_time
  } else if (args$type == "hazard") {
    target <- NULL
    yhat <- t(sapply(pred$.pred, function(x) x$.pred_hazard))
    colnames(yhat) <- pred$.pred[[1L]]$.eval_time
  } else if (args$type %in% c("prob", "quantile")) {
    yhat <- as.matrix(pred)
  }
  checkout(yhat, newdata, target)
}


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.model_fit <- function(
    object, newdata, ..., target = NULL
) {
  predict_parsnip(object, newdata, mode = object$spec$mode, ..., target = target)
}


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.workflow <- function(
    object, newdata, ..., target = NULL
) {
  predict_parsnip(object, newdata, mode = object$fit$fit$spec$mode, ..., target = target)
}


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.rpf <- function(
    object, newdata, ..., target = NULL
) {
  predict_parsnip(object, newdata, mode = object$mode, ..., target = target)
}


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.coxph <- function(
    object, newdata, ...
) {
  .newdata <- stats::na.exclude(newdata)
  exclude <- stats::na.action(.newdata)
  args <- list(formula = object, newdata = .newdata, se.fit = FALSE)
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  fun <- getExportedValue("survival", "survfit")
  sfit <- do.call(fun, args)
  yhat <- t(sfit$surv)
  colnames(yhat) <- sfit$time
  checkout(yhat, newdata, exclude = exclude)
}


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.flexsurvreg <- function(
    object, newdata, ...
) {
  args <- list(object = object, newdata = newdata, type = "survival")
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  if (args$type %in% c("survival", "hazard", "cumhaz"))
    args[["t"]] <- args[["t"]] %||% sort(unique(object$data$Y[, 1L]))
  surv <- do.call(summary, args)
  yhat <- do.call(rbind, lapply(surv, function(x) x$est))
  rownames(yhat) <- NULL
  if (!is.null(args$t) && ncol(yhat) == length(args$t))
    colnames(yhat) <- args$t
  checkout(yhat, newdata)
}


#' @rdname get_yhat
#' @exportS3Method midr::get_yhat
#'
get_yhat.mboost <- function(
    object, newdata, ..., target = NULL
) {
  args <- list(object = object, newdata = newdata)
  if (grepl("Cox|Weibull|Log-normal|Log-logistic|PropOdds",
            object$family@name, ignore.case = TRUE))
    args$type <- "survival"
  args <- utils::modifyList(args, list(...), keep.null = TRUE)
  if (identical(args$type, "survival")) {
    target <- NULL
    args$type <- NULL
    sfun <- getExportedValue("mboost", "survFit")
    sfit <- do.call(sfun, args)
    yhat <- t(sfit$surv)
    colnames(yhat) <- sfit$time
  } else {
    yhat <- do.call(stats::predict, args)
  }
  checkout(yhat, newdata, target)
}
