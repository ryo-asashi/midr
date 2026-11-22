# R/eval_breakdown.R

#' Evaluate Breakdown of Model Predictions
#'
#' @description
#' \code{eval_breakdown()} calculates the contribution of each component function
#' (or each variable when \code{by = "variable"}) of a fitted model to one
#' or more predictions.
#'
#' @details
#' For a target observation \eqn{\mathbf{x}}, the linear predictor is decomposed
#' into the intercept and individual component contributions:
#'
#' \deqn{g(\mathbf{x}) = g_\emptyset + \sum_{j} g_j(x_j) + \sum_{j<k} g_{jk}(x_j, x_k)}
#'
#' When \code{by = "variable"}, each main effect is attributed to its variable
#' and each interaction effect is split equally among its constituent variables.
#' For a single \code{"mid"} object, passing multiple indices or row names to
#' \code{rows} returns a \code{c("midbrks", "midlist")} collection for comparing
#' breakdowns across observations.
#'
#' @param object a \code{"mid"} or \code{"mids"} object.
#' @param ... additional arguments passed to methods.
#'
#' @returns
#' For a single observation, an object of class \code{"midbrk"} containing
#' \code{breakdown}, \code{data}, \code{intercept}, \code{prediction}, and
#' \code{by} (plus \code{linear.predictor} when a link function is present).
#' For a \code{"mids"} collection or a \code{"mid"} object with multiple \code{rows}
#' selections, a collection of class \code{c("midbrks", "midlist")}.
#'
#' @seealso \code{\link{interpret}}, \code{\link{plot.midbrk}}, \code{\link{ggmid.midbrk}}
#'
#' @export eval_breakdown
#'
eval_breakdown <- function(object, ...)
UseMethod("eval_breakdown")


#' @rdname eval_breakdown
#' @param data a data.frame containing one or more observations. Extracted
#'   automatically from \code{object} when omitted.
#' @param rows optional row index/name (or vector of indices/names for a single
#'   \code{"mid"} object) selecting observation(s) from \code{data}. If omitted
#'   and \code{data} has multiple rows, only the first observation is used.
#' @param sort logical. If \code{TRUE} (default), sorts contributions by descending
#'   absolute magnitude.
#' @param by character: \code{"term"} (default) for component-term breakdown or
#'   \code{"variable"} for variable-level breakdown (splitting interactions equally).
#' @exportS3Method midr::eval_breakdown
#'
eval_breakdown.mid <- function(
    object, data = NULL, rows = NULL, sort = TRUE,
    by = c("term", "variable"), ...
) {
  by <- match.arg(by)
  if (is.null(data)) {
    data <- model_data(object, env = parent.frame())
    if (is.null(data)) {
      stop("'data' must be supplied when model data cannot be recovered", call. = FALSE)
    }
  }
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  multi_rows <- !is.null(rows) && length(rows) > 1L
  if (!is.null(rows)) {
    data <- data[rows, , drop = FALSE]
  }
  if (nrow(data) == 0L) {
    stop("'data' contains no observations to be used", call. = FALSE)
  }
  if (!multi_rows && nrow(data) > 1L) {
    message("'data' contains multiple observations: the first observation is used")
    data <- data[1L, , drop = FALSE]
  }

  preds <- stats::predict(object, newdata = data, type = "terms", na.action = stats::na.pass)
  if (by == "variable") {
    preds <- aggregate_effects_by_variable(preds)
  }
  rdat <- model_reframe(object, data)
  ints <- get_intercept(object)

  if (nrow(preds) == 1L) {
    return(build_midbrk(preds[1L, ], rdat[1L, , drop = FALSE], ints, object$link, by, sort))
  }

  rnames <- rownames(data) %||% as.character(seq_len(nrow(data)))
  out <- lapply(seq_len(nrow(preds)), function(i) {
    build_midbrk(preds[i, ], rdat[i, , drop = FALSE], ints, object$link, by, sort)
  })
  names(out) <- rnames
  class(out) <- c("midbrks", "midlist")
  out
}


#' @rdname eval_breakdown
#' @exportS3Method midr::eval_breakdown
#'
eval_breakdown.mids <- function(
    object, data = NULL, rows = NULL, sort = TRUE,
    by = c("term", "variable"), ...
) {
  by <- match.arg(by)
  if (!is.null(rows) && length(rows) > 1L) {
    stop("'rows' must select a single observation for 'mids' objects", call. = FALSE)
  }
  mods <- as.list(object)
  out <- suppressMessages(lapply(
    X = mods, FUN = eval_breakdown.mid, data = data, rows = rows,
    sort = sort, by = by, ...
  ))
  class(out) <- c("midbrks", "midlist")
  out
}


#' @exportS3Method base::print
#'
print.midbrk <- function(x, digits = max(3L, getOption("digits") - 2L), ...) {
  cat("\nMID Breakdown of a Prediction\n")
  cat("\nIntercept: ", format(x$intercept, digits = digits), "\n", sep = "")
  if (!is.null(x$linear.predictor)) {
    cat("\nLinear Predictor: ", format(x$linear.predictor, digits = digits), "\n", sep = "")
  } else {
    cat("\nPrediction: ", format(x$prediction, digits = digits), "\n", sep = "")
  }
  cat("\nBreakdown of Effects:\n")
  print.data.frame(x$breakdown, digits = digits, ...)
  invisible(x)
}


#' @exportS3Method base::print
#'
print.midbrks <- function(x, digits = max(3L, getOption("digits") - 2L), n = 20L, ...) {
  objs <- as.list(x)
  cat("\nMID Breakdown of Predictions\n")
  intercept <- vapply(objs, function(y) y$intercept, numeric(1L))
  cat("\nIntercept: ", examples(intercept, digits = digits), "\n", sep = "")
  if (!is.null(objs[[1L]]$linear.predictor)) {
    lp <- vapply(objs, function(y) y$linear.predictor, numeric(1L))
    cat("\nLinear Predictor: ", examples(lp, digits = digits), "\n", sep = "")
  } else {
    pred <- vapply(objs, function(y) y$prediction, numeric(1L))
    cat("\nPrediction: ", examples(pred, digits = digits), "\n", sep = "")
  }
  cat("\nBreakdown of Effects:\n")
  smry <- summary(x)
  print.data.frame(utils::head(smry, n), digits = digits, ...)
  invisible(x)
}


#' @exportS3Method base::summary
#'
summary.midbrk <- function(object, ...) {
  object$breakdown
}


#' @exportS3Method base::summary
#'
summary.midbrks <- function(object, ...) {
  summarize_midlist(
    object = object, shape = "wide",
    dfname = "breakdown", keycol = "term", valcol = "mid",
    keys = "term", fill = 0
  )
}


#' @exportS3Method base::as.data.frame
#'
as.data.frame.midbrk <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$breakdown
}


#' @exportS3Method base::as.data.frame
#'
as.data.frame.midbrks <- function(x, row.names = NULL, optional = FALSE, ...) {
  summarize_midlist(
    object = x, shape = "long",
    dfname = "breakdown", keycol = "term", valcol = "mid",
    keys = "term"
  )
}


# internal helpers ----

build_midbrk <- function(vals, row_dat, ints, link, by, sort) {
  terms <- names(if (isTRUE(sort)) base::sort(abs(vals), decreasing = TRUE) else vals)
  df <- data.frame(
    term = factor(terms, levels = rev(terms)),
    mid = unname(vals[terms]),
    stringsAsFactors = FALSE
  )
  if (by == "term") {
    df$order <- as.factor(vapply(
      terms,
      function(t) length(get_variables(t)),
      integer(1L)
    ))
  }
  lp <- ints + sum(vals)
  out <- list(
    breakdown = df,
    data      = row_dat,
    intercept = ints,
    by        = by
  )
  if (!is.null(link)) {
    out$linear.predictor <- lp
    out$prediction <- link$linkinv(lp)
  } else {
    out$prediction <- lp
  }
  attr(out, "term.labels") <- terms
  class(out) <- "midbrk"
  out
}
