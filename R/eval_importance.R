# R/eval_importance.R

#' Evaluate Importance of Model Components
#'
#' @description
#' \code{eval_importance()} calculates the term/variable importance of a fitted model.
#' This measures the average contribution of each component function, or each
#' variable when \code{by = "variable"}, across a dataset.
#'
#' @details
#' For a component function \eqn{g_S}, the MID importance is defined as the
#' mean absolute effect (\code{measure = "mae"}, default), root mean square
#' effect (\code{"rmse"}), or median absolute effect (\code{"medae"}) over the
#' observations in \code{data}:
#'
#' \deqn{\mathbf{I}(g_S) = \frac{1}{n} \sum_{i=1}^n |g_S(\mathbf{x}_{i,S})|}
#'
#' When \code{by = "variable"}, each main effect is attributed to its variable
#' and each interaction effect is split equally among its constituent variables
#' before computing the importance summary.
#'
#' @param object a \code{"mid"} or \code{"mids"} object.
#' @param ... additional arguments passed to methods.
#'
#' @returns
#' For a \code{"mid"} object, an object of class \code{"midimp"} containing
#' \code{importance}, \code{predictions}, \code{measure}, and \code{by}.
#' For a \code{"mids"} collection, a collection of class \code{c("midimps", "midlist")}.
#'
#' @seealso \code{\link{interpret}}, \code{\link{plot.midimp}}, \code{\link{ggmid.midimp}}
#'
#' @export eval_importance
#'
eval_importance <- function(object, ...)
UseMethod("eval_importance")


#' @rdname eval_importance
#' @param data a data.frame containing observations for computing importance.
#'   Extracted automatically from \code{object} when omitted.
#' @param weights an optional numeric vector of sample weights.
#' @param sort logical. If \code{TRUE} (default), sorts output in descending order of importance.
#' @param measure character (or integer \code{1}--\code{3} for backward compatibility):
#'   \code{"mae"} (or \code{1}) for mean absolute effect, \code{"rmse"} (or \code{2})
#'   for root mean square effect, or \code{"medae"} (or \code{3}) for median absolute effect.
#' @param by character: \code{"term"} (default) for component-term importance or
#'   \code{"variable"} for variable-level importance (splitting interactions equally).
#' @param max.nsamples maximum number of rows to retain in \code{predictions}.
#'   Larger datasets are randomly subsampled (weighted if \code{weights} is supplied).
#' @param seed optional integer seed for subsampling.
#' @exportS3Method midr::eval_importance
#'
eval_importance.mid <- function(
    object, data = NULL, weights = NULL, sort = TRUE,
    measure = c("mae", "rmse", "medae"), by = c("term", "variable"),
    max.nsamples = 1e4L, seed = NULL, ...
) {
  by <- match.arg(by)
  measure <- parse_importance_measure(measure)

  if (is.null(data)) {
    data <- model_data(object, env = parent.frame())
    if (is.null(data)) {
      stop("'data' must be supplied when model data cannot be recovered", call. = FALSE)
    }
    if (is.null(weights)) {
      weights <- stats::model.weights(data) %||% object$weights
    }
  } else if (is.null(weights)) {
    weights <- stats::model.weights(data)
  }

  preds <- stats::predict(object, newdata = data, type = "terms", na.action = stats::na.pass)
  if (!is.null(weights)) {
    if (length(weights) != nrow(preds)) {
      stop("length of 'weights' must match the number of rows in 'data'", call. = FALSE)
    }
    if (diff(range(weights, na.rm = TRUE)) == 0) {
      weights <- NULL
    }
  }

  if (by == "variable") {
    preds <- aggregate_effects_by_variable(preds)
  }

  n <- nrow(preds)
  fun <- switch(
    measure,
    mae   = weighted_mae,
    rmse  = weighted_rmse,
    medae = weighted_medae
  )
  imp <- vapply(
    seq_len(ncol(preds)),
    function(j) fun(preds[, j], w = weights),
    numeric(1L)
  )
  names(imp) <- colnames(preds)
  if (isTRUE(sort)) {
    imp <- base::sort(imp, decreasing = TRUE)
  }

  df <- data.frame(
    term = factor(names(imp), levels = rev(names(imp))),
    importance = unname(imp),
    stringsAsFactors = FALSE
  )
  if (by == "term") {
    df$order <- as.factor(vapply(
      as.character(df$term),
      function(t) length(get_variables(t)),
      integer(1L)
    ))
  }

  if (!is.null(max.nsamples) && n > max.nsamples) {
    if (!is.null(seed)) set.seed(seed)
    keeprows <- sample(n, max.nsamples, replace = FALSE, prob = weights)
    preds <- preds[keeprows, , drop = FALSE]
  }

  out <- list(
    importance  = df,
    predictions = preds,
    measure     = switch(
      measure,
      mae   = "Mean Absolute Contribution",
      rmse  = "Root Mean Square Contribution",
      medae = "Median Absolute Contribution"
    ),
    by          = by
  )
  attr(out, "n") <- n
  attr(out, "term.labels") <- as.character(df$term)
  class(out) <- "midimp"
  out
}


#' @rdname eval_importance
#' @exportS3Method midr::eval_importance
#'
eval_importance.mids <- function(
    object, data = NULL, weights = NULL, sort = TRUE,
    measure = c("mae", "rmse", "medae"), by = c("term", "variable"),
    max.nsamples = 1e4L, seed = NULL, ...
) {
  by <- match.arg(by)
  measure <- parse_importance_measure(measure)
  mods <- as.list(object)
  if (missing(max.nsamples)) {
    max.nsamples <- max(100L, 1e4L %/% max(1L, length(mods)))
  }
  if (is.null(seed)) {
    seed <- sample.int(1e6L, 1L)
  }
  out <- suppressMessages(lapply(
    X = mods, FUN = eval_importance.mid, data = data, weights = weights,
    sort = sort, measure = measure, by = by,
    max.nsamples = max.nsamples, seed = seed, ...
  ))
  class(out) <- c("midimps", "midlist")
  out
}


#' @rdname eval_importance
#' @param x a \code{"midimp"} or \code{"midimps"} object.
#' @param digits number of significant digits to print.
#' @exportS3Method base::print
#'
print.midimp <- function(x, digits = max(3L, getOption("digits") - 2L), ...) {
  n <- attr(x, "n", exact = TRUE) %||% nrow(x$predictions)
  cat("\nMID Importance based on ", n, " Observation", if (n != 1L) "s", "\n", sep = "")
  cat("\nMeasure: ", x$measure, "\n", sep = "")
  cat("\nImportance:\n")
  print.data.frame(x$importance, digits = digits, ...)
  invisible(x)
}


#' @rdname eval_importance
#' @param n maximum number of rows to print for \code{"midimps"}.
#' @exportS3Method base::print
#'
print.midimps <- function(x, digits = max(3L, getOption("digits") - 2L), n = 20L, ...) {
  first <- x[[1L]]
  nobs <- attr(first, "n", exact = TRUE) %||% nrow(first$predictions)
  cat("\nMID Importance based on ", nobs, " Observation", if (nobs != 1L) "s", "\n", sep = "")
  cat("\nMeasure: ", first$measure, "\n", sep = "")
  cat("\nImportance:\n")
  smry <- summary(x, shape = "wide")
  print.data.frame(utils::head(smry, n), digits = digits, ...)
  invisible(x)
}


#' @rdname eval_importance
#' @exportS3Method base::summary
#'
summary.midimp <- function(object, ...) {
  object$importance
}


#' @rdname eval_importance
#' @param shape output table shape for \code{summary.midimps()}: \code{"wide"} (default) or \code{"long"}.
#' @param terms optional character vector of terms/variables to include.
#' @param fill value used for terms absent from a model in \code{wide} format (default \code{NA_real_}).
#' @exportS3Method base::summary
#'
summary.midimps <- function(object, shape = c("wide", "long"), terms = NULL, fill = NA_real_, ...) {
  summarize_midlist(
    object = object, shape = shape,
    dfname = "importance", keycol = "term", valcol = "importance",
    keys = "term", targets = terms, fill = fill
  )
}


#' @rdname eval_importance
#' @param row.names,optional passed to \code{as.data.frame()}.
#' @param long logical. If \code{TRUE}, returns a long-format data.frame of
#'   sample-level term contributions merged with \code{importance} (and \code{order}).
#' @exportS3Method base::as.data.frame
#'
as.data.frame.midimp <- function(x, row.names = NULL, optional = FALSE, long = FALSE, ...) {
  if (!isTRUE(long)) {
    return(x$importance)
  }
  imp <- x$importance
  terms <- as.character(imp$term)
  preds <- x$predictions[, terms, drop = FALSE]
  nr <- nrow(preds)
  out <- data.frame(
    term = factor(rep(terms, each = nr), levels = levels(imp$term)),
    mid = as.vector(preds),
    importance = rep(imp$importance, each = nr),
    stringsAsFactors = FALSE
  )
  if ("order" %in% names(imp)) {
    out$order <- rep(imp$order, each = nr)
  }
  out
}


# internal helpers ----

parse_importance_measure <- function(measure) {
  if (is.numeric(measure)) {
    idx <- as.integer(measure[1L])
    if (is.na(idx) || idx < 1L || idx > 3L) {
      stop("numeric 'measure' must be 1, 2, or 3", call. = FALSE)
    }
    return(c("mae", "rmse", "medae")[idx])
  }
  match.arg(measure, c("mae", "rmse", "medae"))
}


aggregate_effects_by_variable <- function(preds) {
  terms <- colnames(preds)
  xvars <- unique(unlist(lapply(terms, get_variables), use.names = FALSE))
  out <- matrix(0, nrow = nrow(preds), ncol = length(xvars))
  colnames(out) <- xvars
  for (i in seq_along(terms)) {
    term <- terms[i]
    vars <- get_variables(term)
    k <- length(vars)
    if (k == 1L) {
      out[, vars[1L]] <- out[, vars[1L]] + preds[, i]
    } else if (k > 1L) {
      share <- preds[, i] / k
      for (v in vars) {
        out[, v] <- out[, v] + share
      }
    }
  }
  out
}


weighted_mae <- function(x, w = NULL) {
  if (is.null(w)) {
    mean(abs(x), na.rm = TRUE)
  } else {
    stats::weighted.mean(abs(x), w = w, na.rm = TRUE)
  }
}


weighted_rmse <- function(x, w = NULL) {
  if (is.null(w)) {
    sqrt(mean(x^2, na.rm = TRUE))
  } else {
    sqrt(stats::weighted.mean(x^2, w = w, na.rm = TRUE))
  }
}


weighted_medae <- function(x, w = NULL) {
  unname(weighted_quantile(abs(x), w = w, probs = 0.5, na.rm = TRUE, names = FALSE))
}
