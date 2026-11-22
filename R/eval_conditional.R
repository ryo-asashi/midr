# R/eval_conditional.R

#' Evaluate Individual Conditional Expectations (ICE)
#'
#' @description
#' \code{eval_conditional()} calculates Individual Conditional Expectation (ICE)
#' curves from a fitted model for a single predictor variable.
#'
#' @details
#' For each observation \eqn{\mathbf{x}_i}, the ICE curve evaluates the model
#' across a grid of values \eqn{x'} for the target \code{variable} while holding
#' all other predictors \eqn{\mathbf{x}_{i,\setminus j}} fixed:
#'
#' \deqn{f_{\text{ICE}}(x') = g(x', \mathbf{x}_{i,\setminus j})}
#'
#' @param object a \code{"mid"} or \code{"mids"} object.
#' @param ... additional arguments passed to methods.
#'
#' @returns
#' For a \code{"mid"} object, an object of class \code{"midcon"} containing
#' \code{observed}, \code{conditional}, \code{variable}, \code{values}, and
#' \code{ids}.
#' For a \code{"mids"} collection, a collection of class \code{c("midcons", "midlist")}.
#'
#' @seealso \code{\link{interpret}}, \code{\link{plot.midcon}}, \code{\link{ggmid.midcon}}
#'
#' @export eval_conditional
#'
eval_conditional <- function(object, ...)
UseMethod("eval_conditional")


#' @rdname eval_conditional
#' @param variable character string naming the predictor variable to vary.
#' @param data a data.frame containing observations for ICE evaluation.
#'   Extracted automatically from \code{object} when omitted.
#' @param resolution number of evaluation grid points for continuous variables.
#' @param max.nsamples maximum number of observations to evaluate. Larger datasets
#'   are randomly subsampled.
#' @param seed optional integer seed for subsampling.
#' @param type character: \code{"response"} (default) or \code{"link"}.
#' @exportS3Method midr::eval_conditional
#'
eval_conditional.mid <- function(
    object, variable, data = NULL, resolution = 100L, max.nsamples = 500L,
    seed = NULL, type = c("response", "link"), ...
) {
  type <- match.arg(type)
  if (!is.character(variable) || length(variable) != 1L) {
    stop("'variable' must be a single character string", call. = FALSE)
  }
  tvar <- get_labels(filter_terms(object, require = variable, raw = FALSE))
  if (length(tvar) == 0L) {
    stop(sprintf("variable '%s' is not found in 'object'", variable), call. = FALSE)
  }
  tfix <- get_labels(filter_terms(object, remove = variable, raw = FALSE))

  if (is.null(data)) {
    data <- model_data(object, env = parent.frame())
    if (is.null(data)) {
      stop("'data' must be supplied when model data cannot be recovered", call. = FALSE)
    }
  }
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  data <- model_reframe(object, data)

  enc <- get_encoder(object, variable)
  if (is.null(enc)) {
    stop(sprintf("encoder for '%s' is not found in 'object'", variable), call. = FALSE)
  }
  if (enc$type != "factor") {
    rng <- encoder_range(enc)
    values <- seq(rng[1L], rng[2L], length.out = resolution)
  } else {
    olvs <- encoder_levels(enc, original = TRUE)
    values <- factor(olvs, levels = olvs)
    data[[variable]] <- enc$transform(data[[variable]], lumped = FALSE)
  }

  m <- length(values)
  n <- nrow(data)
  if (!is.null(max.nsamples) && n > max.nsamples) {
    message(
      "number of observations exceeds 'max.nsamples': a sample of ",
      max.nsamples, " observations from 'data' is used"
    )
    if (!is.null(seed)) set.seed(seed)
    data <- data[sample(n, max.nsamples, replace = FALSE), , drop = FALSE]
    n <- nrow(data)
  }

  ids <- rownames(data)
  rownames(data) <- NULL
  ints <- get_intercept(object)

  pmat_obs <- stats::predict(object, newdata = data, type = "terms", na.action = stats::na.pass)
  pfix <- if (length(tfix) > 0L) rowSums(pmat_obs[, tfix, drop = FALSE]) else numeric(n)
  pvar_obs <- pmat_obs[, tvar, drop = FALSE]
  yhat <- pfix + rowSums(pvar_obs) + ints
  if (type == "response" && !is.null(object$link)) {
    yhat <- object$link$linkinv(yhat)
  }

  longdata <- data[rep.int(seq_len(n), times = m), , drop = FALSE]
  longdata[[variable]] <- rep(values, each = n)
  pmat_con <- stats::predict(
    object, newdata = longdata, type = "terms", terms = tvar, na.action = stats::na.pass
  )
  longyhat <- rep.int(pfix, times = m) + rowSums(pmat_con) + ints
  if (type == "response" && !is.null(object$link)) {
    longyhat <- object$link$linkinv(longyhat)
  }

  con_df <- data.frame(
    .id = rep.int(ids, m),
    vcol = longdata[[variable]],
    yhat = longyhat,
    stringsAsFactors = FALSE
  )
  names(con_df)[2L] <- variable

  res <- list(
    observed    = data.frame(
      .id = ids,
      yhat = yhat,
      data[, setdiff(names(data), c(".id", "yhat")), drop = FALSE],
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    conditional = con_df,
    ids         = ids,
    variable    = variable,
    values      = values
  )
  attr(res, "term.labels") <- tvar
  attr(res, "n") <- n
  class(res) <- "midcon"
  res
}


#' @rdname eval_conditional
#' @exportS3Method midr::eval_conditional
#'
eval_conditional.mids <- function(
    object, variable, data = NULL, resolution = 100L, max.nsamples = 500L,
    seed = NULL, type = c("response", "link"), ...
) {
  type <- match.arg(type)
  mods <- as.list(object)
  if (missing(max.nsamples)) {
    max.nsamples <- max(1L, 100L %/% max(1L, length(mods)))
  }
  if (is.null(seed)) {
    seed <- sample.int(1e6L, 1L)
  }
  out <- suppressMessages(lapply(
    X = mods, FUN = eval_conditional.mid, variable = variable, data = data,
    resolution = resolution, max.nsamples = max.nsamples, seed = seed,
    type = type, ...
  ))
  class(out) <- c("midcons", "midlist")
  out
}


#' @exportS3Method base::print
#'
print.midcon <- function(x, digits = max(3L, getOption("digits") - 2L), ...) {
  n <- attr(x, "n", exact = TRUE) %||% nrow(x$observed)
  cat("\nMID Conditional Expectation for '", x$variable, "' (", n, " Observation",
      if (n != 1L) "s", ")\n", sep = "")
  cat("\nGrid Values: ", examples(x$values, digits = digits), "\n", sep = "")
  cat("\nObserved Head:\n")
  print.data.frame(utils::head(x$observed, 6L), digits = digits, ...)
  invisible(x)
}


#' @exportS3Method base::print
#'
print.midcons <- function(x, digits = max(3L, getOption("digits") - 2L), ...) {
  objs <- as.list(x)
  first <- objs[[1L]]
  n <- attr(first, "n", exact = TRUE) %||% nrow(first$observed)
  cat("\nMID Conditional Expectations for '", first$variable, "' (", length(objs),
      " Models, ", n, " Observation", if (n != 1L) "s", ")\n", sep = "")
  cat("\nModels: ", paste(labels(x), collapse = ", "), "\n", sep = "")
  cat("\nGrid Values: ", examples(first$values, digits = digits), "\n", sep = "")
  invisible(x)
}


#' @exportS3Method base::summary
#'
summary.midcon <- function(object, ...) {
  object$conditional
}


#' @exportS3Method base::summary
#'
summary.midcons <- function(object, ...) {
  first <- as.list(object)[[1L]]
  summarize_midlist(
    object = object, shape = "long",
    dfname = "conditional", keycol = ".id", valcol = "yhat",
    keys = c(".id", first$variable), targets = first$ids
  )
}


#' @exportS3Method base::as.data.frame
#'
as.data.frame.midcon <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$conditional
}


#' @exportS3Method base::as.data.frame
#'
as.data.frame.midcons <- function(x, row.names = NULL, optional = FALSE, ...) {
  summary.midcons(x)
}
