#' Print MID Models
#'
#' @description
#' \code{print()} methods for a fitted MID model ("mid") or a collection of models ("mids").
#'
#' @details
#' By default, the \code{print()} method for "mid" objects provides a quick overview of the model structure by listing the number of main effect and interaction terms.
#' If \code{main.effects = TRUE} is specified, the method will also print the contribution of each main effect at its sample points, providing a more detailed look at the model's components.
#'
#' For a collection of models in the structure-of-array format ("midrib"), the method prints a summarized overview. For array-of-structures collections ("midlist"), it prints the first few models up to \code{max.nmodels}.
#'
#' @param x a "mid" or "mids" object to be printed.
#' @param digits an integer specifying the number of significant digits for printing.
#' @param main.effects logical. If \code{TRUE}, the MID values of each main effect are also printed (only applicable for single "mid" objects).
#' @param max.nmodels an integer specifying the maximum number of models to print for a "midlist" collection.
#' @param ... arguments to be passed to other methods.
#'
#' @examples
#' data(cars, package = "datasets")
#' mid <- interpret(dist ~ speed, cars)
#'
#' # Default print provides a concise summary
#' print(mid)
#'
#' # Setting main.effects = TRUE prints the contributions of each main effect
#' print(mid, main.effects = TRUE)
#' @returns
#' \code{print.mid()} returns the original "mid" object invisibly.
#'
#' \code{print.mids()} returns the original "mids" object invisibly.
#'
#' @seealso \code{\link{interpret}}, \code{\link{summary.mid}}
#'
#' @exportS3Method base::print
#'
print.mid <- function(
    x, digits = max(3L, getOption("digits") - 2L), main.effects = FALSE, ...) {
  cl <- paste0(trimws(deparse(x$call)), sep = "", collapse = "\n ")
  cat("\nCall:\n", cl, "\n", sep = "")
  if (!is.null(x$model.class))
    cat("\nModel Class: ", paste0(x$model.class, collapse = ", "), "\n", sep = "")
  cat("\nIntercept: ", examples(x$intercept, digits = digits), "\n", sep = "")
  m <- length(x$main.effects)
  if (m > 0L) {
    cat("\nMain Effects:\n")
    if (main.effects) {
      for (i in seq_len(m)) {
        cat("---\n$", names(x$main.effects)[i], "\n", sep = "")
        vl <- x$main.effects[[i]][, "mid"]
        nm <- x$main.effects[[i]][, 1L]
        if (is.numeric(nm))
          nm <- format(nm, digits = digits)
        names(vl) <- nm
        print.default(vl, digits = digits)
      }
    } else {
      cat(m, " main effect term", if (m > 1L) "s", "\n", sep = "")
    }
  }
  m <- length(x$interactions)
  if (m > 0L) {
    cat("\nInteractions:\n",
        m, " interaction term", if (m > 1L) "s", "\n", sep = "")
  }
  ur <- x$ratio
  ur <- if (inherits(x, "mid")) ur[1L] else if (is.matrix(ur)) ur[1L, ] else ur
  cat("\nUninterpreted Variation Ratio: ", examples(ur, digits = digits), "\n",
      sep = "")
  invisible(x)
}


#' @rdname print.mid
#' @exportS3Method base::print
#'
print.mids <- function(x, max.nmodels = 1L, ...) {
  if (inherits(x, "midrib")) {
    args <- list(...)
    args$main.effects <- FALSE
    return(do.call(print.mid, c(list(x = x), args)))
  }
  nms <- labels(x)
  nmodels <- length(nms)
  n <- min(nmodels, max.nmodels)
  Map(
    function(obj, nm) {
      cat("\n$", nm, "\n", sep = "")
      print.mid(obj, ...)
    }, x[seq_len(n), drop = FALSE], nms[seq_len(n)]
  )
  if (n < nmodels && n > 0L)
    cat(sprintf("\n... and %d more models\n", nmodels - n))
  invisible(x)
}



#' @exportS3Method base::print
#'
print.midimp <- function(
    x, digits = max(3L, getOption("digits") - 2L), ...
) {
  n <- attr(x, "n", exact = TRUE)
  cat("\nMID Importance based on ",
      n, " Observation", if (n > 1L) "s", "\n", sep = "")
  cat("\nMeasure: ", x$measure, "\n", sep = "")
  cat("\nImportance:\n")
  print.data.frame(x$importance, digits = digits, ...)
  invisible(x)
}

#' @exportS3Method base::print
#'
print.midimps <- function(
    x, digits = max(3L, getOption("digits") - 2L), n = 20L, ...
) {
  nobs <- attr(x[[1L]], "n", exact = TRUE)
  cat("\nMID Importance based on ",
      nobs, " Observation", if (nobs > 1L) "s", "\n", sep = "")
  cat("\nMeasure: ", x[[1L]]$measure, "\n", sep = "")
  cat("\nImportance:\n")
  smry <- summary(x, shape = "wide")
  print.data.frame(utils::head(smry, n), digits = digits, ...)
  invisible(x)
}



#' @exportS3Method base::print
#'
print.midbrk <- function(
    x, digits = max(3L, getOption("digits") - 2L), ...
) {
  cat("\nMID Breakdown of a Prediction\n")
  cat("\nIntercept: ",
      format(x$intercept, digits = digits), "\n", sep = "")
  if (!is.null(x$linear.predictor)) {
    cat("\nLinear Predictor: ",
        format(x$linear.predictor, digits = digits), "\n", sep = "")
  } else {
    cat("\nPrediction: ",
        format(x$prediction, digits = digits), "\n", sep = "")
  }
  cat("\nBreakdown of Effects:\n")
  print.data.frame(x$breakdown, digits = digits, ...)
  invisible(x)
}

#' @exportS3Method base::print
#'
print.midbrks <- function(
    x, digits = max(3L, getOption("digits") - 2L), n = 20L, ...
) {
  cat("\nMID Breakdown of a Prediction\n")
  intercept <- vapply(X = x, FUN = function(y) y$intercept, 0.0)
  cat("\nIntercept: ",
      examples(intercept, digits = digits), "\n", sep = "")
  lp <- x[[1L]]$linear.predictor
  if (!is.null(lp)) {
    lp <- vapply(X = x, FUN = function(y) y$linear.predictor, 0.0)
    cat("\nLinear Predictor: ",
        examples(lp, digits = digits), "\n", sep = "")
  } else {
    prediction <- vapply(X = x, FUN = function(y) y$prediction, 0.0)
    cat("\nPrediction: ",
        examples(prediction, digits = digits), "\n", sep = "")
  }
  cat("\nBreakdown of Effects:\n")
  smry <- summary(x, shape = "wide")
  print.data.frame(utils::head(smry, n), digits = digits, ...)
  invisible(x)
}



#' @exportS3Method base::print
#'
print.midcon <- function(
    x, digits = max(3L, getOption("digits") - 2L), n = 20L, ...
) {
  nobs <- attr(x, "n", exact = TRUE)
  cat("\nIndividual Conditional Expectation for ",
      nobs, " Observation", if (nobs > 1L) "s", "\n", sep = "")
  variable <- x$variable
  cat("\nVariable: ", variable, "\n", sep = "")
  cat("\nSample Points: ", examples(x$values, digits = digits), "\n", sep = "")
  cat("\nConditional Expectations:\n")
  print.data.frame(
    utils::head(x$conditional[, c(".id", variable, "yhat")], n),
    digits = digits, ...
  )
  invisible(x)
}

#' @exportS3Method base::print
#'
print.midcons <- function(
    x, digits = max(3L, getOption("digits") - 2L), n = 20L, ...
) {
  nobs <- attr(x[[1L]], "n", exact = TRUE)
  cat("\nIndividual Conditional Expectation for ",
      nobs, " Observation", if (nobs > 1L) "s", "\n", sep = "")
  variable <- x$variable %||% x[[1L]]$variable
  cat("\nVariable: ", variable, "\n", sep = "")
  cat("\nSample Points: ",
      examples(x[[1L]]$values, digits = digits), "\n", sep = "")
  cat("\nConditional Expectations:\n")
  smry <- summary(x, shape = "long")
  print.data.frame(utils::head(smry, n), digits = digits, ...)
  invisible(x)
}
