#' Calculate and Visualize MID-based Individual Conditional Expectation
#'
#' Creates a data frame to be used to visualize the individual conditional expectation.
#'
#' @param object a mid object
#' @param data a data frame representing the observation results.
#' @param variable a name of the predictor variable to calculate the individual conditional expectations for.
#' @param keep.effects logical. If TRUE, the effects of component terms are stored in the output object.
#' @param partition an integer specifying the coarseness of the grid for a "raster" type interaction plot.
#' @param max.nrow the maximum number of rows of the output data frame.
#' @param type the type of the prediction to use when the model has a link function. The default is "response".
#'
#' @export mid.conditional
#'
mid.conditional <- function(
    object, data, variable, keep.effects = TRUE, partition = 100L,
    max.nrow = 1e5L, type = c("response", "link")) {
  type <- match.arg(type)
  rf <- length(tf <- mid.terms(object, remove = variable))
  rv <- length(tv <- mid.terms(object, require = variable))
  if (length(variable) != 1L || rv == 0L)
    stop("'variable' must be a character denoting a valid predictor variable of the model")
  if (!is.data.frame(data))
    data <- as.data.frame(data)
  if ("mid" %in% colnames(data))
    colnames(data)[colnames(data) == "mid"] <- ".mid"
  if (!is.null(formula <- eval(object$call$formula))) {
    yvar <- deparse(formula[[2L]])
    data[, yvar] <- 0L
    data <- stats::model.frame(formula = formula,
                               data = data, na.action = "na.pass")
    data <- data[, -which(colnames(data) == yvar)]
  }
  mf <- mid.frames(object)[[variable]]
  if (is.list(mf))
    mf <- mf[[1L]]
  if (inherits(mf, "numeric.frame")) {
    br <- attr(mf, "breaks")
    values <- seq.int(br[1L], br[length(br)], length.out = partition)
  } else {
    values <- attr(mf, "levels")
  }
  m <- length(values)
  n <- nrow(data)
  if (!is.null(max.nrow) && m * n > max.nrow) {
    max.n <- max.nrow %/% m
    message(paste0("the number of evaluation points exceeds the limit: the data is reduced to ", max.n," observations"))
    data <- data[sample(n, max.n), ]
    n <- nrow(data)
  }
  ids <- rownames(data)
  pmat <- matrix(0, nrow = n, ncol = rf)
  for (i in seq_len(rf))
    pmat[, i] <- mid.f(object, tf[i], x = data)
  pf <- apply(pmat, MARGIN = 1L, FUN = sum)
  pmat <- matrix(0, nrow = n, ncol = rv, dimnames = list(NULL, tv))
  for (i in seq_len(rv))
    pmat[, i] <- mid.f(object, tv[i], x = data)
  pv <- apply(pmat, MARGIN = 1L, FUN = sum)
  yhat <- pf + pv + object$intercept
  if (type == "response" && !is.null(object$link))
    yhat <- object$link$linkinv(yhat)
  res <- list()
  res$terms <- tv
  res$observed <- cbind(id = ids, yhat = yhat, data)
  if (keep.effects)
    res$observed.effects <- pmat
  ldata <- list()
  for (col in colnames(data)) {
    if (col == variable) {
      ldata[[col]] <- rep(values, each = n)
      next
    }
    ldata[[col]] <- rep.int(data[[col]], times = m)
  }
  ldata <- as.data.frame(ldata)
  pmat <- matrix(0, nrow = n * m, ncol = rv, dimnames = list(NULL, tv))
  for (i in seq_len(rv))
    pmat[, i] <- mid.f(object, tv[i], x = ldata)
  pv <- apply(pmat, MARGIN = 1L, FUN = sum)
  pf <- rep.int(pf, m)
  lyhat <- pf + pv + object$intercept
  if (type == "response" && !is.null(object$link))
    lyhat <- object$link$linkinv(lyhat)
  res$conditional <- cbind(id = rep.int(ids, m), yhat = lyhat, ldata)
  if (keep.effects)
    res$conditional.effects <- pmat
  res$grid.points <- values
  class(res) <- c("mid.conditional",
                  if (keep.effects) "mid.conditional.effects")
  attr(res, "variable") <- variable
  attr(res, "n") <- n
  res
}

#'
#' @rdname mid.conditional
#' @param x a mid.conditional object to print.
#' @param ... additional arguments to be passed to the methods for \code{data.frame}.
#' @exportS3Method base::print
#'
print.mid.conditional <- function(x, ...) {
  n <- attr(x, "n")
  cat(paste0("\nIndividual Conditional Expectation for ",
             n, " Observation", if (n > 1L) "s", "\n"))
  cat(paste0("\nVariable: ", attr(x, "variable"), "\n"))
  cat("\nObserved:\n")
  print.data.frame(utils::head(x$observed), ...)
  cat("\nConditional:\n")
  print.data.frame(utils::head(x$conditional), ...)
}


#'
#' @rdname mid.conditional
#' @exportS3Method base::summary
#'
summary.mid.conditional <- function(object, ...) {
  n <- attr(object, "n")
  cat(paste0("\nIndividual Conditional Expectation for ",
             n, " Observation", if (n > 1L) "s", "\n"))
  cat(paste0("\nVariable: ", attr(object, "variable"), "\n"))
  cat("\nObserved:\n")
  print(summary.data.frame(object$observed, ...))
  cat("\nConditional:\n")
  print(summary.data.frame(object$conditional, ...))
}

