#' Calculate ICE of MID Models
#'
#' \code{mid.conditional()} creates an object to draw ICE curves of a MID model.
#'
#' \code{mid.conditional()} obtains predictions for hypothetical observations from a 'mid' object and returns a 'mid.conditional' object.
#' The graphing functions \code{ggmid()} and \code{plot()} can be used to generate the ICE curve plots.
#'
#' @param object a 'mid' object.
#' @param variable a character or expression specifying the variable for the ICE calculation.
#' @param data a data frame of observations.
#' @param keep.effects logical. If \code{TRUE}, the effects of component functions are stored in the output object.
#' @param n.samples an integer specifying the number of the sample points.
#' @param max.nrow an integer specifying the maximum number of rows of the output data frame.
#' @param type the type of the prediction. The default is "response". "link" is possible if the MID model uses a link function.
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
#' mc <- mid.conditional(mid, "Wind", airquality)
#' mc
#' @returns
#' \code{mid.conditional()} returns a 'mid.conditional' object with the following components:
#' \item{terms}{a character vector of relevant terms.}
#' \item{observed}{a data frame of the actual observations and the corresponding predictions.}
#' \item{conditional}{a data frame of the hypothetical observations and the corresponding predictions.}
#' \item{values}{a numeric vector of the sample points for the target variable.}
#' @export mid.conditional
#'
mid.conditional <- function(
    object, variable, data, keep.effects = TRUE, n.samples = 100L,
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
  mf <- object$encoders[["main.effects"]][[variable]]$frame
  if (is.null(mf))
    mf <- object$encoders[["interactions"]][[variable]]$frame
  if (inherits(mf, "numeric.frame")) {
    br <- attr(mf, "breaks")
    values <- seq.int(br[1L], br[length(br)], length.out = n.samples)
  } else {
    values <- mf[, 1L]
    attr(values, "catchall") <- attr(mf, "catchall")
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
  pm <- matrix(0, nrow = n, ncol = rf)
  for (i in seq_len(rf))
    pm[, i] <- mid.f(object, tf[i], x = data)
  pf <- rowSums(pm)
  pm <- matrix(0, nrow = n, ncol = rv, dimnames = list(NULL, tv))
  for (i in seq_len(rv))
    pm[, i] <- mid.f(object, tv[i], x = data)
  pv <- rowSums(pm)
  yhat <- pf + pv + object$intercept
  if (type == "response" && !is.null(object$link))
    yhat <- object$link$linkinv(yhat)
  res <- list()
  res$terms <- tv
  res$observed <- cbind(id = ids, yhat = yhat, data)
  if (keep.effects)
    res$observed.effects <- pm
  ldata <- list()
  for (col in colnames(data)) {
    if (col == variable) {
      ldata[[col]] <- rep(values, each = n)
    } else {
      ldata[[col]] <- rep.int(data[[col]], times = m)
    }
  }
  ldata <- as.data.frame(ldata)
  colnames(ldata) <- colnames(data)
  pm <- matrix(0, nrow = n * m, ncol = rv, dimnames = list(NULL, tv))
  for (i in seq_len(rv))
    pm[, i] <- mid.f(object, tv[i], x = ldata)
  pv <- rowSums(pm)
  pf <- rep.int(pf, m)
  lyhat <- pf + pv + object$intercept
  if (type == "response" && !is.null(object$link))
    lyhat <- object$link$linkinv(lyhat)
  res$conditional <- cbind(id = rep.int(ids, m), yhat = lyhat, ldata)
  if (keep.effects)
    res$conditional.effects <- pm
  res$values <- values
  class(res) <- c("mid.conditional",
                  if (keep.effects) "mid.conditional.effects")
  attr(res, "variable") <- variable
  attr(res, "n") <- n
  res
}


#' @rdname mid.conditional
#' @param x a 'mid.conditional' object to print.
#' @param ... additional parameters to be passed to \code{print.default()} to print sample points.
#' @exportS3Method base::print
#'
print.mid.conditional <- function(x, ...) {
  n <- attr(x, "n")
  cat(paste0("\nIndividual Conditional Expectation for ",
             n, " Observation", if (n > 1L) "s", "\n"))
  cat(paste0("\nVariable: ", attr(x, "variable"), "\n"))
  cat("\nSample Points:\n")
  print.default(x$values, ...)
}
