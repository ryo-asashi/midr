#' Calculate ICE of MID Models
#'
#' \code{mid.conditional()} creates an object to draw ICE curves of a MID model.
#'
#' \code{mid.conditional()} obtains predictions for hypothetical observations from a MID model and returns a "mid.conditional" object.
#' The graphing functions \code{ggmid()} and \code{plot()} can be used to generate the ICE curve plots.
#'
#' @param object a "mid" object.
#' @param variable a character string or expression specifying the variable for the ICE calculation.
#' @param data a data frame containing observations for which ICE values are calculated. If not passed, data is extracted from \code{parent.env()} based on the function call of the "mid" object.
#' @param keep.effects logical. If \code{TRUE}, the effects of component functions are stored in the output object.
#' @param n.samples integer. The number of sample points for the calculation.
#' @param max.nrow an integer specifying the maximum number of rows of the output data frames.
#' @param type the type of prediction required. The default is "response". "link" is possible if the MID model uses a link function.
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
#' mc <- mid.conditional(mid, "Wind", airquality)
#' mc
#' @returns
#' \code{mid.conditional()} returns an object of class "mid.conditional" with the following components:
#' \item{terms}{the character vector of relevant terms.}
#' \item{observed}{the data frame of the actual observations and the corresponding predictions.}
#' \item{conditional}{the data frame of the hypothetical observations and the corresponding predictions.}
#' \item{values}{the sample points of the variable.}
#' @export mid.conditional
#'
mid.conditional <- function(
    object, variable, data = NULL, keep.effects = TRUE, n.samples = 100L,
    max.nrow = 1e5L, type = c("response", "link")) {
  type <- match.arg(type)
  rf <- length(tf <- mid.terms(object, remove = variable))
  rv <- length(tv <- mid.terms(object, require = variable))
  if (length(variable) != 1L || rv == 0L)
    stop("'variable' must be a character denoting a valid predictor variable of the model")
  if (is.null(data))
    data <- model.data(object)
  if (!is.data.frame(data))
    data <- as.data.frame(data)
  if ("mid" %in% colnames(data))
    colnames(data)[colnames(data) == "mid"] <- ".mid"
  data <- model.reframe(object, data)
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
  res$observed <- cbind(.id = ids, yhat = yhat, data)
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
  res$conditional <- cbind(.id = rep.int(ids, m), yhat = lyhat, ldata)
  if (keep.effects)
    res$conditional.effects <- pm
  res$values <- values
  class(res) <- c("mid.conditional")
  attr(res, "variable") <- variable
  attr(res, "n") <- n
  res
}


#' @rdname mid.conditional
#' @param x a "mid.conditional" object to be printed.
#' @param digits an integer specifying the minimum number of significant digits to be printed.
#' @param ... additional parameters to be passed to \code{print.default()} to print the sample point vector.
#' @exportS3Method base::print
#'
print.mid.conditional <- function(
    x, digits = max(3L, getOption("digits") - 2L), ...
  ) {
  n <- attr(x, "n")
  cat(paste0("\nIndividual Conditional Expectation for ",
             n, " Observation", if (n > 1L) "s", "\n"))
  cat(paste0("\nVariable: ", attr(x, "variable"), "\n"))
  cat("\nSample Points:\n")
  print(x$values, digits = digits, ...)
}
