#' Calculate MID Conditional Expectations
#'
#' @description
#' \code{mid.conditional()} calculates the data required to draw Individual Conditional Expectation (ICE) curves from a fitted MID model.
#' ICE curves visualize how a single observation's prediction changes as a specified variable's value varies, while all other variable are held constant.
#'
#' @details
#' The function generates a set of hypothetical observations by creating copies of the original data and varying the specified \code{variable} across a range of sample points.
#' It then obtains a prediction for each of these hypothetical observations from the MID model. The returned object can be plotted to visualize the ICE curves.
#'
#' @param object a "mid" object.
#' @param variable a character string or expression specifying the single predictor variable for which to calculate ICE curves.
#' @param data a data frame containing the observations to be used for the ICE calculations. If not provided, data is automatically extracted based on the function call.
#' @param resolution an integer specifying the number of evaluation points for the \code{variable}'s range.
#' @param max.nsamples an integer specifying the maximum number of samples. If the number of observations exceeds this limit, the \code{data} is randomly sampled.
#' @param type the type of prediction to return. "response" (default) for the original scale or "link" for the scale of the linear predictor.
#' @param keep.effects logical. If \code{TRUE}, the effects of individual component functions are stored in the output object.
#'
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, data = airquality, lambda = 1)
#'
#' # Calculate the ICE values for a fitted MID model
#' ice <- mid.conditional(mid, variable = "Wind", data = airquality)
#' print(ice)
#' @returns
#' \code{mid.conditional()} returns an object of class "mid.conditional". This is a list with the following components:
#' \item{observed}{a data frame of the original observations used, along with their predictions.}
#' \item{conditional}{a data frame of the hypothetical observations and their corresponding predictions.}
#' \item{values}{a vector of the sample points for the \code{variable} used in the ICE calculation}
#'
#' @seealso \code{\link{interpret}}, \code{\link{plot.mid.conditional}}, \code{\link{ggmid.mid.conditional}}
#'
#' @export mid.conditional
#'
mid.conditional <- function(
    object, variable, data = NULL, resolution = 100L,
    max.nsamples = 1e3L, type = c("response", "link"), keep.effects = TRUE) {
  type <- match.arg(type)
  rf <- length(tf <- mid.terms(object, remove = variable))
  rv <- length(tv <- mid.terms(object, require = variable))
  if (length(variable) != 1L || rv == 0L)
    stop("'variable' must be a character string denoting a valid predictor variable")
  if (is.null(data))
    data <- model.data(object, env = parent.frame())
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
    values <- seq.int(br[1L], br[length(br)], length.out = resolution)
  } else {
    values <- mf[, 1L]
    attr(values, "catchall") <- attr(mf, "catchall")
  }
  m <- length(values)
  n <- nrow(data)
  if (!is.null(max.nsamples) && n > max.nsamples) {
    message("number of observations exceeds 'max.nsamples': a sample of ",
    max.nsamples," observations from 'data' is used")
    data <- data[sample(n, max.nsamples, replace = FALSE), ]
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
  attr(res, "term.labels") <- tv
  attr(res, "variable") <- variable
  attr(res, "n") <- n
  res
}


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
