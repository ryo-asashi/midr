#' Calculate MID Conditional Expectations
#'
#' @description
#' \code{mid.conditional()} calculates the data required to draw Individual Conditional Expectation (ICE) curves from a fitted MID model.
#' ICE curves visualize how a single observation's prediction changes as a specified variable's value varies, while all other variable are held constant.
#'
#' @details
#' This function generates Individual Conditional Expectation (ICE) data by evaluating the MID model over a range of values for a specific variable.
#' For a given observation \eqn{\mathbf{x}_i}, the ICE value at \eqn{X_j = x'} is computed by replacing the value \eqn{x_{i,j}} with \eqn{x'} while keeping all other features \eqn{\mathbf{x}_{i,\setminus j}} fixed:
#'
#' \deqn{f_{\text{ICE}}(x') = g(x', \mathbf{x}_{i,\setminus j})}
#'
#' The function creates a set of hypothetical observations across a grid of evaluation points for the specified \code{variable}.
#' The resulting object can be plotted to visualize how the prediction changes for individuals as a specific feature varies, revealing both global trends and local departures (heterogeneity).
#'
#' @param object a "mid" object.
#' @param variable a character string or expression specifying the single predictor variable for which to calculate ICE curves.
#' @param data a data frame containing the observations to be used for the ICE calculations. If not provided, data is automatically extracted based on the function call.
#' @param resolution an integer specifying the number of evaluation points for the \code{variable}'s range.
#' @param max.nsamples an integer specifying the maximum number of samples. If the number of observations exceeds this limit, the \code{data} is randomly sampled.
#' @param seed an integer seed for random sampling. Default is \code{NULL}.
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
#' \item{variable}{name of the target variable.}
#' \item{values}{a vector of the sample points for the \code{variable} used in the ICE calculation.}
#'
#' For "midlist", \code{mid.conditional()} returns an object of class "midlist.conditional", a list of "mid.conditional" objects.
#'
#' @seealso \code{\link{interpret}}, \code{\link{plot.mid.conditional}}, \code{\link{ggmid.mid.conditional}}
#'
#' @export mid.conditional
#'
mid.conditional <- function(
    object, variable, data = NULL, resolution = 100L, max.nsamples = 1e3L,
    seed = NULL, type = c("response", "link"), keep.effects = TRUE) {
  if (inherits(object, "midlist")) {
    if (missing(max.nsamples))
      max.nsamples <- max(10L, 1e3L %/% length(object$intercept))
    if (missing(keep.effects)) keep.effects <- FALSE
    if (missing(seed)) seed <- sample(1e6L, 1L)
    out <- suppressMessages(lapply(
      X = object, FUN = mid.conditional, variable = variable, data = data,
      resolution = resolution, max.nsamples = max.nsamples, seed = seed,
      type = type, keep.effects = keep.effects
    ))
    attr(out, "ids") <- out[[1L]]$ids
    attr(out, "variable") <- out[[1L]]$variable
    attr(out, "values") <- out[[1L]]$values
    class(out) <- "midlist.conditional"
    return(out)
  }
  if (!inherits(object, "mid"))
    stop("'object' must be 'mid' or 'midlist'")
  type <- match.arg(type)
  tvar <- mid.terms(object, require = variable)
  nvar <- length(tvar)
  tfix <- mid.terms(object, remove = variable)
  nfix <- length(tfix)
  if (length(variable) != 1L || nvar == 0L)
    stop("'variable' must be a character string denoting a valid predictor variable")
  if (is.null(data))
    data <- model.data(object, env = parent.frame())
  if (!is.data.frame(data))
    data <- as.data.frame(data)
  data <- model.reframe(object, data)
  enc <- object$encoders$main.effects[[variable]] %||%
    object$encoders$interactions[[variable]]
  if (enc$type != "factor") {
    br <- enc$envir$br
    values <- seq.int(br[1L], br[length(br)], length.out = resolution)
  } else {
    olvs <- enc$envir$olvs
    values <- factor(olvs, levels = olvs)
    data[, variable] <- enc$transform(data[, variable], lumped = FALSE)
  }
  m <- length(values)
  n <- nrow(data)
  if (!is.null(max.nsamples) && n > max.nsamples) {
    message("number of observations exceeds 'max.nsamples': a sample of ",
    max.nsamples," observations from 'data' is used")
    if (!is.null(seed)) set.seed(seed)
    data <- data[sample(n, max.nsamples, replace = FALSE), ]
    n <- nrow(data)
  }
  ids <- rownames(data)
  rownames(data) <- NULL
  pmat <- matrix(0, nrow = n, ncol = nfix)
  for (i in seq_len(nfix))
    pmat[, i] <- mid.f(object, tfix[i], x = data)
  pfix <- rowSums(pmat)
  pmat <- matrix(0, nrow = n, ncol = nvar, dimnames = list(NULL, tvar))
  for (i in seq_len(nvar))
    pmat[, i] <- mid.f(object, tvar[i], x = data)
  pvar <- rowSums(pmat)
  yhat <- pfix + pvar + object$intercept
  if (type == "response" && !is.null(object$link))
    yhat <- object$link$linkinv(yhat)
  res <- list()
  res$observed <- cbind(.id = ids, yhat = yhat, data)
  if (keep.effects)
    res$observed.effects <- pmat
  tags <- unique(term.split(tvar))
  tags <- tags[tags != variable]
  names(tags) <- tags
  longdata <- lapply(tags, function(tag) rep.int(data[, tag], times = m))
  longdata[[variable]] <- rep(values, each = n)
  longdata <- as.data.frame(longdata, check.names = FALSE)
  pfix <- rep.int(pfix, times = m)
  pmat <- matrix(0, nrow = n * m, ncol = nvar, dimnames = list(NULL, tvar))
  for (i in seq_len(nvar))
    pmat[, i] <- mid.f(object, tvar[i], x = longdata)
  pvar <- rowSums(pmat)
  longyhat <- pfix + pvar + object$intercept
  if (type == "response" && !is.null(object$link))
    longyhat <- object$link$linkinv(longyhat)
  res$conditional <- cbind(.id = rep.int(ids, m), yhat = longyhat, longdata)
  if (keep.effects)
    res$conditional.effects <- pmat
  res$ids <- ids
  res$variable <- variable
  res$values <- values
  class(res) <- c("mid.conditional")
  attr(res, "term.labels") <- tvar
  attr(res, "n") <- n
  res
}


#' @exportS3Method base::print
#'
print.mid.conditional <- function(
    x, digits = max(3L, getOption("digits") - 2L), n = 20L, ...
  ) {
  nobs <- attr(x, "n", exact = TRUE)
  cat(paste0("\nIndividual Conditional Expectation for ",
             nobs, " Observation", if (nobs > 1L) "s", "\n"))
  variable <- x$variable
  cat(paste0("\nVariable: ", variable, "\n"))
  cat("\nSample Points: ", examples(x$values, digits = digits), "\n")
  cat("\nConditional Expectations:\n")
  print.data.frame(utils::head(x$conditional[, c(".id", "yhat", variable)], n),
                   digits = digits, ...)
  invisible(x)
}

#' @exportS3Method base::print
#'
print.midlist.conditional <- function(
    x, digits = max(3L, getOption("digits") - 2L), n = 20L, ...
) {
  nobs <- attr(x[[1L]], "n", exact = TRUE)
  cat(paste0("\nIndividual Conditional Expectation for ",
             nobs, " Observation", if (nobs > 1L) "s", "\n"))
  variable <- x$variable %||% x[[1L]]$variable
  cat(paste0("\nVariable: ", variable, "\n"))
  cat("\nSample Points: ", examples(x[[1L]]$values, digits = digits), "\n")
  cat("\nConditional Expectations:\n")
  smry <- summary.midlist.conditional(x, shape = "long")
  print.data.frame(utils::head(smry, n), digits = digits, ...)
  invisible(smry)
}
