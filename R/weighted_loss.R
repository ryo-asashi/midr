#' Weighted Loss Function
#'
#' @description
#' \code{weighted_loss()} computes various loss metrics (e.g., RMSE, MAE) between two numeric vectors, or for the deviations from the weighted mean of a numeric vector.
#'
#' @param x a numeric vector.
#' @param y an optional numeric vector. If \code{NULL}, \code{x} is compared against its weighted mean.
#' @param weights a numeric vector of sample weights for each value in \code{x}.
#' @param na.rm logical. If \code{TRUE}, any \code{NA} and \code{NaN}s are removed from all input vectors before the calculation.
#' @param method the loss measure. One of "mse" (mean square error), "rmse" (root mean square error), mae" (mean absolute error), "medae" (median absolute error), or "r2" (R-squared).
#'
#' @examples
#' # Calculate loss metrics between x and y with weights
#' weighted_loss(x = c(0, 10), y = c(0, 0), weights = c(99, 1), method = "rmse")
#' weighted_loss(x = c(0, 10), y = c(0, 0), weights = c(99, 1), method = "mae")
#' weighted_loss(x = c(0, 10), y = c(0, 0), weights = c(99, 1), method = "medae")
#'
#' # Verify uninterpreted variation ratio of a fitted MID model without weights
#' mid <- interpret(dist ~ speed, cars)
#' 1 - weighted_loss(cars$dist, predict(mid, cars), method = "r2")
#' mid$ratio
#'
#' # Verify uninterpreted variation ratio of a fitted MID model with weights
#' w <- 1:nrow(cars)
#' mid <- interpret(dist ~ speed, cars, weights = w)
#' 1 - weighted_loss(cars$dist, predict(mid, cars), weights = w, method = "r2")
#' mid$ratio
#' @returns
#' \code{weighted_loss()} returns a single numeric value.
#'
#' @export weighted_loss
#'
weighted_loss <- function(
    x, y = NULL, weights = NULL, na.rm = FALSE,
    method = c("rmse", "mse", "mae", "medae", "r2")
  ) {
  method <- match.arg(method)
  nx <- length(x)
  if (!is.null(weights) && length(weights) != nx)
    stop("'x' and 'weights' must have the same length")
  if (!is.null(y)) {
    if (length(y) == 1L)
      y <- rep.int(y, times = nx)
    else if (length(y) != nx)
      stop("'x' and 'y' must have the same length")
  }
  if (na.rm) {
    ok <- !is.na(x)
    if (!is.null(y))
      ok <- ok & !is.na(y)
    if (!is.null(weights))
      ok <- ok & !is.na(weights) & (weights > 0)
    x <- x[ok]
    weights <- weights[ok]
    y <- y[ok]
  }
  if (is.null(y)) {
    y <- if (is.null(weights)) sum(x) / length(x) else sum(x * weights) / sum(weights)
  }
  if (method == "r2") {
    if (is.null(y))
      return(0)
    rms <- weighted_loss(x, y, weights, FALSE, "mse")
    tms <- weighted_loss(x, NULL, weights, FALSE, "mse")
    return(1 - rms / tms)
  }
  x <- x - y
  if (method == "rmse") {
    if (is.null(weights)) sqrt(sum(x ^ 2) / length(x))
    else sqrt(sum(x ^ 2 * weights) / sum(weights))
  } else if (method == "mse") {
    if (is.null(weights)) sum(x ^ 2) / length(x)
    else sum(x ^ 2 * weights) / sum(weights)
  } else if (method == "mae") {
    if (is.null(weights)) sum(abs(x)) / length(x)
    else sum(abs(x) * weights) / sum(weights)
  } else if (method == "medae") {
    weighted.quantile(
      abs(x), weights, probs = 0.5, na.rm = na.rm, names = FALSE, type = 1L
    )
  }
}


weighted_rmse <- function(x, y = NULL, weights = NULL, na.rm = FALSE) {
  weighted_loss(x, y = y, weights = weights, na.rm = na.rm, method = "rmse")
}

weighted_mae <- function(x, y = NULL, weights = NULL, na.rm = FALSE) {
  weighted_loss(x, y = y, weights = weights, na.rm = na.rm, method = "mae")
}

weighted_medae <- function(x, y = NULL, weights = NULL, na.rm = FALSE) {
  weighted_loss(x, y = y, weights = weights, na.rm = na.rm, method = "medae")
}
