#' Weighted Loss Function
#'
#' @description
#' \code{weighted.loss()} computes various loss metrics (e.g., RMSE, MAE) between two numeric vectors, or for the deviations from the weighted mean of a numeric vector.
#'
#' @param x a numeric vector.
#' @param y an optional numeric vector. If \code{NULL}, \code{x} is compared against its weighted mean.
#' @param w a numeric vector of sample weights for each value in \code{x}.
#' @param na.rm logical. If \code{TRUE}, any \code{NA} and \code{NaN}s are removed from all input vectors before the calculation.
#' @param method the loss measure. One of "mse" (mean square error), "rmse" (root mean square error), "mae" (mean absolute error), or "medae" (median absolute error).
#'
#' @examples
#' # Calculate loss metrics between x and y with weights
#' weighted.loss(x = c(0, 10), y = c(0, 0), w = c(99, 1), method = "rmse")
#' weighted.loss(x = c(0, 10), y = c(0, 0), w = c(99, 1), method = "mae")
#' weighted.loss(x = c(0, 10), y = c(0, 0), w = c(99, 1), method = "medae")
#'
#' # Verify uninterpreted variation ratio of a fitted MID model without weights
#' mid <- interpret(dist ~ speed, cars)
#' RSS <- weighted.loss(cars$dist, predict(mid, cars), method = "mse")
#' TSS <- weighted.loss(cars$dist, method = "mse")
#' RSS / TSS
#' mid$ratio
#'
#' # Verify uninterpreted variation ratio of a fitted MID model with weights
#' w <- 1:nrow(cars)
#' mid <- interpret(dist ~ speed, cars, weights = w)
#' RSS <- weighted.loss(cars$dist, predict(mid, cars), w = w, method = "mse")
#' TSS <- weighted.loss(cars$dist, w = w, method = "mse")
#' RSS / TSS
#' mid$ratio
#' @returns
#' \code{weighted.loss()} returns a single numeric value.
#'
#' @export weighted.loss
#'
weighted.loss <- function(
    x, y = NULL, w = NULL, na.rm = FALSE, method = "rmse"
  ) {
  if (na.rm) {
    ok <- !is.na(x)
    if (!is.null(w))
      ok <- ok & !is.na(w) & w > 0
    if (!is.null(y))
      ok <- ok & !is.na(y)
    x <- x[ok]
    if (!is.null(w))
      w <- w[ok]
    if (!is.null(y))
      y <- y[ok]
  }
  if (!is.null(w) && length(w) != length(x))
    stop("'x' and 'w' must have the same length")
  if (is.null(y)) {
    y <- if (is.null(w)) sum(x) / length(x) else sum(x * w) / sum(w)
  } else if (length(y) != length(x)) {
    stop("'x' and 'y' must have the same length")
  }
  x <- x - y
  if (method == "rmse") {
    if (is.null(w)) sqrt(sum(x ^ 2) / length(x))
    else sqrt(sum(x ^ 2 * w) / sum(w))
  } else if (method == "mse") {
    if (is.null(w)) sum(x ^ 2) / length(x)
    else sum(x ^ 2 * w) / sum(w)
  } else if (method == "mae") {
    if (is.null(w)) sum(abs(x)) / length(x)
    else sum(abs(x) * w) / sum(w)
  } else if (method == "medae") {
    if (is.null(w)) stats::quantile(abs(x), probs = 0.5, na.rm = na.rm,
                                    names = FALSE, type = 1L)
    else weighted.quantile(abs(x), w, probs = 0.5, na.rm = na.rm,
                           names = FALSE, type = 1L)
  } else
    stop("'method' must be one of 'mse', 'rmse', 'mae', or 'medae'")
}


weighted.rmse <- function(x, y = NULL, w = NULL, na.rm = FALSE) {
  weighted.loss(x, y = y, w = w, na.rm = na.rm, method = "rmse")
}

weighted.mae <- function(x, y = NULL, w = NULL, na.rm = FALSE) {
  weighted.loss(x, y = y, w = w, na.rm = na.rm, method = "mae")
}

weighted.medae <- function(x, y = NULL, w = NULL, na.rm = FALSE) {
  weighted.loss(x, y = y, w = w, na.rm = na.rm, method = "medae")
}
