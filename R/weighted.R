#' Weighted Data Frames
#'
#' Returns a data frame of class \code{weighted} and \code{data.frame}, which contains the additional attribute "weights".
#' Weights can be extracted using functions like \code{attr()} or \code{stats::weights()}.
#'
#' @param data a data frame for which weights are to be added.
#' @param weights a numeric vector, specifying the weights of each record.
#' @param ... not used.
#' @examples
#' x1 <- runif(1000L, -1, 1)
#' x2 <- x1 + runif(1000L, -1, 1)
#' X <- weighted(cbind(x1, x2), (abs(x1) + abs(x2)) / 2)
#' Y1 <- weighted(X)
#' ggplot2::ggplot(Y1, ggplot2::aes(x1, x2, alpha = weights(Y1))) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "weighted") +
#'   ggplot2::theme_bw()
#' Y2 <- shuffled(X)
#' ggplot2::ggplot(Y2, ggplot2::aes(x1, x2, alpha = weights(Y2))) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "shuffled") +
#'   ggplot2::theme_bw()
#' Y3 <- augmented(X)
#' ggplot2::ggplot(Y3, ggplot2::aes(x1, x2, alpha = weights(Y3))) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "augmented") +
#'   ggplot2::theme_bw()
#' Y4 <- latticized(X)
#' ggplot2::ggplot(Y4, ggplot2::aes(x1, x2, size = weights(Y4))) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "latticized") +
#'   ggplot2::theme_bw()
#' @export weighted
#'
weighted <- function(data, weights = NULL, ...) {
  if (!is.data.frame(data))
    data <- as.data.frame(data)
  if (missing(weights))
    weights <- attr(data, "weights")
  if (is.null(weights))
    weights <- rep.int(1, nrow(data))
  if (!is.numeric(weights) || anyNA(weights))
    stop("'weights' must be a numeric vector without missing values")
  if (length(weights) != nrow(data))
    stop("length of 'weights' doesn't match the number of rows in 'data'")
  if (any(weights < 0))
    stop("negative weights not allowed")
  structure(data, class = c("weighted", "data.frame"), weights = weights)
}

#' @rdname weighted
#' @param size an integer, specifying the number of new rows to be added to the original data frame.
#' @param ratio a numeric value. Weights for the new rows are calculated as \code{nrow(data) * ratio / size}.
#' @export augmented
#'
augmented <- function(data, weights = NULL, size = nrow(data), ratio = .01) {
  cl <- as.list(match.call())
  cl[[1L]] <- NULL
  data <- do.call(weighted, cl)
  weights <- attr(data, "weights")
  new.data <- list()
  size <- as.integer(size)
  for (i in 1:ncol(data))
    new.data[[i]] <- sample(data[, i], size, TRUE, prob = weights)
  new.data <- as.data.frame(new.data)
  colnames(new.data) <- colnames(data)
  w <- sum(weights) * ratio / size
  wts <- c(weights, rep.int(w, size))
  data <- rbind(data, new.data)
  structure(data, class = c("weighted", "data.frame"), weights = wts)
}

#' @rdname weighted
#' @export shuffled
#'
shuffled <- function(data, weights = NULL, size = nrow(data)) {
  cl <- as.list(match.call())
  cl[[1L]] <- NULL
  data <- do.call(weighted, cl)
  weights <- attr(data, "weights")
  new.data <- list()
  size <- as.integer(size)
  for (i in 1:ncol(data))
    new.data[[i]] <- sample(data[, i], size, TRUE, prob = weights)
  new.data <- as.data.frame(new.data)
  colnames(new.data) <- colnames(data)
  wts <- rep.int(1, size)
  structure(new.data, class = c("weighted", "data.frame"), weights = wts)
}


#' @rdname weighted
#' @param k an integer or a numeric vector of length two for main effects and interactions, specifying the maximum number of sample points for each numeric predictor variable. If an integer is passed, k is used for main effect terms and the square root of k is used for interaction terms. If not positive, all unique values are used as sample points.
#' @param type an integer or a vector of length two, specifying the type of piecewise functions to be fit on numeric variables. '0' is for step functions on discretised intervals, and '1' is for piecewise linear functions connecting at representative values.
#' @param use.catchall logical. If TRUE, less frequent levels of factor variables are dropped and replaced with the catchall level.
#' @param catchall a character used as the name of "catchall" level for unused levels of each factor variable. Used only when \code{factor.option} is not 0.
#' @param frames a named list of encoding frames, which specifies bins for quantitative features or levels for qualitative features.
#' @param keep.mean logical. If TRUE, the mean is used
#' @export latticized
#'
latticized <- function(
    data, weights = NULL, k = 10L, type = 1L, use.catchall = TRUE,
    catchall = "(others)", frames = list(), keep.mean = TRUE) {
  cl <- as.list(match.call())
  cl[[1L]] <- NULL
  data <- do.call(weighted, cl)
  weights <- attr(data, "weights")
  for (i in 1L:ncol(data)) {
    tag <- colnames(data)[i]
    x <- data[, i]
    if (is.numeric(x)) {
      enc <- numeric.encoder(x = x, k = k, type = type,
                             tag = tag, frame = frames[[tag]])
      br <- attr(enc$frame, "breaks")
      x <- pmax(pmin(x, br[length(br)]), br[1L])
      x <- .bincode(x, br, right = TRUE, include.lowest = TRUE)
      reps <- attr(enc$frame, "reps")
      if (keep.mean)
        reps <- tapply(data[, i], x, mean)
      data[, i] <- reps[x]
    } else {
      enc <- factor.encoder(x = x, k = k,
                            use.catchall = use.catchall, catchall = catchall,
                            tag = tag, frame = frames[[tag]])
      x <- factor(x, levels = attr(enc$frame, "levels"))
      x[is.na(x)] <- attr(enc$frame, "catchall")
      data[, i] <- x
    }
  }
  new.data <- stats::aggregate(attr(data, "weights") ~ ., data, sum)
  wts <- new.data[, ncol(new.data)]
  new.data <- new.data[-ncol(new.data)]
  structure(new.data, class = c("weighted", "data.frame"), weights = wts)
}


#' @rdname weighted
#' @param object a weighted data frame.
#' @exportS3Method stats::weights
#'
weights.weighted <- function(object, ...) {
  attr(object, "weights")
}
