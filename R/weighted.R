#' Weighted Data Frames
#'
#' \code{weighted()} returns a data frame with sample weights.
#'
#' \code{weighted()} returns a data frame with the "weights" attribute that can be extracted using \code{stats::weights()}.
#' \code{augmented()}, \code{shuffled()} and \code{latticized()} return a weighted data frame with some data modifications.
#' These functions are designed for use with \code{interpret()}.
#' As the modified data frames do not preserve the original correlation structure of the variables, the response variable (y) should always be replaced by the model predictions (yhat).
#'
#' @param data a data frame.
#' @param weights a numeric vector of sample weights for each observation in \code{data}.
#' @examples
#' set.seed(42)
#' x1 <- runif(1000L, -1, 1)
#' x2 <- x1 + runif(1000L, -1, 1)
#' weights <- (abs(x1) + abs(x2)) / 2
#' x <- data.frame(x1, x2)
#' xw <- weighted(x, weights)
#' ggplot2::ggplot(xw, ggplot2::aes(x1, x2, alpha = weights(xw))) +
#'   ggplot2::geom_point() +
#'   ggplot2::ggtitle("weighted")
#' xs <- shuffled(xw)
#' ggplot2::ggplot(xs, ggplot2::aes(x1, x2, alpha = weights(xs))) +
#'   ggplot2::geom_point() +
#'   ggplot2::ggtitle("shuffled")
#' xa <- augmented(xw)
#' ggplot2::ggplot(xa, ggplot2::aes(x1, x2, alpha = weights(xa))) +
#'   ggplot2::geom_point() +
#'   ggplot2::ggtitle("augmented")
#' xl <- latticized(xw)
#' ggplot2::ggplot(xl, ggplot2::aes(x1, x2, size = weights(xl))) +
#'   ggplot2::geom_point() +
#'   ggplot2::ggtitle("latticized")
#' @returns
#' \code{weighted()} returns a data frame with the attribute "weights".
#' \code{augmented()} returns a weighted data frame of the original data and the shuffled data with relatively small weights.
#' \code{shuffled()} returns a weighted data frame of the shuffled data.
#' \code{latticized()} returns a weighted data frame of latticized data, whose values are grouped and replaced by the representative value of the corresponding group.
#' @export weighted
#'
weighted <- function(data, weights = NULL) {
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
#' @param size integer. The number of random observations whose values are sampled from the marginal distribution of each variable.
#' @param r a numeric value specifying the ratio of the total weights for the random observations to the sum of sample weights. The weight for the random observations is calculated as \code{sum(attr(data, "weights")) * r / size}.
#' @export augmented
#'
augmented <- function(data, weights = NULL, size = nrow(data), r = .01) {
  data <- if (missing(weights)) weighted(data) else weighted(data, weights)
  weights <- attr(data, "weights")
  new.data <- list()
  size <- as.integer(size)
  for (i in 1L:ncol(data))
    new.data[[i]] <- sample(data[, i], size, replace = TRUE, prob = weights)
  new.data <- as.data.frame(new.data)
  colnames(new.data) <- colnames(data)
  w <- sum(weights) * r / size
  weights <- c(weights, rep.int(w, size))
  data <- rbind(data, new.data)
  structure(data, class = c("weighted", "data.frame"), weights = weights)
}

#' @rdname weighted
#' @export shuffled
#'
shuffled <- function(data, weights = NULL, size = nrow(data)) {
  data <- if (missing(weights)) weighted(data) else weighted(data, weights)
  weights <- attr(data, "weights")
  new.data <- list()
  size <- as.integer(size)
  for (i in 1L:ncol(data))
    new.data[[i]] <- sample(data[, i], size, replace = TRUE, prob = weights)
  new.data <- as.data.frame(new.data)
  colnames(new.data) <- colnames(data)
  weights <- rep.int(1, size)
  structure(new.data, class = c("weighted", "data.frame"), weights = weights)
}


#' @rdname weighted
#' @param k integer. The maximum number of sample points for each variable. If not positive, all unique values are used as sample points.
#' @param type integer. The type of encoding of quantitative variables to be passed to \code{numeric.encoder()}.
#' @param use.catchall logical. If \code{TRUE}, less frequent levels of factor variables are dropped and replaced by the catchall level.
#' @param catchall a character string to be used as the catchall level.
#' @param frames a named list of encoding frames ("numeric.frame" or "factor.frame" objects).
#' @param keep.mean logical. If \code{TRUE}, the representative values of each group is the average of the corresponding group.
#' @export latticized
#'
latticized <- function(
    data, weights = NULL, k = 10L, type = 0L, use.catchall = TRUE,
    catchall = "(others)", frames = list(), keep.mean = TRUE) {
  data <- if (missing(weights)) weighted(data) else weighted(data, weights)
  weights <- attr(data, "weights")
  for (i in 1L:ncol(data)) {
    tag <- colnames(data)[i]
    x <- data[, i]
    if (is.numeric(x)) {
      enc <- numeric.encoder(x = x, k = k, type = type,
                             tag = tag, frame = frames[[tag]])
      br <- attr(enc$frame, "breaks")
      x <- findInterval(x, br, all.inside = TRUE)
      reps <- if (keep.mean) {
        tapply(data[, i], x, mean)
      } else {
        attr(enc$frame, "reps")
      }
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
  weights <- new.data[, ncol(new.data)]
  new.data <- new.data[-ncol(new.data)]
  structure(new.data, class = c("weighted", "data.frame"), weights = weights)
}


#' @rdname weighted
#' @param object a data frame with the attribute "weights".
#' @param ... not used.
#' @exportS3Method stats::weights
#'
weights.weighted <- function(object, ...) {
  attr(object, "weights")
}
