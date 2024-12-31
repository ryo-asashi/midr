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
#' @param ... not used.
#' @examples
#' x1 <- runif(1000L, -1, 1)
#' x2 <- x1 + runif(1000L, -1, 1)
#' X <- weighted(cbind(x1, x2), (abs(x1) + abs(x2)) / 2)
#' Y1 <- weighted(X)
#' ggplot2::ggplot(Y1, ggplot2::aes(x1, x2, alpha = weights(Y1))) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "weighted")
#' Y2 <- shuffled(X)
#' ggplot2::ggplot(Y2, ggplot2::aes(x1, x2, alpha = weights(Y2))) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "shuffled")
#' Y3 <- augmented(X)
#' ggplot2::ggplot(Y3, ggplot2::aes(x1, x2, alpha = weights(Y3))) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "augmented")
#' Y4 <- latticized(X)
#' ggplot2::ggplot(Y4, ggplot2::aes(x1, x2, size = weights(Y4))) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "latticized")
#' @returns
#' \code{weighted()} returns a data frame with the attribute "weights".
#' \code{augmented()} returns a weighted data frame of the original data and the shuffled data with relatively small weights.
#' \code{shuffled()} returns a weighted data frame of the shuffled data.
#' \code{latticized()} returns a weighted data frame of latticized data, whose values are grouped and replaced by the representative value of the corresponding group.
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
#' @param size integer. The number of random observations whose values are sampled from the marginal distribution of each variable.
#' @param r a numeric value specifying the ratio of the total weights for the random observations to the sum of sample weights. The weight for the random observations is calculated as \code{sum(attr(data, "weights")) * r / size}.
#' @export augmented
#'
augmented <- function(data, weights = NULL, size = nrow(data), r = .01) {
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
  w <- sum(weights) * r / size
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
  wts <- new.data[, ncol(new.data)]
  new.data <- new.data[-ncol(new.data)]
  structure(new.data, class = c("weighted", "data.frame"), weights = wts)
}


#' @rdname weighted
#' @param object a data frame with the attribute "weights".
#' @exportS3Method stats::weights
#'
weights.weighted <- function(object, ...) {
  attr(object, "weights")
}
