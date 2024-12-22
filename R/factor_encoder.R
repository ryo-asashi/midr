#' Encoder for Qualitative Variables
#'
#' Returns a list consisting of the encoding information of a predictor variable as a qualitative variable.
#'
#' @param x a vector to be encoded as a qualitative variable.
#' @param k an integer specifying the max number of the levels after the encoding. If not positive, all unique values of x are chosen as sample points.
#' @param use.catchall logical. If TRUE, less frequent levels are dropped and replaced with the catchall level.
#' @param catchall a character used as the name of "catchall" level for unused levels.
#' @param tag name of the variable.
#' @param frame a \code{factor.frame} object or a vector, containing the information about the levels of the variable.
#' @param weights optional. a numeric vector indicating the weight of each value of 'x'.
#' @examples
#' data(iris, package = "datasets")
#' enc <- factor.encoder(x = iris$Species, use.catchall = FALSE)
#' enc$frame
#' enc$encode(new_x = c("setosa", "virginica", "ensata", NA, "versicolor"))
#' @returns
#' \code{factor.encoder()} returns a list containing the following components:
#' \item{frame}{a data frame containing the encoding information.}
#' \item{encode}{a function to encode new data into a dummy matrix.}
#' \item{n}{the number of encoding levels.}
#' \item{type}{type of encoding.}
#' @export factor.encoder
#'
factor.encoder <- function(
    x, k, use.catchall = TRUE, catchall = "(others)", tag = "x", frame = NULL,
    weights = NULL) {
  # set levels --------
  if (!is.null(frame)) {
    if (is.vector(frame))
      frame <- factor.frame(levels = frame, catchall = frame[length(frame)])
    if (!inherits(frame, "factor.frame"))
      stop("invalid factor.frame supplied")
    flvs <- attr(frame, "levels")
    catchall <- attr(frame, "catchall")
    use.catchall <- !is.null(catchall)
  } else {
    if (!is.factor(x))
      x <- factor(x)
    flvs <- levels(x)
    if (use.catchall && k > 0L && length(flvs) >= k) {
      tbl <- weighted.tabulate(bin = match(x, flvs), w = weights)
      ord <- order(tbl, decreasing = TRUE)
      flvs <- flvs[ord][seq_len(k - 1L)]
    }
    flvs <- flvs[flvs %in% x]
    if (use.catchall)
      flvs <- c(flvs, catchall)
  }
  nlvs <- length(flvs)
  if (!use.catchall)
    catchall <- NULL
  frame <- factor.frame(levels = flvs, catchall = catchall, tag = tag)
  # define encoder function --------
  encode <- function(new_x, ...) {
    n <- length(new_x)
    mat <- matrix(0, nrow = n, ncol = nlvs)
    if (!is.factor(new_x) || !identical(levels(new_x), flvs))
      new_x <- factor(new_x, levels = flvs)
    if (use.catchall)
      new_x[is.na(new_x)] <- catchall
    new_x <- as.integer(new_x)
    for (i in seq_len(n)) {
      if (is.na(new_x[i]))
        next
      mat[i, new_x[i]] <- 1
    }
    mat
  }
  list(frame = frame, encode = encode, n = nlvs, type = "factor")
}

#' @rdname factor.encoder
#' @param levels a vector to be used as the levels of the variable.
#'
#' @export factor.frame
#'
factor.frame <- function(levels, catchall = "(others)", tag = "x") {
  levels <- unique(c(levels, catchall))
  frame <- data.frame(factor(levels, levels = levels))
  frame[, 2L] <- as.integer(frame[, 1L])
  colnames(frame) <- paste0(tag, c("", "_level"))
  class(frame) <- c("data.frame", "factor.frame")
  structure(frame, levels = levels, catchall = catchall)
}
