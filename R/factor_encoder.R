#' Encoder for Qualitative Variables
#'
#' \code{factor.encoder()} returns an encoder for a qualitative variable.
#'
#' \code{factor.encoder()} extracts the unique values (levels) from the vector \code{x} and returns a list containing the \code{encode()} function to convert a vector into a dummy matrix using one-hot encoding.
#' If \code{use.catchall} is \code{TRUE} and the number of levels exceeds \code{k}, only the most frequent k - 1 levels are used and the other values are replaced by the \code{catchall}.
#'
#' @param x a vector to be encoded as a qualitative variable.
#' @param k an integer specifying the maximum number of distinct levels. If not positive, all unique values of \code{x} are used as levels.
#' @param use.catchall logical. If \code{TRUE}, less frequent levels are dropped and replaced by the catchall level.
#' @param catchall a character string to be used as the catchall level.
#' @param tag character string. The name of the variable.
#' @param frame a "factor.frame" object or a character vector that defines the levels of the variable.
#' @param weights optional. A numeric vector of sample weights for each value of \code{x}.
#' @examples
#' data(iris, package = "datasets")
#' enc <- factor.encoder(x = iris$Species, use.catchall = FALSE, tag = "Species")
#' enc$frame
#' enc$encode(new_x = c("setosa", "virginica", "ensata", NA, "versicolor"))
#'
#' frm <- factor.frame(c("setosa", "virginica"), "other iris")
#' enc <- factor.encoder(x = iris$Species, frame = frm)
#' enc$encode(c("setosa", "virginica", "ensata", NA, "versicolor"))
#'
#' enc <- factor.encoder(x = iris$Species, frame = c("setosa", "versicolor"))
#' enc$encode(c("setosa", "virginica", "ensata", NA, "versicolor"))
#' @returns
#' \code{factor.encoder()} returns a list containing the following components:
#' \item{frame}{an object of class "factor.frame".}
#' \item{encode}{a function to encode \code{new_x} into a dummy matrix.}
#' \item{n}{the number of encoding levels.}
#' \item{type}{the type of encoding.}
#' \code{factor.frame()} returns a "factor.frame" object containing the encoding information.
#' @export factor.encoder
#'
factor.encoder <- function(
    x, k, use.catchall = TRUE, catchall = "(others)", tag = "x", frame = NULL,
    weights = NULL) {
  # set levels --------
  if (!is.null(frame)) {
    if (is.vector(frame))
      frame <- factor.frame(levels = frame, catchall = NULL)
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
    colnames(mat) <- flvs
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
