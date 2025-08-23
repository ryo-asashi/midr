#' Encoder for Qualitative Variables
#'
#' @description
#' \code{factor.encoder()} creates an encoder function for a qualitative (factor or character) variable.
#' This encoder converts the variable into a one-hot encoded (dummy) design matrix.
#'
#' @details
#' This function is designed to handle qualitative data for use in the MID model's linear system formulation.
#' It has two key features:
#' \itemize{
#'   \item One-Hot Encoding:
#'
#'   The primary mechanism is one-hot encoding.
#'   Each unique level of the input variable becomes a column in the output matrix.
#'   For a given observation, the column corresponding to its level is assigned a \code{1}, and all other columns are assigned \code{0}.
#'   \item Catch-All Mechanism for High Cardinality:
#'
#'   When a variable has many unique levels (high cardinality), you can use the \code{use.catchall = TRUE} and \code{k} arguments.
#'   This will group the \code{k - 1} most frequent levels into their own columns, while all other less frequent levels are consolidated into a single \code{catchall} level (e.g., "(others)" by default).
#'   This is crucial for preventing MID models from becoming overly complex.
#' }
#'
#' @param x a vector to be encoded as a qualitative variable.
#' @param k an integer specifying the maximum number of distinct levels to retain (including the catch-all level). If not positive, all unique values of \code{x} are used.
#' @param use.catchall logical. If \code{TRUE}, less frequent levels are grouped into the catch-all level.
#' @param catchall a character string for the catch-all level.
#' @param tag the name of the variable.
#' @param frame a "factor.frame" object or a character vector that explicitly defines the levels of the variable.
#' @param weights an optional numeric vector of sample weights for \code{x}.
#'
#' @examples
#' # Encode a character vector with NA
#' data(iris, package = "datasets")
#' enc <- factor.encoder(x = iris$Species, use.catchall = FALSE, tag = "Species")
#' enc
#' enc$encode(x = c("setosa", "virginica", "ensata", NA, "versicolor"))
#'
#' frm <- factor.frame(c("setosa", "virginica"), "other iris")
#' enc <- factor.encoder(x = iris$Species, frame = frm)
#' enc
#' enc$encode(c("setosa", "virginica", "ensata", NA, "versicolor"))
#'
#' enc <- factor.encoder(x = iris$Species, frame = c("setosa", "versicolor"))
#' enc$encode(c("setosa", "virginica", "ensata", NA, "versicolor"))
#' @returns
#' \code{factor.encoder()} returns an object of class "encoder". This is a list containing the following components:
#' \item{frame}{a "factor.frame" object containing the encoding information (levels).}
#' \item{encode}{a function to convert a vector \code{x} into a one-hot encoded matrix.}
#' \item{n}{the number of encoding levels (i.e., columns in the design matrix).}
#' \item{type}{a character string describing the encoding type: "factor" or "null".}
#' \code{factor.frame()} returns a "factor.frame" object containing the encoding information.
#'
#' @seealso \code{\link{numeric.encoder}}
#'
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
    if (use.catchall)
      flvs <- c(flvs, catchall)
  }
  type <- "factor"
  if (length(flvs) == 0L) {
    flvs <- character(1L)
    type <- "null"
  }
  nlvs <- length(flvs)
  if (!use.catchall)
    catchall <- NULL
  frame <- factor.frame(levels = flvs, catchall = catchall, tag = tag)
  # define encoder function --------
  if (type == "factor") {
    encode <- function(x, ...) {
      n <- length(x)
      mat <- matrix(0, nrow = n, ncol = nlvs)
      if (!is.factor(x) || !identical(levels(x), flvs))
        x <- factor(x, levels = flvs)
      if (use.catchall)
        x[is.na(x)] <- catchall
      ok <- !is.na(x)
      mat[cbind(which(ok), as.integer(x[ok]))] <- 1
      colnames(mat) <- flvs
      mat
    }
  } else {
    encode <- function(x, ...) {
      mat <- matrix(0, nrow = length(x), ncol = 1L)
      colnames(mat) <- "Void"
      mat
    }
  }
  environment(encode) <- rlang::env(
    rlang::ns_env("midr"),
    nlvs = nlvs, flvs = flvs, use.catchall = use.catchall, catchall = catchall
  )
  enc <- list(frame = frame, encode = encode, n = nlvs, type = type)
  structure(enc, class = "encoder")
}

#' @rdname factor.encoder
#' @param levels a vector to be used as the levels of the variable.
#'
#' @export factor.frame
#'
factor.frame <- function(levels, catchall = "(others)", tag = "x") {
  levels <- unique(c(levels, catchall))
  frame <- data.frame(factor(levels, levels = levels))
  frame[[2L]] <- as.integer(frame[[1L]])
  colnames(frame) <- paste0(tag, c("", "_level"))
  class(frame) <- c("factor.frame", "data.frame")
  structure(frame, levels = levels, catchall = catchall)
}
