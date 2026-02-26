#' Extract Parts of a Collection of MID Models
#'
#' @description
#' S3 methods to extract parts of a "midrib" or "midlist" object.
#'
#' @details
#' The "midrib" object stores multiple fitted MID models in a optimized struct-of-arrays format.
#'
#' When extracting multiple models using \code{[}, it returns a subsetted "midrib" or "midlist" object.
#' By default, if a single model is extracted (\code{length(i) == 1L}) and \code{drop = TRUE}, the object is simplified to a single "mid" object.
#' \code{[[} always extracts a single model as a "mid" object.
#'
#' Similar extraction rules apply to summary objects like "midlist.importance".
#'
#' @param x a "midrib" object, a "midlist" object, or its summary objects (e.g., "midlist.importance").
#' @param i indices specifying elements to extract. Can be numeric, character, or logical vectors.
#' @param drop logical. If \code{TRUE} the result is coerced to the lowest possible dimension. For a "midrib", extracting a single element drops it to a "mid" object.
#' @param exact logical. If \code{TRUE}, exact matching is used for character strings.
#'
#' @returns
#' \code{[} returns a "midrib", "midlist" or "mid" object.
#'
#' \code{[[} returns a single "mid" object.
#'
#' @name extract.midlist
#' @rdname extract.midlist
#' @export
#'
`[.midrib` <- function(
    x, i, drop = if (missing(i)) TRUE else length(i) == 1L
) {
  nm <- names(x$intercept)
  if (missing(i)) {
    i <- seq_along(nm)
  } else if (is.character(i) && !anyNA(i)) {
    i <- match(i, nm)
  } else if (is.logical(i) && !anyNA(i)) {
    i <- which(rep(i, length.out = length(nm)))
  } else if (is.numeric(i) && !anyNA(i)) {
    i <- as.integer(i)
    if (any(i < 0L)) {
      if (any(i > 0L))
        stop("only 0's may be mixed with negative subscripts")
      i <- seq_along(nm)[i]
    }
  }
  if (anyNA(i))
    stop("undefined item selected")
  if (is.numeric(i) && any(i > length(nm) | i < 0L))
    stop("subscript out of bounds")
  if (is.numeric(i)) {
    i <- i[i != 0L]
  }
  if (length(i) == 0L) return(NULL)
  drop <- drop && (length(i) == 1L)
  x$intercept <- x$intercept[i]
  if (drop) names(x$intercept) <- NULL
  if (!is.null(x$main.effects))
    x$main.effects <- lapply(x$main.effects, .extract, i, drop)
  if (!is.null(x$interactions))
    x$interactions <- lapply(x$interactions, .extract, i, drop)
  x$fitted.values <- x$fitted.values[, i, drop = drop]
  x$residuals <- x$residuals[, i, drop = drop]
  if (!is.null(x$linear.predictors))
    x$linear.predictors <- x$linear.predictors[, i, drop = drop]
  if (!is.null(x$response.residuals))
    x$response.residuals <- x$response.residuals[, i, drop = drop]
  if (is.matrix(x$ratio)) {
    x$ratio <- x$ratio[, i, drop = drop]
  } else {
    x$ratio <- if (drop) as.numeric(x$ratio) else x$ratio[i]
  }
  class(x) <- if (drop) "mid" else c("midrib", "midlist")
  x
}

#' @rdname extract.midlist
#' @export
#'
`[[.midrib` <- function(x, i, exact = TRUE) {
  if (length(i) != 1L)
    stop("attempt to select more than one element in vectorIndex")
  if (is.character(i) && !exact) {
    i <- pmatch(i, names(x$intercept))
  }
  x[i, drop = TRUE]
}

.extract <- function(x, i, drop = FALSE) {
  x$mid <- if (drop && length(i) == 1L)
    as.numeric(x$mid[, i]) else I(x$mid[, i, drop = FALSE])
  x
}

#' @rdname extract.midlist
#' @export
#'
`[.midlist` <- function(
    x, i, drop = if (missing(i)) TRUE else length(i) == 1L
) {
  cls <- class(x)
  x <- unclass(x)[i]
  if (drop && length(x) == 1L) return(x[[1L]])
  structure(x, class = cls)
}

#' @export
#'
`[.midlist.importance` <- `[.midlist`

#' @export
#'
`[.midlist.conditional` <- `[.midlist`

#' @export
#'
`[.midlist.breakdown` <- `[.midlist`
