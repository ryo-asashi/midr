#' Subset MID Objects
#'
#' @description
#' S3 methods to extract parts of a "midrib" or "midlist" collection object.
#'
#' @details
#' A "midlist" or "midrib" object stores multiple objects of the same single base class: "mid", "midimp", "midcon", or "midbrk".
#'
#' When extracting items using \code{[}, it returns a subsetted "midlist" or "midrib" object, preserving its collection class (e.g., "mids", "midimps").
#' By default, if a single base object is extracted (\code{length(i) == 1L}) and \code{drop = TRUE}, the object is simplified to a single base object (e.g., "mid", "midimp").
#' \code{[[} always extracts a single base object.
#'
#' @param x a collection object of class "midlist" or "midrib".
#' @param i indices specifying elements to extract. Can be numeric, character, or logical vectors.
#' @param drop logical. If \code{TRUE} the result is coerced to the lowest possible dimension. For a collection (e.g., "mids"), extracting a single element drops it to a base object (e.g., "mid").
#' @param exact logical. If \code{TRUE}, exact matching is used for character strings.
#'
#' @examples
#' # Fit a multivariate linear model
#' fit <- lm(cbind(y1, y2, y3) ~ x1 + I(x1^2), data = anscombe)
#'
#' # Interpret the linear models
#' mid_collection <- interpret(cbind(y1, y2, y3) ~ x1, data = anscombe, model = fit)
#'
#' # Check the default labels
#' labels(mid_collection)
#'
#' # Rename the models in the collection
#' labels(mid_collection) <- letters[1L:3L]
#' labels(mid_collection)
#'
#' # Extract a single base "mid" object by its new name using [[
#' mid <- mid_collection[["a"]]
#' class(mid)
#'
#' # Subset the collection to keep only the first two models using [
#' mid_subset <- mid_collection[1:2]
#' class(mid_subset) # Maintains the collection class (e.g., "mids"-"midrib")
#' @returns
#' \code{[} returns a subsetted collection object (e.g., "midrib", "midlist") or a single base object if \code{drop = TRUE}.
#'
#' \code{[[} returns a single base object (e.g., "mid").
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
  if (anyNA(i)) stop("undefined item selected")
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
  class(x) <- if (drop) "mid" else c("mids", "midrib")
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
  out <- NextMethod("[")
  if (drop && length(out) == 1L) return(out[[1L]])
  class(out) <- class(x)
  out
}
