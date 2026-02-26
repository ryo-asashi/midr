#' Label MID Objects
#'
#' @description
#' S3 methods to get or set the labels (names) of a "midrib" or "midlist" object.
#'
#' @details
#' While a "midlist" object is a standard R list containing only one of a single base class, a "midrib" object stores multiple MID models in an optimized struct-of-arrays format.
#'
#' Because of the internal struct-of-arrays ("AsIs") structure, using \code{names()} on a "midrib" object returns internal component names (e.g., "intercept", "main.effects").
#' To safely access or modify the names of the models, always use \code{labels()} and \code{labels<-}.
#'
#' @param object a collection object of class "midlist" or "midrib".
#' @param value a character vector of the same length as the number of base objects in the collection object.
#' @param ... optional parameters passed to other methods.
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
#' \code{labels} returns a character vector of labels of the stored base objects.
#'
#' \code{labels<-} returns the updated collection object with new labels.
#'
#' @exportS3Method base::labels
#'
labels.midlist <- function(object, ...) {
  names(object) %||% as.character(seq_along(object))
}

#' @rdname labels.midlist
#' @export
#'
labels.midrib <- function(object, ...) {
  names(object$intercept) %||% as.character(seq_along(object$intercept))
}

#' @rdname labels.midlist
#' @export
#'
`labels<-` <- function(object, value) {
  UseMethod("labels<-")
}

#' @rdname labels.midlist
#' @export
#'
`labels<-.midrib` <- function(object, value) {
  nlab <- length(object$intercept)
  if (length(value) != nlab) {
    stop("length of 'value' must match the number of labels (", nlab, ")")
  }
  value <- as.character(value)
  names(object$intercept) <- value
  if (!is.null(object$main.effects))
    object$main.effects <- lapply(object$main.effects, .relabel, value)
  if (!is.null(object$interactions))
    object$interactions <- lapply(object$interactions, .relabel, value)
  colnames(object$fitted.values) <- value
  colnames(object$residuals) <- value
  if (!is.null(object$linear.predictors))
    colnames(object$linear.predictors) <- value
  if (!is.null(object$response.residuals))
    colnames(object$response.residuals) <- value
  if (is.matrix(object$ratio)) {
    colnames(object$ratio) <- value
  } else {
    names(object$ratio) <- value
  }
  object
}

.relabel <- function(object, value) {
  colnames(object$mid) <- value
  object
}

#' @rdname labels.midlist
#' @export
#'
`labels<-.midlist` <- function(object, value) {
  if (length(value) != length(object))
    stop("length of 'value' must match the number of models")
  names(object) <- as.character(value)
  object
}
