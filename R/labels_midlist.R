#' Modify Labels of a Collection of MID Models
#'
#' @description
#' S3 methods to get/set the labels (model names) of a "midrib" or "midlist" object.
#'
#' @details
#' The "midrib" object stores multiple fitted MID models in a optimized struct-of-arrays format.
#'
#' Because of the internal struct-of-arrays ("AsIs") structure, using \code{names()} on a "midrib" object returns internal component names (e.g., "intercept", "main.effects").
#' To safely access or modify the names of the models, always use \code{labels()} and \code{labels<-}.
#'
#' @param object a "midrib" object, or its summary objects (e.g., "midlist.importance").
#' @param value a character vector of the same length as the number of models in the "midlist" object.
#' @param ... optional parameters passed to other methods.
#'
#' @returns
#' \itemize{
#'   \item \code{labels} returns a character vector of model names.
#'   \item \code{labels<-} returns the updated object with new labels.
#' }
#'
#' @exportS3Method base::labels
#'
labels.midrib <- function(object, ...) {
  names(object$intercept) %||% as.character(seq_along(object$intercept))
}

#' @rdname labels.midrib
#' @export
#'
`labels<-` <- function(object, value) {
  UseMethod("labels<-")
}

#' @rdname labels.midrib
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

#' @rdname labels.midrib
#' @export
#'
`labels<-.midlist` <- function(object, value) {
  if (length(value) != length(object))
    stop("length of 'value' must match the number of models")
  names(object) <- as.character(value)
  object
}

#' @export
#'
`labels<-.midlist.importance` <- `labels<-.midlist`

#' @export
#'
`labels<-.midlist.conditional` <- `labels<-.midlist`

#' @export
#'
`labels<-.midlist.breakdown` <- `labels<-.midlist`
