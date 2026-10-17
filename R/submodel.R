#' Extract Submodels
#'
#' @description
#' \code{submodel()} creates a new, lightweight model object containing only a specified subset of terms (main effects and/or interactions) from a fitted model.
#'
#' @param object a fitted model object.
#' @param ... further arguments.
#'
#' @export submodel
#'
submodel <- function(object, ...)
UseMethod("submodel")


#' @rdname submodel
#'
#' @param terms a character vector or object coercible to \code{\link[state]{terms.object}}.
#' @param intercept logical. If \code{TRUE}, the intercept is preserved.
#'
#' @exportS3Method midr::submodel
#'
submodel.mid <- function(
    object, terms = NULL, intercept = NULL, ...
) {
  .terms <- stats::terms(object)
  terms <- as_terms(
    terms, data = model_data(object, env = parent.frame())
  ) %||% .terms
  mlabs <- unique(match_labels(terms, .terms, nomatch = 0L))
  intercept <- intercept %||% has_intercept(terms)
  obj <- object
  if (!intercept)
    obj$intercept <- 0
  if (!is.null(obj$main.effects)) {
    keep <- intersect(names(obj$main.effects), mlabs)
    obj$main.effects <- obj$main.effects[keep]
  }
  if (!is.null(obj$interactions)) {
    keep <- intersect(names(obj$interactions), mlabs)
    obj$interactions <- obj$interactions[keep]
  }
  obj$terms <- as_terms(mlabs, intercept = intercept)
  obj$ratio <- NA
  obj
}

#' @exportS3Method midr::submodel
#'
submodel.midrib <- function(object, ...) {
  submodel.mid(object, ...)
}
