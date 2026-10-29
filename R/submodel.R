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
  full <- stats::terms(object)
  sub <- as_terms(
    x = terms,
    data = model_data(object, env = parent.frame()),
    env = environment(full)
  ) %||% full
  mlabs <- unique(match_labels(sub, full, nomatch = "fail"))
  intercept <- intercept %||% has_intercept(sub)
  obj <- object
  if (!intercept)
    obj$intercept <- numeric(length(obj$intercept))
  if (!is.null(obj$main.effects)) {
    keep <- intersect(names(obj$main.effects), mlabs)
    obj$main.effects <- obj$main.effects[keep]
  }
  if (!is.null(obj$interactions)) {
    keep <- intersect(names(obj$interactions), mlabs)
    obj$interactions <- obj$interactions[keep]
  }
  obj$terms <- as_terms(mlabs, intercept = intercept, env = environment(full))
  obj$ratio <- NA
  obj
}

#' @exportS3Method midr::submodel
#'
submodel.midrib <- function(object, ...) {
  submodel.mid(object, ...)
}
