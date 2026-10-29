#' Evaluate Models on Grid
#'
#' @description
#' \code{evaluate()} evaluates a fitted model on its attached \code{gma.grid} (or generates a default grid if none exists) and returns a structured evaluation array.
#'
#' @param object a fitted model object.
#' @param ... further arguments passed to methods.
#'
#' @export evaluate
evaluate <- function(object, ...)
UseMethod("evaluate")


#' @rdname evaluate
#'
#' @param type character string specifying the evaluation level: \code{"terms"} (term contributions), \code{"response"} (overall prediction), or \code{"variables"} (variable attributions / SHAP).
#'
#' @exportS3Method midr::evaluate
#'
evaluate.mid <- function(
    object, type = c("terms", "response", "link", "variables"), ...
) {
  type <- match.arg(type)
  grid <- object$gma.grid %||% set_grid(object, ...)$gma.grid
  if (type == "response" || type == "link") {
    out <- as.matrix(stats::predict(object, newdata = grid, type = type))
    if (ncol(out) == 1L)
      colnames(out) <- "prediction"
  } else if (type == "terms" || type == "variables") {
    out <- as.matrix(stats::predict(object, newdata = grid, type = "terms"))
  }
  if (type == "variables") {
    varlist <- lapply(colnames(out), get_variables)
    varlens <- lengths(varlist)
    allvars <- unique(unlist(varlist))
    trueout <- matrix(0, nrow = nrow(out), ncol = length(allvars))
    for (j in seq_along(varlens)) {
      vars <- varlist[[j]]
      trueout[, vars] <- trueout[, vars] + out[, j] / varlens[j]
    }
    out <- trueout
  }
  structure(out, type = type, grid = grid, class = c("gma_eval", class(out)))
}


#' @exportS3Method midr::evaluate
#'
evaluate.mids <- function(object, ...) {

}
