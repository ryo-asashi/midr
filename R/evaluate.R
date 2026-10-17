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
    object, type = c("terms", "response", "linear", "variables"), ...
) {
  type <- match.arg(type)
  grid <- object$gma.grid %||% set_grid(object, ...)$gma.grid
  if (type == "response" || type == "linear") {
    vmat <- as.matrix(stats::predict(object, newdata = grid, type = type))
    if (ncol(vmat) == 1L)
      colnames(vmat) <- ".prediction"
  } else if (type == "terms" || type == "variables") {
    vmat <- stats::predict(object, newdata = grid, type = "terms")
  } else if (type == "variables") {
    vlist <- lapply(colnames(vmat), get_variables)
    vlens <- lengths(vlist)
    for (j in seq_along(vlens)) {
      if (vlens[j] > 1L) {
        vmat[, vlist[j]] <- vmat[, vlist[j]] + vmat[, j] / vlens[j]
      }
    }
    vmat <- vmat[, vlens == 1L]
  }
  structure(
    vmat,
    grid = grid,
    type = type,
    row.weights = attr(grid, "gma.weights"),
    axis = attr(grid, "gma.axis")
  )
}
