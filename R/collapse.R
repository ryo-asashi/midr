#' Collapse Evaluation Arrays
#'
#' @param object a \code{gma_evaluation} object returned by \code{evaluate()}.
#' @param direction a character string specifying the dimension to collapse: \code{"none"}, \code{"row"} (across grid points), \code{"column"} (across terms/variables), or \code{"layer"} (across models).
#' @param method a character string or function specifying the method to collapse (e.g., \code{"mean"}, \code{"sum"}, \code{"mean_abs"}, \code{"sd"}).
#' @param by a character vector of grid variables to group by when \code{direction = "row"}.
#'   Defaults to the \code{axis} attribute of the grid (Use \code{by = FALSE} or \code{NULL} to collapse all rows into a scalar).
#'
#' @export collapse
collapse <- function(
    object, direction = c("none", "row", "column", "layer"),
    method = NULL, by = attr(object, "gma.axis"), ...
) {
  direction <- match.arg(direction)
  colnames(object) <- paste0(".", colnames(object))
  cbind(attr(object, "grid"), object)
}
