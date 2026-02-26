#' Print MID Models
#'
#' @description
#' \code{print.mid()} is an S3 method for "mid" objects that prints a concise summary of a fitted MID model.
#'
#' @details
#' By default, the \code{print()} method for "mid" objects provides a quick overview of the model structure by listing the number of main effect and interaction terms.
#' If \code{main.effects = TRUE} is specified, the method will also print the contribution of each main effect at its sample points, providing a more detailed look at the model's components.
#'
#' @param x a "mid" object to be printed.
#' @param digits an integer specifying the number of significant digits for printing.
#' @param main.effects logical. If \code{TRUE}, the MID values of each main effect are also printed.
#' @param ... arguments to be passed to other methods (not used in this method).
#'
#' @examples
#' data(cars, package = "datasets")
#' mid <- interpret(dist ~ speed, cars)
#'
#' # Default print provides a concise summary
#' print(mid)
#'
#' # Setting main.effects = TRUE prints the contributions of each main effect
#' print(mid, main.effects = TRUE)
#' @returns
#' \code{print.mid()} returns the original "mid" object invisibly.
#'
#' \code{print.midlist()} returns the original "midlist" object invisibly.
#'
#' @seealso \code{\link{interpret}}, \code{\link{summary.mid}}
#'
#' @exportS3Method base::print
#'
print.mid <- function(
    x, digits = max(3L, getOption("digits") - 2L), main.effects = FALSE, ...) {
  cl <- paste0(trimws(deparse(x$call)), sep = "", collapse = "\n ")
  cat(paste0("\nCall:\n", cl, "\n", collapse = ""))
  if (!is.null(x$model.class))
    cat(paste0("\nModel Class: ", paste0(x$model.class, collapse = ", "), "\n"))
  cat(paste0("\nIntercept: ", examples(x$intercept, digits = digits),
             "\n", collapse = ""))
  m <- length(x$main.effects)
  if (m > 0L) {
    cat("\nMain Effects:\n")
    if (main.effects) {
      for (i in seq_len(m)) {
        cat(paste0("---\n$",
                   names(x$main.effects)[i], "\n", collapse=""))
        vl <- x$main.effects[[i]][, "mid"]
        nm <- x$main.effects[[i]][, 1L]
        if (is.numeric(nm))
          nm <- format(nm, digits = digits)
        names(vl) <- nm
        print.default(vl, digits = digits)
      }
    } else {
      cat(paste0(m, " main effect term", if (m > 1L) "s", "\n"))
    }
  }
  m <- length(x$interactions)
  if (m > 0L) {
    cat(paste0("\nInteractions:\n",
               m, " interaction term", if (m > 1L) "s", "\n"))
  }
  ur <- x$ratio
  ur <- if (inherits(x, "mid")) ur[1L] else if (is.matrix(ur)) ur[1L, ] else ur
  cat(paste0("\nUninterpreted Variation Ratio: ",
             examples(ur, digits = digits), "\n"))
  invisible(x)
}


#' @exportS3Method base::print
#'
print.midlist <- function(x, max.nmodels = 1L, ...) {
  if (inherits(x, "midrib"))
    return(print.mid(x))
  nms <- labels(x)
  nmodels <- length(nms)
  n <- min(nmodels, max.nmodels)
  Map(
    function(obj, nm) {
      cat("\n$", nm, "\n", sep = "")
      print.mid(obj, ...)
    }, x[seq_len(n), drop = FALSE], nms[seq_len(n)]
  )
  if (n < nmodels && n > 0L)
    cat(sprintf("\n... and %d more models", nmodels - max.nmodels))
  invisible(x)
}
