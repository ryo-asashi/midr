#' Print MID Values
#'
#' For 'mid' objects, \code{print()} prints the MID values and the uninterpreted rate.
#'
#' The S3 method of \code{print()} for 'mid' objects prints the MID values of a fitted MID model and its uninterpreted rate.
#'
#' @param x a 'mid' object to be printed.
#' @param digits an integer specifying the number of significant digits.
#' @param omit.values logical. If \code{TRUE}, MID values of main effect are not printed.
#' @param ... not used.
#' @examples
#' data(cars, package = "datasets")
#' print(interpret(dist ~ speed, cars))
#' @returns
#' \code{print.mid()} returns the 'mid' object passed to the function without any modification.
#' @exportS3Method base::print
#'
print.mid <- function(
    x, digits = max(3L, getOption("digits") - 2L), omit.values = FALSE, ...) {
  cl <- paste0(trimws(deparse(x$call)), sep = "", collapse = "\n ")
  cat(paste0("\nCall:\n", cl, "\n", collapse = ""))
  cat(paste0("\nIntercept: ", format(x$intercept, digits = digits),
             "\n", collapse = ""))
  m <- length(x$main.effects)
  if (m > 0L) {
    cat("\nMain Effects:\n")
    if (!omit.values) {
      for (i in 1L:m) {
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
  ur <- x$uninterpreted.rate
  cat(paste0("\nUninterpreted Rate: ", format(ur[1L], digits = digits), "\n"))
  invisible(x)
}

