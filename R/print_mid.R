#' Print Method for MID Objects
#'
#' Prints the summary of the fitted mid object.
#'
#' @param x a mid object to be printed.
#' @param digits number of significant digits.
#' @param omit.values logical. If TRUE, mid values of main effect terms are not printed.
#' @param ... not used.
#'
#' @examples
#' data(cars, package = "datasets")
#' print(interpret(dist ~ speed, cars))
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

