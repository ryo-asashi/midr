#' Deprecated Functions in midr
#'
#' @name midr-deprecated
#' @rdname midr-deprecated
#' @title Deprecated Functions in midr
#'
#' @description
#' These functions are provided for compatibility with older versions of
#' \pkg{midr} (and downstream packages transitioning to \pkg{midr} 0.7.0) and
#' will be removed in a future release.
#'
#' \itemize{
#'   \item \code{mid.importance()}: use \code{\link{eval_importance}()}.
#'   \item \code{mid.breakdown()}: use \code{\link{eval_breakdown}()}.
#'   \item \code{mid.conditional()}: use \code{\link{eval_conditional}()}.
#'   \item \code{theme_midr()}: use \code{\link{theme_monograph}()}.
#'   \item \code{par.midr()}: use \code{\link{par_monograph}()}.
#'   \item \code{mid.plots()}: use \code{\link{ggmid}()} or \code{\link{plot}()}.
#' }
#'
#' @param ... Additional arguments passed to the replacement function.
#'
#' @return The return value of the corresponding replacement function.
#'
#' @keywords internal
NULL

#' @rdname midr-deprecated
#' @export
mid.importance <- function(...) {
  .Deprecated("eval_importance", package = "midr")
  eval_importance(...)
}

#' @rdname midr-deprecated
#' @export
mid.breakdown <- function(...) {
  .Deprecated("eval_breakdown", package = "midr")
  mc <- match.call(expand.dots = TRUE)
  if (!is.null(mc$row) && is.null(mc$rows)) {
    mc$rows <- mc$row
  }
  mc$row <- NULL
  mc[[1L]] <- quote(eval_breakdown)
  eval(mc, envir = parent.frame())
}

#' @rdname midr-deprecated
#' @export
mid.conditional <- function(...) {
  .Deprecated("eval_conditional", package = "midr")
  eval_conditional(...)
}

#' @rdname midr-deprecated
#' @export
theme_midr <- function(...) {
  .Deprecated("theme_monograph", package = "midr")
  theme_monograph(...)
}

#' @rdname midr-deprecated
#' @export
par.midr <- function(...) {
  .Deprecated("par_monograph", package = "midr")
  par_monograph(...)
}

#' @rdname midr-deprecated
#' @export
mid.plots <- function(...) {
  .Deprecated(
    msg = "'mid.plots()' is deprecated. Use 'ggmid()' or 'plot()' instead."
  )
  mc <- match.call(expand.dots = TRUE)
  if (identical(mc$engine, "graphics")) {
    mc[[1L]] <- quote(graphics::plot)
    mc$engine <- NULL
  } else {
    mc[[1L]] <- quote(ggmid)
  }
  eval(mc, envir = parent.frame())
}
