#' Summarize MID Models
#'
#' For "mid" objects, \code{summary()} prints information about the fitted MID model.
#'
#' The S3 method of \code{summary()} for "mid" objects prints basic information about the MID model including the uninterpreted rate, residuals, encoding schemes, and MID importance.
#'
#' @param object a "mid" object to be summarized.
#' @param digits an integer specifying the number of significant digits.
#' @param top.n an integer specifying the maximum number of terms to be printed with the MID importance values.
#' @param ... not used.
#' @examples
#' data(cars, package = "datasets")
#' summary(interpret(dist ~ speed, cars))
#' @returns
#' \code{summary.mid()} returns the "mid" object passed to the function without any modification.
#' @exportS3Method base::summary
#'
summary.mid <- function(
    object, digits = max(3L, getOption("digits") - 2L), top.n = 10L, ...) {
  cl <- paste0(trimws(deparse(object$call)), sep = "", collapse = "\n ")
  cat(paste0("\nCall:\n", cl, "\n", collapse = ""))
  if(use.link <- !is.null(object$link))
    cat(paste0("\nLink: ", object$link$name, "\n", collapse = ""))
  cat(paste0("\nUninterpreted Rate:\n"))
  print.default(object$uninterpreted.rate, digits = digits)
  rsd <- object$residuals
  yhat <- if (use.link) {
    object$linear.predictors
  } else {
    object$fitted.values
  }
  cat(paste0("\n", if (use.link) "Working ", "Residuals:\n"))
  if (length(rsd) > 5L) {
    nam <- c("Min", "1Q", "Median", "3Q", "Max")
    rq <- structure(zapsmall(stats::quantile(rsd), digits + 1L), names = nam)
    print(rq, digits = digits, ...)
  } else {
    print(rsd, digits = digits, ...)
  }
  graphics::plot.default(
    yhat, rsd,
    xlab = ifelse(use.link, "Linear Predictors", "Fitted Values"),
    ylab = paste0(if (use.link) "Working ", "Residuals"),
    main = "Residuals vs Fitted Values", font.main = 1L, type = "n"
  )
  graphics::panel.smooth(yhat, rsd, iter = 3L)
  graphics::abline(h = 0L, lty = 3L, col = "gray65")
  cat("\nEncoding:\n")
  print.data.frame(mid.encoding.scheme(object))
  cat("\nImportance:\n")
  imp <- utils::head(mid.importance(object)$importance, top.n)
  imp$importance <- format(imp$importance, digits = digits)
  if ((tot.n <- length(object$terms)) - top.n > 0L)
    cat(paste0("(Top ", top.n, " out of ", tot.n, ")\n"))
  print.data.frame(imp)
  invisible(object)
}

