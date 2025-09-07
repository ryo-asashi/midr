#' Summarize MID Models
#'
#' @description
#' For "mid" objects, an S3 method of \code{summary()} prints a comprehensive summary of a fitted MID model.
#'
#' @details
#' The S3 method \code{summary.mid()} generates a comprehensive overview of the fitted MID model.
#' The output includes the following components:
#' (1) "Call" - the function call used to fit the MID model.
#' (2) "Uninterpreted Variation Ratio" - a key metric indicating the proportion of the target model's variance that is not explained by the MID model.
#' Lower values suggest a better fit.
#' (3) "Residuals" - a five-number summary (Min, 1Q, Median, 3Q, Max) of the working residuals.
#' This aids in assessing model fit and identifying potential biases.
#' (4) "Encoding" - a summary of the encoding schemes used for each variable in the MID model.
#' (5) "Importance" - a list of the top terms ranked by their MID importance, which quantifies their average contribution to the model's predictions.
#'
#' @param object a "mid" object to be summarized.
#' @param digits the number of significant digits for printing numeric values.
#' @param top.n the maximum number of top-ranked terms to be printed in the MID importance table.
#' @param ... arguments to be passed to other methods (not used in this method).
#'
#' @examples
#' # Summarize a fitted MID model
#' data(cars, package = "datasets")
#' mid <- interpret(dist ~ speed, cars)
#' summary(mid)
#' @returns
#' \code{summary.mid()} returns the original "mid" object invisibly.
#'
#' @seealso \code{\link{interpret}}, \code{\link{print.mid}}
#'
#' @exportS3Method base::summary
#'
summary.mid <- function(
    object, digits = max(3L, getOption("digits") - 2L), top.n = 10L, ...) {
  cl <- paste0(trimws(deparse(object$call)), sep = "", collapse = "\n ")
  cat(paste0("\nCall:\n", cl, "\n", collapse = ""))
  if(use.link <- !is.null(object$link))
    cat(paste0("\nLink: ", object$link$name, "\n", collapse = ""))
  cat(paste0("\nUninterpreted Variation Ratio:\n"))
  print.default(object$ratio, digits = digits)
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

mid.encoding.scheme <- function(object, ...) {
  fun <- function(enc) paste0(enc$type, "(", enc$n, ")")
  mencs <- sapply(object$encoders[["main.effects"]], fun)
  iencs <- sapply(object$encoders[["interactions"]], fun)
  tags <- unique(c(mtags <- names(mencs), itags <- names(iencs)))
  df <- data.frame(row.names = tags)
  if (length(mtags) > 0L) {
    df[["main.effect"]] <- character(1L)
    for (tag in mtags)
      df[tag, "main.effect"] <- mencs[tag]
  }
  if (length(itags) > 0L) {
    df[["interaction"]] <- character(1L)
    for (tag in itags)
      df[tag, "interaction"] <- iencs[tag]
  }
  df
}
