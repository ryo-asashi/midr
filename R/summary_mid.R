#' Summarize MID Models
#'
#' @description
#' For "mid" objects, an S3 method of \code{summary()} prints a comprehensive summary of a fitted MID model.
#'
#' @details
#' The S3 method \code{summary.mid()} generates a comprehensive overview of the fitted MID model.
#' The output includes:
#' \itemize{
#'   \item \strong{Call}: the function call used to fit the MID model.
#'   \item \strong{Link}: name of the link function used to fit the MID model, if applicable.
#'   \item \strong{Uninterpreted Variation Ratio}: proportion of target model variance not explained by MID model.
#'   \item \strong{Residuals}: five-number summary of (working) residuals.
#'   \item \strong{Encoding}: summary of encoding schemes per variable.
#'   \item \strong{Diagnosis}: residuals vs fitted values plot (displayed only when \code{diagnosis = TRUE}).
#' }
#'
#' @param object a "mid" object to be summarized.
#' @param diagnosis logical. If \code{TRUE}, the diagnosis plot is displayed. Defaults to \code{FALSE}.
#' @param digits the number of significant digits for printing numeric values.
#' @param ... arguments to be passed to \code{graphics::panel.smooth()} for the diagnosis plot.
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
    object, diagnosis = FALSE, digits = max(3L, getOption("digits") - 2L), ...
  ) {
  use.link <- !is.null(object$link)
  rsd <- object$residuals
  # call
  cl <- paste0(trimws(deparse(object$call)), sep = "", collapse = "\n ")
  cat(paste0("\nCall:\n", cl, "\n", collapse = ""))
  # link
  if(use.link)
    cat(paste0("\nLink: ", object$link$name, "\n", collapse = ""))
  # ratio
  cat(paste0("\nUninterpreted Variation Ratio:\n"))
  print.default(object$ratio, digits = digits)
  # residuals
  cat(paste0("\n", if (use.link) "Working ", "Residuals:\n"))
  if (length(rsd) > 5L) {
    nam <- c("Min", "1Q", "Median", "3Q", "Max")
    rq <- structure(zapsmall(stats::quantile(rsd), digits + 1L), names = nam)
    print(rq, digits = digits)
  } else {
    print(rsd, digits = digits)
  }
  # encoding
  cat("\nEncoding:\n")
  print.data.frame(mid.encoding.scheme(object))
  # diagnosis
  if (diagnosis) {
    yhat <- if (use.link) object$linear.predictors else object$fitted.values
    graphics::plot.default(
      yhat, rsd,
      xlab = ifelse(use.link, "Linear Predictors", "Fitted Values"),
      ylab = paste0(if (use.link) "Working ", "Residuals"),
      main = "Residuals vs Fitted Values", font.main = 1L, type = "n"
    )
    args <- list(...)
    args$x <- yhat
    args$y <- rsd
    args$iter <- args$iter %||% 3L
    do.call(graphics::panel.smooth, args)
    graphics::abline(h = 0L, lty = 3L, col = "gray65")
  }
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
