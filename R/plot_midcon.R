#' Plot MID Conditional Expectation
#'
#' @description
#' For "midcon" objects, \code{plot()} visualizes Individual Conditional Expectation (ICE) curves derived from a fitted MID model.
#'
#' @details
#' This is an S3 method for the \code{plot()} generic that produces ICE curves from a "midcon" object.
#' ICE plots are a model-agnostic tool for visualizing how a model's prediction for a single observation changes as one feature varies.
#' This function plots one line for each observation in the data.
#'
#' The \code{type} argument controls the visualization style:
#' The default, \code{type = "iceplot"}, plots the raw ICE curves.
#' The \code{type = "centered"} option creates the centered ICE (c-ICE) plot, where each curve is shifted to start at zero, making it easier to compare the slopes of the curves.
#'
#' The \code{var.color}, \code{var.alpha}, etc., arguments allow you to map aesthetics to other variables in your data using (possibly) unquoted expressions.
#'
#' @param x a "midcon" object to be visualized.
#' @param type the plotting style. One of "iceplot" or "centered".
#' @param theme a character string or object defining the color theme. See \code{\link{color.theme}} for details.
#' @param term an optional character string specifying an interaction term. If passed, the ICE curve for the specified term is plotted.
#' @param var.alpha a variable name or expression to map to the alpha aesthetic.
#' @param var.color a variable name or expression to map to the color aesthetic.
#' @param var.linetype a variable name or expression to map to the linetype aesthetic.
#' @param var.linewidth a variable name or expression to map to the linewidth aesthetic.
#' @param reference an integer specifying the index of the evaluation point to use as the reference for centering the c-ICE plot.
#' @param points logical. If \code{TRUE}, points representing the actual predictions for each observation are plotted.
#' @param sample an optional vector specifying the names of observations to be plotted.
#' @param ... optional parameters passed on to the graphing functions.
#'
#' @examples
#' data(airquality, package = "datasets")
#' library(midr)
#' mid <- interpret(Ozone ~ .^2, data = airquality, lambda = 0.1)
#' ice <- mid.conditional(mid, "Temp", data = airquality)
#'
#' # Create an ICE plot, coloring lines by 'Wind'
#' plot(ice, var.color = "Wind")
#'
#' # Create a centered ICE plot, mapping color and linetype to other variables
#' plot(ice, type = "centered", theme = "Purple-Yellow",
#'      var.color = factor(Month), var.linetype = Wind > 10)
#' @returns
#' \code{plot.midcon()} produces an ICE plot as a side-effect and returns \code{NULL} invisibly.
#'
#' @seealso \code{\link{mid.conditional}}, \code{\link{ggmid.midcon}}
#'
#' @exportS3Method base::plot
#'
plot.midcon <- function(
    x, type = c("iceplot", "centered"), theme = NULL, term = NULL,
    var.alpha = NULL, var.color = NULL, var.linetype = NULL, var.linewidth = NULL,
    reference = 1L, points = TRUE, sample = NULL, ...) {
  dots <- override(list(), list(...))
  type <- match.arg(type)
  theme <- color.theme(theme)
  use.theme <- inherits(theme, "color.theme")
  variable <- x$variable
  obs <- x$observed
  con <- x$conditional
  values <- x$values
  yvar <- "yhat"
  if (!is.null(term)) {
    if (is.null(x$conditional.effects))
      stop("the term effects are not stored in the object")
    term <- term.check(term, mid.terms(x), stop = TRUE)
    yvar <- paste0("mid(", term, ")")
    obs[, yvar] <- x$observed.effects[, term]
    con[, yvar] <- x$conditional.effects[, term]
  }
  if (type == "centered") {
    if (reference < 0) reference <- length(values)
    refval <- values[min(length(values), max(1L, reference))]
    ref <- con[con[[variable]] == refval, , drop = FALSE]
    ynew <- paste0("centered ", yvar)
    obs[, ynew] <- obs[, yvar] - ref[[yvar]][match(obs$.id, ref$.id)]
    con[, ynew] <- con[, yvar] - ref[[yvar]][match(con$.id, ref$.id)]
    yvar <- ynew
  }
  if (!is.null(sample)) {
    obs <- obs[obs$.id %in% sample, ]
    con <- con[con$.id %in% sample, ]
  }
  n <- nrow(obs)
  if (n == 0L) {
    message("no observations found")
    return(invisible(NULL))
  }
  mat <- matrix(NA_real_, ncol = n, nrow = length(values),
                dimnames = list(as.character(values), obs$.id))
  cols <- match(con$.id, obs$.id)
  rows <- match(con[[variable]], values)
  ok <- !is.na(rows) & !is.na(cols)
  mat[cbind(rows[ok], cols[ok])] <- con[[yvar]][ok]
  aes <- list(col = 1, lty = 1, lwd = 1, alpha = 1)
  if (!is.null(colexpr <- substitute(var.color))) {
    if (is.character(colexpr)) colexpr <- str2lang(colexpr)
    ref <- eval(colexpr, envir = obs)
    if (!use.theme) {
      theme <- if (is.discrete(ref))
        getOption("midr.qualitative", "HCL") else
        getOption("midr.sequential", "bluescale")
    }
    aes$col <- to.colors(ref, theme)
  }
  if (!is.null(alphaexpr <- substitute(var.alpha))) {
    if (is.character(alphaexpr)) alphaexpr <- str2lang(alphaexpr)
    ref <- rescale(eval(alphaexpr, envir = obs))
    aes$alpha <- ref * .75 + .25
  }
  if (!is.null(ltyexpr <- substitute(var.linetype))) {
    if (is.character(ltyexpr)) ltyexpr <- str2lang(ltyexpr)
    ref <- rescale(eval(ltyexpr, envir = obs))
    aes$lty <- pmin(ref * 6 + 1, 6L)
  }
  if (!is.null(lwdexpr <- substitute(var.linewidth))) {
    if (is.character(lwdexpr)) lwdexpr <- str2lang(lwdexpr)
    ref <- rescale(eval(lwdexpr, envir = obs))
    aes$lwd <- ref * 3
  }
  aes$col <- dots$col %||% aes$col
  aes$alpha <- dots$alpha %||% aes$alpha
  if (any(aes$alpha < 1)) {
    clr <- grDevices::col2rgb(col = aes$col)
    aes$col <- grDevices::rgb(
      clr[1L,], clr[2L,], clr[3L,],
      round(aes$alpha * 255), maxColorValue = 255L
    )
  }
  fv <- is.discrete(values)
  if (fv) flvs <- levels(factor(values))
  xvals <- if (fv) seq_along(flvs) else values
  args <- list(
    x = xvals, xlim = range(xvals, na.rm = TRUE), ylim = range(mat, na.rm = TRUE),
    xlab = variable, ylab = yvar, type = "n", xaxt = if (fv) "n"
    )
  do.call(graphics::plot.default, override(args, dots))
  if (fv) graphics::axis(side = 1L, at = seq_along(values), labels = flvs)
  args <- list(x = xvals, y = mat, type = "l", add = TRUE)
  args <- override(override(args, dots), aes)
  do.call(graphics::matplot, args)
  if (points) {
    if (fv) {
      obs[, variable] <- factor(obs[ , variable], levels = flvs)
      if (!is.null(attr(values, "others")))
        obs[is.na(obs[, variable]), variable] <- attr(values, "others")
    }
    args <- list(x = obs[, variable], y = obs[, yvar], pch = 16L, col = aes$col)
    do.call(graphics::points.default, override(args, dots))
  }
  invisible(NULL)
}
