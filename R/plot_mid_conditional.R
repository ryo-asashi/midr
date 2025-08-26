#' Plot MID Conditional Expectations
#'
#' @description
#' For "mid.conditional" objects, \code{plot()} visualizes Individual Conditional Expectation (ICE) curves derived from a fitted MID model.
#'
#' @details
#' This is an S3 method for the \code{plot()} generic that produces ICE curves from a "mid.conditional" object.
#' ICE plots are a model-agnostic tool for visualizing how a model's prediction for a single observation changes as one feature varies.
#' This function plots one line for each observation in the data.
#'
#' The \code{type} argument controls the visualization style:
#' The default, \code{type = "iceplot"}, plots the row ICE curves.
#' The \code{type = "centered"} option creates the centered ICE (c-ICE) plot, where each curve is shifted so start at zero, which makes it easier to compare the slopes of the curves.
#'
#' The \code{var.color}, \code{var.alpha}, etc., arguments allow you to map aesthetics to other variables in your data using (possibly) unquoted expressions.
#'
#' @param x a "mid.conditional" object to be visualized.
#' @param type the type of the plot. One of "iceplot" or "centered". If "centered", the ICE values of each observation are set to zero at the leftmost point of the varriable.
#' @param theme a character string or object defining the color theme.
#' @param term an optional character string specifying a single term. If provided, the plot will show the conditional effect of that term instead of the total prediction.
#' @param var.alpha a variable name or expression to map to the alpha aesthetic.
#' @param var.color a variable name or expression to map to the color aesthetic.
#' @param var.linetype a variable name or expression to map to the linetype aesthetic.
#' @param var.linewidth a variable name or expression to map to the linewidth aesthetic.
#' @param reference an integer specifying the index of the sample points to use as the reference for centering the c-ICE plot.
#' @param dots logical. If \code{TRUE}, points representing the actual predictions for each observation are plotted.
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
#' \code{plot.mid.conditional()} produces an ICE plot as a side-effect and invisibly returns the ICE matrix used for the plot.
#'
#' @seealso \code{\link{plot.mid}}, \code{\link{ggmid.mid.conditional}}
#'
#' @exportS3Method base::plot
#'
plot.mid.conditional <- function(
    x, type = c("iceplot", "centered"), theme = NULL, term = NULL,
    var.alpha = NULL, var.color = NULL, var.linetype = NULL, var.linewidth = NULL,
    reference = 1L, dots = TRUE, sample = NULL, ...) {
  dt <- list(...)
  type <- match.arg(type)
  theme <- color.theme(theme)
  use.theme <- inherits(theme, "color.theme")
  variable <- attr(x, "variable")
  n <- attr(x, "n")
  obs <- x$observed
  con <- x$conditional
  values <- x$values
  yvar <- "yhat"
  if (!is.null(term)) {
    if (is.null(x$conditional.effects))
      stop("the term effects are not stored in the object")
    term <- term.check(term, x$terms, stop = TRUE)
    yvar <- paste0("mid(", term, ")")
    obs[, yvar] <- x$observed.effects[, term]
    con[, yvar] <- x$conditional.effects[, term]
  }
  if (type == "centered") {
    if (reference < 0) reference <- length(values)
    ref <- values[min(length(values), max(1L, reference))]
    stp <- con[con[[variable]] == ref, yvar]
    ynew <- paste0("centered ", yvar)
    obs[, ynew] <- obs[, yvar] - stp
    con[, ynew] <- con[, yvar] - stp
    yvar <- ynew
  }
  if (!is.null(sample)) {
    obs <- obs[obs$id %in% sample, ]
    con <- con[con$id %in% sample, ]
    n <- nrow(obs)
  }
  mat <- matrix(con[[yvar]], nrow = n, ncol = length(values))
  colnames(mat) <- values
  if (n == 0L) {
    message("no observations found")
    return(invisible(mat))
  }
  rownames(mat) <- obs$ids
  aes <- list(col = rep.int(1L, n), lty = rep.int(1L, n), lwd = rep.int(1L, n))
  if (!is.null(col <- substitute(var.color))) {
    if (is.character(col)) col <- str2lang(col)
    ref <- eval(col, envir = obs)
    if (!use.theme) {
      theme <- if (is.discrete(ref))
        getOption("midr.qualitative", "HCL") else
        getOption("midr.sequential", "bluescale")
    }
    aes$col <- to.colors(ref, theme)
  }
  if (!is.null(alp <- substitute(var.alpha))) {
    if (is.character(alp)) alp <- str2lang(alp)
    ref <- rescale(eval(alp, envir = obs))
    aes$alpha <- ref * .75 + .25
  }
  if (!is.null(lty <- substitute(var.linetype))) {
    if (is.character(lty)) lty <- str2lang(lty)
    ref <- rescale(eval(lty, envir = obs))
    aes$lty <- pmin(ref * 6 + 1, 6L)
  }
  if (!is.null(lwd <- substitute(var.linewidth))) {
    if (is.character(lwd)) lwd <- str2lang(lwd)
    ref <- rescale(eval(lwd, envir = obs))
    aes$lwd <- ref * 3
  }
  if (fv <- is.factor(values)) {
    flvs <- levels(values)
    values <- as.numeric(values)
  }
  args <- list(x = values, ylim = range(mat), xlab = variable,
               ylab = yvar, type = "l", xaxt = if (fv) "n")
  dcol <- aes$col
  args <- override(args, dt)
  for (arg in c("col", "lty", "lwd", "alpha")) {
    if (!is.null(dt[[arg]]))
      aes[[arg]] <- rep_len(dt[[arg]], length.out = n)
  }
  if (!is.null(aes$alpha)) {
    clr <- grDevices::col2rgb(col = aes$col)
    aes$col <- grDevices::rgb(clr[1L,], clr[2L,], clr[3L,],
                              round(aes$alpha * 255), maxColorValue = 255L)
  }
  args$y = mat[1L,]
  for (p in c("col", "lty", "lwd"))
    args[[p]] <- aes[[p]][1L]
  do.call(graphics::plot.default, args)
  if (fv)
    graphics::axis(side = 1L, at = seq_len(length(values)), labels = flvs)
  for (i in seq_len(n - 1L)) {
    args$y <- mat[i + 1L,]
    for (p in c("col", "lty", "lwd"))
      args[[p]] <- aes[[p]][i + 1L]
    do.call(graphics::points.default, args)
  }
  if (dots) {
    if (fv) {
      obs[, variable] <- factor(obs[ , variable], levels = flvs)
      if (!is.null(attr(values, "catchall")))
        obs[is.na(obs[, variable]), variable] <- attr(values, "catchall")
    }
    graphics::points.default(x = obs[, variable], y = obs[, yvar],
                             pch = 16L, col = dcol)
  }
  invisible(mat)
}
