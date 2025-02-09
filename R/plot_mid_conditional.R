#' Plot ICE of MID Model with Basic Functions
#'
#' For "mid.conditional" objects, \code{plot()} visualizes ICE curves of a MID model.
#'
#' The S3 method of \code{plot()} for "mid.conditional" objects creates an visualization of ICE curves of a fitted MID model using \code{base:plot()}.
#'
#' @param x a "mid.conditional" object to be visualized.
#' @param type a character string specifying the type of the plot. One of "iceplot" or "centered". If "centered", the ICE values of each observation are set to zero at the leftmost point of the varriable.
#' @param theme a character string specifying the color theme or any item that can be used to define "color.theme" object.
#' @param term an optional character string specifying the interaction term. If passed, the ICE for the specified term is plotted.
#' @param var.alpha a name of the variable to be used to set \code{alpha}.
#' @param var.color a name of the variable to be used to set \code{colour}.
#' @param var.linetype a name of the variable to be used to set \code{linetype}.
#' @param var.linewidth a name of the variable to be used to set \code{linewidth}.
#' @param dots logical. If \code{TRUE}, the points representing the predictions for each observation are plotted.
#' @param sample an optional vector specifying the names of observations to be plotted.
#' @param ... optional parameters to be passed to \code{base::plot()}.
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
#' ice <- mid.conditional(mid, "Wind", na.omit(airquality))
#' plot(ice, theme = "Mako", var.color = "Temp")
#' plot(ice, type = "centered", theme = "Cividis", var.color = "Temp")
#' @returns
#' \code{plot.mid.conditional()} produces an ICE plot and invisibly returns the ICE matrix used for the plot.
#' @exportS3Method base::plot
#'
plot.mid.conditional <- function(
    x, type = c("iceplot", "centered"), theme = NULL, term = NULL,
    var.alpha = NULL, var.color = NULL, var.linetype = NULL, var.linewidth = NULL,
    dots = TRUE, sample = NULL, ...) {
  cl <- match.call(expand.dots = TRUE)
  dt <- list(...)
  type <- match.arg(type)
  theme <- color.theme(theme)
  use.theme <- inherits(theme, "color.theme")
  variable <- attr(x, "variable")
  n <- attr(x, "n")
  obs <- x$observed
  con <- x$conditional
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
    stp <- con[, yvar][1L:n]
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
  values <- x$values
  mat <- matrix(con[[yvar]], nrow = n, ncol = length(values))
  colnames(mat) <- values
  if (n == 0L) {
    message("no observations found")
    return(invisible(mat))
  }
  rownames(mat) <- obs$ids
  aes <- list(col = rep.int(1L, n), lty = rep.int(1L, n), lwd = rep.int(1L, n))
  if (!is.null(cl$var.color)) {
    cln <- characterize(cl$var.color)
    if (!use.theme)
      theme <- "bluescale"
    aes$col <- to.colors(obs[, cln], theme)
  }
  if (!is.null(cl$var.alpha)) {
    ref <- rescale(obs[, characterize(cl$var.alpha)])
    aes$alpha <- ref * .75 + .25
  }
  if (!is.null(cl$var.linetype)) {
    ref <- rescale(obs[, characterize(cl$var.linetype)])
    aes$lty <- pmin(ref * 6 + 1, 6L)
  }
  if (!is.null(cl$var.linewidth)) {
    ref <- rescale(obs[, characterize(cl$var.linewidth)])
    aes$lwd <- ref * 3
  }
  if (fv <- is.factor(values)) {
    flvs <- levels(values)
    values <- as.numeric(values)
  }
  args <- list(x = values, ylim = range(mat), xlab = variable,
               ylab = yvar, type = "l", xaxt = if (fv) "n")
  args <- override(args, dt)
  for (arg in c("col", "lty", "lwd", "alpha")) {
    if (!is.null(dt[[arg]]))
      aes[[arg]] <- rep_len(dt[[arg]], length.out = n)
  }
  aes$dotcol <- aes$col
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
    graphics::points(x = obs[, variable], y = obs[, yvar],
                     pch = 16L, col = aes$dotcol)
  }
  invisible(mat)
}
