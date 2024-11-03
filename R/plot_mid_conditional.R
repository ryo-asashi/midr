#' Plot MID Individual Conditional Expectations with graphics Package
#'
#' Creates a plot showing the MID-based individual conditional expectations
#'
#' @param x a mid.conditional object to visualize.
#' @param centered logical.
#' @param draw.dots logical. If TRUE, points representing the predictions at the observed values are
#' @param sample a vector specifying the set of names of the observations to be plotted.
#' @param term an optional character specifying one of the relevant terms. If passed, the individual conditional expectations for the specified term are plotted.
#' @param variable.alpha a name of the predictor variable to use to set \code{alpha} for each plot.
#' @param variable.colour a name of the predictor variable to use to set \code{color} for each plot.
#' @param variable.linetype a name of the predictor variable to use to set \code{linetype} for each plot.
#' @param variable.linewidth a name of the predictor variable to use to set \code{linewidth} for each plot.
#' @param scale.palette a character vector of color names, specifying the colors to be used.
#' @param ... optional parameters to be directly passed to \code{ggplot2::geom_line()}.
#' @exportS3Method base::plot
#'
plot.mid.conditional <- function(
    x, centered = FALSE, draw.dots = TRUE, sample = NULL, term = NULL,
    variable.alpha = NULL, variable.colour = NULL,
    variable.linetype = NULL, variable.linewidth = NULL,
    scale.palette = NULL, ...) {
  cl <- match.call(expand.dots = TRUE)
  dots <- list(...)
  variable <- attr(x, "variable")
  n <- attr(x, "n")
  obs <- x$observed
  con <- x$conditional
  yvar <- "yhat"
  if (!is.null(term)) {
    if (!inherits(x, "mid.conditional.effects"))
      stop("the term effects are not stored in the object")
    term <- term.check(term, x$terms, stop = TRUE)
    yvar <- paste0("mid(", term, ")")
    obs[, yvar] <- x$observed.effects[, term]
    con[, yvar] <- x$conditional.effects[, term]
  }
  if (centered) {
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
  if (!is.null(cl$variable.colour)) {
    cln <- characterize(cl$variable.colour)
    if (is.null(scale.palette)) {
      scale.palette <- if (is.discrete(obs[, cln])) {
        grDevices::hcl.colors(24L, "Zissou 1", rev = TRUE)
      } else {
        c("#132B43", "#56B1F7")
      }
    }
    ramp <- grDevices::colorRamp(scale.palette, alpha = FALSE)
    rgbs <- ramp(rescale(obs[, cln]))
    aes$col <- grDevices::rgb(rgbs[, 1L], rgbs[, 2L], rgbs[, 3L],
                              maxColorValue = 255L)
  }
  if (!is.null(cl$variable.alpha)) {
    ref <- rescale(obs[, characterize(cl$variable.alpha)])
    aes$alpha <- ref * .75 + .25
  }
  if (!is.null(cl$variable.linetype)) {
    ref <- rescale(obs[, characterize(cl$variable.linetype)])
    aes$lty <- pmin(ref * 6 + 1, 6L)
  }
  if (!is.null(cl$variable.linewidth)) {
    ref <- rescale(obs[, characterize(cl$variable.linewidth)])
    aes$lwd <- ref * 3
  }
  if (fv <- is.factor(values)) {
    flvs <- levels(values)
    values <- as.numeric(values)
  }
  args <- list(x = values, ylim = range(mat), xlab = variable,
               ylab = yvar, type = "l", xaxt = if (fv) "n")
  for (arg in names(dots)) {
    if (arg %in% c("col", "lty", "lwd", "alpha")) {
      aes[[arg]] <- rep_len(dots[[arg]], length.out = n)
    } else {
      args[[arg]] <- dots[[arg]]
    }
  }
  aes$dotcol <- aes$col
  if (!is.null(aes$alpha)) {
    clr <- grDevices::col2rgb(col = aes$col)
    aes$col <- grDevices::rgb(clr[1L,], clr[2L,], clr[3L,],
                              round(aes$alpha * 255), maxColorValue = 255L)
  }
  args$y = mat[1L,]
  for (p in c("col", "lty", "lwd")) args[[p]] <- aes[[p]][1L]
  do.call("plot", args)
  if (fv)
    graphics::axis(side = 1L, at = seq_len(length(values)), labels = flvs)
  for (i in seq_len(n - 1L)) {
    args$y <- mat[i + 1L,]
    for (p in c("col", "lty", "lwd")) args[[p]] <- aes[[p]][i + 1L]
    do.call("points", args)
  }
  if (draw.dots) {
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
