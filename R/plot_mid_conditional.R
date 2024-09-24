#' Plot MID Individual Conditional Expectations with graphics Package
#'
#' Creates a plot representing the MID-based individual conditional expectations
#'
#' @param x a mid.conditional object to visualize.
#' @param centered logical.
#' @param show.dots logical. If TRUE, points representing the predictions at the observed values are
#' @param sample a vector specifying the set of names of the observations to be plotted.
#' @param term an optional character specifying one of the relevant terms. If passed, the individual conditional expectations for the specified term are plotted.
#' @param variable.alpha a name of the predictor variable to use to set \code{alpha} for each plot.
#' @param variable.colour a name of the predictor variable to use to set \code{color} for each plot.
#' @param variable.linetype a name of the predictor variable to use to set \code{linetype} for each plot.
#' @param variable.linewidth a name of the predictor variable to use to set \code{linewidth} for each plot.
#' @param scale.palette a character vector of color names, specifying the colors to be used in the interaction plot.
#' @param ... optional parameters to be directly passed to \code{ggplot2::geom_line()}.
#' @exportS3Method base::plot
#'
plot.mid.conditional <- function(
    x, centered = FALSE, show.dots = TRUE, sample = NULL, term = NULL,
    variable.alpha = NULL, variable.colour = NULL,
    variable.linetype = NULL, variable.linewidth = NULL,
    scale.palette = grDevices::hcl.colors(24L, "Zissou 1"), ...) {
  mc <- match.call(expand.dots = TRUE)
  dots <- list(...)
  v <- attr(x, "variable")
  n <- attr(x, "n")
  obs <- x$observed
  con <- x$conditional
  yvar <- "yhat"
  if (!is.null(term)) {
    if (!inherits(x, "mid.conditional.effects"))
      stop("the term effects are not stored in the object")
    if (!term %in% x$terms) {
      term <- paste0(rev(unlist(strsplit(term, ":"))), collapse = ":")
      if (!term %in% x$terms)
        stop(paste0("'", term, "' is not a relevant term"))
    }
    yvar <- paste0("mid(", term, ")")
    obs[, yvar] <- x$observed.effects[, term]
    con[, yvar] <- x$conditional.effects[, term]
  }
  if (centered) {
    stp <- con[, yvar][1:n]
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
  mat <- matrix(con[[yvar]], nrow = n)
  rownames(mat) <- obs$ids
  colnames(mat) <- values <- x$values
  if (isf <- !is.numeric(values)) {
    values <- as.factor(values)
    lvs <- levels(values)
    values <- as.numeric(values)
  }
  aes <- list(col = rep.int(1L, n), lty = rep.int(1L, n), lwd = rep.int(1L, n))
  e2c <- function(expr) {
    ifelse(is.character(expr), expr, deparse(expr))
  }
  if (!is.null(mc$variable.colour)) {
    ref <- obs[, e2c(mc$variable.colour)]
    if (!is.numeric(ref))
      ref <- as.numeric(as.factor(ref))
    rng <- range(ref, na.rm = TRUE)
    ref <- (ref - rng[1L]) / (rng[2L] - rng[1L])
    ramp <- grDevices::colorRamp(scale.palette, alpha = FALSE)
    ref <- ramp(ref)
    aes[["col"]] <-
      grDevices::rgb(ref[, 1L], ref[, 2L], ref[, 3L], maxColorValue = 255L)
  }
  if (!is.null(mc$variable.alpha)) {
    ref <- obs[, e2c(mc$variable.alpha)]
    if (!is.numeric(ref))
      ref <- as.numeric(as.factor(ref))
    rng <- range(ref, na.rm = TRUE)
    ref <- (ref - rng[1L]) / (rng[2L] - rng[1L])
    aes[["alpha"]] <- ref * 3 / 4 + .25
  }
  if (!is.null(mc$variable.linetype)) {
    ref <- obs[, e2c(mc$variable.linetype)]
    if (!is.numeric(ref))
      ref <- as.numeric(as.factor(ref))
    rng <- range(ref, na.rm = TRUE)
    ref <- (ref - rng[1L]) / (rng[2L] - rng[1L])
    aes[["lty"]] <- pmin(ref * 6 + 1, 6L)
  }
  if (!is.null(mc$variable.linewidth)) {
    ref <- obs[, e2c(mc$variable.linewidth)]
    if (!is.numeric(ref))
      ref <- as.numeric(as.factor(ref))
    rng <- range(ref, na.rm = TRUE)
    ref <- (ref - rng[1L]) / (rng[2L] - rng[1L])
    aes[["lwd"]] <- ref * 3
  }
  args <- list(x = values, ylim = range(mat), xlab = v, ylab = yvar, type = "l")
  for (arg in names(dots)) {
    if (arg %in% c("col", "lty", "lwd", "alpha")) {
      aes[[arg]] <- rep_len(dots[[arg]], length.out = n)
    } else {
      args[[arg]] <- dots[[arg]]
    }
  }
  if (!is.null(aes[["alpha"]])) {
    clr <- grDevices::col2rgb(col = aes[["col"]])
    aes[["col"]] <-
      grDevices::rgb(clr[1L, ], clr[2L, ], clr[3L, ],
                     round(aes[["alpha"]] * 255), maxColorValue = 255L)
  }
  if (isf)
    args$xaxt <- "n"
  args$y = mat[1L,]
  for (p in c("col", "lty", "lwd")) args[[p]] <- aes[[p]][1L]
  do.call("plot", args)
  if (isf)
    graphics::axis(side = 1L, at = seq_len(length(lvs)), labels = lvs)
  for (i in seq_len(n - 1L)) {
    args$y <- mat[i + 1L,]
    for (p in c("col", "lty", "lwd")) args[[p]] <- aes[[p]][i + 1L]
    do.call("points", args)
  }
  if (show.dots) {
    graphics::points(x = obs[[v]], y = obs[[yvar]], pch = 16L, col = aes$col)
  }
  invisible(mat)
}
