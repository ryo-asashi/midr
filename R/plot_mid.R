#' Plot MID Values with graphics Package
#'
#' Creates a plot representing mid values of the functional decomposition term.
#'
#' @param x mid object to be visualized.
#' @param term name of term to be plotted.
#' @param add.intercept logical. If TRUE, the intercept is added to the mid values and the scale for the plot is shifted.
#' @param include.main.effects logical. If TRUE, the main effects are added to the interaction mid values.
#' @param scale.type color type of interaction plots. One of "default", "viridis", "gradient" or a function that returns a continuous colour scale for \code{fill} aestetics like \code{ggplot2::scale_fill_viridis_c}.
#' @param scale.palette a character vector of color names, specifying the colors to be used in the interaction plot.
#' @param m an integer specifying the coarseness of the grid for a interaction plot.
#' @param ... optional parameters to be passed to \code{plot()}, \code{barplot()} or \code{filled.contour()}.
#'
#' @examples
#' data(diamonds, package = "ggplot2")
#' model <- lm(price ~ carat + cut + color + clarity + carat:clarity, diamonds)
#' mid <- interpret(price ~ carat + cut + color + clarity + carat:clarity,
#'                  data = diamonds, model = model)
#' plot(mid, "carat")
#' plot(mid, "clarity")
#' plot(mid, "carat:clarity")
#' plot(mid, "carat:clarity", add.intercept = TRUE,
#'      include.main.effects = TRUE)
#' @exportS3Method base::plot
#'
plot.mid <- function(
    x, term, add.intercept = FALSE, include.main.effects = FALSE,
    scale.type = "default", scale.palette = c("#2f7a9a", "#FFFFFF", "#7e1952"),
    m = 100L, ...) {
  dots <- list(...)
  tags <- unlist(strsplit(term, ":"))
  if ((len <- length(tags)) == 1) {
    # main effect
    df <- x$main.effects[[term]]
    df <- stats::na.omit(df)
    if (add.intercept)
      df$mid <- df$mid + x$intercept
    if (is.numeric(df[, 1])) {
      if (x$me.encoders[[term]]$type == "constant") {
        cns <- paste0(term, c("_min", "_max"))
        rdf <- data.frame(x = as.numeric(t(as.matrix(df[, cns]))),
                          y = rep(df$mid, each = 2))
        args <- list(x = rdf$x, y = rdf$y, type = "l",
                     ylab = "mid", xlab = term)
      } else {
        args <- list(x = df[[term]], y = df$mid, type = "l",
                     ylab = "mid", xlab = term)
      }
      for (arg in names(dots))
        args[[arg]] <- dots[[arg]]
      do.call("plot", args)
    } else {
      ht <- df$mid
      names(ht) <- df[[term]]
      args <- list(height = ht, ylab = "mid", xlab = term)
      for (arg in names(dots))
        args[[arg]] <- dots[[arg]]
      do.call("barplot", args)
    }
  } else if (len == 2) {
    # interaction
    if (!(term %in% x$terms))
      term <- paste0(rev(tags), collapse=":")
    ms <- c(m, m)
    xy <- list(NULL, NULL)
    lat <- list(NULL, NULL)
    lab <- list(NULL, NULL)
    encs <- list(x$ie.encoders[[tags[1L]]], x$ie.encoders[[tags[2L]]])
    for (i in 1L:2L) {
      if (encs[[i]]$type == "factor") {
        ms[i] <- encs[[i]]$n * 2L
        xy[[i]] <- rep(encs[[i]]$frame[[1L]], each = 2L)
        lat[[i]] <- seq_len(encs[[i]]$n)
        lab[[i]] <- encs[[i]]$frame[[1L]]
      } else {
        cns <- paste0(tags[i], c("_min", "_max"))
        xy[[i]] <- seq(min(encs[[i]]$frame[[cns[1L]]]),
                       max(encs[[i]]$frame[[cns[2L]]]),
                       length.out = m)
      }
    }
    rdf <- data.frame(rep(xy[[1L]], times = ms[2L]),
                      rep(xy[[2L]], each = ms[1L]))
    colnames(rdf) <- tags
    z <- mid.f(x, term, rdf)
    zmid <- 0
    if (add.intercept)
      z <- z + (zmid <- x$intercept)
    if (include.main.effects)
      z <- z + mid.f(x, tags[1L], rdf) + mid.f(x, tags[2L], rdf)
    zmat <- matrix(z, nrow = ms[1L], ncol = ms[2L])
    zlim <- range(z)
    for (i in 1L:2L) {
      if (encs[[i]]$type == "factor")
        xy[[i]] <- as.numeric(xy[[i]]) + c(-.499, +.499)
    }
    if (is.function(scale.type)) {
      stop("function is not allowed for 'scale.type' of 'plot.mid'.")
    } else if (scale.type == "default") {
      zmax <- max(abs(zlim - zmid))
      zlim <- c(-zmax, zmax) + zmid
    } else if (scale.type == "viridis") {
      scale.palette <-
        c("#440154FF", "#482878FF", "#3E4A89FF", "#31688EFF", "#26828EFF",
          "#1F9E89FF", "#35B779FF", "#6DCD59FF", "#B4DE2CFF", "#FDE725FF")
    } else if (scale.type == "gradient") {
      scale.palette <- c("#132B43", "#56B1F7")
    } else if (scale.type != "uncentralized") {
      stop("invalid 'scale.type' is passed.")
    }
    if (is.function(scale.palette)) {
      pal <- scale.palette
    } else {
      pal <- grDevices::colorRampPalette(scale.palette)
    }
    plot.axes <- substitute(
      if (args$axes) {
        graphics::title(main = "", xlab = "", ylab = "")
        graphics::axis(side = 1L, at = lat[[1L]], labels = lab[[1L]])
        graphics::axis(side = 2L, at = lat[[2L]], labels = lab[[2L]])
      }
    )
    args <- list(x = xy[[1L]], y = xy[[2L]], z = zmat, zlim = zlim,
                 xlab = tags[1L], ylab = tags[2L], color.palette = pal,
                 plot.axes = plot.axes, axes = TRUE)
    for (arg in names(dots))
      args[[arg]] <- dots[[arg]]
    do.call("filled.contour", args)
  }
  invisible(NULL)
}
