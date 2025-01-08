#' Plot MID with Basic Functions
#'
#' For "mid" objects, \code{plot()} visualizes a MID component function.
#'
#' The S3 method of \code{plot()} for "mid" objects creates a visualization of a MID component function using \code{base::plot()} for a main effect of a quantitative variable, \code{graphics::barplot()} for a main effect of a qualitative variable, and \code{graphics::filled.contour()} for an interaction.
#'
#' @param x a "mid" object to be visualized.
#' @param term a character string specifying the component function to be plotted.
#' @param add.intercept logical. If \code{TRUE}, the intercept is added to the MID values and the plotting scale is shifted.
#' @param include.main.effects logical. If \code{TRUE}, the main effects are included in the interaction plot.
#' @param scale.type character string. The color type of the interaction plot. One of "default", "viridis", "gradient".
#' @param scale.palette a character vector of color names specifying the colors to be used in the interaction plot.
#' @param cells.count an integer or integer-valued vector of length two specifying the number of cells for the raster type interaction plot.
#' @param ... optional parameters to be passed to the graphing function.
#' @examples
#' data(airquality, package = "datasets")
#' airquality$Month <- factor(airquality$Month)
#' mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
#' plot(mid, "Temp")
#' plot(mid, "Month")
#' plot(mid, "Wind:Temp")
#' plot(mid, "Solar.R:Month", scale.type = "viridis",
#'      add.intercept = TRUE, include.main.effects = TRUE)
#' @returns
#' \code{plot.mid()} produces a line plot or bar plot for a main effect and a filled contour plot for an interaction and returns \code{NULL}.
#' @exportS3Method base::plot
#'
plot.mid <- function(
    x, term, add.intercept = FALSE, include.main.effects = FALSE,
    scale.type = "default", scale.palette = c("#2f7a9a", "#FFFFFF", "#7e1952"),
    cells.count = c(100L, 100L), ...) {
  dots <- list(...)
  tags <- term.split(term)
  term <- term.check(term, x$terms, stop = TRUE)
  if ((len <- length(tags)) == 1L) {
    # main effect
    df <- x$main.effects[[term]]
    df <- stats::na.omit(df)
    if (add.intercept)
      df$mid <- df$mid + x$intercept
    if (is.numeric(df[, 1L])) {
      if (x$encoders[["main.effects"]][[term]]$type == "constant") {
        cns <- paste0(term, c("_min", "_max"))
        rdf <- data.frame(x = as.numeric(t(as.matrix(df[, cns]))),
                          y = rep(df$mid, each = 2L))
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
  } else if (len == 2L) {
    # interaction
    ms <- cells.count
    if (length(ms) == 1L)
      ms <- c(ms, ms)
    xy <- list(NULL, NULL)
    lat <- list(NULL, NULL)
    lab <- list(NULL, NULL)
    encs <- list(x$encoders[["interactions"]][[tags[1L]]],
                 x$encoders[["interactions"]][[tags[2L]]])
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
                       length.out = ms[i])
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
      stop("function is not allowed for 'scale.type' of this function")
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
      stop("invalid 'scale.type' is passed")
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
