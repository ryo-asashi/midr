#' Plot MID Component Functions
#'
#' @description
#' For "mid" objects (i.e., fitted MID models), \code{plot()} visualizes a single component function specified by the \code{term} argument.
#'
#' @details
#' This is an S3 method for the \code{plot()} generic that produces a plot from a "mid" object, visualizing a component function of the fitted MID model.
#'
#' The \code{type} argument controls the visualization style.
#' The default, \code{type = "effect"}, plots the component function itself.
#' In this style, the plotting method is automatically selected based on the effect's type:
#' a line plot for quantitative main effects; a bar plot for qualitative main effects; and a filled contour (level) plot for interactions.
#' The \code{type = "data"} option creates a scatter plot of \code{data}, colored by the values of the component function.
#' The \code{type = "compound"} option combines both approaches, plotting the component function alongside the data points.
#'
#' @param x a "mid" object to be visualized.
#' @param term a character string specifying the component function to be plotted.
#' @param type the plotting style. One of "effect", "data" or "compound".
#' @param theme a character string or object defining the color theme. See \code{\link{color.theme}} for details.
#' @param intercept logical. If \code{TRUE}, the intercept is added to the MID values.
#' @param main.effects logical. If \code{TRUE}, main effects are included in the interaction plot.
#' @param data a data frame to be plotted with the corresponding MID values. If not provided, data is automatically extracted from the function call.
#' @param limits a numeric vector of length two specifying the limits of the plotting scale.
#' @param jitter a numeric value specifying the amount of jitter for the data points.
#' @param resolution an integer or vector of two integers specifying the resolution of the raster plot for interactions.
#' @param ... optional parameters to be passed to the graphing function. Possible arguments are "col", "fill", "pch", "cex", "lty", "lwd" and aliases of them.
#'
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' idx <- sample(nrow(diamonds), 1e4)
#' mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
#'
#' # Plot a quantitative main effect
#' plot(mid, "carat")
#'
#' # Plot a qualitative main effect
#' plot(mid, "clarity")
#'
#' # Plot an interaction effect with data points and a raster layer
#' plot(mid, "carat:clarity", type = "compound", data = diamonds[idx, ])
#'
#' # Use a different color theme
#' plot(mid, "clarity:color", theme = "RdBu")
#' @returns
#' \code{plot.mid()} produces a plot as a side-effect and returns \code{NULL} invisibly.
#'
#' @seealso \code{\link{interpret}}, \code{\link{ggmid}}
#'
#' @exportS3Method base::plot
#'
plot.mid <- function(
    x, term, type = c("effect", "data", "compound"), theme = NULL,
    intercept = FALSE, main.effects = FALSE, data = NULL, limits = NULL,
    jitter = .3, resolution = c(100L, 100L), ...) {
  dots <- list(...)
  tags <- term.split(term)
  term <- term.check(term, x$terms, stop = TRUE)
  type <- match.arg(type)
  if (missing(theme) && length(tags) == 2L)
    theme <- if(type == "data") {
      getOption("midr.sequential", "bluescale")
    } else {
      getOption("midr.diverging", "midr")
    }
  theme <- color.theme(theme)
  use.theme <- inherits(theme, "color.theme")
  if (type == "data" || type == "compound") {
    if (is.null(data))
      data <- model.data(x, env = parent.frame())
    if (is.null(data))
      stop(paste0("'data' must be supplied for the '", type, "' plot"))
    preds <- predict.mid(x, data, terms = unique(c(tags, term)),
                         type = "terms", na.action = "na.pass")
    data <- model.reframe(x, data)
  }
  # main effect
  if ((len <- length(tags)) == 1L) {
    df <- stats::na.omit(x$main.effects[[term]])
    enc <- x$encoders[["main.effects"]][[term]]
    if (intercept)
      df$mid <- df$mid + x$intercept
    middle <- if (intercept) x$intercept else 0
    if (type == "effect" || type == "compound") {
      if (enc$type == "constant") {
        cns <- paste0(term, c("_min", "_max"))
        rdf <- data.frame(x = as.numeric(t(as.matrix(df[, cns]))),
                          y = rep(df$mid, each = 2L))
        cols <- if (use.theme) to.colors(rdf$y, theme, middle = middle) else 1L
        args <- list(x = rdf$x, y = rdf$y, type = "l", col = cols,
                     ylab = "mid", xlab = term, ylim = limits)
        args <- override(args, dots)
        do.call(graphics::plot.default, args)
      } else if (enc$type == "linear") {
        cols <- if (use.theme) to.colors(df$mid, theme, middle = middle) else 1L
        args <- list(x = df[[term]], y = df$mid, type = "l", col = cols,
                     ylab = "mid", xlab = term, ylim = limits)
        args <- override(args, dots)
        do.call(graphics::plot.default, args)
      } else if (enc$type == "factor") {
        cols <- if (use.theme)
          to.colors(df$mid, theme, middle = middle) else "gray35"
        args <- list(to = df$mid, labels = df[[term]], limits = limits,
                     ylab = "mid", xlab = term, fill = cols, col = NA)
        args <- override(args, dots)
        do.call(barplot2, args)
      }
    }
    if (type == "data" || type == "compound") {
      xval <- data[, term]
      mids <- as.numeric(preds[, term])
      if (intercept) mids <- mids + x$intercept
      cols <- if (use.theme) to.colors(mids, theme, middle = middle) else 1L
      if (enc$type == "factor") {
        xval <- apply.catchall(xval, enc)
        jit <- jitter[1L]
        xval <- as.integer(xval) - stats::runif(length(xval), -jit, jit)
        if (type == "data") {
          args <- list(to = df$mid, labels = df[[term]], type = "n",
                       ylab = "mid", xlab = term, limits = limits,
                       x = xval, y = mids, col = cols, pch = 16L, cex = 1L)
          args <- override(args, dots)
          do.call(barplot2, args)
          args <- args[c("x", "y", "col", "pch", "cex")]
          do.call(graphics::points.default, args)
        } else if (type == "compound") {
          graphics::points.default(x = xval, y = mids, pch = 16L)
        }
      } else if (enc$type != "factor") {
        if (type == "data") {
            args <- list(x = xval, y = mids, xlab = term, ylab = "mid",
                         ylim = limits, type = "p", col = cols, pch = 16L)
            args <- override(args, dots)
            do.call(graphics::plot.default, args)
        } else if (type == "compound") {
          graphics::points.default(x = xval, y = mids, pch = 16L, col = cols)
        }
      }
    }
  # interaction
  } else if (len == 2L) {
    ms <- resolution
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
    if (intercept)
      z <- z + (zmid <- x$intercept)
    if (main.effects)
      z <- z + mid.f(x, tags[1L], rdf) + mid.f(x, tags[2L], rdf)
    zmat <- matrix(z, nrow = ms[1L], ncol = ms[2L])
    zlim <- ifnot.null(limits, range(z))
    for (i in 1L:2L) {
      if (encs[[i]]$type == "factor")
        xy[[i]] <- as.numeric(xy[[i]]) + c(-.499, +.499)
    }
    if (!use.theme)
      theme <- color.theme("midr")
    if (theme$type == "diverging") {
      zmax <- max(abs(zlim - zmid))
      zlim <- c(-zmax, zmax) + zmid
    }
    pal <- theme$palette
    if (type == "data" || type == "compound") {
      xval <- data[[tags[1L]]]
      if (encs[[1L]]$type == "factor") {
        xval <- apply.catchall(xval, encs[[1L]])
        jit <- jitter[1L]
        xval <- as.integer(xval) - stats::runif(length(xval), -jit, jit)
      }
      yval <- data[[tags[2L]]]
      if (encs[[2L]]$type == "factor") {
        yval <- apply.catchall(yval, encs[[2L]])
        jit <- if (length(jitter) > 1L) jitter[2L] else jitter[1L]
        yval <- as.integer(yval) - stats::runif(length(yval), -jit, jit)
      }
    }
    if (type == "effect" || type == "compound") {
      plot.axes <- substitute(
        if (args$axes) {
          graphics::title(main = "", xlab = "", ylab = "")
          graphics::axis(side = 1L, at = lat[[1L]], labels = lab[[1L]])
          graphics::axis(side = 2L, at = lat[[2L]], labels = lab[[2L]])
          if (type == "compound") {
            points(x = xval, y = yval, pch = 16L)
          }
        }
      )
      args <- list(x = xy[[1L]], y = xy[[2L]], z = zmat, zlim = zlim,
                   xlab = tags[1L], ylab = tags[2L], color.palette = pal,
                   plot.axes = plot.axes, las = graphics::par("las"),
                   axes = TRUE, fill = NULL, col = NULL)
      args <- override(args, dots)
      args$col <- args$fill
      args$fill <- NULL
      do.call(graphics::filled.contour, args)
    } else if (type == "data") {
      mid <- rowSums(preds[, c(term, if (main.effects) tags), drop = FALSE])
      if (intercept)
        mid <- mid + x$intercept
      middle <- if (intercept) x$intercept else 0
      cols <- to.colors(mid, theme, middle = middle)
      args <- list(x = xval, y = yval, type = "n", col = cols, pch = 16L,
                   cex = 1L, xlab = tags[1L], ylab = tags[2L], axes = FALSE)
      args <- override(args, dots)
      do.call(graphics::plot.default, args)
      graphics::box()
      for (i in 1L:2L) {
        if (encs[[i]]$type == "factor") {
          lvs <- levels(encs[[i]]$frame[[1L]])
          graphics::axis(side = i, at = seq_len(encs[[i]]$n), labels = lvs)
        } else {
          graphics::axis(side = i)
        }
      }
      args <- args[c("x", "y", "col", "pch", "cex")]
      do.call(graphics::points.default, args)
    }
  }
  invisible(NULL)
}
