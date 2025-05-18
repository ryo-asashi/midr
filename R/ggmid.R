#' Plot MID with ggplot2 Package
#'
#' For "mid" objects, \code{ggmid()} visualizes a MID component function using the ggplot2 package.
#'
#' The S3 method of \code{ggmid()} for "mid" objects creates a "ggplot" object that visualizes a MID component function.
#' The main layer is drawn using \code{geom_line()} or \code{geom_path()} for a main effect of a quantitative variable, \code{geom_col()} for a main effect of a qualitative variable, and \code{geom_raster()} or \code{geom_rect()} for an interaction effect.
#' For other methods of \code{ggmid()}, see \code{help(ggmid.mid.importance)}, \code{help(ggmid.mid.breakdown)} or \code{help(ggmid.mid.conditional)}.
#'
#' @param object a "mid" object to be visualized.
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' idx <- sample(nrow(diamonds), 1e4)
#' mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
#' ggmid(mid, "carat")
#' ggmid(mid, "clarity")
#' ggmid(mid, "carat:clarity", main.effects = TRUE)
#' ggmid(mid, "clarity:color", type = "data", theme = "Mako", data = diamonds[idx, ])
#' ggmid(mid, "carat:color", type = "compound", data = diamonds[idx, ])
#' @returns
#' \code{ggmid.mid()} returns a "ggplot" object.
#' @export ggmid
#'
ggmid <- function(object, ...)
UseMethod("ggmid")


#' @rdname ggmid
#' @param term a character string specifying the component function to be plotted.
#' @param type character string. The method for plotting the interaction effects.
#' @param theme a character string specifying the color theme or any item that can be used to define "color.theme" object.
#' @param intercept logical. If \code{TRUE}, the intercept is added to the MID values.
#' @param main.effects logical. If \code{TRUE}, the main effects are included in the interaction plot.
#' @param jitter a numeric value specifying the amount of jitter for points.
#' @param cells.count an integer or integer-valued vector of length two, specifying the number of cells for the raster type interaction plot.
#' @param data a data.frame to be plotted with the corresponding MID values. If not passed, data is extracted from \code{parent.env()} based on the function call of the "mid" object.
#' @param limits \code{NULL} or a numeric vector of length two specifying the limits of the plotting scale. \code{NA}s are replaced by the minimum and/or maximum MID values.
#' @param ... optional parameters to be passed to the main layer.
#' @importFrom rlang .data
#' @exportS3Method midr::ggmid
#'
ggmid.mid <- function(
    object, term, type = c("effect", "data", "compound"), theme = NULL,
    intercept = FALSE, main.effects = FALSE, data = NULL, jitter = .3,
    cells.count = c(100L, 100L), limits = c(NA, NA), ...) {
  tags <- term.split(term)
  term <- term.check(term, object$terms, stop = TRUE)
  type <- match.arg(type)
  if (missing(theme) && length(tags) == 2L)
    theme <- if(type == "data") "bluescale" else "midr"
  theme <- color.theme(theme)
  use.theme <- inherits(theme, "color.theme")
  if (type == "data" || type == "compound") {
    if (is.null(data))
      data <- model.data(object)
    if (is.null(data))
      stop(paste0("'data' must be supplied for the '", type, "' plot"))
    preds <- predict.mid(object, data, terms = unique(c(tags, term)),
                         type = "terms", na.action = "na.pass")
    data <- model.reframe(object, data)
  }
  # main effect
  if ((len <- length(tags)) == 1L) {
    df <- stats::na.omit(object$main.effects[[term]])
    if (intercept)
      df$mid <- df$mid + object$intercept
    pl <- ggplot2::ggplot(
      data = df, ggplot2::aes(x = .data[[term]], y = .data[["mid"]]))
    # main layer
    enc <- object$encoders[["main.effects"]][[term]]
    if (type == "effect" || type == "compound") {
      if (enc$type == "constant") {
        cols <- paste0(term, c("_min", "_max"))
        rdf <- data.frame(x = as.numeric(t(as.matrix(df[, cols]))),
                          y = rep(df$mid, each = 2L))
        colnames(rdf) <- c(term, "mid")
        pl <- pl + ggplot2::geom_path(
          ggplot2::aes(x = .data[[term]], y = .data[["mid"]]), data = rdf, ...)
      } else if (enc$type == "linear") {
        pl <- pl + ggplot2::geom_line(...)
      } else if (enc$type == "factor") {
        pl <- pl + ggplot2::geom_col(...)
      }
    }
    if (type == "data" || type == "compound") {
      data$mid <- as.numeric(preds[, term])
      if (intercept)
        data$mid <- data$mid + object$intercept
      jit <- 0
      if (enc$type == "factor") {
        data[[term]] <- apply.catchall(data[[term]], enc)
        jit <- jitter[1L]
      }
      pl <- pl + if (type == "data") {
        ggplot2::geom_jitter(
          ggplot2::aes(y = .data[["mid"]]), data, width = jit, height = 0, ...)
      } else {
        ggplot2::geom_jitter(
          ggplot2::aes(y = .data[["mid"]]), data, width = jit, height = 0)
      }
    }
    if (use.theme) {
      middle <- if (intercept) object$intercept else 0
      if (enc$type == "factor") {
        pl <- pl + ggplot2::aes(fill = .data[["mid"]]) +
          scale_fill_theme(theme = theme, limits = limits, middle = middle)
      }
      if (enc$type != "factor" || type == "data") {
        pl <- pl + ggplot2::aes(color = .data[["mid"]]) +
          scale_color_theme(theme = theme, limits = limits, middle = middle)
      }
    }
    if (!is.null(limits))
      pl <- pl + ggplot2::scale_y_continuous(limits = limits)
  # interaction
  } else if (len == 2L) {
    df <- object$interactions[[term]]
    df <- stats::na.omit(df)
    if (intercept)
      df$mid <- df$mid + object$intercept
    if (main.effects) {
      df$mid <- df$mid +
        mid.f(object, tags[1L], df) + mid.f(object, tags[2L], df)
    }
    for (tag in tags) {
      tagl <- paste0(tag, "_level")
      if (any(tagl == colnames(df))) {
        df[[paste0(tag, "_min")]] = df[[tagl]] - .5
        df[[paste0(tag, "_max")]] = df[[tagl]] + .5
      }
    }
    cols <- paste0(rep(tags, each = 2L), c("_min", "_max"))
    encs <- list(object$encoders[["interactions"]][[tags[1L]]],
                 object$encoders[["interactions"]][[tags[2L]]])
    pl <- ggplot2::ggplot(
      data = df, ggplot2::aes(x = .data[[tags[1L]]], y = .data[[tags[2L]]]))
    # main.layer
    if (type == "effect" || type == "compound") {
      use.raster <- encs[[1L]]$type == "linear" ||
        encs[[2L]]$type == "linear" || main.effects
      if (use.raster) {
        ms <- cells.count
        if (length(ms) == 1L)
          ms <- c(ms, ms)
        xy <- list()
        for (i in 1L:2L) {
          if (encs[[i]]$type == "factor") {
            xy[[i]] <- encs[[i]]$frame[[1L]]
            ms[i] <- encs[[i]]$n
          } else {
            xy[[i]] <- seq(min(df[[cols[i * 2L - 1L]]]),
                           max(df[[cols[i * 2L]]]), length.out = ms[i])
          }
        }
        rdf <- data.frame(rep(xy[[1L]], times = ms[2L]),
                          rep(xy[[2L]], each = ms[1L]))
        colnames(rdf) <- tags
        rdf$mid <- mid.f(object, term, rdf)
        if (intercept)
          rdf$mid <- rdf$mid + object$intercept
        if (main.effects)
          rdf$mid <- rdf$mid +
          mid.f(object, tags[1L], rdf) +
          mid.f(object, tags[2L], rdf)
        mpg <- ggplot2::aes(x = .data[[tags[1L]]], y = .data[[tags[2L]]],
                            fill = .data[["mid"]])
        pl <- pl + ggplot2::geom_raster(mapping = mpg, data = rdf, ...)
      } else {
        mpg <- ggplot2::aes(fill = .data[["mid"]],
                            xmin = .data[[cols[1L]]], xmax = .data[[cols[2L]]],
                            ymin = .data[[cols[3L]]], ymax = .data[[cols[4L]]])
        pl <- pl + ggplot2::geom_rect(mapping = mpg, ...)
      }
      if (use.theme) {
        middle <- if (intercept) object$intercept else 0
        pl <- pl + scale_fill_theme(theme = theme, limits = limits,
                                    middle = middle)
      } else {
        pl <- pl + ggplot2::scale_fill_continuous(limits = limits)
      }
    }
    if (type == "data" || type == "compound") {
      jit <- c(0, 0)
      for (i in 1L:2L) {
        if (encs[[i]]$type == "factor") {
          data[[tags[i]]] <- apply.catchall(data[[tags[i]]], encs[[i]])
          jit[i] <- if (length(jitter) > 1L) jitter[i] else jitter[1L]
        }
      }
      if (type == "compound") {
        pl <- pl + ggplot2::geom_jitter(data = data,
                                        width = jit[1L], height = jit[2L])
      } else if (type == "data") {
        data$mid <- rowSums(preds[, c(term, if (main.effects) tags), drop = FALSE])
        if (intercept)
          data$mid <- data$mid + object$intercept
        pl <- pl + ggplot2::geom_jitter(
          ggplot2::aes(colour = .data[["mid"]]), data = data,
          width = jit[1L], height = jit[2L], ...)
        if (use.theme) {
          middle <- if (intercept) object$intercept else 0
          pl <- pl + scale_color_theme(theme = theme, limits = limits,
                                       middle = middle)
        } else {
          pl <- pl + ggplot2::scale_colour_continuous(limits = limits)
        }
      }
    }
  }
  pl
}

#' @rdname ggmid
#' @exportS3Method ggplot2::autoplot
#'
autoplot.mid <- function(object, ...) {
  ggmid.mid(object = object, ...)
}
