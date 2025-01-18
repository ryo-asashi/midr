#' Plot MID with ggplot2 Package
#'
#' For "mid" objects, \code{ggmid()} visualizes a MID component function using the ggplot2 package.
#'
#' The S3 method of \code{ggmid()} for "mid" objects creates a "ggplot" object that visualizes a MID component function.
#' The main layer is drawn using \code{geom_line()} or \code{geom_path()} for a main effect of a quantitative variable, \code{geom_col()} for a main effect of a qualitative variable, and \code{geom_raster()} or \code{geom_rect()} for an interaction effect.
#' For other methods of \code{ggmid()}, see \code{help(ggmid.mid.importance)} or \code{help(ggmid.mid.conditional)}.
#'
#' @param object a "mid" object to be visualized.
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' idx <- sample(nrow(diamonds), 1e4)
#' mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
#' ggmid(mid, "carat")
#' ggplot2::autoplot(mid, "clarity")
#' ggmid(mid, "carat:clarity")
#' ggmid(mid, "carat:clarity", add.intercept = TRUE,
#'       include.main.effects = TRUE, scale.type = "viridis")
#' @returns
#' \code{ggmid.mid()} returns a "ggplot" object.
#' @export ggmid
#'
ggmid <- function(object, ...)
UseMethod("ggmid")


#' @rdname ggmid
#'
#' @param term a character string specifying the component function to be plotted.
#' @param limits \code{NULL} or a numeric vector of length two specifying the limits of the plotting scale. \code{NA}s are replaced by the minimum and/or maximum MID values.
#' @param plot.main logical. If \code{FALSE}, the main layer is not drawn.
#' @param add.intercept logical. If \code{TRUE}, the intercept is added to the MID values.
#' @param include.main.effects logical. If \code{TRUE}, the main effects are included in the interaction plot.
#' @param interaction.type character string. The method for plotting the interaction effects.
#' @param scale.type a character string or function specifying the color type of interaction plot. One of "default", "viridis", "gradient" or a function that returns a continuous color scale for \code{fill} aesthetics.
#' @param scale.palette a character vector of color names. The colors are used for the interaction plot when \code{scale.type} is "default".
#' @param cells.count an integer or integer-valued vector of length two, specifying the number of cells for the raster type interaction plot.
#' @param data a data frame to be plotted with the corresponding MID values.
#' @param shape an integer specifying the shape of the plotted data points.
#' @param ... optional parameters to be passed to the main layer.
#'
#' @importFrom rlang .data
#' @exportS3Method midr::ggmid
#'
ggmid.mid <- function(
    object, term, limits = c(NA, NA), plot.main = TRUE,
    add.intercept = FALSE, include.main.effects = FALSE,
    interaction.type = c("default", "raster", "rectangle"),
    scale.type = "default", scale.palette = c("#2f7a9a", "#FFFFFF", "#7e1952"),
    cells.count = c(100L, 100L), data = NULL, shape = 16L, ...) {
  interaction.type = match.arg(interaction.type)
  tags <- term.split(term)
  term <- term.check(term, object$terms, stop = TRUE)
  if ((len <- length(tags)) == 1L) {
    # main effect
    df <- object$main.effects[[term]]
    df <- stats::na.omit(df)
    if (add.intercept)
      df$mid <- df$mid + object$intercept
    pl <- ggplot2::ggplot(data = df,
                          ggplot2::aes(x = .data[[term]], y = .data[["mid"]]))
    enc <- object$encoders[["main.effects"]][[term]]
    if (plot.main) {
      if (enc$type == "constant") {
        cns <- paste0(term, c("_min", "_max"))
        rdf <- data.frame(x = as.numeric(t(as.matrix(df[, cns]))),
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
    if (!is.null(data)) {
      data$mid <- predict.mid(object, data, terms = term, na.action = "na.pass")
      if (!add.intercept) data$mid <- data$mid - object$intercept
      position <- if (enc$type == "factor") "jitter" else "identity"
      pl <- pl + ggplot2::geom_point(ggplot2::aes(y = .data[["mid"]]), data,
                                     position = position, shape = shape)
    }
    if (!is.null(limits))
      pl <- pl + ggplot2::scale_y_continuous(limits = limits)
  } else if (len == 2L) {
    # interaction
    df <- object$interactions[[term]]
    df <- stats::na.omit(df)
    if (add.intercept)
      df$mid <- df$mid + object$intercept
    if (include.main.effects) {
      df$mid <- df$mid +
        mid.f(object, tags[1L], df) + mid.f(object, tags[2L], df)
    }
    for (tag in tags) {
      tagl <- paste0(tag, "_level")
      if (tagl %in% colnames(df)) {
        df[[paste0(tag, "_min")]] = df[[tagl]] - .5
        df[[paste0(tag, "_max")]] = df[[tagl]] + .5
      }
    }
    cns <- paste0(rep(tags, each = 2L), c("_min", "_max"))
    mpt <- if (add.intercept) object$intercept else 0
    pl <- ggplot2::ggplot(data = df,
      ggplot2::aes(x = .data[[tags[1L]]], y = .data[[tags[2L]]]))
    if (is.function(scale.type) || scale.type %in% c("viridis", "gradient")) {
      pl <- pl + ggplot2::scale_fill_continuous(
        type = scale.type, limits = limits)
    } else if (scale.type == "default") {
      spl <- scale.palette
      if (is.function(spl))
        spl <- spl(3L)
      if (length(spl != 3L))
        spl <- grDevices::colorRampPalette(spl)(3L)
      pl <- pl + ggplot2::scale_fill_gradient2(
        limits = limits, midpoint = mpt,
        low = spl[1L], mid = spl[2L], high = spl[3L])
    } else {
      stop("invalid 'scale.type' is passed")
    }
    encs <- list(object$encoders[["interactions"]][[tags[1L]]],
                 object$encoders[["interactions"]][[tags[2L]]])
    if (plot.main) {
      if (interaction.type == "default") {
        if (encs[[1L]]$type == "linear" || encs[[2L]]$type == "linear" ||
            include.main.effects) {
          interaction.type <- "raster"
        } else {
          interaction.type <- "rectangle"
        }
      }
      if (interaction.type == "raster") {
        ms <- cells.count
        if (length(ms) == 1L)
          ms <- c(ms, ms)
        xy <- list()
        for (i in 1L:2L) {
          if (encs[[i]]$type == "factor") {
            xy[[i]] <- encs[[i]]$frame[[1L]]
            ms[i] <- encs[[i]]$n
          } else {
            xy[[i]] <- seq(min(df[[cns[i * 2L - 1L]]]), max(df[[cns[i * 2L]]]),
                           length.out = ms[i])
          }
        }
        rdf <- data.frame(rep(xy[[1L]], times = ms[2L]),
                          rep(xy[[2L]], each = ms[1L]))
        colnames(rdf) <- tags
        rdf$mid <- mid.f(object, term, rdf)
        if (add.intercept)
          rdf$mid <- rdf$mid + object$intercept
        if (include.main.effects)
          rdf$mid <- rdf$mid + mid.f(object, tags[1L], rdf) + mid.f(object, tags[2L], rdf)
        mpg <- ggplot2::aes(x = .data[[tags[1L]]], y = .data[[tags[2L]]],
                            fill = .data[["mid"]])
        pl <- pl + ggplot2::geom_raster(mapping = mpg, data = rdf, ...)
      } else {
        mpg <- ggplot2::aes(fill = .data[["mid"]],
                            xmin = .data[[cns[1L]]], xmax = .data[[cns[2L]]],
                            ymin = .data[[cns[3L]]], ymax = .data[[cns[4L]]])
        pl <- pl + ggplot2::geom_rect(mapping = mpg, ...)
      }
    }
    if (!is.null(data)) {
      data$mid <- predict.mid(object, data, na.action = "na.pass",
                              terms = c(term, if (include.main.effects) tags))
      if (!add.intercept) data$mid <- data$mid - object$intercept
      position <- if (encs[[1L]]$type == "factor" || encs[[2L]]$type == "factor")
        "jitter" else "identity"
      pl <- pl + ggplot2::geom_point(ggplot2::aes(colour = .data[["mid"]]),
                                     data, position = position, shape = shape)
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
