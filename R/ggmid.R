#' Plot MID Values with ggplot2 Package
#'
#' Creates a ggplot object representing mid values of the functional decomposition term.
#'
#' @param object a mid object to be visualized.
#' @examples
#' data(diamonds, package = "ggplot2")
#' model <- lm(price ~ carat + cut + color + clarity + carat:clarity, diamonds)
#' mid <- interpret(price ~ carat + cut + color + clarity + carat:clarity,
#'                  data = diamonds, model = model)
#' ggmid(mid, "carat")
#' ggplot2::autoplot(mid, "clarity")
#' ggmid(mid, "carat:clarity")
#' ggmid(mid, "carat:clarity", add.intercept = TRUE,
#'       include.main.effects = TRUE, scale.type = "viridis")
#' @export ggmid
#'
ggmid <- function(object, ...)
UseMethod("ggmid")


#' @rdname ggmid
#'
#' @param term a name of the functional decomposition term to be plotted.
#' @param limits NULL or a numeric vector of length two providing limits of the scale. NA is replaced by the minimum or maximum mid value.
#' @param plot.main logical. If TRUE, lines, bars or rectangles representing mid values are drawn.
#' @param add.intercept logical. If TRUE, the intercept is added to the mid values and the scale for the plot is shifted.
#' @param include.main.effects logical. If TRUE, the main effects are added to the interaction mid values.
#' @param interaction.type a character, specifying the plotting method of interaction effects.
#' @param scale.type color type of interaction plots. One of "default", "viridis", "gradient" or a function that returns a continuous colour scale for \code{fill} aestetics like \code{ggplot2::scale_fill_viridis_c}.
#' @param scale.palette a character vector of color names, specifying the colors to be used in the interaction plot.
#' @param partition an integer specifying the coarseness of the grid for a "raster" type interaction plot.
#' @param ... optional parameters to be passed to the main layer (\code{geom_line}, \code{geom_path}, \code{geom_bar}, or \code{geom_rect}) of each plot.
#'
#' @importFrom rlang .data
#' @exportS3Method midr::ggmid
#'
ggmid.mid <- function(
    object, term, limits = c(NA, NA), plot.main = TRUE,
    add.intercept = FALSE, include.main.effects = FALSE,
    interaction.type = c("default", "raster", "rectangle"),
    scale.type = "default", scale.palette = c("#2f7a9a", "#FFFFFF", "#7e1952"),
    partition = 100L, ...) {
  interaction.type = match.arg(interaction.type)
  tags <- unlist(strsplit(term, ":"))
  if ((len <- length(tags)) == 1L) {
    # main effect
    df <- object$main.effects[[term]]
    df <- stats::na.omit(df)
    if (add.intercept)
      df$mid <- df$mid + object$intercept
    pl <- ggplot2::ggplot(data = df,
                          ggplot2::aes(x = .data[[term]], y = .data[["mid"]]))
    if (plot.main) {
      enc <- object$me.encoders[[term]]
      if (enc$type != "factor") {
        if (enc$type == "constant") {
          cns <- paste0(term, c("_min", "_max"))
          rdf <- data.frame(x = as.numeric(t(as.matrix(df[, cns]))),
                            y = rep(df$mid, each = 2L))
          pl <- pl + ggplot2::geom_path(
            ggplot2::aes(x = .data[["x"]], y = .data[["y"]]), data = rdf, ...)
        } else {
          pl <- pl + ggplot2::geom_line(...)
        }
      } else {
        pl <- pl + ggplot2::geom_col(...)
      }
    }
    if (!is.null(limits))
      pl <- pl + ggplot2::scale_y_continuous(limits = limits)
  } else if (len == 2L) {
    # interaction
    if (!(term %in% object$terms))
      term <- paste0(rev(tags), collapse=":")
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
    mpt <- ifelse(add.intercept, object$intercept, 0)
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
      stop("invalid 'scale.type' is passed.")
    }
    if (plot.main) {
      encs <- list(object$ie.encoders[[tags[1L]]],
                   object$ie.encoders[[tags[2L]]])
      if (interaction.type == "default") {
        if (encs[[1L]]$type == "linear" || encs[[2L]]$type == "linear" ||
            include.main.effects) {
          interaction.type <- "raster"
        } else {
          interaction.type <- "rectangle"
        }
      }
      if (interaction.type == "raster") {
        ms <- c(partition, partition)
        xy <- list()
        for (i in 1L:2L) {
          if (encs[[i]]$type == "factor") {
            xy[[i]] <- encs[[i]]$frame[[1L]]
            ms[i] <- encs[[i]]$n
          } else {
            xy[[i]] <- seq(min(df[[cns[i * 2L - 1L]]]), max(df[[cns[i * 2L]]]),
                           length.out = partition)
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
  }
  pl
}

#' @rdname ggmid
#' @exportS3Method ggplot2::autoplot
#'
autoplot.mid <- function(object, ...) {
  ggmid.mid(object = object, ...)
}
