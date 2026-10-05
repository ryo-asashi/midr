#' Plot MID Component Function with ggplot2
#'
#' @description
#' \code{ggmid()} is an S3 generic function for creating various visualizations from MID-related objects using \strong{ggplot2}.
#' For "mid" objects (i.e., fitted MID models), it visualizes one or more component functions specified by the \code{terms} argument.
#'
#' @details
#' For "mid" objects, \code{ggmid()} creates a "ggplot" object that visualizes a component function of the fitted MID model.
#'
#' The \code{type} argument controls the visualization style.
#' The default, \code{type = "effect"}, plots the component function itself.
#' In this style, the plotting method is automatically selected based on the effect's type:
#' a line plot for quantitative main effects; a bar plot for qualitative main effects; and a raster plot for interactions.
#' The \code{type = "data"} option creates a scatter plot of \code{data}, colored by the values of the component function.
#' The \code{type = "compound"} option combines both approaches, plotting the component function alongside the data points.
#'
#' @param object a "mid" object to be visualized.
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' idx <- sample(nrow(diamonds), 1e4)
#' mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
#'
#' # Plot a quantitative main effect
#' ggmid(mid, "carat")
#'
#' # Plot a qualitative main effect
#' ggmid(mid, "clarity")
#'
#' # Plot an interaction effect with data points and a raster layer
#' ggmid(mid, "carat:clarity", type = "compound", data = diamonds[idx, ])
#'
#' # Use a different color theme
#' ggmid(mid, "clarity:color", theme = "RdBu")
#' @returns
#' \code{ggmid.mid()} returns a "ggplot" object if a single term is specified, or a list of "ggplot" objects if multiple terms are specified.
#'
#' @seealso \code{\link{interpret}}, \code{\link{ggmid.midimp}}, \code{\link{ggmid.midcon}}, \code{\link{ggmid.midbrk}}, \code{\link{plot.mid}}
#'
#' @export ggmid
#'
ggmid <- function(object, ...)
UseMethod("ggmid")


#' @rdname ggmid
#'
#' @param terms a character vector or a formula specifying the component functions to be plotted. If a formula is provided (e.g., \code{~ x + y + x:y}), it is automatically parsed to extract the relevant terms.
#' @param type the plotting style. One of "effect", "data" or "compound".
#' @param theme a character string or object defining the color theme. See \code{\link{color.theme}} for details.
#' @param intercept logical. If \code{TRUE}, the intercept is added to the MID values.
#' @param main.effects logical. If \code{TRUE}, main effects are included in the interaction plot.
#' @param data a data frame to be plotted with the corresponding MID values. If not provided, data is automatically extracted based on the function call.
#' @param limits a numeric vector of length two specifying the limits of the plotting scale. \code{NA} values are replaced by the minimum and/or maximum MID values.
#' @param jitter a numeric value specifying the amount of jitter for the data points.
#' @param resolution an integer or vector of two integers specifying the resolution of the raster plot for interactions.
#' @param lumped logical. If \code{TRUE}, uses the lumped factor levels; if \code{FALSE}, uses the original levels from the data. Always \code{FALSE} when \code{main.effects = TRUE}.
#' @param ... optional parameters passed to the main plotting layer.
#'
#' @exportS3Method midr::ggmid
#'
ggmid.mid <- function(
    object, terms = term.labels(object, order = 1L),
    type = c("effect", "data", "compound"), theme = NULL,
    intercept = FALSE, main.effects = FALSE, data = NULL, limits = c(NA, NA),
    jitter = NULL, resolution = c(100L, 100L), lumped = TRUE, ...) {
  type <- match.arg(type)
  tlab <- term.labels(terms, order = 1L:2L)
  mlab <- match.labels(tlab, term.labels(object), single = FALSE)
  tlab <- tlab[!is.na(mlab)]
  mlab <- mlab[!is.na(mlab)]
  n <- length(tlab)
  if (n == 0L) stop("none of the specified 'terms' are in 'object'")
  tags <- lapply(tlab, get.variables)
  intercept <- if (missing(intercept)) {
    vapply(tlab, has.intercept, logical(1L))
  } else rep_len(intercept, n)
  main.effects <- if (missing(main.effects)) {
    vapply(tlab, is.fully.crossed, logical(1L))
  } else rep_len(main.effects, n)
  syncable <- (n > 1L) && (length(unique(intercept)) == 1L) && !any(main.effects)
  if (syncable && !is.null(limits) && anyNA(limits)) {
    shift <- if (intercept[1L]) get.intercept(object) else 0
    dfs <- c(object$main.effects, object$interactions)[mlab]
    values <- unlist(lapply(dfs, `[[`, "mid"), use.names = FALSE)
    if (is.na(limits[1L])) limits[1L] <- min(values, na.rm = TRUE) + shift
    if (is.na(limits[2L])) limits[2L] <- max(values, na.rm = TRUE) + shift
  }
  out <- list()
  for (i in seq_along(tlab)) {
    out[[tlab[i]]] <- .ggmid.mid(
      object, term = mlab[i], tags = tags[[i]], type = type, theme = theme,
      intercept = intercept[i], main.effects = main.effects[i], data = data,
      limits = limits, jitter = jitter, resolution = resolution,
      lumped = lumped, ...
    )
  }
  if (n == 1L) out[[1L]] else out
}

.ggmid.mid <- function(
    object, term, tags, type = c("effect", "data", "compound"), theme = NULL,
    intercept = FALSE, main.effects = FALSE, data = NULL, limits = c(NA, NA),
    jitter = NULL, resolution = c(100L, 100L), lumped = TRUE, ...) {
  type <- match.arg(type)
  ie <- (length(tags) > 1L)
  if (is.null(theme) && ie)
    theme <- color.theme.defaults(if (type == "data") "seq" else "div")
  theme <- color.theme(theme)
  use.theme <- inherits(theme, "color.theme")
  if (type == "data" || type == "compound") {
    if (is.null(data))
      data <- model.data(object, env = parent.frame())
    if (is.null(data))
      stop("'data' must be supplied for the '", type, "' plot")
    preds <- stats::predict(object, data, terms = unique(c(tags, term)),
                            type = "terms", na.action = "na.pass")
    data <- model.reframe(object, data)
  }
  lumped <- isTRUE(lumped) && isFALSE(main.effects)
  ints <- get.intercept(object)
  if (!ie) {
    enc <- object$encoders$main.effects[[term]]
    if (enc$type != "factor" || lumped) {
      df <- stats::na.omit(object$main.effects[[term]])
    } else {
      df <- factor.frame(enc$envir$olvs, tag = tags)
      df$mid <- term.effect(object, term, df)
    }
    if (intercept)
      df$mid <- df$mid + ints
    pl <- ggplot2::ggplot(
      data = df,
      mapping = ggplot2::aes(x = .data[[term]], y = .data[["mid"]])
    )
    # main layer
    if (type == "effect" || type == "compound") {
      if (enc$type == "constant") {
        cols <- paste0(term, c("_min", "_max"))
        rdf <- data.frame(as.numeric(t(as.matrix(df[, cols]))),
                          rep(df$mid, each = 2L))
        colnames(rdf) <- c(term, "mid")
        pl <- pl + .geom_path(data = rdf, ...)
      } else if (enc$type == "linear") {
        pl <- pl + .geom_line(...)
      } else if (enc$type == "factor") {
        pl <- pl + .geom_col(...)
      }
    }
    if (type == "data" || type == "compound") {
      data$mid <- as.numeric(preds[, term])
      dots <- standardize_param_names(list(...))
      if (intercept)
        data$mid <- data$mid + ints
      jit <- 0
      if (enc$type == "factor") {
        jit <- jitter[1L] %||% 0.45
        data[, term] <- enc$transform(data[, term], lumped = lumped)
      }
      pl <- pl + .geom_jitter(data = data, width = jit, height = 0, ...)
    }
    if (use.theme) {
      middle <- if (intercept) ints else 0
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
  } else if (ie) {
    encs <- list(object$encoders$interactions[[tags[1L]]],
                 object$encoders$interactions[[tags[2L]]])
    frms <- list()
    for (i in seq_len(2L)) {
      frms[[i]] <- if (encs[[i]]$type != "factor" || lumped) {
        encs[[i]]$frame
      } else {
        factor.frame(encs[[i]]$envir$olvs, tag = tags[i])
      }
    }
    df <- interaction.frame(frms[[1L]], frms[[2L]])
    for (tag in tags) {
      tagl <- paste0(tag, "_level")
      if (any(tagl == colnames(df))) {
        df[[paste0(tag, "_min")]] = df[[tagl]] - .5
        df[[paste0(tag, "_max")]] = df[[tagl]] + .5
      }
    }
    cols <- paste0(rep(tags, each = 2L), c("_min", "_max"))
    df$mid <- term.effect(object, term, df)
    if (intercept)
      df$mid <- df$mid + ints
    if (main.effects) {
      df$mid <- df$mid +
        term.effect(object, tags[1L], df) + term.effect(object, tags[2L], df)
    }
    pl <- ggplot2::ggplot(
      data = df,
      mapping = ggplot2::aes(x = .data[[tags[1L]]], y = .data[[tags[2L]]])
    )
    # main.layer
    if (type == "effect" || type == "compound") {
      use.raster <- encs[[1L]]$type == "linear" ||
        encs[[2L]]$type == "linear" || main.effects
      if (use.raster) {
        ms <- resolution
        if (length(ms) == 1L)
          ms <- c(ms, ms)
        xy <- list()
        for (i in seq_len(2L)) {
          if (encs[[i]]$type == "factor") {
            xy[[i]] <- frms[[i]][[1L]]
            ms[i] <- length(xy[[i]])
          } else {
            xy[[i]] <- seq(min(df[[cols[i * 2L - 1L]]]),
                           max(df[[cols[i * 2L]]]), length.out = ms[i])
          }
        }
        rdf <- data.frame(rep(xy[[1L]], times = ms[2L]),
                          rep(xy[[2L]], each = ms[1L]))
        colnames(rdf) <- tags
        rdf$mid <- term.effect(object, term, rdf)
        if (intercept)
          rdf$mid <- rdf$mid + ints
        if (main.effects) {
          rdf$mid <- rdf$mid +
            term.effect(object, tags[1L], rdf) +
            term.effect(object, tags[2L], rdf)
        }
        pl <- pl + .geom_raster(
          mapping = ggplot2::aes(fill = .data[["mid"]]), data = rdf, ...
        )
      } else {
        mpg <- ggplot2::aes(
          fill = .data[["mid"]],
          xmin = .data[[cols[1L]]], xmax = .data[[cols[2L]]],
          ymin = .data[[cols[3L]]], ymax = .data[[cols[4L]]]
        )
        pl <- pl + .geom_rect(mapping = mpg, ...)
      }
      if (use.theme) {
        middle <- if (intercept) ints else 0
        pl <- pl + scale_fill_theme(theme = theme, limits = limits,
                                    middle = middle)
      } else {
        pl <- pl + ggplot2::scale_fill_continuous(limits = limits)
      }
    }
    if (type == "data" || type == "compound") {
      jit <- c(0, 0)
      for (i in seq_len(2L)) {
        if (encs[[i]]$type == "factor") {
          jit[i] <- if (length(jitter) > 1L) jitter[i] else (jitter[1L] %||% 0.3)
          data[, tags[i]] <-
            encs[[i]]$transform(data[, tags[i]], lumped = lumped)
        }
      }
      if (type == "compound") {
        pl <- pl + .geom_jitter(
          data = data, width = jit[1L], height = jit[2L], ...
        )
      } else if (type == "data") {
        data$mid <- rowSums(
          preds[, c(term, if (main.effects) tags), drop = FALSE]
        )
        if (intercept)
          data$mid <- data$mid + ints
        pl <- pl + .geom_jitter(
          mapping = ggplot2::aes(colour = .data[["mid"]]),
          data = data, width = jit[1L], height = jit[2L], ...
        )
        if (use.theme) {
          middle <- if (intercept) ints else 0
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
#'
#' @exportS3Method ggplot2::autoplot
#'
autoplot.mid <- function(object, ...) {
  mcall <- match.call(expand.dots = TRUE)
  mcall[[1L]] <- quote(ggmid)
  mcall[["object"]] <- object
  eval(mcall, parent.frame())
}
