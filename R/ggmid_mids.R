#' Compare MID Component Functions with ggplot2
#'
#' @description
#' For "mids" collection objects, \code{ggmid()} visualizes and compares one or more main effects across multiple models.
#'
#' @details
#' This is an S3 method for the \code{ggmid()} generic that evaluates the specified \code{term} over a grid of values and compares the results across all models in the collection.
#'
#' The \code{type} argument controls the visualization style.
#' The default, \code{type = "effect"}, plots the component functions of the specified \code{term} for each model individually.
#' The \code{type = "series"} option transposes the view to plot the effect trend over the models for each feature value.
#'
#' Note: Comparative plotting for interaction terms (2D surfaces) is not supported for collection objects.
#'
#' @param object a "mids" collection object to be visualized.
#' @param terms a character vector or a formula specifying the component functions to be plotted. If a formula is provided (e.g., \code{~ x + y}), it is automatically parsed to extract the relevant terms.
#' @param type the plotting style: "effect" plots the effect curve per model, while "series" plots the effect trend over models per feature value.
#' @param theme a character string or object defining the color theme. See \code{\link{color.theme}} for details.
#' @param intercept logical. If \code{TRUE}, the model intercept is added to the component effect.
#' @param limits a numeric vector of length two specifying the limits of the plotting scale. \code{NA} values are replaced by the minimum and/or maximum MID values.
#' @param resolution an integer specifying the number of evaluation points for continuous variables.
#' @param labels an optional numeric or character vector to specify the model labels. Defaults to \code{labels(object)}. The function attempts to parse these labels into numeric values where possible.
#' @param ... optional parameters passed to the main layer (e.g., \code{linewidth}, \code{alpha}).
#'
#' @examples
#' # Use a lightweight dataset for fast execution
#' data(mtcars, package = "datasets")
#'
#' # Fit two models with different complexities
#' fit1 <- lm(mpg ~ wt, data = mtcars)
#' mid1 <- interpret(mpg ~ wt, data = mtcars, model = fit1)
#' fit2 <- lm(mpg ~ wt + hp, data = mtcars)
#' mid2 <- interpret(mpg ~ wt + hp, data = mtcars, model = fit2)
#'
#' # Combine them into a "midlist" collection (which inherits from "mids")
#' mids <- midlist("wt" = mid1, "wt + hp" = mid2)
#'
#' # Compare the main effect of 'wt' across both models
#' ggmid(mids, term = "wt")
#'
#' # Compare the effect of 'wt' as a series plot across the models
#' ggmid(mids, term = "wt", type = "series")
#' @returns
#' \code{ggmid.mids()} returns a "ggplot" object if a single main effect is specified, or a list of "ggplot" objects if multiple main effects are specified.
#'
#' @seealso \code{\link{ggmid}}, \code{\link{plot.mids}}
#'
#' @exportS3Method midr::ggmid
#'
ggmid.mids <- function(
    object, terms = term.labels(object, order = 1L),
    type = c("effect", "series"), theme = NULL, intercept = FALSE,
    limits = c(NA, NA), resolution = NULL, labels = NULL, ...
) {
  type <- match.arg(type)
  labels <- labels %||% base::labels(object)
  if (length(term.labels(terms, order = 2L)) > 0L)
    message("interaction term plotting is not implemented for 'mids' objects")
  tlab <- term.labels(terms, order = 1L)
  mlab <- match.labels(tlab, term.labels(object, order = 1L))
  tlab <- tlab[!is.na(mlab)]
  mlab <- mlab[!is.na(mlab)]
  n <- length(tlab)
  if (n == 0L) stop("none of the specified 'terms' are in 'object'")
  intercept <- if (missing(intercept)) {
    vapply(tlab, has.intercept, logical(1L))
  } else rep_len(intercept, n)
  syncable <- (n > 1L) && (length(unique(intercept)) == 1L)
  if (syncable && !is.null(limits) && anyNA(limits)) {
    if (inherits(object, "midrib")) {
      mats <- lapply(mlab, function(t) as.matrix(object$main.effects[[t]]$mid))
      values <- do.call(rbind, mats)
      if (intercept[1L]) {
        shift <- get.intercept(object)
        values <- sweep(values, MARGIN = 2L, STATS = shift, FUN = "+")
      }
      values <- as.vector(values)
    } else {
      shift <- if (intercept[1L])
        get.intercept(object) else numeric(length(object))
      values <- unlist(lapply(
        X = seq_along(object),
        FUN = function(i) {
          unlist(lapply(
            X = mlab,
            FUN = function(t) object[[i]]$main.effects[[t]]$mid + shift[i]
          ))
        }
      ), use.names = FALSE)
    }
    if (is.na(limits[1L])) limits[1L] <- min(values, na.rm = TRUE)
    if (is.na(limits[2L])) limits[2L] <- max(values, na.rm = TRUE)
  }
  out <- list()
  for (i in seq_along(tlab)) {
    out[[tlab[i]]] <- .ggmid.mids(
      object, term = mlab[i], type = type, theme = theme,
      intercept = intercept[i], limits = limits,
      resolution = resolution, labels = labels
    )
  }
  if (n == 1L) out[[1L]] else out
}

.ggmid.mids <- function(
    object, term, type = c("effect", "series"), theme = NULL, intercept = FALSE,
    limits = c(NA, NA), resolution = NULL, labels = base::labels(object), ...
) {
  if (inherits(object, "midrib")) {
    base <- object
    if (is.null(base$encoders$main.effects[[term]]))
      stop(sprintf("the term '%s' was not found in the object", term))
  } else {
    ok <- vapply(
      X = object,
      FUN = function(m) !is.null(m$encoders$main.effects[[term]]),
      FUN.VALUE = logical(1L)
    )
    if (!any(ok))
      stop(sprintf("the term '%s' was not found in any of the models", term))
    base <- object[[which(ok)[1L]]]
  }
  enc <- base$encoders$main.effects[[term]]
  if (enc$type == "factor") {
    xvals <- factor(enc$envir$olvs, levels = enc$envir$olvs)
  } else {
    rng <- range(base$main.effects[[term]][, term], na.rm = TRUE)
    resolution <- resolution %||% (
      if (type == "series") 25L else
        min(max(1e4L %/% length(labels), 10L), 500L)
    )
    xvals <- seq(rng[1L], rng[2L], length.out = resolution)
  }
  fmat <- term.effect(object, term = term, x = xvals)
  if (intercept) {
    ints <- get.intercept(object)
    fmat <- sweep(fmat, MARGIN = 2L, STATS = ints, FUN = "+")
  }
  n <- nrow(fmat)
  m <- ncol(fmat)
  if (length(labels) != m)
    stop("length of 'labels' must match the number of models in the collection")
  nums <- suppressWarnings(as.numeric(labels))
  if (!anyNA(nums)) {
    labels <- nums
  } else if (!is.factor(labels)) {
    labels <- factor(labels, levels = unique(labels))
  }
  df <- data.frame(
    x = rep(xvals, times = m),
    label = rep(labels, each = n),
    mid = as.vector(fmat)
  )
  colnames(df)[1L] <- term
  discrete <- is.discrete(labels)
  if (type == "effect") {
    theme <- theme %||% color.theme.defaults(if (discrete) "qual" else "seq")
    theme <- color.theme(theme)
    pl <- ggplot2::ggplot(
      df, ggplot2::aes(x = .data[[term]], y = .data[["mid"]])
    )
    if (enc$type == "factor") {
      pl <- pl + .geom_col(
        ggplot2::aes(fill = .data[["label"]], group = factor(.data[["label"]])),
        position = ggplot2::position_dodge(), ...
      ) + scale_fill_theme(theme, discrete = discrete)
    } else {
      pl <- pl + .geom_line(
        ggplot2::aes(color = .data[["label"]], group = .data[["label"]]), ...
      ) + scale_color_theme(theme, discrete = discrete)
    }
  } else if (type == "series") {
    theme <- theme %||%
      color.theme.defaults(if (is.discrete(xvals)) "qual" else "seq")
    theme <- color.theme(theme)
    pl <- ggplot2::ggplot(
      df, ggplot2::aes(x = .data[["label"]], y = .data[["mid"]])
    )
    mpg <- ggplot2::aes(color = .data[[term]], group = .data[[term]])
    pl <- pl + if (discrete) .geom_linepoint(mpg, ...) else .geom_line(mpg, ...)
    pl <- pl + ggplot2::labs(x = NULL) +
      scale_color_theme(theme, discrete = is.discrete(xvals))
  }
  if (!is.null(limits)) {
    pl <- pl + ggplot2::scale_y_continuous(limits = limits)
  }
  pl
}

#' @rdname ggmid.mids
#'
#' @exportS3Method ggplot2::autoplot
#'
autoplot.mids <- function(object, ...) {
  mcall <- match.call(expand.dots = TRUE)
  mcall[[1L]] <- quote(ggmid)
  mcall[["object"]] <- object
  eval(mcall, parent.frame())
}
