#' Compare MID Component Functions
#'
#' @description
#' For "mids" collection objects, \code{plot()} visualizes and compares one or more main effects across multiple models.
#'
#' @details
#' This is an S3 method for the \code{plot()} generic that evaluates the specified \code{term} over a grid of values and compares the results across all models in the collection.
#'
#' The \code{type} argument controls the visualization style.
#' The default, \code{type = "effect"}, plots the component functions of the specified \code{term} for each model individually.
#' The \code{type = "series"} option transposes the view to plot the effect trend over the models for each feature value.
#'
#' Note: Comparative plotting for interaction terms (2D surfaces) is not supported for collection objects.
#'
#' @param x a "mids" collection object to be visualized.
#' @param terms a character vector or a formula specifying the component functions to be plotted. If a formula is provided (e.g., \code{~ x + y}), it is automatically parsed to extract the relevant terms.
#' @param type the plotting style: "effect" plots the effect curve per model, while "series" plots the effect trend over models per feature value.
#' @param theme a character string or object defining the color theme. See \code{\link{color.theme}} for details.
#' @param intercept logical. If \code{TRUE}, the model intercept is added to the component effect.
#' @param limits a numeric vector of length two specifying the limits of the plotting scale.
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
#' plot(mids, term = "wt")
#'
#' # Compare the effect of 'wt' as a series plot across the models
#' plot(mids, term = "wt", type = "series")
#' @returns
#' \code{plot.mids()} produces one or more plots as a side-effect and returns \code{NULL} invisibly.
#'
#' @seealso \code{\link{plot.mid}}, \code{\link{ggmid.mids}}
#'
#' @exportS3Method base::plot
plot.mids <- function(
    x, terms = term.labels(x, order = 1L),
    type = c("effect", "series"), theme = NULL, intercept = FALSE,
    limits = c(NA, NA), resolution = NULL, labels = NULL, ...
) {
  type <- match.arg(type)
  labels <- labels %||% base::labels(x)
  if (length(term.labels(terms, order = 2L)) > 0L)
    message("interaction term plotting is not implemented for 'mids' objects")
  tlab <- term.labels(terms, order = 1L)
  mlab <- match.labels(tlab, term.labels(x, order = 1L))
  tlab <- tlab[!is.na(mlab)]
  mlab <- mlab[!is.na(mlab)]
  n <- length(tlab)
  if (n == 0L) stop("none of the specified 'terms' are in 'x'")
  intercept <- if (missing(intercept)) {
    vapply(tlab, has.intercept, logical(1L))
  } else rep_len(intercept, n)
  syncable <- (n > 1L) && (length(unique(intercept)) == 1L)
  if (syncable && !is.null(limits) && anyNA(limits)) {
    if (inherits(x, "midrib")) {
      mats <- lapply(mlab, function(t) as.matrix(x$main.effects[[t]]$mid))
      values <- do.call(rbind, mats)
      if (intercept[1L]) {
        shift <- get.intercept(x)
        values <- sweep(values, MARGIN = 2L, STATS = shift, FUN = "+")
      }
      values <- as.vector(values)
    } else {
      shift <- if (intercept[1L])
        get.intercept(x) else numeric(length(x))
      values <- unlist(lapply(
        X = seq_along(x),
        FUN = function(i) {
          unlist(lapply(
            X = mlab,
            FUN = function(t) x[[i]]$main.effects[[t]]$mid + shift[i]
          ))
        }
      ), use.names = FALSE)
    }
    if (is.na(limits[1L])) limits[1L] <- min(values, na.rm = TRUE)
    if (is.na(limits[2L])) limits[2L] <- max(values, na.rm = TRUE)
  }
  if (anyNA(limits)) limits <- NULL
  for (i in seq_along(tlab)) {
    .plot.mids(
      x, term = mlab[i], type = type, theme = theme,
      intercept = intercept[i], limits = limits,
      resolution = resolution, labels = labels
    )
  }
}

.plot.mids <- function(
    x, term, type = c("effect", "series"), theme = NULL, intercept = FALSE,
    limits = NULL, resolution = NULL, labels = base::labels(x), ...
) {
  dots <- override(list(), list(...))
  if (inherits(x, "midrib")) {
    base <- x
    if (is.null(base$encoders$main.effects[[term]]))
      stop(sprintf("the term '%s' was not found in the object", term))
  } else {
    ok <- vapply(
      X = x,
      FUN = function(m) !is.null(m$encoders$main.effects[[term]]),
      FUN.VALUE = logical(1L)
    )
    if (!any(ok))
      stop(sprintf("the term '%s' was not found in any of the models", term))
    base <- x[[which(ok)[1L]]]
  }
  enc <- base$encoders$main.effects[[term]]
  # determine evaluation points
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
  # generate prediction matrix (rows = xvals, cols = models)
  fmat <- term.effect(x, term = term, x = xvals)
  if (intercept) {
    ints <- get.intercept(x)
    fmat <- sweep(fmat, MARGIN = 2L, STATS = ints, FUN = "+")
  }
  n <- nrow(fmat)
  m <- ncol(fmat)
  if (length(labels) != m)
    stop("length of 'labels' must match the number of models in the collection")
  # parse labels
  nums <- suppressWarnings(as.numeric(labels))
  if (!anyNA(nums)) {
    labels <- nums
  } else if (!is.factor(labels)) {
    labels <- factor(labels, levels = unique(labels))
  }
  discrete <- is.discrete(labels)
  if (type == "effect") {
    # effect Plot (X = term, Y = mid, Group = models)
    theme <- theme %||% color.theme.defaults(if (discrete) "qual" else "seq")
    theme <- color.theme(theme)
    cols <- theme$palette(m)
    if (enc$type == "factor") {
      # grouped bar plot for qualitative main effect
      args <- list(
        to = fmat, labels = as.character(xvals),
        fill = cols, xlab = term, ylab = "mid", limits = limits
      )
      args <- set.alpha(override(args, dots), on = "fill")
      do.call(.barplot, args)
    } else {
      # multi-line plot for quantitative main effect
      args <- list(
        x = xvals, y = fmat, type = "l", col = cols, lty = 1L,
        xlab = term, ylab = "mid", ylim = limits
      )
      args <- set.alpha(override(args, dots), on = "col")
      do.call(graphics::matplot, args)
    }
  } else if (type == "series") {
    # series Plot (X = labels, Y = mid, Group = feature values)
    theme <- theme %||%
      color.theme.defaults(if (is.discrete(xvals)) "qual" else "seq")
    theme <- color.theme(theme)
    cols <- theme$palette(n)
    if (discrete) {
      x_pos <- seq_along(labels)
      args <- list(
        x = x_pos, y = t(fmat), type = "b", col = cols, pch = 16L,
        lty = 1L, xaxt = "n", xlab = "", ylab = "mid", ylim = limits
      )
      args <- set.alpha(override(args, dots), on = "col")
      do.call(graphics::matplot, args)
      graphics::axis(side = 1L, at = x_pos, labels = as.character(labels))
    } else {
      args <- list(
        x = labels, y = t(fmat), type = "l", col = cols, lty = 1L,
        xlab = "", ylab = "mid", ylim = limits
      )
      args <- set.alpha(override(args, dots), on = "col")
      do.call(graphics::matplot, args)
    }
  }
  invisible(NULL)
}
