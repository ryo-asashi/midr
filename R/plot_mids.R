#' Compare MID Component Functions
#'
#' @description
#' For "mids" collection objects, \code{plot()} visualizes and compares a single main effect across multiple models using base R graphics.
#'
#' @details
#' This is an S3 method for the \code{plot()} generic that evaluates the specified \code{term} over a grid of values and compares the results across all models in the collection.
#'
#' For continuous variables, it uses a multi-line plot (\code{matplot}). For qualitative factors, it uses a grouped bar plot (\code{barplot}).
#' The \code{type = "series"} option transposes the view to plot the effect trend over the models for each feature value.
#'
#' Note: Comparative plotting for interaction terms (2D surfaces) is not supported for collection objects.
#'
#' @param x a "mids" collection object to be visualized.
#' @param term a character string specifying the main effect to evaluate.
#' @param type the plotting style: "effect" plots the effect curve per model, while "series" plots the effect trend over models per feature value.
#' @param theme a character string or object defining the color theme.
#' @param intercept logical. If \code{TRUE}, the model intercept is added to the component effect.
#' @param limits a numeric vector of length two specifying the limits of the plotting scale.
#' @param resolution an integer specifying the number of evaluation points for continuous variables.
#' @param labels an optional numeric or character vector to specify the x-axis coordinates or labels. Defaults to \code{labels(object)}. The function attempts to parse these labels into numeric values where possible.
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
#' \code{plot.mids()} produces a plot as a side-effect and returns \code{NULL} invisibly.
#'
#' @seealso \code{\link{plot.mid}}, \code{\link{ggmid.mids}}
#'
#' @exportS3Method base::plot
#'
plot.mids <- function(
    x, term, type = c("effect", "series"), theme = NULL, intercept = FALSE,
    limits = NULL, resolution = NULL, labels = base::labels(x), ...
) {
  dots <- override(list(), list(...))
  tags <- term.split(term)
  term <- term.check(term, mid.terms(x), stop = TRUE)
  if (length(tags) > 1L) {
    stop("comparative plotting for interaction terms is not supported for collections")
  }
  type <- match.arg(type)
  base_mod <- as.list(x)[[1L]]
  enc <- base_mod$encoders$main.effects[[term]]
  # determine evaluation points
  if (enc$type == "factor") {
    xvals <- factor(enc$envir$olvs, levels = enc$envir$olvs)
  } else {
    rng <- range(base_mod$main.effects[[term]][, term], na.rm = TRUE)
    resolution <- resolution %||% (
      if (type == "series") 25L else
        min(max(1e4L %/% length(labels), 10L), 500L)
    )
    xvals <- seq(rng[1L], rng[2L], length.out = resolution)
  }
  # generate prediction matrix (rows = xvals, cols = models)
  fmat <- mid.effect(x, term = term, x = xvals)
  if (intercept) {
    ints <- vapply(as.list(x), `[[`, 0.0, "intercept")
    fmat <- sweep(fmat, 2L, ints, "+")
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
    theme <- theme %||% (
      if (discrete) getOption("midr.qualitative", "HCL")
      else getOption("midr.sequential", "bluescale")
    )
    theme <- color.theme(theme)
    cols <- theme$palette(m)
    if (enc$type == "factor") {
      # grouped bar plot for qualitative main effect
      args <- list(
        to = fmat, labels = as.character(xvals),
        fill = cols, xlab = term, ylab = "mid", limits = limits
      )
      args <- override(args, dots)
      do.call(barplot2, args)
    } else {
      # multi-line plot for quantitative main effect
      args <- list(
        x = xvals, y = fmat, type = "l", col = cols, lty = 1L,
        xlab = term, ylab = "mid", ylim = limits
      )
      args <- override(args, dots)
      do.call(graphics::matplot, args)
    }
  } else if (type == "series") {
    # series Plot (X = labels, Y = mid, Group = feature values)
    theme <- theme %||% (
      if (is.discrete(xvals)) getOption("midr.qualitative", "HCL")
      else getOption("midr.sequential", "bluescale")
    )
    theme <- color.theme(theme)
    cols <- theme$palette(n)
    if (discrete) {
      x_pos <- seq_along(labels)
      args <- list(
        x = x_pos, y = t(fmat), type = "b", col = cols, pch = 16L,
        lty = 1L, xaxt = "n", xlab = "label", ylab = "mid", ylim = limits
      )
      args <- override(args, dots)
      do.call(graphics::matplot, args)
      graphics::axis(side = 1L, at = x_pos, labels = as.character(labels))
    } else {
      args <- list(
        x = labels, y = t(fmat), type = "l", col = cols, lty = 1L,
        xlab = "label", ylab = "mid", ylim = limits
      )
      args <- override(args, dots)
      do.call(graphics::matplot, args)
    }
  }
  invisible(NULL)
}
