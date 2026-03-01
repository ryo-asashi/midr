#' Compare MID Component Functions with ggplot2
#'
#' @description
#' For "mids" collection objects, \code{ggmid()} visualizes and compares a single main effect across multiple models.
#'
#' @details
#' This method evaluates the specified \code{term} over a grid of values and compares the results across all models in the collection.
#' For continuous variables, it uses line plots (or step plots for constant encodings). For factors, it uses grouped bar plots.
#'
#' Note: Comparative plotting for interaction terms (2D surfaces) is not supported for collection objects.
#'
#' @param object a "mids" or "midlist" collection object to be visualized.
#' @param term a character string specifying the main effect to evaluate.
#' @param type the plotting style: "effect" plots the effect curve per model, while "series" plots the effect trend over models per feature value.
#' @param theme a character string or object defining the color theme.
#' @param intercept logical. If \code{TRUE}, the model intercept is added to the component effect.
#' @param resolution an integer specifying the number of evaluation points for continuous variables.
#' @param labels an optional numeric or character vector to specify the x-axis coordinates or labels. Defaults to \code{labels(object)}. The function attempts to parse these labels into numeric values where possible.
#' @param ... optional parameters passed to the main layer (e.g., \code{linewidth}, \code{alpha}).
#'
#' @exportS3Method midr::ggmid
#'
ggmid.mids <- function(
    object, term, type = c("effect", "series"), theme = NULL,
    intercept = FALSE, resolution = NULL, labels = base::labels(object), ...
) {
  tags <- term.split(term)
  term <- term.check(term, mid.terms(object), stop = TRUE)
  if (length(tags) > 1L) {
    stop("comparative plotting for interaction terms is not supported for collections")
  }
  type <- match.arg(type)
  base <- as.list(object)[[1L]]
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
  fmat <- mid.effect(object, term = term, x = xvals)
  if (intercept) {
    ints <- vapply(as.list(object), `[[`, 0.0, "intercept")
    fmat <- sweep(fmat, 2L, ints, "+")
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
    mid = as.vector(fmat),
    label = rep(labels, each = n)
  )
  colnames(df)[1L] <- term
  if (type == "effect") {
    theme <- theme %||% (
      if (is.discrete(labels)) getOption("midr.qualitative", "HCL")
      else getOption("midr.sequential", "bluescale")
    )
    theme <- color.theme(theme)
    pl <- ggplot2::ggplot(
      df, ggplot2::aes(x = .data[[term]], y = .data[["mid"]])
    )
    if (enc$type == "factor") {
      pl <- pl + ggplot2::geom_col(
        ggplot2::aes(fill = .data[["label"]], group = factor(.data[["label"]])),
        position = ggplot2::position_dodge(), ...
      ) + scale_fill_theme(theme, discrete = is.discrete(labels))
    } else {
      pl <- pl + ggplot2::geom_line(
        ggplot2::aes(color = .data[["label"]], group = .data[["label"]]), ...
      ) + scale_color_theme(theme, discrete = is.discrete(labels))
    }
  } else if (type == "series") {
    theme <- theme %||% (
      if (is.discrete(xvals)) getOption("midr.qualitative", "HCL")
      else getOption("midr.sequential", "bluescale")
    )
    theme <- color.theme(theme)
    pl <- ggplot2::ggplot(
      df, ggplot2::aes(x = .data[["label"]], y = .data[["mid"]],
                       color = .data[[term]], group = .data[[term]])
    )
    if (is.discrete(labels)) {
      pl <- pl + geom_dotline(...)
    } else {
      pl <- pl + ggplot2::geom_line(...)
    }
    pl <- pl + scale_color_theme(theme, discrete = is.discrete(xvals))
  }
  pl
}


#' @rdname ggmid.mids
#' @exportS3Method ggplot2::autoplot
autoplot.mids <- function(object, ...) {
  ggmid.mids(object = object, ...)
}
