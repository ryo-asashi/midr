#' Plot Multiple MID Component Functions
#'
#' \code{mid.plots()} applies \code{ggmid()} or \code{plot()} to the component functions of a "mid" object.
#'
#' @param object a "mid" object.
#' @param terms a character vector. The names of the terms to be visualized.
#' @param limits \code{NULL} or a numeric vector of length two specifying the limits of the plotting scale. \code{NA}s are replaced by the minimum and/or maximum MID values.
#' @param intercept logical. If \code{TRUE}, the intercept is added to the MID values and the plotting scale is shifted.
#' @param main.effects logical. If \code{TRUE}, the main effects are included in the interaction plot.
#' @param max.plots an integer specifying the number of maximum number of plots.
#' @param engine character string. One of "ggplot2" or "graphics".
#' @param ... optional parameters to be passed to \code{ggmid()} or \code{plot()}.
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' idx <- sample(nrow(diamonds), 1e4L)
#' mid <- interpret(price ~ (carat + cut + color + clarity) ^ 2, diamonds[idx, ])
#' mid.plots(mid, c("carat", "color", "carat:color", "clarity:color"), limits = NULL)
#' @returns
#' If \code{engine} is "ggplot2", \code{mid.plots()} returns a list of "ggplot" objects. Otherwise \code{mid.plots()} produces plots and returns \code{NULL}.
#' @export mid.plots
#'
mid.plots <- function(
    object, terms = mid.terms(object, interaction = FALSE),
    limits = c(NA, NA), intercept = FALSE, main.effects = FALSE,
    max.plots = NULL, engine = c("ggplot2", "graphics"), ...) {
  engine <- match.arg(engine)
  if (length(terms) == 0L)
    return(NULL)
  if (!is.null(max.plots) && length(terms) > max.plots) {
    message("the number of terms exceeded the maximum number of plots")
    terms <- terms[1L:max.plots]
  }
  true_terms <- terms
  for (i in seq_len(length(terms))) {
    true_terms[i] <- term.check(terms[i], object$terms, stop = FALSE)
  }
  terms <- terms[!is.na(true_terms)]
  true_terms <- true_terms[!is.na(true_terms)]
  if (main.effects && !is.null(limits)) {
    limits <- NULL
  }
  if (!is.null(limits) && (is.na(limits[1L]) || is.na(limits[2L]))) {
    dfs <- c(object$main.effects, object$interactions)[true_terms]
    if (is.na(limits[1L])) {
      limits[1L] <-
        min(sapply(dfs, function(x) min(x$mid, na.rm = TRUE))) +
        if (intercept) object$intercept else 0
    }
    if (is.na(limits[2L])) {
      limits[2L] <-
        max(sapply(dfs, function(x) max(x$mid, na.rm = TRUE))) +
        if (intercept) object$intercept else 0
    }
  }
  if (engine == "ggplot2") {
    plots <- list()
    for (term in terms) {
      plots[[term]] <- ggmid(
        object, term, limits = limits, intercept = intercept,
        main.effects = main.effects, ...
      )
    }
    return(plots)
  } else {
    for (term in terms)
      plot.mid(object, term, intercept = intercept,
               main.effects = main.effects, ...)
  }
}
