#' Create Plots for Multiple Functional Decomposition Terms
#'
#' Returns a list of ggplot objects, each representing the mid values for the corresponding functional decomposition term.
#'
#' @param object a mid object.
#' @param terms a character vector that specifies the names of the terms to be visualized.
#' @param limits NULL or a numeric vector of length two providing limits of the scale. NA will be replaced by the minimum or maximum mid value in all terms.
#' @param add.intercept logical. If TRUE, the intercept is added to the mid values and the scale for the plot is shifted.
#' @param max.plots the number of maximum number of plots.
#' @param engine a name of the package used to create plots. Possible values are "ggplot2" and "base" ("graphics").
#' @param ... optional parameters to be passed to ggmid() function.
#'
#' @examples
#' data(diamonds, package = "ggplot2")
#' diamonds <- diamonds[sample(nrow(diamonds), 1e4L), ]
#' mid <- interpret(price ~ (carat + cut + color + clarity) ^ 2, diamonds)
#' mid.plots(mid, c("carat", "color", "carat:color", "clarity:color"), NULL)
#' @export mid.plots
#'
mid.plots <- function(
    object, terms = mid.terms(object, interaction = FALSE),
    limits = c(NA, NA), add.intercept = FALSE, max.plots = NULL,
    engine = c("ggplot2", "base", "graphics"), ...) {
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
  if (!is.null(limits) && (is.na(limits[1L]) || is.na(limits[2L]))) {
    dfs <- c(object$main.effects, object$interactions)[true_terms]
    if (is.na(limits[1L])) {
      limits[1L] <-
        min(sapply(dfs, function(x) min(x$mid, na.rm = TRUE))) +
        ifelse(add.intercept, object$intercept, 0)
    }
    if (is.na(limits[2L])) {
      limits[2L] <-
        max(sapply(dfs, function(x) max(x$mid, na.rm = TRUE))) +
        ifelse(add.intercept, object$intercept, 0)
    }
  }
  if (engine == "ggplot2") {
    plots <- list()
    for (term in terms) {
      pl <- ggmid(object, term, limits = limits,
                  add.intercept = add.intercept, ...)
      plots[[term]] <- pl
    }
    return(plots)
  } else {
    for (term in terms)
      plot.mid(object, term, add.intercept = add.intercept, ...)
  }
}
