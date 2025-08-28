#' Plot Multiple MID Component Functions
#'
#' @description
#' \code{mid.plots()} is a convenience function for applying \code{ggmid()} or \code{plot()} to multiple component functions of a "mid" object at once.
#' It can automatically determine common plotting scales and manage the layout.
#'
#' @param object a "mid" object.
#' @param terms a character vector of the terms to be visualized. By default, only the main effect terms are used.
#' @param limits a numeric vector of length two specifying the mid value limits. \code{NA} values are replaced by the minimum and/or maximum of the plotted MID values. If \code{intercept = TRUE} is set, the intercept is also included in the limit calculation.
#' @param intercept logical. If \code{TRUE}, the intercept is added to the MID values and the plotting scale is shifted accordingly.
#' @param main.effects logical. If \code{TRUE}, main effects are added to the interaction plots to show conditional effects. This argument disables automatic limit calculations.
#' @param max.nplots the maximum number of plots to generate.
#' @param engine the plotting engine to use, either "ggplot2" or "graphics".
#' @param ... optional parameters passed on to \code{plot.mid()} or \code{ggmid()}.
#'
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' idx <- sample(nrow(diamonds), 1e4L)
#' mid <- interpret(price ~ (carat + cut + color + clarity) ^ 2, diamonds[idx, ])
#'
#' # Plot selected main effects and interaction using the ggplot2 engine
#' mid.plots(mid, mid.terms(mid, require = "color", remove = "cut"), limits = NULL)
#' @returns
#' If \code{engine} is "ggplot2", \code{mid.plots()} returns a list of "ggplot" objects.
#' Otherwise (i.e., if \code{engine} is "graphics"), \code{mid.plots()} produces plots as side-effects and returns \code{NULL} invisibly.
#'
#' @seealso \code{\link{interpret}}, \code{\link{plot.mid}}, \code{\link{ggmid}}
#'
#' @export mid.plots
#'
mid.plots <- function(
    object, terms = mid.terms(object, interactions = FALSE),
    limits = c(NA, NA), intercept = FALSE, main.effects = FALSE,
    max.nplots = NULL, engine = c("ggplot2", "graphics"), ...) {
  engine <- match.arg(engine)
  if (length(terms) == 0L)
    return(NULL)
  if (!is.null(max.nplots) && length(terms) > max.nplots) {
    message("the number of terms exceeded the maximum number of plots")
    terms <- terms[1L:max.nplots]
  }
  true_terms <- terms
  for (i in seq_len(length(terms))) {
    true_terms[i] <- term.check(terms[i], object$terms, stop = FALSE)
  }
  terms <- terms[!is.na(true_terms)]
  true_terms <- true_terms[!is.na(true_terms)]
  if (main.effects) {
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
      plot.mid(object, term, limits = limits, intercept = intercept,
               main.effects = main.effects, ...)
  }
}
