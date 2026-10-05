#' Extract and Filter Term Labels
#'
#' @description
#' \code{term.labels()} extracts term labels from a fitted model object or a character vector.
#' Its primary strength is the ability to filter terms based on their order or their associated underlying variables.
#'
#' @details
#' A "term" refers to an individual component in a formula, such as a main effect (e.g., \code{"Wind"}) or an interaction effect (e.g., \code{"Wind:Temp"}).
#' This function safely parses the model's terms and provides a flexible way to select a subset of them, which is especially useful for plotting, summarizing, or other downstream analyses.
#'
#' @param object an object containing model terms (such as a "lm", "glm", or "mid" object) or term labels directly, a "terms" object, or a character vector of term labels.
#' @param order an integer vector specifying the order of terms to retain (e.g., \code{1} for only main effects, \code{2} for only two-way interactions).
#' @param require a character vector of variable names. If provided, only terms containing at least one of these variables are returned.
#' @param remove a character vector of variable names. If provided, terms containing any of these variables are completely excluded.
#' @param ... not used.
#'
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
#'
#' # Get only main effect terms
#' term.labels(mid, order = 1)
#'
#' # Get terms related to "Wind" or "Temp"
#' term.labels(mid, require = c("Wind", "Temp"))
#'
#' # Get terms related to "Wind" or "Temp", but exclude any with "Day"
#' term.labels(mid, require = c("Wind", "Temp"), remove = "Day")
#' @returns A character vector of the selected term labels.
#' @export term.labels
#'
term.labels <- function(object, ...)
UseMethod("term.labels")

#' @rdname term.labels
#' @exportS3Method midr::term.labels
#'
term.labels.default <- function(
    object, order = NULL, require = NULL, remove = NULL, ...
  ) {
  flt.order <- !is.null(order)
  flt.require <- !is.null(require)
  flt.remove <- !is.null(remove)
  terms <- try(stats::terms(object), silent = TRUE)
  if (inherits(terms, "terms")) {
    labs <- attr(terms, "term.labels")
    if (length(labs) == 0L) return(character(0L))
    if (flt.require || flt.remove) {
      facs <- attr(terms, "factors")
      facs <- facs[rowSums(facs) > 0L, , drop = FALSE]
      vars <- rownames(facs)
    }
    if (flt.order) ords <- attr(terms, "order")
  } else {
    labs <- attr(object, "term.labels") %||% object
    if (!is.character(labs)) stop("'object' does not contain any term labels")
    if (length(labs) == 0L) return(character(0L))
    if (flt.order || flt.require || flt.remove)
      vlis <- lapply(labs, get.variables)
    if (flt.require || flt.remove) {
      vars <- unique(unlist(vlis))
      facs <- matrix(
        vapply(X = vlis, FUN = function(v) as.integer(vars %in% v),
               FUN.VALUE = integer(length(vars))),
        nrow = length(vars), ncol = length(labs), dimnames = list(vars, labs)
      )
    }
    if (flt.order) ords <- lengths(vlis, use.names = FALSE)
  }
  keep <- rep(TRUE, length(labs))
  if (flt.order)
    keep <- keep & (ords %in% order)
  if (flt.require) {
    reqs <- vars %in% require
    if (any(reqs)) {
      keep <- keep & (colSums(facs[reqs, , drop = FALSE]) > 0L)
    } else {
      keep <- rep(FALSE, length(labs))
    }
  }
  if (flt.remove) {
    rems <- vars %in% remove
    if (any(rems))
      keep <- keep & (colSums(facs[rems, , drop = FALSE]) == 0L)
  }
  labs[keep]
}

#' @rdname term.labels
#' @exportS3Method midr::term.labels
#'
term.labels.midlist <- function(object, ...) {
  unique(unlist(lapply(X = object, FUN = term.labels, ...), use.names = FALSE))
}

#' @exportS3Method base::labels
#'
labels.mid <- function(object, ...) term.labels(object = object, ...)

#' @exportS3Method base::labels
#'
labels.midimp <- function(object, ...) term.labels(object = object, ...)

#' @exportS3Method base::labels
#'
labels.midbrk <- function(object, ...) term.labels(object = object, ...)

#' @exportS3Method base::labels
#'
labels.midcon <- function(object, ...) term.labels(object = object, ...)
