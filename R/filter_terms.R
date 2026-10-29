#' Extract and Filter Term Labels
#'
#' @description
#' \code{filter_terms()} extracts term labels from a fitted model object.
#' Its primary strength is the ability to filter terms based on their order or their associated underlying variables.
#'
#' @details
#' A "term" refers to an individual component in a formula, such as a main effect (e.g., \code{"Wind"}) or an interaction effect (e.g., \code{"Wind:Temp"}).
#' \code{filter_terms()} parses the model's terms and provides a way to select a subset of them, which is useful for plotting, summarizing, or other downstream analyses.
#'
#' @param object a \code{\link[stats]{terms.object}} or an object containing term labels.
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
#' filter_terms(mid, order = 1)
#'
#' # Get terms related to "Wind" or "Temp"
#' filter_terms(mid, require = c("Wind", "Temp"))
#'
#' # Get terms related to "Wind" or "Temp", but exclude any with "Day"
#' filter_terms(mid, require = c("Wind", "Temp"), remove = "Day")
#' @returns A character vector of the selected term labels.
#' @export filter_terms
#'
filter_terms <- function(object, ...)
UseMethod("filter_terms")

#' @rdname filter_terms
#' @exportS3Method midr::filter_terms
#'
filter_terms.default <- function(
    object, order = NULL, require = NULL, remove = NULL, ...
  ) {
  filt.order <- !is.null(order)
  filt.require <- !is.null(require)
  filt.remove <- !is.null(remove)
  terms <- try(stats::terms(object), silent = TRUE)
  if (inherits(terms, "terms")) {
    labs <- attr(terms, "term.labels")
    if (length(labs) == 0L)
      return(character(0L))
    if (filt.order)
      ords <- attr(terms, "order")
    if (filt.require || filt.remove) {
      facs <- attr(terms, "factors")
      facs <- facs[rowSums(facs) > 0L, , drop = FALSE]
      vars <- rownames(facs)
    }
  } else {
    labs <- tryCatch(
      attr(object, "term.labels") %||% object$term.labels,
      error = function(e) NULL
    )
    labs <- labs %||% object
    if (!is.character(labs))
      stop("'object' does not contain any term labels")
    if (length(labs) == 0L)
      return(character(0L))
    if (filt.order || filt.require || filt.remove)
      vlis <- lapply(labs, get.variables)
    if (filt.order)
      ords <- lengths(vlis, use.names = FALSE)
    if (filt.require || filt.remove) {
      vars <- unique(unlist(vlis))
      facs <- matrix(
        vapply(X = vlis, FUN = function(v) as.integer(vars %in% v),
               FUN.VALUE = integer(length(vars))),
        nrow = length(vars), ncol = length(labs), dimnames = list(vars, labs)
      )
    }
  }
  keep <- rep.int(TRUE, length(labs))
  if (filt.order)
    keep <- keep & (ords %in% order)
  if (filt.require) {
    reqs <- vars %in% require
    if (any(reqs)) {
      keep <- keep & (colSums(facs[reqs, , drop = FALSE]) > 0L)
    } else {
      keep <- rep(FALSE, length(labs))
    }
  }
  if (filt.remove) {
    rems <- vars %in% remove
    if (any(rems))
      keep <- keep & (colSums(facs[rems, , drop = FALSE]) == 0L)
  }
  labs[keep]
}

#' @rdname filter_terms
#' @exportS3Method midr::filter_terms
#'
filter_terms.midlist <- function(object, ...) {
  unique(unlist(lapply(X = object, FUN = filter_terms, ...), use.names = FALSE))
}

#' @exportS3Method base::labels
#'
labels.mid <- function(object, ...) filter_terms(object = object, ...)

#' @exportS3Method base::labels
#'
labels.midimp <- function(object, ...) filter_terms(object = object, ...)

#' @exportS3Method base::labels
#'
labels.midbrk <- function(object, ...) filter_terms(object = object, ...)

#' @exportS3Method base::labels
#'
labels.midcon <- function(object, ...) filter_terms(object = object, ...)
