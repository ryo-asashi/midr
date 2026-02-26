#' Extract Terms from MID Models
#'
#' @description
#' \code{mid.terms()} extracts term labels from a fitted MID model or derivative objects.
#' Its primary strength is the ability to filter terms based on their type (main effects vs. interactions) or their associated variable names.
#'
#' @details
#' A "term" in a MID model refers to either a main effect (e.g., "Wind") or an interaction effect (e.g., "Wind:Temp").
#' This function provides a flexible way to select a subset of these terms, which is useful for plotting, summarizing, or other downstream analyses.
#'
#' @param object a "mid" object or another object that contains model terms. Can be a "mid.importance", "mid.conditional", or "mid.breakdown" object.
#' @param main.effects logical. If \code{FALSE}, the main effect terms are excluded.
#' @param interactions logical. If \code{FALSE}, the interactions terms are excluded.
#' @param require a character vector of variable names. Only terms related to at least one of these variables are returned.
#' @param remove a character vector of variable names. Terms related to any of these variables are excluded.
#' @param ... aliases are supported for convenience: "me" for \code{main.effects} and "ie" for \code{interactions}.
#'
#' @note
#' This function provides the common underlying logic for the \code{stats::terms()} S3 methods for "mid", "mid.importance", "mid.conditional", and "mid.breakdown" objects.
#'
#' @seealso \code{\link{interpret}}
#'
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
#'
#' # Get only main effect terms
#' mid.terms(mid, interactions = FALSE)
#'
#' # Get terms related to "Wind" or "Temp"
#' mid.terms(mid, require = c("Wind", "Temp"))
#'
#' # Get terms related to "Wind" or "Temp", but exclude any with "Day"
#' mid.terms(mid, require = c("Wind", "Temp"), remove = "Day")
#'
#' # Get the predicted contributions of only the terms associated with "Wind"
#' terms_wind <- mid.terms(mid, require = "Wind")
#' predict(mid, airquality[1:3,], terms = terms_wind, type = "terms")
#' @returns
#' \code{mid.terms()} returns a character vector of the selected term labels.
#' @export mid.terms
#'
mid.terms <- function(
    object, main.effects = TRUE, interactions = TRUE,
    require = NULL, remove = NULL, ...) {
  if (inherits(object, "midlist"))
    return(mid.terms(object[[1L]]))
  dots <- list(...)
  if (missing(main.effects) && !is.null(dots$me))
    main.effects <- dots$me
  if (missing(interactions) && !is.null(dots$ie))
    interactions <- dots$ie
  terms <- attr(object$terms %||% attr(object, "terms"), "term.labels")
  terms <- terms %||% attr(object, "term.labels")
  if (!main.effects)
    terms <- terms[grepl(":", terms)]
  if (!interactions)
    terms <- terms[!grepl(":", terms)]
  if (!is.null(require)) {
    tlist <- strsplit(terms, ":")
    terms <- terms[sapply(tlist, function(x) any(require %in% x))]
  }
  if (!is.null(remove)) {
    tlist <- strsplit(terms, ":")
    terms <- terms[!sapply(tlist, function(x) any(remove %in% x))]
  }
  terms
}


#' @exportS3Method base::labels
#'
labels.mid <- function(object, ...)
  mid.terms(object = object, ...)

#' @exportS3Method base::labels
#'
labels.midimp <- function(object, ...)
  mid.terms(object = object, ...)

#' @exportS3Method base::labels
#'
labels.midbrk <- function(object, ...)
  mid.terms(object = object, ...)

#' @exportS3Method base::labels
#'
labels.midcon <- function(object, ...)
  mid.terms(object = object, ...)
